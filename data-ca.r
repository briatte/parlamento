bills = "data/bills-ca.csv"
sponsors = "data/sponsors-ca.csv"
start = 9 # first legislature to examine

#
# parse sponsors, using SPARQL to get the full listings of each legislature
#
if (!file.exists(sponsors)) {

  s = data_frame()

  # find sponsors
  q = "SELECT DISTINCT
  ?url ?name ?surname ?born ?sex ?constituency
  ?party ?start ?end ?committee ?photo
  WHERE {
  ?url ocd:rif_mandatoCamera ?mandato; a foaf:Person.
  ?d a ocd:deputato; ocd:aderisce ?aderisce;
  ocd:rif_leg <http://dati.camera.it/ocd/legislatura.rdf/repubblica_00>;
  ocd:rif_mandatoCamera ?mandato.
  ?d foaf:firstName ?name; foaf:surname ?surname.
  OPTIONAL { ?d foaf:gender ?sex. }
  OPTIONAL { ?d foaf:depiction ?photo. }
  OPTIONAL {
  ?url <http://purl.org/vocab/bio/0.1/Birth> ?nascita.
  ?nascita <http://purl.org/vocab/bio/0.1/date> ?born.
  }
  OPTIONAL {
  ?mandato ocd:rif_elezione ?elezione.
  ?elezione dc:coverage ?constituency.
  }
  OPTIONAL {
  ?aderisce ocd:startDate ?start.
  }
  OPTIONAL {
  ?aderisce ocd:endDate ?end.
  }
  OPTIONAL {
  ?aderisce ocd:rif_gruppoParlamentare ?gruppo.
  ?gruppo dc:title ?party.
  }
  OPTIONAL {
  ?d ocd:membro ?membro.?membro ocd:rif_organo ?organo.
  ?organo dc:title ?committee.
  }
  }"

  for (i in 1:17) { # full listing for exact seniority

    cat("Legislature", sprintf("%2.0f", i))

    f = paste0("raw_ca/mp-lists/mps-", i, ".xml")
    if (!file.exists(f)) {

      r = gsub("_00", paste0("_", sprintf("%02.0f", i)), q) # legislature

      h = GET("http://dati.camera.it/sparql",
              query = list(query = r, debug = "on", format = "text%2Fplain"))
      writeLines(content(h, "text"), f)

    }
    h = read_html(f)

    # extract variables
    x = html_nodes(h, xpath = "//result") %>%
      lapply(html_nodes, xpath = "binding") %>%
      lapply(function(x) {
        y = t(html_text(x))
        y = data.frame(y, stringsAsFactors = FALSE)
        names(y) = html_attr(x, "name")
        y
      }) %>% bind_rows # handles missing values

    # collapse committees
    x = group_by(x, url) %>%
      mutate(committee = ifelse(all(is.na(committee)), NA_character_,
                                na.omit(committee) %>%
                                  unique %>%
                                  paste0(collapse = ";"))) %>%
      unique

    # keep only longest party affiliation
    x$end = ifelse(is.na(x$end), gsub("-", "", Sys.Date()), x$end)
    x$t = strptime(x$end, "%Y%m%d") - strptime(x$start, "%Y%m%d")
    x = group_by(x, url) %>%
      filter(t == max(t)) %>%
      select(-start, -end, -t)

    # unique identifier
    x$uid = paste0(gsub("persona.rdf/p", "deputato.rdf/d", x$url), "_", i)

    cat(":", n_distinct(x$url), "unique sponsors,",
        "removed", sum(duplicated(x$url)), "duplicate row(s), ")

    # keep first (constituency) mandate when there are still multiple ones
    x = x[ !duplicated(x$url), ]

    # full name, handling duplicates
    x = mutate(x, name = paste(name, surname)) %>%
      group_by(name) %>%
      mutate(n = n(), o = 1:n()) %>%
      group_by() %>%
      mutate(name = ifelse(n > 1, paste0(name, "-", o), name)) %>%
      select(-surname, -n, -o)

    cat("changed", sum(grepl("\\d", x$name)), "duplicate name(s)\n")
    stopifnot(!duplicated(x$name))

    # filter(x, url %in% url[ duplicated(url) ]) %>% View
    s = rbind(s, cbind(legislature = i, x, stringsAsFactors = FALSE))

  }

  s = group_by(s, url) %>%
    mutate(nyears = paste0(sort(unique(legislature)), collapse = ";"))

  s$nyears = sapply(s$nyears, function(x) {
    x = strsplit(x, ";") %>% unlist
    x = sapply(yrs[ x ], function(x) {
      x = strsplit(x, "-") %>% unlist %>% as.integer
      paste0(seq(x[1], x[2]), collapse = ";")
    })
    paste0(x, collapse = ";")
  })

  # compute seniority as years from previous mandates (requires unique rows)
  for (i in 1:nrow(s)) {
    s$nyears[ i ] = sum(s$nyears[ i ] %>%
                          strsplit(";") %>%
                          unlist %>%
                          unique < substr(yrs[ s$legislature[ i] ], 1, 4))
  }

  s$nyears = as.integer(s$nyears)
  s$url = s$uid
  s$born = substr(s$born, 1, 4) %>% as.integer
  s$sex = ifelse(s$sex == "female", "F", "M")
  s$party = str_replace(s$party, "\\s+\\((.*)", "")

  # ============================================================================
  # CHECK CONSTITUENCIES
  # ============================================================================

  # convert to WP-IT handles (grouping multi-seat regions into single entities)
  s$constituency = gsub("\\s?\\d", "", s$constituency)
  s$constituency = gsub("\\s", "_", sapply(tolower(s$constituency), simpleCap))
  s$constituency[ s$constituency == "Abruzzi" ] = "Abruzzo"
  s$constituency[ grepl("(A|a)osta", s$constituency) ] = "Aosta"
  s$constituency[ grepl("Emilia(.*)omagna", s$constituency) ] = "Emilia-Romagna"
  s$constituency[ s$constituency == "Friuli-venezia_Giulia" ] = "Friuli-Venezia_Giulia"
  s$constituency[ s$constituency == "Trentino-alto_Adige" ] = "Trentino-Alto_Adige"

  # special cases: abroad constituencies (grouped as one entity) and PR college
  s$constituency[ grepl("Asia|America|Europa", s$constituency) ] =
    "Anagrafe_degli_italiani_residenti_all'estero"
  s$constituency[ s$constituency == "Cun" ] = "Collegio_unico"

  # missing values
  s$constituency[ s$legislature == 7 & s$name == "VITTORIO FOA" ] = "Torino"

  cat("Checking constituencies,", sum(is.na(s$constituency)), "missing...\n")
  for (i in na.omit(unique(s$constituency))) {

    g = GET(paste0("https://", meta[ "lang"], ".wikipedia.org/wiki/", i))

    if (status_code(g) != 200)
      cat("Missing Wikipedia entry:", i, "\n")

    g = read_html(g) %>% html_node("title") %>% html_text
    g = gsub("(.*) - Wikipedia(.*)", "\\1", g)

    if (gsub("\\s", "_", g) != i)
      cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")

  }

  # photos (remove condition to get those before l. 7)

  j = unique(s$photo[ s$legislature >= start ]) %>% na.omit %>% sample
  cat("Downloading", length(j), "photos\n")

  pb = txtProgressBar(max = length(j), style = 3)

  for (i in j) {

    f = str_replace(i, "(.*)?id=(\\d+)&leg(.*?)(\\d+)", "photos_ca/\\4-\\2.jpg")
    if (!file.exists(f)) {
      try(download.file(i, f, mode = "wb", quiet = TRUE))
    }
    if (file.exists(f) && !file.info(f)$size) {
      file.remove(f)
    }
    s$photo[ s$photo == i ] = ifelse(file.exists(f), f, NA)

    setTxtProgressBar(pb, which(i == j))

  }

  cat("\n")

  write.csv(select(s, -uid) %>% arrange(url, legislature), sponsors, row.names = FALSE)

}

# postprocess sponsors

s = read.csv(sponsors, stringsAsFactors = FALSE) %>%
  filter(legislature >= start)

# Christian Democrats:
# DC -> PPI (l. 12, 1994), split -> CCD (1994)
# ... no overlap: DC (7-10), PPI (11-12)
s$party[ grepl("DEMOCRAZIA CRISTIANA|DEMOCRATICO CRISTIANO$", s$party) ] = "DC" # C.7-10
s$party[ grepl("CENTRO CRISTIANO DEMOCRATICO", s$party) & s$legislature > 11 ] = "CCD" # C.12-13, leaves 1 in l. 11
s$party[ grepl("CENTRO CRISTIANO DEMOCRATICO|PARTITO POPOLARE ITALIANO", s$party) ] = "PPI" # C.11-12

# CCD: alone first (l. 12-13), then CCD-CDU (l. 14), then UDC (l. 15)
# ... no overlap: CCD, CCD-CDU, UDC, UDC-TP, NDC-UDC
s$party[ grepl("^UDC UNIONE DEI DEMOCRATICI CRISTIANI E DEI DEMOCRATICI DI CENTRO", s$party) ] = "CCD-CDU" # C.14
# Third Pole, C.16: coded as parties
s$party[ grepl("^FUTURO E LIBERTA' PER IL TERZO POLO", s$party) ] = "FLI-TP" # C.16
s$party[ grepl("^UNIONE DI CENTRO PER IL TERZO POLO", s$party) ] = "UDC-TP" # C.16
s$party[ grepl("AREA POPOLARE", s$party) ] = "NCD-UDC" # C.17

s$party[ grepl("ALLEANZA NAZIONALE", s$party) ] = "AN" # C.12-15
s$party[ grepl("COSTITUENTE DI DESTRA - DEMOCRAZIA NAZIONALE", s$party) ] = "DN" # C. 7, MSI spin-off
s$party[ grepl("FORZA ITALIA|POPOLO DELLA LIBERTA", s$party) ] = "FI-PDL" # C.12-17
s$party[ grepl("FEDERALISTI E LIBERALDEMOCRATICI", s$party) ] = "FLD" # C.12 (mostly splinters from LN)
s$party[ grepl("^I DEMOCRATICI", s$party) ] = "ID" # C.12
s$party[ grepl("ITALIA DEI VALORI", s$party) ] = "IDV" # C.15-16
s$party[ grepl("LEGA NORD", s$party) ] = "LN" # C.11-17
s$party[ grepl("MOVIMENTO 5 STELLE", s$party) ] = "M5S" # C.17
s$party[ grepl("MOVIMENTO PER LA DEMOCRAZIA: LA RETE", s$party) ] = "RETE" # C.11
s$party[ grepl("MOVIMENTO SOCIALE ITALIANO|^MSI-DESTRA", s$party) ] = "MSI-DN" # C.7-11
s$party[ grepl("^(PARTITO )?LIBERALE( ITALIANO)?$", s$party) ] = "PLI" # C.1-11
s$party[ grepl("PARTITO RADICALE|FEDERALISTA EUROPEO", s$party) ] = "RAD" # C. 7-11
s$party[ grepl("POPOLO E TERRITORIO", s$party) ] = "PT" # C.16
s$party[ grepl("PER L'ITALIA - CENTRO DEMOCRATICO", s$party) ] = "CD" # C.17
s$party[ grepl("^(PARTITO )?REPUBBLICANO( ITALIANO)?$", s$party) ] = "PRI" # C. 1, 4-11
s$party[ grepl("RINNOVAMENTO ITALIANO", s$party) ] = "RINNOV" # C.13
s$party[ grepl("SCELTA CIVICA", s$party) ] = "SC" # C.17
s$party[ grepl("^SINISTRA INDIPENDENTE", s$party) ] = "SIN" # C. 9-10
s$party[ grepl("SOCIALISTI E RADICALI-RNP", s$party) ] = "RNP" # C.15
s$party[ grepl("UNIONE DEMOCRATICA PER L'EUROPA|POPOLARI-UDEUR", s$party) ] = "UDEUR" # C.13, 15
s$party[ grepl("UNITA' SOCIALISTA", s$party) ] = "US" # C.1 (n = 3, split from PSI)

# Monarchists
s$party[ grepl("PARTITO NAZIONALE MONARCHICO", s$party) ] = "PNM" # C. 1-3
s$party[ grepl("PARTITO MONARCHICO POPOLARE", s$party) ] = "PMP"  # C. 2-3 (n = 1 in each)
s$party[ grepl("^PARTITO DEMOCRATICO ITALIANO$|UNITA' MONARCHICA", s$party) ] = "PDIUM" # C. 3-5

# Communists
# ... no overlap: PDUP-DP, PDUP, DP
s$party[ grepl("PARTITO DI UNITA' PROLETARIA PER IL COMUNISMO-DEMOCRAZIA PROLETARIA", s$party) ] = "PDUP-DP" # C.7
s$party[ grepl("PARTITO DI UNITA' PROLETARIA PER IL COMUNISMO", s$party) ] = "PDUP" # C.8
s$party[ grepl("^DEMOCRAZIA PROLETARIA|DP-COMUNISTI", s$party) ] = "DP" # C.9-10

# Greens
# ... no overlap
s$party[ grepl("^VERD(E|I)$", s$party) ] = "VERD" # C.10, 11, 15
s$party[ grepl("^SINISTRA ECOLOGIA LIBERTA", s$party) ] = "SEL" # C.17

# PSI and PDSI, C.7-11
s$party[ grepl("PARTITO SOCIALISTA DEMOCRATICO ITALIANO", s$party) ] = "PSDI" # C.7-11
s$party[ grepl("PARTITO SOCIALISTA ITALIANO", s$party) ] = "PSI" # C.7-11

# Left: PCI (historical, 7-9) -> PDS (SINDEM, 10-11), split -> PRC (11-15, 1991)
# SINDEM joins Alleanza dei Progressisti (l. 12, 1994), then ULIVO (l. 13, 1996)
# SINDEM merges with minor leftwings in 1998, so does not appear after l. 13
# PD takes over from 2006 onwards (15-17)
# ... no overlap: PCI, SINDEM, AP, ULIV, PD
s$party[ grepl("^COMUNISTA$", s$party) & s$legislature < 5 ] = "PCI" # C.1-4
s$party[ grepl("PARTITO COMUNISTA ITALIANO", s$party) ] = "PCI" # C.7-9
s$party[ grepl("GRUPPO COMUNISTA - PDS|DEMOCRATICO DELLA SINISTRA", s$party) ] = "DEMSIN" # C.10-11
s$party[ grepl("PROGRESSISTI - FEDERATIVO", s$party) ] = "AP" # C.12
s$party[ grepl("^DEMOCRATICI (DI SINISTRA)?( )?-( )?L'ULIVO$", s$party) ] = "ULIVO" # C.13-14
s$party[ grepl("PARTITO DEMOCRATICO(-L'ULIVO)?$", s$party) ] = "PD" # C.15-17

# Other Ulivists
s$party[ grepl("POPOLARI DEMOCRATICI - L'ULIVO", s$party) ] = "PPP" # C.13, Pop. per Prodi
s$party[ grepl("MARGHERITA, DL-L'ULIVO", s$party) ] = "MARGH" # C.14

# Communist splinters, more leftwing
s$party[ grepl("RIFONDAZIONE COMUNISTA|^COMUNISTA$", s$party) ] = "PRC" # C.11-15
s$party[ grepl("COMUNISTI ITALIANI", s$party) ] = "PDCI" # C. 15

s$party[ grepl("UNIONE DEMOCRATICA PER LA REPUBBLICA", s$party) ] = "IND" # n = 1
s$party[ s$party == "MISTO" ] = "IND"

#===============================================================================
# QUALITY CONTROL
#===============================================================================

# - might be missing: born (int of length 4), constituency (chr),
#   photo (chr, folder/file.ext)
# - never missing: sex (chr, F/M), nyears (int), url (chr, URL),
#   party (chr, mapped to colors)

cat("Missing", sum(is.na(s$born)), "years of birth\n")
stopifnot(is.integer(s$born) & nchar(s$born) == 4 | is.na(s$born))

cat("Missing", sum(is.na(s$constituency)), "constituencies\n")
stopifnot(is.character(s$constituency))

cat("Missing", sum(is.na(s$photo)), "photos\n")
stopifnot(is.character(s$photo) & grepl("^photos(_\\w{2})?/(.*)\\.\\w{3}", s$photo) | is.na(s$photo))

stopifnot(!is.na(s$sex) & s$sex %in% c("F", "M"))
stopifnot(!is.na(s$nyears) & is.integer(s$nyears))
stopifnot(!is.na(s$url) & grepl("^http(s)?://(.*)", s$url))
stopifnot(s$party %in% names(colors))

#
# parse bills, using SPARQL to get each MP's sponsorships (slow technique)
#
# note: RDF dumps of all Camera bills are available at [1], but the data dump
# for l. 15 is damaged, and parsing RDF as if it were XML is too error-prone to
# get correct sponsorship information out of the files; make 5,000+ queries and
# saving as many files is far from efficient, but it works if the server is not
# sent too many queries too quickly (hence sluggishness).
#
# [1]: http://dati.camera.it/it/dati/iter-atti-camera.html
#
if (!file.exists(bills)) {

  a = data_frame()

  # find sponsorships
  q = "SELECT DISTINCT
  ?role ?ref ?date ?title
  WHERE {
  {
  ?atto ?ruolo ?deputato;
  dc:date ?date;
  dc:identifier ?ref;
  dc:title ?title;
  dc:type ?tipo.
  FILTER(?ruolo = ocd:primo_firmatario)
  }
  UNION {
  ?atto ?ruolo ?deputato;
  dc:date ?date;
  dc:identifier ?ref;
  dc:title ?title;
  dc:type ?tipo.
  FILTER(?ruolo = ocd:altro_firmatario)
  }
  ## filter bills
  FILTER(?tipo = 'Progetto di Legge')
  ?ruolo rdfs:label ?role.
  ## filter sponsor
  ?deputato ocd:rif_leg <http://dati.camera.it/ocd/legislatura.rdf/repubblica_00>
  FILTER(REGEX(?deputato,'uid','i'))
  }"

  for (i in start:17) {

    cat("Legislature", sprintf("%2.0f", i))

    x = s$url[ s$legislature == i ] %>% unique %>% sample
    cat(": getting bills for", length(x), "sponsors\n")

    b = data_frame()
    pb = txtProgressBar(max = length(x), style = 3)

    for (j in x) {

      f = paste0("raw_ca/mp-bills/bills-", basename(j), ".xml")
      if (!file.exists(f)) {

        r = gsub("_00", paste0("_", sprintf("%02.0f", i)), q) # legislature
        r = gsub("uid", basename(j), r) # MP

        h = try(GET("http://dati.camera.it/sparql",
                    query = list(query = r, debug = "on", format = "text%2Fplain")),
                silent = TRUE)

        if (!"try-error" %in% class(h)) {
          writeLines(content(h, "text"), f)
        }
        # Sys.sleep(0.25) # avoid choking the server

      }

      # deal with server choking (Error 500); valid empty files are 370 bytes
      if (file.exists(f) && file.info(f)$size < 100) {
        #file.remove(f)
      }

      if (!file.exists(f)) {
        next
      }

      h = read_html(f, encoding = "UTF-8")

      # attribute names
      y = html_nodes(h, xpath = "//binding") %>% html_attr("name") %>% unique

      if (length(y)) { # empty when MP has not sponsored any bills

        # get data frame
        z = lapply(y, function(x) {
          html_nodes(h, xpath = paste0("//binding[@name='", x, "']")) %>%
            html_text %>%
            data_frame
        }) %>% bind_cols
        names(z) = y

        b = rbind(b, cbind(uid = j, z, stringsAsFactors = FALSE))

      }

      setTxtProgressBar(pb, which(x == j))

    }

    cat("\n")

    b = group_by(b, ref, date, title) %>%
      summarise(authors = uid[ role == "primo firmatario" ] %>%
                  basename %>%
                  unique %>%
                  paste0(collapse = ";"),
                cosponsors = uid[ role == "altro firmatario" ] %>%
                  basename %>%
                  unique %>%
                  paste0(collapse = ";"))

    b$n_a = ifelse(b$authors %in% c("", NA), 0, 1 + str_count(b$authors, ";"))
    b$authors[ b$authors == "" ] = NA

    b$n_c = ifelse(b$cosponsors %in% c("", NA), 0, 1 + str_count(b$cosponsors, ";"))
    b$cosponsors[ b$cosponsors == "" ] = NA

    cat("Parsed", nrow(b), "bills,", sum(b$n_c > 1), "cosponsored, ")

    # filter out a few bills that started in the Senate or through mass petitions
    x = grepl("S\\.|INIZIATIVA\\sPOPOLARE", b$title)
    cat("removed", sum(x), "non-private, ")

    # filter out a few bills with no first author
    y = (b$n_a == 0)
    cat(sum(y), "with no first author\n\n")

    a = rbind(a, cbind(legislature = i, b[ !x & !y, ], stringsAsFactors = FALSE))

  }

  write.csv(a, bills, row.names = FALSE)

}

# postprocess bills

a = read.csv(bills, stringsAsFactors = FALSE) %>%
  filter(legislature >= start)

a$n_a = a$n_a + a$n_c
a$sponsors = a$authors
a$sponsors[ !is.na(a$cosponsors) ] = paste0(
  a$sponsors[ !is.na(a$cosponsors) ], ";",
  a$cosponsors[ !is.na(a$cosponsors) ]
)

# last, set sponsor identifiers
s$id = basename(s$url)
