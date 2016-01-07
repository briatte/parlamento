# note: the Senato server is (mis)configured in such a way that multiple queries
# will trigger random network errors; these errors can be avoided by running the
# scraper from behind a VPN

# CAMERA
bills = "data/bills-se.csv"
sponsors = "data/sponsors-se.csv"

if (!file.exists(sponsors) || !file.exists(bills)) {

  b = data_frame() # bills
  d = data_frame() # sponsors

  for (i in 17:13) { # no sponsorship pages before l. 13

    cat("Legislature", sprintf("%2.0f", i))

    f = paste0("raw_se/mp-lists/mps-", i, "-a.html")
    if (!file.exists(f)) {
      download.file(paste0("http://www.senato.it/leg/", sprintf("%02.0f", i),
                           "/BGT/Schede/Attsen/Sena.html"),
                    f, mode = "wb", quiet = TRUE)
    }

    s = read_html(f) %>%
      html_nodes(".linkSenatore a") %>%
      html_attr("href") # sponsor links

    a = read_html(f) %>%
      html_nodes(".divNavOrizS li a") %>%
      html_text %>%
      tolower # sponsor pages B ... Z

    for (j in a) {

      f = paste0("raw_se/mp-lists/mps-", i, "-", j, ".html")
      if (!file.exists(f)) {
        download.file(paste0("http://www.senato.it/leg/", sprintf("%02.0f", i),
                             "/BGT/Schede/Attsen/Sen", j, ".html"),
                      f, mode = "wb", quiet = TRUE)
      }

      h = read_html(f) %>%
        html_nodes(".linkSenatore a") %>%
        html_attr("href") # sponsor links

      s = c(s, h)

    }

    s = unique(s) %>% sample # sponsor links
    a = data_frame() # sponsorship variables

    cat(":", length(s), "senators\n")
    pb = txtProgressBar(max = length(s), style = 3)

    for (j in s) {

      f = paste0("raw_se/mp-pages/mp-", gsub("(.*)id=(\\d+)", "\\2", j), "-", i, ".html")
      if (!file.exists(f)) {
        download.file(paste0("http://www.senato.it", j), f, mode = "wb", quiet = TRUE)
      }

      h = read_html(f, encoding = "UTF-8")

      # skip parliamentary group pages
      if (grepl("Composizione", html_node(h, "title") %>% html_text)) {
        next
      }

      born = html_nodes(h, "p") %>% html_text %>% strsplit("\\n|\\t|\\r") %>% unlist
      constituency = born[ grepl("di elezione|Senatore(.*)a vita|Costituzione", born) ] %>%
        str_replace("(.*)di elezione:\\s+", "") %>%
        str_replace("Senatore(.*)a vita(.*)", "Senatore a vita")
      born = born[ grepl("^Nat(o|a)", born) ]

      photo = html_node(h, "img.foto") %>% html_attr("src")

      nyears = html_nodes(h, xpath = "//h2[text()='Mandati']/following-sibling::ul[1]/li/a")
      nyears = nyears[ grepl("Senato", html_text(nyears)) ] %>%
        html_attr("href") %>%
        str_replace("(.*)leg=(\\d+)", "\\2")

      party = html_nodes(h, xpath = "//a[contains(@href, 'tipodoc=sgrp')]/../..") %>%
        html_text %>%
        paste0(collapse = "") %>%
        strsplit("(G|g)ruppo") %>%
        unlist %>%
        str_clean %>% # next line makes the 'Misto' affiliations more precise
        str_replace("^Misto( :)? (.*)\\((.*),(.*)", "Misto - \\3 : \\2") %>%
        str_extract(paste("(.*): (Membro|Segretario|(Vicep|P)residente( Vicario)?)",
                          "dal( |l')\\d{1,2} \\w+ \\d{4}( al \\d{1,2} \\w+ \\d{4})?")) %>%
        na.omit

      if (!length(party)) {

        party = NA

      } else {

        # duration of party affiliations
        t = sapply(party, function(x) {
          x = str_extract_all(x, "\\d+ \\w+ \\d{4}") %>% unlist
          x = parse_date_time(x, orders = "%d %b %Y", locale = "it_IT")
          x = as.Date(x)
          if (length(x) == 1) x = c(x, as.Date(Sys.Date()))
          diff(x)
        })

        # privilege parties over mixed groups
        t[ grepl("^Misto", party) ] = t[ grepl("^Misto", party) ] - 1

        # select longest affiliation
        party = party[ t == max(t) ] %>% str_replace("(.*)\\s:(.*)", "\\1")
        stopifnot(length(party) == 1)

      }

      d = rbind(d, data_frame(
        legislature = i,
        uid = basename(f) %>% str_replace(".html", ""),
        name = html_node(h, "h1.titolo") %>% html_text,
        sex = ifelse(grepl("Nata", born), "F", "M"),
        born = str_extract(born, "\\d{4}") %>% as.integer,
        constituency,
        party,
        nyears = nyears[ as.integer(nyears) < i ] %>% paste0(collapse = ";"),
        committee = html_nodes(h, xpath = "//a[contains(@href, 'tipodoc=scom')]") %>%
          html_text %>%
          paste0(collapse = ";"),
        url = paste0("http://www.senato.it", j),
        photo
      ))

      h = html_nodes(h, xpath = "//a[contains(@href, 'tipo=iniziativa')]") %>%
        html_attr("href")

      if (length(h)) {

        f = gsub("-pages/", "-bills/", f)
        if (!file.exists(f)) {
          download.file(paste0("http://www.senato.it", h), f, mode = "wb", quiet = TRUE)
        }

        h = read_html(f)

        pf = html_nodes(h, xpath = "//h4[contains(text(), 'primo firmatario')]/following-sibling::ol/li")
        af = html_nodes(h, xpath = "//h4[contains(text(), 'cofirmatario')]/following-sibling::p/a")

        a = rbind(a, data_frame(
          legislature = i,
          uid = basename(f) %>% str_replace(".html", ""),
          role = c( rep("primo", length(pf)), rep("altro", length(af)) ),
          bill = c( html_nodes(pf, "a") %>% html_attr("href"),
                    html_attr(af, "href")),
          ref = c( html_nodes(pf, "a") %>% html_text,
                   html_text(af)),
          title = c( html_text(pf) %>% str_clean, rep(NA, length(af)) )
        ))

      }

      setTxtProgressBar(pb, which(s == j))

    }

    cat("\n")

    # remove bills with no titles: these are governmental bills, so there is no
    # first author to set the title; then aggregate sponsors over bills

    a = unique(a) %>%
      group_by(legislature, bill, ref) %>%
      mutate(title = ifelse(!length(title[ role == "primo" ]), NA_character_,
                            title[ role == "primo" ])) %>%
      filter(!is.na(title)) %>%
      summarise(title = unique(title),
                authors = uid[ role == "primo" ] %>% paste0(collapse = ";"),
                cosponsors = uid[ role == "altro" ] %>% paste0(collapse = ";"))

    stopifnot(!duplicated(a$bill))

    # some bills can have two authors, and government members can also be first
    # authors (some senators like Maurizio SACCONI have also served ministerial
    # mandates, and have sponsored bills as such)

    a$n_a = ifelse(a$authors %in% c("", NA), 0, 1 + str_count(a$authors, ";"))
    a$authors[ a$authors == "" ] = NA

    a$n_c = ifelse(a$cosponsors %in% c("", NA), 0, 1 + str_count(a$cosponsors, ";"))
    a$cosponsors[ a$cosponsors == "" ] = NA

    cat("Parsed", nrow(a), "bills,", sum(a$n_c > 1), "cosponsored, ")

    # filter out a few bills that started in the Camera
    x = grepl("^C\\.", a$title)
    cat("removed", sum(x), "non-private, ")

    # filter out a few bills with no first author
    y = (a$n_a == 0)
    cat(sum(y), "with no first author\n\n")

    b = rbind(b, a[ !x & !y, ])

  }

  write.csv(b, bills, row.names = FALSE)

  d$nyears = sapply(d$nyears, function(x) {
    x = strsplit(x, ";") %>% unlist
    x = sapply(yrs[ x ], function(x) {
      x = strsplit(x, "-") %>% unlist %>% as.integer
      paste0(seq(x[1], x[2]), collapse = ";")
    })
    paste0(x, collapse = ";")
  })

  # compute seniority as years from previous mandates (requires unique rows)
  for (i in 1:nrow(d)) {
    d$nyears[ i ] = sum(d$nyears[ i ] %>%
                          strsplit(";") %>%
                          unlist %>%
                          unique < substr(yrs[ d$legislature[ i] ], 1, 4))
  }

  d$nyears = as.integer(d$nyears)

  #=============================================================================
  # CHECK CONSTITUENCIES
  #=============================================================================

  # convert to WP-IT handles
  d$constituency = gsub(" - Collegio(.*)", "", d$constituency)
  d$constituency = gsub("\\s", "_", sapply(tolower(d$constituency), simpleCap))
  d$constituency[ grepl("(A|a)osta", d$constituency) ] = "Aosta"
  d$constituency[ grepl("Emilia(.*)omagna", d$constituency) ] = "Emilia-Romagna"
  d$constituency[ d$constituency == "Friuli-venezia_Giulia" ] = "Friuli-Venezia_Giulia"
  d$constituency[ d$constituency == "Trentino-alto_Adige" ] = "Trentino-Alto_Adige"

  # special cases: abroad constituencies (grouped as one entity) and lifelong terms
  d$constituency[ grepl("Asia|America|Europa", d$constituency) ] =
    "Anagrafe_degli_italiani_residenti_all'estero"
  d$constituency[ d$constituency == "Senatore_A_Vita" ] = "Senatore_a_vita"

  cat("Checking constituencies,", sum(is.na(d$constituency)), "missing...\n")
  for (i in na.omit(unique(d$constituency))) {

    g = GET(paste0("https://", meta[ "lang"], ".wikipedia.org/wiki/", i))

    if (status_code(g) != 200)
      cat("Missing Wikipedia entry:", i, "\n")

    g = read_html(g) %>% html_node("title") %>% html_text
    g = gsub("(.*) - Wikipedia(.*)", "\\1", g)

    if (gsub("\\s", "_", g) != i)
      cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")

  }

  # photos

  j = unique(d$photo) %>% na.omit %>% sample
  cat("Downloading", length(j), "photos\n")

  pb = txtProgressBar(max = length(j), style = 3)

  for (i in j) {

    f = str_replace(i, "/leg/(\\d+)/(.*)/(\\d+)(.*)", "photos_se/\\1-\\3\\4")
    if (!file.exists(f)) {
      try(download.file(paste0("http://www.senato.it", i), f, mode = "wb", quiet = TRUE))
    }
    if (file.exists(f) && !file.info(f)$size) {
      file.remove(f)
    }
    d$photo[ d$photo == i ] = ifelse(file.exists(f), f, NA)

    setTxtProgressBar(pb, which(i == j))

  }

  cat("\n")

  # final check: all names are unique per legislature
  stopifnot(!duplicated(paste(s$legislature, s$name)))
  
  write.csv(d, sponsors, row.names = FALSE)

}

# postprocess sponsors

s = read.csv(sponsors, stringsAsFactors = FALSE) %>%
  unique %>%
  rename(id = uid)

# duplicate rows caused by parser error
s = s[ !(s$name == "Carla ROCCHI" & s$born != 1942), ]

s$party[ grepl("^Alleanza Nazionale", s$party) ] = "AN" #
s$party[ grepl("ApI-FLI", s$party) ] = "FLI-TP" # S. 16
s$party[ grepl("Forza Italia|Popolo della Libertà", s$party) ] = "FI-PDL" # S.13-17
s$party[ grepl("^Grandi Autonomie e Libertà", s$party) ] = "GAL" # S.17
s$party[ grepl("^Italia dei Valori", s$party) ] = "IDV" # S.16
s$party[ grepl("^Lega$|^Lega (Forza Padania|Nord)", s$party) ] = "LN" # S.13-17
s$party[ grepl("^Margherita", s$party) ] = "MARGH" # S.14
s$party[ grepl("^Movimento 5 Stelle", s$party) ] = "M5S" #
s$party[ grepl("NCD-UDC", s$party) ] = "NCD-UDC" # S.17 (n = 1)
s$party[ grepl("^Nuovo Centrodestra", s$party) ] = "NCD" # S.17
s$party[ grepl("^Rifondazione Comunista", s$party) ] = "PRC" # S. 13, 15
s$party[ grepl("^Rinnov(\\.|amento)", s$party) ] = "RINNOV" # S.13
s$party[ grepl("^Scelta Civica", s$party) ] = "SC" # S.17
s$party[ grepl("^Unione dei Democraticicristiani e di Centro", s$party) ] = "UDC" # S.15
s$party[ grepl("Unione Verdi - Comunisti Italiani$", s$party) ] = "VERD-PDCI" # S.15
s$party[ grepl("^Verdi", s$party) ] = "VERD" # S.13-14

# Regionalists (MPA + allies: DC, PRI in l. 15, UDC, UDC in l. 16, SVP, PSI in l. 14, 15, 17)
s$party[ grepl("per l'Autonomia$", s$party) ] = "MPA" # S.15 (incl. some DC + PRI)
s$party[ grepl("^Per le Autonomie", s$party) ] = "MPA" # S.14-15, 17 (incl. some SVP + PSI + MAIE)
s$party[ grepl("^UDC,\\s?SVP(.*)Autonomie$|Unione di Centro, SVP e Autonomie", s$party) ] = "MPA" # S.16

# Christian Democrats (includes two UDC from S.14 to match Camera)
s$party[ grepl("^CCD-CDU|Unione Democristiana e di Centro", s$party) ] = "CCD-CDU" # S. 14
s$party[ grepl("^(Centro Cristiano Democratico|(CCD-)?Cristiani Dem)|- CCD$", s$party) ] = "CCD" # S.13
s$party[ grepl("- CDU$", s$party) ] = "CDU" # S.13
s$party[ grepl("^Partito Popolare Italiano$", s$party) ] = "PPI" # S.13

# Left (all of them match Camera recodings)
s$party[ grepl("^(Sinistra Democratica|Democratici di Sinistra) - l'Ulivo", s$party) ] = "ULIVO" # S.13-14
s$party[ grepl("^L'Ulivo$|Democratico-L'Ulivo$", s$party) & s$legislature == 15 ] = "PD" # S.15
s$party[ grepl("^Partito Democratico$", s$party) ] = "PD" # S.16-17

# Minor groups (n = 1, n = 2 for UDR)
s$party[ grepl("^Alleanza Liberalpopolare", s$party) ] = "IND" # S.17, splinter from FI
s$party[ grepl("^Coesione Nazionale", s$party) ] = "IND" # S.16
s$party[ grepl("^Democrazia Europea", s$party) ] = "IND" # S.13; ParlGov = 6.2 (488)
s$party[ grepl("^Unione Democratica per la Repubblica|per l'UDR", s$party) ] = "IND" # S.13, ParlGov = 6.2 (1145)

# Missing values (Berlusconi minister with several invalidated mandates)
s$party[ s$name == "Giancarlo GALAN" & s$legislature == 16 ] = "FI-PDL"

# Mixed: includes 'Movimento X', n = 3 in S.17, and 'Italia Lavori in Corso',
# n = 6 in l. 17, both of which more or less merged (some members are ex-M5S)
# the only other group at n > 3 is SEL (n = 7, S.17, largest 'Misto' party),
# and the only other known party within 'Misto; is IDV at n = 2 in l. 15
s$party[ grepl("^Misto - Sinistra Ecologia e Libertà", s$party) ] = "SEL"
s$party[ grepl("^Misto($| -)", s$party) ] = "IND" # includes some minor parties

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

s_se = s

# postprocess bills

a = read.csv(bills, stringsAsFactors = FALSE)

a$n_a = a$n_a + a$n_c
a$sponsors = a$authors
a$sponsors[ !is.na(a$cosponsors) ] = paste0(
  a$sponsors[ !is.na(a$cosponsors) ], ";",
  a$cosponsors[ !is.na(a$cosponsors) ]
)

a_se = select(a, -bill)
