# ==============================================================================
# GET MP LISTINGS, LEGISLATURES 13-15 (for party affiliations)
# ==============================================================================
  
# start with MP who does not show up in the listings
q = data_frame()

for (leg in paste0("http://leg", c("13", "xiv", "xv"), ".camera.it")) {
  
  for (a in rev(LETTERS)) {
    
    # cat("Legislature", str_pad(str_extract(leg, "13|xiv|xv"), 4, "right"), "letter", a)
    f = paste0("raw/mp-lists/", gsub("http://(.*)\\.camera\\.it", "\\1", leg), "_", a, ".html")
    
    if (!file.exists(f)) {
      
      h = paste0(leg, "/deputatism/240/documentoxml.asp?")
      h = paste0(h, ifelse(grepl("xv", leg), "sezione=&Lettera=", "Let="), a)
      h = GET(h)
      writeLines(content(h, "text"), f)
      
    }
    f = htmlParse(f)
    
    t = try(readHTMLTable(f, which = 1, stringsAsFactors = FALSE), silent = TRUE)
    if ("try-error" %in% class(t) | is.null(t)) {
      
      # cat(": empty\n")
      
    } else {
      
      t = data_frame(
        name = str_clean(xpathSApply(f, "//table//tr/td[1]", xmlValue)), # name
        party = str_clean(xpathSApply(f, "//table//tr/td[2]", xmlValue)) # party
      )
      
      # fix empty row on legislature 13 letter M
      t = subset(t, name != "MANISCO Lucio (non in carica)")
      
      # add URL
      t$url_chamber = paste0(leg, xpathSApply(f, "//table//tr/td[1]/a/@href"))
      
      # cat(":", sprintf("%3.0f", nrow(t)), "MPs\n")
      q = rbind(q, t)
      
    }
    
  }
  
}

# ============================================================================
# PARSE MP PAGES
# ============================================================================

s = data_frame()

j = list.files("raw/mp-pages", pattern = "dep-1[345]-d?\\d", full.names = TRUE)
j = j[ j != "raw/mp-pages/dep-15-50433.html" ] # completely empty page, no name
cat("Parsing", length(j), "MPs...\n")

for (i in rev(j)) {
  
  # cat(str_pad(which(j == i), 4), i)
  l = gsub("(.*)dep-(\\d+)-(.*)", "\\2", i) %>% as.integer
  x = htmlParse(i)
  
  name = xpathSApply(x, "//div[@id='innerContentColumn']//strong[1]", xmlValue)
  photo = xpathSApply(x, "//img[contains(@src, '.jpg')]/@src")
  
  if (l == 15) {
    
    born = xpathSApply(x, "//div[@id='innerContentColumn']//p[2]", xmlValue)
    
    constituency = xpathSApply(x, "//div[@id='innerContentColumn']", xmlValue)
    constituency = gsub("(.*) circoscrizione [A-Z]+ \\((.*)", "\\2", constituency)
    constituency = gsub("(\\w+)\\)(.*)", "\\1", constituency)
    
    nyears = NA
    
  }
  
  if (l == 14) {
    
    born = xpathSApply(x, "//div[@id='schedaDepDatiPers']", xmlValue) # %>% str_clean
    
    constituency = xpathSApply(x, "//div[@id='schedaDepDatiPers']", xmlValue)
    constituency = unlist(strsplit(constituency, "\\s{2,}"))
    constituency = constituency[ grepl("circoscrizione", constituency) ]
    
    nyears = gsub("(.*) nell(a|e) legislatur(a|e) (.*)Iscritto (.*)", "\\4", born) # %>% str_clean
    
  }
  
  if (l == 13) {
    
    born = xpathSApply(x, "//div[@id='innerContentColumn']//div[2]", xmlValue)
    
    constituency = unlist(strsplit(born, "\\s{2,}"))
    constituency = constituency[ grepl("circoscrizione", constituency) ]
    
    nyears = gsub("(.*) nell(a|e) legislatur(a|e) (.*)", "\\4", born) # %>% str_clean
    
  }
  
  s = rbind(s, data_frame(
    legislature = l, url = i,
    name, born, sex = NA, nyears,
    constituency = ifelse(!length(constituency), NA, constituency),
    photo = ifelse(is.null(photo), NA, photo)
  ))
  
  # cat(":", name, "\n")
  
}

# ============================================================================
# GET PHOTOS
# ============================================================================

k = paste0(as.integer(!is.na(s$photo)), s$legislature)
s$photo[ k == 113 ] = paste0("13.camera.it/cartellecomuni/leg13/Deputati/scheda_deputato/",
                             s$photo[ k == 113 ])
s$photo[ k == 114 ] = paste0("xiv.camera.it", s$photo[ k == 114 ])
s$photo[ k == 115 ] = paste0("xv.camera.it", s$photo[ k == 115 ])
s$photo[ k > 100 ] = paste0("http://leg", s$photo[ k > 100 ])

stopifnot(str_count(s$photo, "http") == 1 | is.na(s$photo))
stopifnot(grepl("^http", s$photo) | is.na(s$photo))

k = which(!is.na(s$photo))
cat("Getting", length(k), "photos...\n")

for (i in rev(k)) {
  
  j = gsub("raw/mp-pages/dep-(\\d+)-d?(\\d+)\\.html", "photos_ca/\\1_\\2.jpg",
           s$url[ i ])
  
  if (!file.exists(j)) {
    
    h = try(GET(s$photo[ i ]), silent = TRUE)
    
    if (!"try-error" %in% class(h) && status_code(h) == 200)
      writeBin(content(h, "raw"), j)
    
  }
  
  if (file.exists(j)) {
    
    s$photo[ i ] = j
    
  } else {
    
    cat("Photo", s$photo[ i ], "failed\n")
    s$photo[ i ] = NA
    
  }
  
}

# ============================================================================
# FINALIZE VARIABLES
# ============================================================================

# name
s$name = str_clean(s$name)
s$name = gsub("(.*)\\s\\((.*)", "\\1", s$name)
s$name = gsub("Ã¬", "ì", s$name) # Alì Khalil detto Alì Rashid
s$name = gsub("d'\\s", "D'", s$name) # d' ALCONTRES, d' IPPOLITO

# reorder first and family names
s$name = sapply(s$name, function(x) {
  x = strsplit(x, "\\s") %>% unlist
  y = grepl("[a-z]", x)
  paste0(c(x[ y ], x[ !y ]), collapse = " ")
})

# gender
s$sex = str_extract(s$born, "Nat(o|a)Â?\\s?")
s$sex[ grepl("Nata", s$sex) ] = "F"
s$sex[ grepl("Nato", s$sex) ] = "M"
s$sex[ !s$sex %in% c("F", "M") ] = NA
s$sex[ s$name == "Grazia SESTINI" ] = "F"
s$sex[ s$name == "Pasquale GIULIANO" ] = "M"

# year of birth
s$born[ s$legislature != 15 ] = str_clean(s$born[ s$legislature != 15 ])
s$born = str_extract(s$born, "Nat(o|a).*?\\d{4}") %>% str_extract("\\d{4}")
s$born[ s$name == "Grazia SESTINI" ] = 1958
s$born[ s$name == "Pasquale GIULIANO" ] = 1942
s$born = as.integer(s$born)

# constituency
s$constituency[ s$legislature == 14] = str_extract(s$constituency[ s$legislature == 14], "\\((.*)\\)")
s$constituency[ s$legislature == 13] = gsub("^circoscrizione\\s|Procla(.*)", "", s$constituency[ s$legislature == 13])
s$constituency = gsub("\\(|\\s\\d|\\)|^(I|V|X)+\\s|^X(V|X)V?III?|Inizio(.*)", "", s$constituency)
# # find missing constituencies
# s$constituency[ s$constituency == "" ] = NA
# WP-IT additions, missing from official pages
s$constituency[ grepl("dep-14-d300674", s$url) ] = "FRIULI-VENEZIA GIULIA" # ROSATO Ettore
s$constituency[ grepl("dep-14-d301054", s$url) ] = "LOMBARDIA" # ZACCARIA Roberto
s$constituency[ grepl("dep-14-d301055", s$url) ] = "LIGURIA" # ZARA Stefano (by-election)
s$constituency[ grepl("dep-14-d301056", s$url) ] = "TOSCANA" # GIACOMELLI Antonello
s$constituency[ grepl("dep-14-d301057", s$url) ] = "EMILIA-ROMAGNA" # TEDESCHI Massimo (by-election)
s$constituency[ grepl("dep-14-d301058", s$url) ] = "SICILIA" # D'ANTONI Sergio Antonio
s$constituency[ grepl("dep-14-d301059", s$url) ] = "PUGLIA" # RIA Lorenzo
s$constituency[ grepl("dep-14-d301408", s$url) ] = "CALABRIA" # OLIVERIO Nicodemo
s$constituency[ grepl("dep-14-d301409", s$url) ] = "LAZIO" # META Michele Pompeo
s$constituency[ grepl("dep-14-d33160", s$url) ] = "TOSCANA" # GALANTE Severino
s$constituency[ grepl("dep-14-d35640", s$url) ] = "LOMBARDIA" # BUTTIGLIONE Rocco (l. 13-14)
s$constituency[ grepl("dep-14-d36070", s$url) ] = "LAZIO"   # CONTE Gianfranco
s$constituency[ grepl("dep-14-d36500", s$url) ] = "LIGURIA" # FASSINO Piero (could be PIEMONTE)
# table(s$constituency, exclude = NULL)
# subset(s, is.na(constituency))$url

# impute mandates for l. 15 (approximate but reasonable)
old = s$name[ s$legislature == 14 ]
new = s$name[ s$legislature == 15 ]
table(new %in% old)

s$nyears[ s$legislature == 15 & s$name %in% new[ new %in% old ] ] =
  paste0(s$nyears[ s$legislature == 14 & s$name %in% new[ new %in% old ] ], ", XIV")

# extract previous mandates
s$nyears = sapply(s$nyears, function(x) {
  x = unlist(strsplit(x, ",\\s?"))
  paste0(sort(rom[ x ]), collapse = ";")
})

# legislature minus previous mandates, times mandate length
for (i in 1:nrow(s)) {
  
  j = strsplit(s$nyears[i], ";") %>% unlist %>% as.integer
  s$nyears[ i ] = 5 * sum(j < s$legislature[i])
  
}

# seniority
s$nyears = as.integer(s$nyears)

# convert filenames back to generic sponsor URLs
s$url = gsub("raw/mp-pages/dep-(\\d+)-(d)?(\\d+)\\.html",
             "/loc/link.asp\\?tipodoc=CAM.DEP&leg=\\1&id=\\2\\3", s$url)

# merge to sponsor URLs
stopifnot(s$url %in% sp$url)
s = left_join(s, sp, by = "url")

# prepare URLs to merge party affiliations with other details
s$url_chamber[ s$name == "Giuseppe TATARELLA" ] = "http://leg13.camera.it/cartellecomuni/deputati/composizione/leg13/Composizione/schede_/d00583.asp"
q$url_chamber = gsub("%5C", "\\\\", q$url_chamber) # l. 13
q$url_chamber = gsub("(.*)&nominativo=(.*)", "\\1", q$url_chamber) # l. xiv
q$url_chamber = gsub("(.*)&Vis=1(.*)", "\\1", q$url_chamber) # l. xv

stopifnot(s$url_chamber %in% q$url_chamber)
s = left_join(s, select(q, -name), by = "url_chamber")

# parties (none identical to abbreviation)
s$party[ s$url == "/loc/link.asp?tipodoc=CAM.DEP&leg=15&id=301523" ] = "ULIV" # Leone Pietro Antonio ZAPPIA
s$party[ grepl("ALLEANZA NAZIONALE", s$party ,ignore.case = TRUE) ] = "AN" # C15-16
s$party[ grepl("FORZA ITALIA", s$party, ignore.case = TRUE) ] = "FI-PDL" # C14-15
s$party[ s$party == "ITALIA DEI VALORI" ] = "IDV" # C15
s$party[ s$party == "MISTO (MPA-MOVIMENTO PER L'AUTONOMIA)" ] = "MPA" # C15
s$party[ s$party == "MISTO (MINORANZE LINGUISTICHE)" ] = "MIN" # C14-15
s$party[ grepl("LEGA NORD", s$party, ignore.case = TRUE) ] = "LN" # C13-15

# breakup of L'Ulivo and L'Unione parties
s$party[ s$party == "I Democratici - L'Ulivo" ] = "ID" # C13
s$party[ s$party %in% c("MISTO (VERDI-L'UNIONE)", "VERDI") ] = "VERD" # C14-15
s$party[ grepl("DEMOCRATICI DI SINISTRA", s$party, ignore.case = TRUE) ] = "DEMSIN" # C13-14
s$party[ grepl("SINISTRA DEMOCRATICA", s$party, ignore.case = TRUE) ] = "SINDEM" # C15
s$party[ s$party %in% c("L'ULIVO", "PARTITO DEMOCRATICO-L'ULIVO") ] = "ULIV" # C15; n = 1 on 2nd one
s$party[ grepl("^MARGHERITA", s$party) ] = "MARGH" # C14
s$party[ grepl("POPOLARI|UDEUR|Democratici per l'Europa", s$party, ignore.case = TRUE) ] =  "UDEUR" # C13-15
s$party[ grepl("COMUNISTI ITALIANI", s$party, ignore.case = TRUE) ] = "PCI" # C14-15
s$party[ grepl("(RIFONDAZIONE )?COMUNISTA", s$party, ignore.case = TRUE) ] = "PRC" # C13-15

# Christian Democrats: coalition in C14, UDC afterwards
s$party[ grepl("DEMOCRATICI CRISTIANI", s$party) & s$legislature == 14 ] = "CCD-CDU" # C14
s$party[ grepl("DEMOCRATICI CRISTIANI", s$party, ignore.case = TRUE) ] = "UDC" # C15

# coalitions
s$party[ s$party %in% c("SOCIALISTI E RADICALI-RNP", "MISTO (LA ROSA NEL PUGNO)") ] = "RNP" # C14-15

# two small coalitions with Nuovo PSI in both (one DCA-NPSI, one mixed)
s$party[ s$party == "MISTO (LIBERAL-DEMOCRATICI, REPUBBLICANI, NUOVO PSI)" ] = "LD-PRI-NPSI" # C14
s$party[ s$party == "DCA-DEMOCRAZIA CRISTIANA PER LE AUTONOMIE-NUOVO PSI" ] = "DCA-NPSI" # C15

# Residuals: Ecologisti Democratici (n = 4), La Destra (n = 4), Movimento Repubblicani Europei (n = 3)
s$party[ grepl("^MISTO", s$party, ignore.case = TRUE) ] = "IND" # residuals -- leave at end

# checks and breakdowns
table(s$party, exclude = NULL)
table(s$party, s$legislature, exclude = NULL)
sort(unique(s$party))[ !sort(unique(s$party)) %in% names(colors) ]

# number of groups per legislature
tapply(s$party, s$legislature, n_distinct)

# ============================================================================
# QUALITY CONTROL
# ============================================================================

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
stopifnot(!is.na(s$url_chamber) & grepl("^http(s)?://(.*)", s$url_chamber))
stopifnot(s$party %in% names(colors))

# ==============================================================================
# EXPORT DATASET
# ==============================================================================

s$chamber = "ca"

write.csv(select(s, chamber, legislature, url, url_chamber,
                 name, sex, born, constituency, party, nyears, photo),
          sponsors_ca_old, row.names = FALSE)
