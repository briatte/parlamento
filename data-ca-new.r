# ==============================================================================
# PARSE MP PAGES, LEGISLATURES 16-17
# ==============================================================================

s = data_frame()

j = list.files("raw/mp-pages", pattern = "dep-1[67]-d?\\d", full.names = TRUE)
cat("Parsing", length(j), "MPs...\n")

for (i in rev(j)) {
  
  # cat(str_pad(which(j == i), 4), i)
  l = gsub("(.*)dep-(\\d+)-(.*)", "\\2", i) %>% as.integer
  x = htmlParse(i)
  
  name = xpathSApply(x, "//div[@class='nominativo']", xmlValue)
  party = xpathSApply(x, "//div[@class='datielettoriali']", xmlValue)
  photo = xpathSApply(x, "//img[contains(@src, 'scheda_big')]/@src")
  born = xpathSApply(x, "//div[@class='datibiografici']", xmlValue)
  nyears = xpathSApply(x, "//a[contains(@href, 'http://storia.camera.it/deputato')]/../..", xmlValue)
  
  s = rbind(s, data_frame(
    legislature = l, url = i,
    name, born, sex = NA, nyears = ifelse(!length(nyears), NA, nyears),
    party, constituency = NA,
    photo = ifelse(is.null(photo), NA, photo)
  ))
  
  # cat(":", name, "\n")
  
}

# ============================================================================
# GET PHOTOS
# ============================================================================

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

# names
s$name = str_clean(s$name)
s$name = gsub("(.*)\\s-\\s(.*)", "\\1", s$name)
s$name = gsub("(.*)\\s\\((.*)", "\\1", s$name)

# reorder first and family names
s$name = sapply(s$name, function(x) {
  x = strsplit(x, "\\s") %>% unlist
  y = grepl("[a-z]", x)
  paste0(c(x[ y ], x[ !y ]), collapse = " ")
})

# gender
s$sex = str_extract(s$born, "Nat(o|a)\\s?")
s$sex[ grepl("Nata", s$sex) ] = "F"
s$sex[ grepl("Nato", s$sex) ] = "M"
s$sex[ !s$sex %in% c("F", "M") ] = NA

# year of birth
s$born = str_clean(s$born) %>% str_extract("Nat(o|a).*?\\d{4}")
s$born = str_extract(s$born, "\\d{4}") %>% as.integer

# constituency
s$constituency = str_extract(s$party, "\\((.*)\\)")
s$constituency = gsub("\\(|\\s\\d|\\)", "", s$constituency)
# table(s$constituency, exclude = NULL)
# subset(s, is.na(constituency))$url

# previous mandates
s$nyears = str_clean(s$nyears)
s$nyears = gsub("(.*): (.*)", "\\2", s$nyears)
s$nyears = gsub("(.*) Già (.*)", "\\1", s$nyears) # remove Senato mandates for two MPs

# extract previous mandates
s$nyears = sapply(s$nyears, function(x) {
  x = unlist(strsplit(x, ",\\s?"))
  paste0(sort(rom [ x ]), collapse = ";")
})

# legislature minus previous mandates, times mandate length
for (i in 1:nrow(s)) {
  
  j = strsplit(s$nyears[i], ";") %>% unlist %>% as.integer
  s$nyears[ i ] = 5 * sum(j < s$legislature[i])
  
}

# seniority
s$nyears = as.integer(s$nyears)

# convert filenames back to generic sponsor URLs
s$url = gsub("raw/mp-pages/dep-(\\d+)-(\\d+)\\.html",
             "/loc/link.asp\\?tipodoc=CAM.DEP&leg=\\1&id=\\2\\3", s$url)

# merge to sponsor URLs
stopifnot(sp$url[ grepl("DEP&leg=1(6|7)", sp$url) ] %in% s$url)
s = left_join(s, sp, by = "url") %>%
  filter(!is.na(url_chamber))

# duplicates
s$name[ s$url == "/loc/link.asp?tipodoc=CAM.DEP&leg=16&id=38120" ] = "Mario PEPE-1"
s$name[ s$url == "/loc/link.asp?tipodoc=CAM.DEP&leg=16&id=300368" ] = "Mario PEPE-2"

# parties
s$party = gsub("(.*)(Lista di elezione|Simbolo della candidatura)\\s?(.*?)\\r(.*)", "\\3", s$party)

s$party[ s$party == "PARTITO DEMOCRATICO" ] = "PD" # C16-17
s$party[ s$party == "SINISTRA ECOLOGIA LIBERTA'" ] = "SEL" # C17
s$party[ s$party == "CENTRO DEMOCRATICO" ] = "CD" # C17
s$party[ s$party == "UNIONE DI CENTRO" ] = "UDC" # C16-17
s$party[ s$party == "DI PIETRO ITALIA DEI VALORI" ] = "IDV" # C16
s$party[ s$party == "IL POPOLO DELLA LIBERTA'" ] = "FI-PDL" # C16-17
s$party[ grepl("MONTI PER L'ITALIA", s$party) ] = "SC" # C17
s$party[ s$party == "FRATELLI D'ITALIA" ] = "FRAT" # C17
s$party[ s$party == "LEGA NORD" ] = "LN" # C16-17
s$party[ s$party == "MOVIMENTO 5 STELLE BEPPEGRILLO.IT" ] = "M5S" # C17
s$party[ s$party == "MOVIMENTO PER L'AUTONOMIA ALLEANZA PER IL SUD" ] = "MPA" # C16
s$party[ s$party %in% c("SVP", "SUDTIROLER VOLKSPARTEI") ] = "SVP" # C16-17
# Residuals:
# - regionalists with less than 3 seats in legislatures 16-17 (ALD, Aosta)
#   - Autonomie Liberté Democratie
#   - Aosta
# - Italians abroad (n < 5 in both legislatures):
#   - South American Union Italian Emigrants (Unione Sudamericana Emigranti Italiani) -- USEI
#   - Associative Movement Italians Abroad (Movimento Associativo Italiani all'Estero) -- MAIE
s$party[ grepl("^USEI$|ALL'ESTERO|AUTONOMIE LIBERTE|VALLEE D'AOSTE", s$party) ] = "IND"

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

# ============================================================================
# EXPORT DATASET
# ============================================================================

s$chamber = "ca"

write.csv(select(s, chamber, legislature, url, url_chamber,
                 name, sex, born, constituency, party, nyears, photo),
          sponsors_ca_new, row.names = FALSE)
