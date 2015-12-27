# ==============================================================================
# PARSE SENATOR PAGES
# ==============================================================================

s = data_frame()

j = list.files("raw/mp-pages", pattern = "sen-", full.names = TRUE)
cat("Parsing", length(j), "senators...\n")

for (i in rev(j)) {
  
  # cat(str_pad(which(j == i), 4))
  x = htmlParse(i)
  
  # scrape senator details
  name = xpathSApply(x, "//h1[@class='titolo']", xmlValue)
  photo = xpathSApply(x, "//img[contains(@src, 'Immagini')]/@src")
  
  # party_url = xpathSApply(x, "//div[@id='content']//a[contains(@href, 'sgrp')][1]/@href")
  # party_url = gsub(root, "", party_url)
  party = str_clean(xpathSApply(x, "//div[@id='content']//a[contains(@href, 'sgrp')][1]", xmlValue))
  
  born = xpathSApply(x, "//div[@id='content']//table//td", xmlValue)
  born = unlist(str_split(born, "\\n"))
  
  constituency = born[ grepl("(Circoscrizione estera|Regione) di elezione", born) ]
  constituency = gsub("(Circoscrizione estera|Regione) di elezione: ", "", constituency)
  constituency = gsub(" - Collegio(.*)", "", constituency)
  constituency = ifelse(!length(constituency),
                        ifelse(any(grepl("a vita", born)), "SENATORE A VITA", NA),
                        constituency)
  
  born = born[ grepl("Nat(o|a)", born) ]
  
  sex = ifelse(grepl("Nat", born), ifelse(grepl("Nata ", born), "F", "M"), NA)
  
  nyears = paste0(xpathSApply(x, "//ul[@class='composizione']/li/a/@href"), collapse = ";")
  
  s = rbind(s, data_frame(
    legislature = gsub("(.*)sen-(\\d+)-(.*)", "\\2", i) %>% as.integer, url = i,
    name, sex, born, constituency, party, nyears, photo
  ))
  
  # cat(":", name, "\n")
  
}

# ==============================================================================
# GET PHOTOS
# ==============================================================================

k = s$photo
cat("Getting", length(k), "photos...\n")

for (i in rev(k)) {
  
  j = paste0("photos_se/", gsub("/leg/\\d+/Immagini/Senatori/", "", i))
  
  if (!file.exists(j)) {
    
    h = try(GET(paste0(root, i), j, mode = "wb", quiet = TRUE), silent = TRUE)
    
    if (!"try-error" %in% class(h) && status_code(h) == 200)
      writeLines(content(h, "raw"), j)
    
  }
  
  if (file.exists(j)) {
    
    s$photo[ s$photo == i ] = j
    
  } else {
    
    cat("Photo", i, "failed\n")
    s$photo[ s$photo == i ] = NA
    
  }
  
}

# ==============================================================================
# FINALIZE VARIABLES
# ==============================================================================

# convert filenames back to generic sponsor URLs
s$url = gsub("raw/mp-pages/sen-(\\d+)-(\\d+)\\.html",
             "/loc/link.asp\\?tipodoc=SATTSEN&leg=\\1&id=\\2", s$url)

# merge to sponsor URLs
stopifnot(sp$url[ grepl("SATTSEN", sp$url) ] %in% s$url)
s = left_join(s, sp, by = "url") %>%
  filter(!is.na(url_chamber))

# finalize year of birth
s$born = str_extract(s$born, "[0-9]{4}") %>% as.integer

# finalize constituency
s$constituency = str_clean(s$constituency) %>% toupper

# seniority that goes back to legislature 1 (Senato only, for comparability)
s$nyears = sapply(s$nyears, function(x) {
  x = unlist(strsplit(x, ";"))
  x = x[ grepl("sattsen", x) ]
  x = unlist(str_extract_all(x, "&leg=[0-9]+"))
  paste0(sort(unique(as.numeric(gsub("&leg=", "", x)))), collapse = ";")
})

# legislature minus previous mandates, times mandate length
for (i in 1:nrow(s)) {
  
  j = strsplit(s$nyears[i], ";") %>% unlist %>% as.integer
  s$nyears[ i ] = 5 * sum(j < s$legislature[i])
  
}

# seniority
s$nyears = as.integer(s$nyears)

# parties (full recodings, even if identical to abbreviation)
s$party[ grepl("Rifondazione Comunista", s$party) ] = "PRC" # S13, S15
s$party[ s$party %in% c("L'Ulivo", "Partito Democratico-L'Ulivo") ] = "ULIV" # S15, n = 1 on 2nd one
s$party[ grepl("^Lega$|Lega Nord|Padania|^LN-Aut$", s$party) ] = "LN" # S13-17
s$party[ grepl("^Forza Italia|FI-PdL XVII|Popolo della Libertà", s$party) ] = "FI-PDL" # S13-17
s$party[ grepl("^Margherita", s$party) ] = "MARGH" # S14, part of L'Ulivo
s$party[ s$party == "Alleanza Nazionale" ] = "AN" # S13-S15
s$party[ s$party %in% c("Partito Democratico", "PD") ] = "PD" # S16-17
s$party[ s$party == "Italia dei Valori" ] = "IDV" # S16
s$party[ s$party == "Partito Popolare Italiano" ] = "PPI" # S13
s$party[ s$party == "Rinnovamento Italiano" ] = "RINNOV" # S13
s$party[ grepl("er le Autonomie", s$party) ] = "MPA" # S14
s$party[ s$party == "AL-A" ] = "AL-A" # S17
s$party[ s$party == "M5S" ] = "M5S" # S17
s$party[ s$party %in% c("CoR", "CRi") ] = "CRI" # S17; coalition now dropped the 'I'

# Christian-Democrats
s$party[ grepl("^Centro Cristiano Democratico| - CCD$", s$party) ] = "CCD" # S13; score 5.9
s$party[ grepl("Democratica - CDU$", s$party) ] = "CDU" # S13; score 6.2
s$party[ s$party == "Unione dei Democraticicristiani e di Centro (UDC)" ] = "UDC" # S15; score 6

# coalitions
s$party[ s$party == "AP (NCD-UDC)" ] = "NCD-UDC" # S17
s$party[ s$party == "Insieme con l'Unione Verdi - Comunisti Italiani" ] = "VERD-PCI" # S15
s$party[ s$party == "CCD-CDU: Biancofiore" ] = "CCD-CDU" # S14
s$party[ s$party == "UDC, SVP e Autonomie" ] = "UDC-SVP-MPA" # S16
s$party[ grepl("GAL \\(", s$party) ] = "GAL" # S17; coalition changed names a few times
s$party[ s$party == "Aut (SVP, UV, PATT, UPT)-PSI-MAIE" ] = "AUT-PSI-MAIE" # S17

# breakup of L'Ulivo factions
s$party[ s$party == "Verdi - l'Ulivo" ] = "VERD" # S13-14
s$party[ s$party == "Sinistra Democratica - l'Ulivo" ] = "SINDEM" # S13
s$party[ s$party == "Democratici di Sinistra - l'Ulivo" ] = "DEMSIN" # S13-14

# Residuals: mixed and Third Pole (S16; n = 3, disbanded in 2012)
s$party[ s$party == "Misto" | grepl("Per il Terzo Polo", s$party) ] = "IND"

# checks and breakdowns
table(s$party, exclude = NULL)
table(s$party, s$legislature, exclude = NULL)
sort(unique(s$party))[ !sort(unique(s$party)) %in% names(colors) ]

# number of groups per legislature
tapply(s$party, s$legislature, n_distinct)

# ==============================================================================
# QUALITY CONTROL
# ==============================================================================

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

s$chamber = "se"

write.csv(select(s, chamber, legislature, url, url_chamber,
                 name, sex, born, constituency, party, nyears, photo),
          sponsors_se, row.names = FALSE)
