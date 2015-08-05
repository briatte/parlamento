# http://stackoverflow.com/a/6364905/635806
simpleCap <- function(x) {
  s = strsplit(x, "\\s")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

root = "http://www.senato.it"
bills = "data/bills.csv"
sponsors = "data/sponsors.csv"

# in Italy, everything is both old and new, and more complex than elsewhere, so
# we need to go through intermediary datasets to get the final sponsors data...

sponsors_ca_old = "data/sponsors-ca-old.csv"
sponsors_ca_new = "data/sponsors-ca-new.csv"
sponsors_se = "data/sponsors-se.csv"

# legislatures
rom = c("I" = 1, "II" = 2, "III" = 3, "IV" = 4, "V" = 5, "VI" = 6, "VII" = 7,
        "VIII" = 8, "IX" = 9, "X" = 10, "XI" = 11, "XII" = 12, "XIII" = 13,
        "XIV" = 14, "XV" = 15, "XVI" = 16)

# ==============================================================================
# INITIALIZE BILLS DATASET
# ==============================================================================

if (!file.exists(bills)) {
  
  b = data_frame()
  
  for (i in 17:13) {
    
    f = paste0("raw/bill-lists/bills-", i, ".html")
    
    if (!file.exists(f)) {
      
      h = GET(paste0(root, "/ric/sddl/risultati.do?params.commissioneBoolOp=AND&params.statoDiv=0,0,0,1,0,0,0&des=&params.parlamentareIniziativaBoolOp=AND&selmode=&params.interventiBoolOp=AND&params.campoOrdinamento=dataPresentazione&params.tipoIniziativa=1&params.gruppoTrattazioneBoolOp=AND&params.rows=100&livelloTeseo=&params.teseoTuttiTermini=T&params.relatoriBoolOp=AND&params.start=0&params.legislatura=", i, "&params.tipoFirmatari=1&teseo=&sel=&params.ordinamento=desc"))
      writeLines(content(h, "text"), f)
      
    }
    
    # first page
    h = htmlParse(f)
    
    # number of pages
    p = xpathSApply(h, "//a[contains(@href, 'params.start')]/@href")
    p = gsub("\\D", "", str_extract(p, "params.start=[0-9]+"))
    p = seq(0, max(as.numeric(p)), 100)
    
    for (j in rev(p)) {
      
      cat("Legislature", i, "page", sprintf("%3.0f", which(p == j)))
      
      f = paste0("raw/bill-lists/bills-", i, "-page-", j, ".html")
      
      if (!file.exists(f)) {
        
        h = GET(paste0(root, "/ric/sddl/risultati.do?params.commissioneBoolOp=AND&params.statoDiv=0,0,0,1,0,0,0&des=&params.parlamentareIniziativaBoolOp=AND&selmode=&params.interventiBoolOp=AND&params.campoOrdinamento=dataPresentazione&params.tipoIniziativa=1&params.gruppoTrattazioneBoolOp=AND&params.rows=100&livelloTeseo=&params.teseoTuttiTermini=T&params.relatoriBoolOp=AND&params.start=", j, "&params.legislatura=", i, "&params.tipoFirmatari=1&teseo=&sel=&params.ordinamento=desc"))
        writeLines(content(h, "text"), f)
        
      }
      
      h = htmlParse(f)
      h = xpathSApply(h, "//div[@id='content']//a[contains(@href, 'sddliter')]/@href")
      
      b = rbind(b, data_frame(legislature = as.character(i), url = h))
      cat(":", sprintf("%5.0f", nrow(b)), "bills in total\n")
      
    }
    
  }
  
  # bills metadata
  b$cosponsors_dummy = b$cosponsors = b$authors = b$teseo = b$title = b$date = b$ref = b$url_chamber = NA
  
  # - url_chamber is the actual address of the bill on the parliamentary website
  #   of the lower or upper chamber
  # - cosponsors_dummy discriminates between cosponsors and merged bill authors,
  #   e.g. http://www.senato.it/leg/13/BGT/Schede/Ddliter/13229.htm or
  #        http://www.senato.it/leg/17/BGT/Schede/Ddliter/39637.htm
  
  write.csv(b, bills, row.names = FALSE)
  
}

# ==============================================================================
# PARSE BILLS (skipping over 4 empty pages)
# ==============================================================================

b = read.csv(bills, stringsAsFactors = FALSE)

u = b$url[ is.na(b$authors) | is.na(b$url_chamber) ] # rows with no sponsor information
u = sample(u[ !grepl("id=(19902|18800|18870|18686)", u) ])

for (i in rev(u)) {
  
  f = paste0("raw/bill-pages/bill-", gsub("(.*)leg=(\\d+)&id=(\\d+)", "\\2-\\3", i), ".html")
  h = try(GET(paste0(root, i)), silent = TRUE)
  
  if ("try-error" %in% class(h)) {
    
    cat(sprintf("%6.0f", which(u == i)), ": failed\n") 
    
  } else {
    
    cat(sprintf("%6.0f", which(u == i)), ":", h$url, "\n")
    b$url_chamber[ b$url == i ] = h$url
    writeLines(content(h, "text"), f)
    
  }
  
  if (file.exists(f)) {
    
    h = htmlParse(f)
    
    ref = xpathSApply(h, "//div[@id='content']//h1", xmlValue)
    title = str_clean(xpathSApply(h, "//div[@class='boxTitolo']", xmlValue))
    
    # date is the last legislative status, not introduction date
    date = str_clean(xpathSApply(h, "//div[@class='bordoNero']/table/tr/td[3]/strong", xmlValue))
    kw = str_clean(xpathSApply(h, "//h2[text()='Classificazione TESEO']/following-sibling::p[1]", xmlValue))
    
    # first authors are located in the first div
    # authors of related legislation are located in the second div
    au = xpathSApply(h, "//div[@class='testoMedium'][1]/a/@href")
    co = xpathSApply(h, "//div[@class='testoSmall']/a/@href")
    
    cofirm = xpathSApply(h, "//div[@id='div1Top']/span/strong/a", xmlValue)
    cofirm = ifelse(is.null(cofirm), NA, cofirm)
    cofirm = ifelse(is.na(cofirm), NA, as.numeric(grepl("Cofirma", cofirm)))
    
    cat(sprintf("%6.0f", which(u == i)), ":",
        sprintf("%3.0f", length(au)), "author(s)",
        sprintf("%3.0f", length(co)), "cosponsor(s)\n")
    
    b$ref[ b$url == i ] = ref
    b$date[ b$url == i ] = date
    b$title[ b$url == i ] = title
    b$teseo[ b$url == i ] = ifelse(length(kw), kw, NA)
    
    b$authors[ b$url == i ] = paste0(au, collapse = ";")
    b$cosponsors[ b$url == i ] = ifelse(is.null(co), NA, paste0(co, collapse = ";"))
    b$cosponsors_dummy[ b$url == i ] = cofirm
    
  }
  
  if (!which(u == i) %% 100 | which(u == i) == 1) {
    
    write.csv(b, bills, row.names = FALSE)
    cat("[saved]\n")
    
  }
  
}

cat("Loaded", nrow(b), "bills\n")

# ==============================================================================
# GET CHAMBER-LEVEL SPONSOR URLs (N ~ 4,800)
# ==============================================================================

if (!file.exists(sponsors)) {
  
  sp = na.omit(c(b$authors, b$cosponsors))
  sp = unlist(str_split(sp, ";"))
  sp = unique(sp[ !grepl("id=$", sp) ])
  write.csv(data_frame(url = sp, url_chamber = NA), sponsors, row.names = FALSE)
  
}

sp = read.csv(sponsors, stringsAsFactors = FALSE)

u = sp$url[ is.na(sp$url_chamber) ]

if (length(u) > 0)
  cat(length(u), "sponsor URLs left to parse\n")

for (i in rev(u)) {
  
  f = gsub("/loc/link\\.asp\\?tipodoc=", "raw/mp-pages/", i)
  f = gsub("CAM.DEP", "dep", gsub("SATTSEN", "sen", f), f)
  f = paste0(gsub("&leg=|&id=", "-", f), ".html")
  
  h = try(GET(paste0(root, i)), silent = TRUE)
  
  if (headers(h)["content-type"] == "application/json; charset=utf-8") {
    
    # infinite thanks to Jeroen Ooms for the trick
    # https://github.com/jeroenooms/curl/issues/35
    h = try(GET(paste0(h$url, "&_random=", rnorm(1)), accept("text/html")), silent = TRUE)
    
    stopifnot(h$headers[["x-cache"]] == "MISS")
    stopifnot(h$headers$age == "0")
    
  }
  
  if ("try-error" %in% class(h)) {
    
    cat(str_pad(which(u == i), 6), str_pad(i, 48, "right"), ": failed\n")
    
  } else if (status_code(h) != 200) {
    
    cat(str_pad(which(u == i), 6), str_pad(i, 48, "right"), ": failed, code",
        status_code(h), "\n")
    
  } else {
    
    sp$url_chamber[ sp$url == i ] = h$url
    writeLines(content(h, "text"), f)
    
  }
  
  if (!which(u == i) %% 100 | which(u == i) == 1) {
    
    cat("[", str_pad(sum(is.na(sp$url_chamber)), 4), "URLs left ]\n")
    write.csv(sp, sponsors, row.names = FALSE)
    
  }
  
}

# ==============================================================================
# ASSEMBLE SPONSORS DATASET
# ==============================================================================

# Senato
if (!file.exists(sponsors_se))
  source("data-se.r") # scrapes ~ 1,600 senator details (sponsors only)

# Camera, l. 13-15
if (!file.exists(sponsors_ca_old))
  source("data-ca-old.r") # scrapes ~ 1,900 MP details from legislatures 13-15

# Camera, l. 16-17
if (!file.exists(sponsors_ca_new))
  source("data-ca-new.r") # scrapes ~ 1,200 MP details from legislatures 16-17

s = rbind(
  read.csv(sponsors_se, stringsAsFactors = FALSE),
  read.csv(sponsors_ca_old, stringsAsFactors = FALSE),
  read.csv(sponsors_ca_new, stringsAsFactors = FALSE)
)

cat("Loaded", nrow(s), "sponsors\n")

# ==============================================================================
# CHECK CONSTITUENCIES
# ==============================================================================

# convert to WP-IT handles
s$constituency = gsub("\\s", "_", sapply(tolower(s$constituency), simpleCap))
s$constituency[ s$constituency == "Abruzzi" ] = "Abruzzo"
s$constituency[ grepl("(A|a)osta", s$constituency) ] = "Aosta"
s$constituency[ grepl("Emilia(.*)omagna", s$constituency) ] = "Emilia-Romagna"
s$constituency[ s$constituency == "Friuli-venezia_Giulia" ] = "Friuli-Venezia_Giulia"
s$constituency[ s$constituency == "Trentino-alto_Adige" ] = "Trentino-Alto_Adige"
# lifetime mandate (Senato)
s$constituency[ s$constituency == "Senatore_A_Vita" ] = "Senatore_a_vita"
# abroad constituencies (Camera)
s$constituency[ grepl("Asia|America|Europa", s$constituency) ] = 
  "Anagrafe_degli_italiani_residenti_all'estero"

cat("Checking constituencies,", sum(is.na(s$constituency)), "missing...\n")
for (i in na.omit(unique(s$constituency))) {
  
  g = GET(paste0("https://", meta[ "lang"], ".wikipedia.org/wiki/", i))
  
  if (status_code(g) != 200)
    cat("Missing Wikipedia entry:", i, "\n")
  
  g = xpathSApply(htmlParse(g), "//title", xmlValue)
  g = gsub("(.*) - Wikipedia(.*)", "\\1", g)
  
  if (gsub("\\s", "_", g) != i)
    cat("Discrepancy:", g, "(WP) !=", i ,"(data)\n")
  
}

# ==============================================================================
# FINALIZE BILLS
# ==============================================================================

# check all sponsors are recognized
a = c(strsplit(b$authors, ";") %>% unlist, strsplit(b$cosponsors, ";") %>% unlist)
a = na.omit(a) %>% unique
a = a[ !a %in% s$url ]

# cat("Removing", length(a), "unidentified sponsors...\n")
for (i in 1:nrow(b)) {
  
  p = strsplit(b$authors[ i ], ";") %>% unlist %>% na.omit
  
  if (any(p %in% a))
    b$authors[ i ] = paste0(p[ !p %in% a], collapse = ";")

  c = strsplit(b$cosponsors[ i ], ";") %>% unlist %>% na.omit
  
  if (any(c %in% a))
    b$cosponsors[ i ] = paste0(c[ !c %in% a], collapse = ";")

}

b$authors[ b$authors == "" ] = NA
b$cosponsors[ b$cosponsors == "" ] = NA

# cat("Removing", sum(is.na(b$authors)), "bill(s) with no author(s)\n")
b = subset(b, !is.na(authors))

b$n_au = 1 + str_count(b$authors, ";")
table(b$n_au, exclude = NULL)

b$n_co = 1 + str_count(b$cosponsors, ";")
b$n_co[ is.na(b$cosponsors) ] = 0
table(b$n_co, exclude = NULL)

b$n_a = b$n_au + b$n_co
table(b$n_a, exclude = NULL)

# less than 0.1% of all bills are ambiguous re: cosponsors: (62 + 7) / nrow(b)
# all other bills are either single-authored (FALSE/0) or cosponsored (TRUE/1)
table(b$n_a > 1, b$cosponsors_dummy, exclude = NULL)

# one bill mixes senators and MPs in their signatories
b$mix = 0
b$mix[ grepl("SATTSEN", b$authors) & grepl("CAM", b$authors) ] = 1       # n = 0
b$mix[ grepl("SATTSEN", b$cosponsors) & grepl("CAM", b$cosponsors) ] = 1 # n = 1
b$mix[ grepl("SATTSEN", b$authors) & grepl("CAM", b$cosponsors) ] = 1    # n = 0
b$mix[ grepl("SATTSEN", b$cosponsors) & grepl("CAM", b$authors) ] = 1    # n = 1

# cat("Removing", sum(b$mix), "bill(s) with mixed sponsors\n")
b = subset(b, !mix)

# no bills with members of the government
b$gov = 0
b$gov[ grepl("COMPGOV", b$authors) ] = 1    # n = 0
b$gov[ grepl("COMPGOV", b$cosponsors) ] = 1 # n = 0

# cat("Removing", sum(b$gov), "bill(s) with governmental sponsors\n")
b = subset(b, !gov)

# dates as years
b$date = str_extract(b$date, "[0-9]{4}") %>% as.integer

# identify sponsor types
b$chamber = str_extract_all(b$authors, "CAM\\.DEP|SATTSEN")
b$chamber = lapply(b$chamber, na.omit)
b$chamber = lapply(b$chamber, unique)

# no mixed sponsor types
stopifnot(sapply(b$chamber, length) == 1)
b$chamber = c("CAM.DEP" = "ca", "SATTSEN" = "se")[ unlist(b$chamber) ]

# # proportion of bills cosponsored in each legislature (50-70%)
# prop.table(table(b$n_a > 1, b$legislature), 2)

# # keywords are present for almost all bills
# with(b, table(legislature, is.na(teseo)))
# # several thousands of different unique keywords
# k = unlist(strsplit(b$teseo, ","))
# k = gsub(" Classificazione provvisoria", "", k)
# k = str_clean(k)
# k = data.frame(table(k))
# k = k[ order(-k$Freq), ]
# nrow(k) # n ~ 4,100 keywords
# # approximately 60 keywords identify > 5% of bills
# .05 * nrow(k) # 205
# nrow(k[ k$Freq >= 205, ])
# # incl. legislation types, constitution, grants, taxes, children, family...
# subset(k, Freq >= 205)
# # export keywords
# write.csv(data.frame(keyword = k$k, freq = k$Freq), "data/keywords.csv", row.names = FALSE)

# done
