# parse MPs, legislatures 16-17

v = unlist(strsplit(b$prima[grepl("CAM\\.DEP", b$prima) & grepl("leg=1(6|7)", b$prima) ], ";"))
v = c(v, unlist(strsplit(b$cofirm[grepl("CAM\\.DEP", b$cofirm) & grepl("leg=1(6|7)", b$cofirm) ], ";")))

v = data.frame(url = unique(v), stringsAsFactors = FALSE)

v$file = gsub("/loc/link.asp\\?tipodoc=CAM\\.DEP&leg=", "raw/", v$url)
v$file = paste0(gsub("&id=", "_", v$file), ".html")

# missing pages

v = subset(v, !grepl("_\\.html", file))
u = subset(v, !file.exists(file))

# download pages

if(nrow(u)) {

  cat("Adding", nrow(u), "MPs\n")

  for(j in nrow(u):1) {

    cat(sprintf("%4.0f", j), u$url[ j ], u$file[ j ])
    f = try(download.file(paste0(root, u$url[ j ]), u$file[ j ], quiet = TRUE), silent = TRUE)
    
    if("try-error" %in% class(f) | !file.info(u$file[ j ])$size) {
    
      cat(": network error")
      file.remove(u$file[ j ])
    
    }
    cat("\n")
  
  }

}

p = data.frame()
diff = c()

for(i in rev(v$file)) {
  
  # cat(i)
  h = try(htmlParse(i), silent = TRUE)
  
  if(!"try-error" %in% class(h)) {
    
    name = str_clean(xpathSApply(h, "//div[@class='nominativo']", xmlValue))
    party = xpathSApply(h, "//div[@class='datielettoriali']", xmlValue)

    photo = xpathSApply(h, "//img[contains(@src, 'scheda_big')]/@src")
    if(!length(photo))
      photo = NA
    
    born = xpathSApply(h, "//div[@class='datibiografici']", xmlValue)
        
    sex = ifelse(grepl("Nat", born), ifelse(grepl("Nata ", born), "F", "M"), NA)
    born = str_extract(born, "[0-9]{4}")
    
    # party_url = xpathSApply(h, "//div[@id='innerContentColumn']//a[contains(@href, 'Gruppo')]/@href")
    # party = xpathSApply(h, "//div[@id='innerContentColumn']//a[contains(@href, 'Gruppo')]", xmlValue)
    
    mandate = xpathSApply(h, "//a[contains(@href, 'http://storia.camera.it/deputato')]/../..", xmlValue)
    if(!length(mandate))
      mandate = NA
    else
      mandate = str_clean(mandate)
    
    if(length(born)) {
      
      p = rbind(p, data.frame(
        name =  gsub("\\((.*)\\)", "", gsub("(.*) - (.*)", "\\1", name)),
        party_abbr = gsub("(.*)( - |\\()(.*)(\\)?)", "\\3", name),
        party = str_clean(gsub("(.*)(Lista di elezione |Simbolo della candidatura)(.*)(Proclamat)(.*)", "\\3", party)),
        url = v$url[ v$file == i ], mandate,
        circo = str_clean(gsub("(.*)(Lista di elezione |Simbolo della candidatura)(.*)", "\\1", party)),
        sex, born, photo, stringsAsFactors = FALSE)) # party, party_url
      
      # cat(":", p$name[ nrow(p) ], "\n")
      
    } else {
      
      # a few pages are written in Frontpage-style HTML code
      # cat(":", t[ which(t[, 3] == i), 1], "failed (different code)\n")
      diff = c(diff, i)
      cat("Problem:", i, " has no details\n")
      
    }
    
  } else {
    
    cat("Problem:", i, " failed to scrape\n")
    
  }
  
}

cat(length(diff), "MPs failed to scrape\n")

p$circo = gsub("(.*)\\((.*)\\)", "\\2", p$circo)
p$circo = gsub("\\s\\d", "", p$circo)
p$circo[ grepl("AOSTA", p$circo) ] = "AOSTA"
p$circo[ grepl("MARCAZZAN", p$circo) ] = "LOMBARDIA" # Anna Teresa FORMISANO, 16_302085
p$circo[ grepl("AFRICA|AMERICA|EUROPA", p$circo) ] = "ALL'ESTERO" # abroad

p$circo = toupper(p$circo)

p$mandate = gsub("(.*): (.*)", "\\2", p$mandate)
p$mandate = gsub("(.*) Già (.*)", "\\1", p$mandate) # remove Senato mandates for two MPs

p$mandate = sapply(p$mandate, function(x) {
  x = unlist(strsplit(x, ",\\s?"))
  paste0(sort(rom [ x ]), collapse = ";")
})

subset(p, grepl("\\(", name))
nrow(subset(p, name == party_abbr))
table(p$party_abbr[p$party_abbr != p$name])

table(p$party, gsub("(.*)leg=(\\d+)(.*)", "\\2", p$url))

p$party[ p$party == "PARTITO DEMOCRATICO" ] = "Partito Democratico"
p$party[ p$party == "SINISTRA ECOLOGIA LIBERTA'" ] = "Sinistra Ecologia Libertà" # l. 16-17 only
p$party[ p$party == "CENTRO DEMOCRATICO" ] = "Centro Democratico"
p$party[ p$party == "UNIONE DI CENTRO" ] = "Unione di Centro"
p$party[ p$party == "DI PIETRO ITALIA DEI VALORI" ] = "Italia dei Valori"
p$party[ p$party == "IL POPOLO DELLA LIBERTA'" ] = "Il Popolo della Libertà"
p$party[ p$party == "FRATELLI D'ITALIA" ] = "Fratelli d'Italia"
p$party[ p$party == "LEGA NORD" ] = "Lega Nord"
p$party[ p$party == "MOVIMENTO 5 STELLE BEPPEGRILLO.IT" ] = "Movimento 5 Stelle"
p$party[ p$party == "MOVIMENTO PER L'AUTONOMIA ALLEANZA PER IL SUD" ] = "Movimento per l'Autonomia"
p$party[ grepl("MONTI PER L'ITALIA", p$party) ] = "Scelta Civica"
p$party[ p$party %in% c("SVP", "SUDTIROLER VOLKSPARTEI") ] = "Südtiroler Volkspartei"
# Residuals:
# - regionalists with less than 3 seats in legislatures 16-17 (ALD, Aosta)
# - Italians abroad:
#   - South American Union Italian Emigrants (Unione Sudamericana Emigranti Italiani) -- USEI
#   - Associative Movement Italians Abroad (Movimento Associativo Italiani all'Estero) -- MAIE
p$party[ grepl("^USEI$|ALL'ESTERO|AUTONOMIE LIBERTE DEMOCRATIE|VALLEE D'AOSTE", p$party) ] = "mixed or minor group"
# table(p$party, gsub("(.*)leg=(\\d+)(.*)", "\\2", p$url))

# fix duplicate names before downloading photos
p$name[ p$url == "/loc/link.asp?tipodoc=CAM.DEP&leg=16&id=38120" ] = "PEPE-1 Mario"
p$name[ p$url == "/loc/link.asp?tipodoc=CAM.DEP&leg=16&id=300368" ] = "PEPE-2 Mario"

# download photos (run a couple of times to solve network errors)

p$photo_url = p$photo
for(i in unique(p$photo_url)) {
  
  j = paste(gsub("(.*)leg=(\\d+)(.*)", "\\2", p$url[ p$photo_url == i ]), p$name[ p$photo_url == i ])
  j = paste0("photos_ca/", gsub("(_)+", "_", gsub("\\s|'", "_", tolower(j))), ".jpg")
  j = gsub("_\\.", ".", j)

  if(!file.exists(j))
    try(download.file(i, j, mode = "wb", quiet = TRUE), silent = TRUE)
  
  if(file.info(j)$size)
    p$photo[ p$photo_url == i ] = j
  else {
    p$photo[ p$photo_url == i ] = NA
    file.remove(j)
  }
  
}

# fix order and particles in MP names

p$name = gsub("d'\\s", "D'", p$name)
p$name = gsub("di\\s", "DI ", p$name)
p$name = gsub("de\\s", "DE ", p$name)
p$name = sapply(p$name, function(i) {
  j = unlist(strsplit(i, " "))
  k = c()
  for(jj in j) {
    if(!grepl("[a-z]", jj))
      k = c(k, jj)
  }
  j = j[ !j %in% k ]
  return(paste(paste0(j, collapse = " "), paste0(k, collapse = " ")))
})

# last name check (della, di, etc.)
p$name[ grepl("\\s[A-Z]{1,5}\\s", p$name) ]

write.csv(p[, c("url", "name", "sex", "born", "party", "mandate", "photo", "circo") ],
          "data/deputati-new.csv", row.names = FALSE)
