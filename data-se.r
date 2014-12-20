# load sponsors

sp = na.omit(c(b$prima, b$cofirm))
sp = unlist(str_split(sp, ";"))
sp = unique(sp)

if(!file.exists(sponsors))
  write.csv(data.frame(id = sp, name = NA, sex = NA, born = NA, 
                       party = NA, party_url = NA, mandate = NA, photo = NA),
            sponsors, row.names = FALSE)

s = read.csv(sponsors, stringsAsFactors = FALSE)

sp = sp[ !sp %in% s$id ]
if(length(sp)) {
  
  cat("Adding", length(sp), "new sponsor(s) to dataset\n")
  s = rbind(data.frame(id = sp, name = NA, sex = NA, born = NA, 
                       party = NA, party_url = NA, photo = NA), s)
  
}

# parse senators

k = s$id[ is.na(s$name) &grepl("SATTSEN", s$id) ]

if(length(k)) {
  
  cat("Parsing", length(k), "new senators\n")
  for(i in rev(k)) {
    
    cat(sprintf("%5.0f", which(k == i)), str_pad(i, 48, "right"))

    file = gsub("/loc/link.asp\\?tipodoc=SATTSEN&leg=", "raw/sen", i)
    file = paste0(gsub("&id=", "_", file), ".html")
    if(!file.exists(file))
      try(download.file(paste0(root, i), file, quiet = TRUE, mode = "wb"), silent = TRUE)
    
    if(!file.info(file)$size) {

      cat(": failed\n")
      file.remove(file)
      
    } else {
      
      x = htmlParse(file)
      
      # scrape senator details
      name = xpathSApply(x, "//h1[@class='titolo']", xmlValue)
      photo = xpathSApply(x, "//img[contains(@src, 'Immagini')]/@src")
      
      party_url = xpathSApply(x, "//div[@id='content']//a[contains(@href, 'sgrp')][1]/@href")
      party_url = gsub(root, "", party_url)
      party = str_clean(xpathSApply(x, "//div[@id='content']//a[contains(@href, 'sgrp')][1]", xmlValue))
      
      born = xpathSApply(x, "//div[@id='content']//table//td", xmlValue)
      born = unlist(str_split(born, "\\n"))
      born = born[ grepl("Nat(o|a)", born) ]
      
      sex = ifelse(grepl("Nat", born), ifelse(grepl("Nata ", born), "F", "M"), NA)
      born = as.numeric(str_extract(born, "[0-9]{4}"))
      
      mandate = paste0(xpathSApply(x, "//ul[@class='composizione']/li/a/@href"), collapse = ";")
      
      s[ s$id == i, ] = c(i, name, sex, born, party, party_url, mandate, photo)
      cat(":", name, "\n")
      
    }
    
  }
  
}

# download photos (run a couple of times to solve network errors)

k = unique(s$photo[ grepl("/leg/\\d+/Immagini/Senatori/", s$photo) ])
for(i in rev(k)) {

  j = paste0("photos_se/", gsub("/leg/\\d+/Immagini/Senatori/", "", i))
  
  if(!file.exists(j)) {
    cat("Photo", sprintf("%4.0f", which(k == i)), i, "\n")
    try(download.file(paste0(root, i), j, mode = "wb", quiet = TRUE), silent = TRUE)
  }
  
  if(file.info(j)$size)
    s$photo[ s$photo == i ] = j
  else {
    s$photo[ s$photo == i ] = NA
    file.remove(j)
  }
  
}

cat(sum(!is.na(s$name)), "identified sponsors",
    sum(is.na(s$name)), "unidentified, of which",
    sum(is.na(s$name) & grepl("SATTSEN", s$id)), "senators and",
    sum(is.na(s$name) & grepl("CAM\\.DEP", s$id)), "MPs\n")

write.csv(s, sponsors, row.names = FALSE)

# final senator sponsors

sen = subset(s, grepl("SATTSEN", id) & !is.na(name))[, c("id", "name", "sex", "born", "party", "mandate", "photo") ]
names(sen)[ which(names(sen) == "id") ] = "url"

# seniority that goes back to legislature 1
sen$mandate = sapply(sen$mandate, function(x) {
  x = unlist(str_extract_all(x, "&leg=[0-9]+"))
  paste0(sort(as.numeric(gsub("&leg=", "", x))), collapse = ";")
})

# legislature minus previous mandates, times mandate length
sen$nyears = as.numeric(gsub("&leg=", "", str_extract(sen$url, "&leg=[0-9]+")))
for(i in 1:nrow(sen)) {
  ii = as.numeric(unlist(strsplit(sen$mandate[i], ";")))
  sen$nyears[ i ] = 5 * sum(ii < sen$nyears[i])
}

sen$party_full = sen$party
sen$party[ grepl("^Lega$|Lega Nord|Padania|^LN-Aut$", sen$party) ] = "Lega Nord"
sen$party[ grepl("Ulivo", sen$party) ] = "L'Ulivo"

sen$party[ grepl("^CCD-CDU|Cristiano Democratic|Democraticicristiani", sen$party) ] = "Unione di Centro"
sen$party[ grepl("Rifondazione Comunista", sen$party) ] = "P. Rifondazione Comunista"
sen$party[ sen$party == "NCD" ] = "Nuovo Centrodestra"
sen$party[ sen$party == "PD" ] = "Partito Democratico"
sen$party[ sen$party == "PI" ] = "Per l'Italia" # coalition
sen$party[ sen$party == "GAL" | sen$party == "GAL (GS, LA-nS, MpA, NPSI, PpI)" ] = "Grandi Autonomie e Libertà" # coalition
sen$party[ sen$party == "Insieme con l'Unione Verdi - Comunisti Italiani" ] = "Verdi e Communisti" # coalition
sen$party[ grepl("FI-PdL XVII|Popolo della Libertà", sen$party) ] = "Forza Italia" # new version of Forza Italia (2013)
# Monti (small in Senate but not in Chamber)
sen$party[ sen$party == "SCpI" ] = "Scelta Civica con Monti"
# residuals: regionalists, Third Pole; n < 10 for each
sen$party[ grepl("Aut |UDC, SVP e Autonomie|Per il Terzo Polo|Per le Autonomie", sen$party) ] = "Misto"

# print(table(sen$party, gsub("(.*)leg=(\\d+)(.*)", "\\2", sen$url), exclude = NULL))

# number of groups per legislature
# tapply(sen$party, gsub("(.*)leg=(\\d+)(.*)", "\\2", sen$url), dplyr::n_distinct)

# assign party abbreviations
sen$partyname = sen$party
sen$party = as.character(parties[ sen$party ])

write.csv(sen, "data/senatori.csv", row.names = FALSE)

# kthxbye
