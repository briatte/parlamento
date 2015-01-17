# parse MPs, legislatures 13-15

cam = "data/camera-old.csv"
if(!file.exists(cam)) {
  
  # start with pages that fail to scrape
  q = data.frame(
    name =  c("VALENSISE Raffaele", "TATARELLA Giuseppe"),
    party = c("Alleanza Nazionale", "Alleanza Nazionale"),
    url =   c("http://leg13.camera.it/cartellecomuni/deputati/composizione/leg13/Composizione/schede_/valera01.asp", "http://leg13.camera.it/cartellecomuni/deputati/composizione/leg13/Composizione/schede_/d00583.asp"),
    sex = c("M", "M"),
    born = c("1921", "1935"),
    photo = c("http://leg13.camera.it/cartellecomuni/deputati/composizione/leg13/Composizione/schede_/img/VALERA01.jpg", "http://leg13.camera.it/cartellecomuni/deputati/composizione/leg13/Composizione/schede_/img/22120.jpg"),
    mandate = c("VI, VII, VIII, IX, X, XI, XII", "VIII, IX, X, XI, XII"),
    circo = c("CALABRIA", "PUGLIA"),
    stringsAsFactors = FALSE)
  diff = c()
  for(leg in paste0("http://leg", c("13", "xiv", "xv"), ".camera.it")) {
    
    for(a in rev(LETTERS)) {
      
      cat("Legislature", str_pad(str_extract(leg, "13|xiv|xv"), 4, "right"), "letter", a)
      file = paste0("raw/", gsub("http://(.*)\\.camera\\.it", "\\1", leg), "_", a, ".html")
      
      if(!file.exists(file)) {
        
        if(grepl("xv", leg))
          download.file(paste0(leg, "/deputatism/240/documentoxml.asp?sezione=&Lettera=", a), 
                        file, mode = "wb", quiet = TRUE)
        else
          download.file(paste0(leg, "/deputatism/240/documentoxml.asp?Let=", a), 
                        file, mode = "wb", quiet = TRUE)
        
      }
      f = htmlParse(file)
      
      t = try(readHTMLTable(f, which = 1, stringsAsFactors = FALSE), silent = TRUE)
      if("try-error" %in% class(t) | is.null(t)) {
        
        cat(": empty\n")
        
      } else {
        
        t = data.frame(name = str_clean(xpathSApply(f, "//table//tr/td[1]", xmlValue)),  # name
                       party = str_clean(xpathSApply(f, "//table//tr/td[2]", xmlValue)), # party
                       stringsAsFactors = FALSE)
        
        # fix empty row on legislature 13 letter M
        t = subset(t, name != "MANISCO Lucio (non in carica)")
        
        # add URL
        t$url = paste0(leg, xpathSApply(f, "//table//tr/td[1]/a/@href"))
                
        cat(":", sprintf("%3.0f", nrow(t)), "MPs\n")
        
        p = data.frame()
        
        # avoid two MPs coded differently
        for(i in rev(t$url[ !t$url %in% c("http://leg13.camera.it/cartellecomuni/deputati/composizione/leg13/Composizione/schede_/valera01.asp", "http://leg13.camera.it/cartellecomuni/deputati/composizione/leg13/Composizione/schede_/d00583.asp") ])) {
                    
          # legislature number
          h = str_extract(i, "13|xiv|xv")
          h[ h == "xiv" ] = 14
          h[ h == "xv" ] = 15
          h = paste0("raw/", h, gsub("(id|deputato)=", "_", str_extract(i, "(id|deputato)=(d)?(\\d+)")), ".html")
          
          if(!file.exists(h))
            try(download.file(gsub("\\s", "%20", i), h, quiet = TRUE, mode = "wb"), silent = TRUE)
          
          if(!file.info(h)$size)
            file.remove(h)
          
          hh = h

          # cat("\nParsing", h)
          h = try(htmlParse(h), silent = TRUE)
                    
          if(!"try-error" %in% class(h)) {
                        
            # name = str_clean(xpathSApply(h, "//div[@id='innerContentColumn']//strong[1]", xmlValue))
            photo = xpathSApply(h, "//img[contains(@src, '.jpg')]/@src")
            if(!length(photo))
              photo = NA
            
            # works only for leg. XV
            circo = xpathSApply(h, "//div[@id='innerContentColumn']", xmlValue)
            circo = gsub("(.*) circoscrizione [A-Z]+ \\((.*)", "\\2", circo)
            circo = gsub("(\\w+)\\)(.*)", "\\1", circo)
            
            born = xpathSApply(h, "//div[@id='innerContentColumn']//p[2]", xmlValue)
            mandate = NA
              
            # fix leg. XIV
            if(grepl("xiv", i)) {
              
              circo = xpathSApply(h, "//div[@id='schedaDepDatiPers']", xmlValue)
              circo = unlist(strsplit(circo, "\\s{2,}"))
              circo = circo[ grepl("circoscrizione", circo) ]
              
              born = str_clean(xpathSApply(h, "//div[@id='schedaDepDatiPers']", xmlValue))
              
              mandate = str_clean(gsub("(.*) nell(a|e) legislatur(a|e) (.*)Iscritto (.*)", "\\4", born))
              if(grepl("\\d", mandate))
                mandate = NA
              
            }
            
            # fix leg. XIII
            if(!length(born)) {
              
              born = xpathSApply(h, "//div[@id='innerContentColumn']//div[2]", xmlValue)
              
              circo = unlist(strsplit(born, "\\s{2,}"))
              circo = circo[ grepl("circoscrizione", circo) ]
              
              mandate = str_clean(gsub("(.*) nell(a|e) legislatur(a|e) (.*)", "\\4", born))
              if(grepl("\\d", mandate))
                mandate = NA
              
              born = str_extract(born, "Nat(o|a)(.*)(\\d{4})")
              
            }
            
            ## cat(":", mandate, "\n")
            
            sex = ifelse(grepl("Nat", born), ifelse(grepl("Nata ", born), "F", "M"), NA)
            born = str_extract(born, "[0-9]{4}")
            
            # party_url = xpathSApply(h, "//div[@id='innerContentColumn']//a[contains(@href, 'Gruppo')]/@href")
            # party = xpathSApply(h, "//div[@id='innerContentColumn']//a[contains(@href, 'Gruppo')]", xmlValue)
            
            if(length(born) > 0) {
              
              circo = ifelse(!length(circo), NA, circo)
              ## cat(":", born, t$name[ which(t$url == i) ])
              p = rbind(p, data.frame(
                name =  t$name[ which(t$url == i) ],
                party = t$party[ which(t$url == i) ],
                url =   t$url[ which(t$url == i) ],
                sex, born, photo, mandate, circo,
                stringsAsFactors = FALSE)) # party, party_url
              
            } else {
              
              # a few pages are written in Frontpage-style HTML code
              diff = c(diff, i)
              
            }
            
          } else {
            
            # cat(":", t[ which(t[, 3] == i), 1], "failed (network issue)\n")
            
          }
          
        }
        
        q = rbind(q, p)
        
      }
      
    }
    
  }
  # constituencies
  q$circo = gsub("(nella\\s)?circoscrizione\\s|Collegio:\\s|(Lista\\sdi\\selezione:\\s|Proclamato)(.*)|\\d", "",
                 q$circo)
  q$circo = gsub("^[XIV]+\\s(.*)", "\\1", q$circo)
  q$circo = gsub("\\(|\\)|\\s-$", "\\1", q$circo)
  q$circo = gsub("^(XVIII|XXIII|XXVII)(.*)", "\\2", q$circo)
  q$circo[ q$circo == "" | grepl("Inizio contenuto", q$circo) ] = NA
  q$circo = toupper(str_clean(q$circo))

  # final simplifications
  q$circo[ q$circo == "ABRUZZI" ] = "ABRUZZO"
  q$circo[ q$circo == "VALLE D'AOSTA" ] = "AOSTA"
  q$circo[ grepl("AFRICA|AMERICA|EUROPA", q$circo) ] = "ALL'ESTERO" # abroad
  
  # pages that failed to scrape
  # two pages from l. 13 are coded completely differently and a few others are empty
  
  cat(length(diff), "MPs failed to scrape\n")
  
  # clean up names
  q$name = gsub("( )?\\((non in carica|deceduto|fino al (.*))\\)", "", q$name)
  subset(q, grepl("\\(", name))
  
  # legislature
  
  q$legislature = NA
  q$legislature[ grepl("leg13", q$url) ]  = "13"
  q$legislature[ grepl("legxiv", q$url) ] = "14"
  q$legislature[ grepl("legxv", q$url) ]  = "15"
  
  # impute mandates in l. 15 (very approximate)
  old = q$name[ q$legislature == "14" ]
  new = q$name[ q$legislature == "15" ]
  table(new %in% old)
  q$mandate[ q$legislature == "15" & q$name %in% new[ new %in% old ] ] =
    paste0(q$mandate[ q$legislature == "14" & q$name %in% new[ new %in% old ] ], ", XIV")
  
  q$mandate = sapply(q$mandate, function(x) {
    x = unlist(strsplit(x, ",\\s?"))
    paste0(sort(rom[ x ]), collapse = ";")
  })
  
  # photo
  
  q$photo[ q$legislature == "13" & !grepl("^http", q$photo) ] = 
    paste0("http://leg13.camera.it/cartellecomuni/leg13/Deputati/scheda_deputato/",
           q$photo[ q$legislature == "13" & !grepl("^http", q$photo) ])
  q$photo[ q$legislature == "14" ] = paste0("http://legxiv.camera.it",
                                            q$photo[ q$legislature == "14" ])
  q$photo[ q$legislature == "15" ] = paste0("http://legxv.camera.it",
                                            q$photo[ q$legislature == "15" ])
  
  q$photo_url = q$photo
  
  write.csv(q, cam, row.names = FALSE)
  
}

q = read.csv(cam, stringsAsFactors = FALSE)

# download photos (run a couple of times to solve network errors)
for(i in unique(q$photo_url[ grepl("^http", q$photo) ])) {
  
  j = paste(q$legislature[ q$photo_url == i ], q$name[ q$photo_url == i ])
  j = paste0("photos_ca/", gsub("(_)+", "_", gsub("\\s|'", "_", tolower(j))), ".jpg")
  j = gsub("_\\.", ".", j)
  
  if(!file.exists(j))
    try(download.file(i, j, mode = "wb", quiet = TRUE), silent = TRUE)
  
  if(file.info(j)$size)
    q$photo[ q$photo_url == i ] = j
  else {
    q$photo[ q$photo_url == i ] = NA
    file.remove(j)
  }
  
}

# buggy profile (no photo so leave after photo loop to get photo URLs erased)
q$name[ grepl("ZAPPIA Leone Pietro Antonio", q$url) ] = "ZAPPIA Leone Pietro Antonio"
q$party[ grepl("ZAPPIA Leone Pietro Antonio", q$url) ] = "L'Ulivo" # missing from scraped data
q$photo[ grepl("ZAPPIA Leone Pietro Antonio", q$url) ] = NA
q$photo_url[ grepl("ZAPPIA Leone Pietro Antonio", q$url) ] = NA

q$uid = NA
q$uid[ q$legislature == "13" ] = gsub("(.*)(id=)(d)(.*)", "\\3\\4", q$url[ q$legislature == "13" ])
q$uid[ q$legislature != "13" ] = gsub("(.*)(deputato=)(d)(\\d+)(.*)", "\\3\\4", q$url[ q$legislature != "13" ])
q$uid = paste0(q$legislature, q$uid)

# bugfix for 'FrontPage'-style sponsors
q$uid[ q$url == "http://leg13.camera.it/cartellecomuni/deputati/composizione/leg13/Composizione/schede_/d00583.asp" ] = "13d00583"

s = read.csv(sponsors, stringsAsFactors = FALSE)
s = subset(s, grepl("CAM\\.DEP", id))

s$uid = NA
s$uid[ is.na(s$name) ] = gsub("/loc/link.asp\\?tipodoc=CAM.DEP&leg=|&id", "", s$id[ is.na(s$name) ])
s$uid = gsub("=(d)?", "d", s$uid)

# final MP sponsors

dep = merge(q, s[, c("uid", "id") ], by = "uid", all = FALSE)
dep = dep[, names(s)[ !names(s) %in% c("party_url", "uid") ] ]
names(dep)[ which(names(dep) == "id") ] = "url"

cat(sum(s$id %in% dep$url), "identified old MPs", sum(!s$id %in% dep$url), "missing\n")

# parties
dep$party_full = dep$party # back up full party name
dep$party[ dep$party == "ALLEANZA NAZIONALE" ] = "Alleanza Nazionale"
dep$party[ dep$party == "FORZA ITALIA" ] = "Forza Italia"
dep$party[ dep$party == "ITALIA DEI VALORI" ] = "Italia dei Valori"
dep$party[ dep$party == "I Democratici - L'Ulivo" ] = "I Democratici"
dep$party[ dep$party == "MISTO (MPA-MOVIMENTO PER L'AUTONOMIA)" ] = "Movimento per l'Autonomia"
dep$party[ dep$party == "MISTO (MINORANZE LINGUISTICHE)" ] = "linguistic minorities"
dep$party[ grepl("LEGA NORD", dep$party, ignore.case = TRUE) ] = "Lega Nord"

# breakup of L'Ulivo and L'Unione parties
dep$party[ dep$party %in% c("MISTO (VERDI-L'UNIONE)", "VERDI") ] = "Verdi" # Federazione dei Verdi
dep$party[ grepl("DEMOCRATICI DI SINISTRA", dep$party, ignore.case = TRUE) ] = "Democratici di Sinistra" # l. 13-14
dep$party[ grepl("SINISTRA DEMOCRATICA", dep$party, ignore.case = TRUE) ] = "Sinistra Democratica" # l. 15
dep$party[ dep$party %in% c("L'ULIVO", "PARTITO DEMOCRATICO-L'ULIVO") ] = "L'Ulivo" # l. 15 -- ULIV-PD
dep$party[ grepl("^MARGHERITA", dep$party) ] = "Margherita"
dep$party[ grepl("POPOLARI|UDEUR|Unione Democratici per l'Europa", dep$party, ignore.case = TRUE) ] =  "Popolari-UDEUR"

# two small coalitions with Nuovo PSI in both (one DCA-NPSI, one mixed)
dep$party[ dep$party == "MISTO (LIBERAL-DEMOCRATICI, REPUBBLICANI, NUOVO PSI)" ] = 
  "Liberal-Democratici, Repubblicani e Nuovo PSI" # l. 14, allied to FI
dep$party[ dep$party == "DCA-DEMOCRAZIA CRISTIANA PER LE AUTONOMIE-NUOVO PSI" ] = 
  "Democrazia Cristiana per le Autonomie e Nuovo PSI" # l. 15, allied to FI

# Socialists/Radicals coalition
dep$party[ dep$party %in% c("SOCIALISTI E RADICALI-RNP", "MISTO (LA ROSA NEL PUGNO)") ] = "Rosa nel Pugno" # l. 14-15

# PdCI, coded as PdCI in l. 15, coded as Misto PdCI in l. 14
dep$party[ grepl("COMUNISTI ITALIANI", dep$party, ignore.case = TRUE) ] = "P. Comunisti Italiani"
# PRC, coded as PRC in l. 14 and PRC - Sinistra Europea in l. 15
dep$party[ grepl("(RIFONDAZIONE )?COMUNISTA", dep$party, ignore.case = TRUE) ] = "P. Rifondazione Comunista" # PRC

# CCD-CDU in l. 14, UDC afterwards
dep$party[ grepl("DEMOCRATICI CRISTIANI", dep$party, ignore.case = TRUE) & 
             gsub("(.*)&leg=(\\d+)&(.*)", "\\2", dep$url) == "14" ] = "CCD-CDU: Biancofiore"
dep$party[ grepl("DEMOCRATICI CRISTIANI", dep$party, ignore.case = TRUE) ] = "Unione di Centro"

# Residuals: Ecologisti Democratici (n = 4), La Destra (n = 4), Movimento Repubblicani Europei (n = 3)
dep$party[ grepl("^MISTO", dep$party, ignore.case = TRUE) ] = "mixed or minor group" # residuals -- leave at end
table(dep$party, exclude = NULL)

# print(table(dep$party, gsub("(.*)&leg=(\\d+)&(.*)", "\\2", dep$url), exclude = NULL))

# number of groups per legislature
# tapply(dep$party, gsub("(.*)leg=(\\d+)(.*)", "\\2", dep$url), dplyr::n_distinct)

# remove unused photos (if you need to reduce the size of the photos_ca folder)
file.remove(q$photo[ !q$photo %in% dep$photo ])

# fix order and particles in MP names
dep$name = gsub("d'\\s", "D'", dep$name)
dep$name = gsub("di\\s", "DI ", dep$name)
dep$name = gsub("de\\s", "DE ", dep$name)
dep$name = sapply(dep$name, function(i) {
  j = unlist(strsplit(i, " "))
  k = c()
  for(jj in j) {
    if(!grepl("[a-z]", jj))
      k = c(k, jj)
  }
  j = j[ !j %in% k ]
  return(paste(paste0(j, collapse = " "), paste0(k, collapse = " ")))
})

# missing genders
dep$sex[ dep$url == "/loc/link.asp?tipodoc=CAM.DEP&leg=13&id=d00283" ] = "M" # Pasquale GIULIANO
dep$sex[ dep$url == "/loc/link.asp?tipodoc=CAM.DEP&leg=13&id=d00640" ] = "F" # Grazia SESTINI

write.csv(dep[, c("url", "name", "sex", "born", "party", "mandate", "photo", "circo") ],
          "data/deputati-old.csv", row.names = FALSE)
