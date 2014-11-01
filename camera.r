
cam = "data/camera.csv"
if(!file.exists(cam)) {
  
  q = data.frame()
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
        
        t = data.frame(name = scrubber(xpathSApply(f, "//table//tr/td[1]", xmlValue)),  # name
                       party = scrubber(xpathSApply(f, "//table//tr/td[2]", xmlValue)), # party
                       stringsAsFactors = FALSE)
        
        # fix empty row on legislature 13 letter M
        t = subset(t, name != "MANISCO Lucio (non in carica)")
        
        # add URL
        t$url = paste0(leg, xpathSApply(f, "//table//tr/td[1]/a/@href"))
        
        ## t = t[ !t[, 3] %in% r, ]
        
        cat(":", sprintf("%3.0f", nrow(t)), "MPs\n")
        
        p = data.frame()
        
        for(i in rev(t$url)) {
          
          # cat(str_pad(str_extract(leg, "13|xiv|xv"), 4, "right"), which(t[, 3] == i))
          h = try(htmlParse(gsub("\\s", "%20", i)), silent = TRUE)
          
          if(!"try-error" %in% class(h)) {
            
            # name = scrubber(xpathSApply(h, "//div[@id='innerContentColumn']//strong[1]", xmlValue))
            photo = xpathSApply(h, "//img[contains(@src, '.jpg')]/@src")
            if(!length(photo))
              photo = NA
            
            born = xpathSApply(h, "//div[@id='innerContentColumn']//p[2]", xmlValue)
            
            # fix leg. XIII
            if(!length(born))
              born = xpathSApply(h, "//div[@id='innerContentColumn']//div[2]", xmlValue)
            
            sex = ifelse(grepl("Nata", born), "F", "M")
            born = str_extract(born, "[0-9]{4}")
            
            # party_url = xpathSApply(h, "//div[@id='innerContentColumn']//a[contains(@href, 'Gruppo')]/@href")
            # party = xpathSApply(h, "//div[@id='innerContentColumn']//a[contains(@href, 'Gruppo')]", xmlValue)
            
            if(length(born)) {
              
              # cat(":", t[ which(t[, 3] == i), 1], "\n")
              
              p = rbind(p, data.frame(
                name =  t$name[ which(t$url == i) ],
                party = t$party[ which(t$url == i) ],
                url =   t$url[ which(t$url == i) ],
                sex, born, photo, stringsAsFactors = FALSE)) # party, party_url
              
            } else {
              
              # a few pages are written in Frontpage-style HTML code
              # cat(":", t[ which(t[, 3] == i), 1], "failed (different code)\n")
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
  
  # pages that failed to scrape )n = 2)
  
  cat(length(diff), "MPs failed to scrape\n")
  
  # finalize dataset
  
  str(q)
  q$name = gsub("( )?\\((non in carica|deceduto|fino al (.*))\\)", "", q$name)
  # subset(q, grepl("\\(", name))
  
  # legislature
  
  q$party[ q$party == "ALLEANZA NAZIONALE" ] = "Alleanza Nazionale"
  q$party[ q$party == "DEMOCRATICI DI SINISTRA-L'ULIVO" ] = "Democratici di Sinistra - L'Ulivo"
  q$party[ q$party == "MISTO" ] = "Misto"
  q$party[ q$party == "LEGA NORD PADANIA" ] = "Lega Nord Padania"
  
  # legislature
  
  q$legislature = NA
  q$legislature[ grepl("leg13", q$url) ]  = "13"
  q$legislature[ grepl("legxiv", q$url) ] = "14"
  q$legislature[ grepl("legxv", q$url) ]  = "15"
  
  # photo
  
  q$photo[ q$legislature == "13" ] = paste0("http://leg13.camera.it/cartellecomuni/leg13/Deputati/scheda_deputato/",
                                            q$photo[ q$legislature == "13" ])
  q$photo[ q$legislature == "14" ] = paste0("http://legxiv.camera.it",
                                            q$photo[ q$legislature == "14" ])
  q$photo[ q$legislature == "15" ] = paste0("http://legxv.camera.it",
                                            q$photo[ q$legislature == "15" ])
  
  # remove two perfect duplicates
  # q = unique(q)
  
  # download photos (run a couple of times to solve network errors)
  
  q$photo_url = q$photo
  for(i in unique(q$photo_url)) {
    
    j = paste(q$legislature[ q$photo_url == i ], q$name[ q$photo_url == i ])
    j = paste0("photos_ca/", gsub("(_)+", "_", gsub("\\s|'", "_", tolower(j))), ".jpg")
    
    if(!file.exists(j))
      download.file(i, j, mode = "wb", quiet = TRUE)
    
    if(file.info(j)$size)
      q$photo[ q$photo_url == i ] = j
    else {
      q$photo[ q$photo_url == i ] = NA
      file.remove(j)
    }
    
  }
  
  write.csv(q, cam, row.names = FALSE)
  
}

q = read.csv(cam, stringsAsFactors = FALSE)
q$party_url = NA

q$uid = NA
q$uid[ q$legislature == "13" ] = gsub("(.*)(id=)(d)(.*)", "\\3\\4", q$url[ q$legislature == "13" ])
q$uid[ q$legislature != "13" ] = gsub("(.*)(deputato=)(d)(\\d+)(.*)", "\\3\\4", q$url[ q$legislature != "13" ])
q$uid = paste0(q$legislature, q$uid)

s = read.csv(sponsors, stringsAsFactors = FALSE)
s = subset(s, grepl("CAM\\.DEP", id))

s$uid = NA
s$uid[ is.na(s$name) ] = gsub("/loc/link.asp\\?tipodoc=CAM.DEP&leg=|&id", "", s$id[ is.na(s$name) ])
s$uid = gsub("=(d)?", "d", s$uid)

# final MP sponsors

dep = merge(q, s[, c("uid", "id") ], by = "uid", all = FALSE)
dep = dep[, names(s)[ !names(s) %in% c("party_url", "uid") ] ]
names(dep)[ which(names(dep) == "id") ] = "url"

cat(sum(s$id %in% dep$url), "identified MPs", sum(!s$id %in% dep$url), "missing\n")

dep$party_full = dep$party # back up full party name
dep$party[ dep$party == "" ] = "Misto"
dep$party[ dep$party == "FORZA ITALIA" ] = "Forza Italia"
dep$party[ dep$party == "ITALIA DEI VALORI" ] = "Italia dei Valori"
dep$party[ grepl("POPOLARI|UDEUR|Unione Democratici per l'Europa", dep$party, ignore.case = TRUE) ] =  "Popolari-UDEUR"
dep$party[ grepl("COMUNISTI ITALIANI", dep$party, ignore.case = TRUE) ] = "P. Comunisti Italiani" # incl. Misto (PCI)
dep$party[ dep$party == "SOCIALISTI E RADICALI-RNP" ] = "Socialisti e Radicali" # coalition
dep$party[ dep$party == "VERDI" ] = "Verdi" # Federazione dei Verdi
dep$party[ grepl("Lega Nord", dep$party, ignore.case = TRUE) ] = "Lega Nord"
dep$party[ grepl("L'ULIVO", dep$party, ignore.case = TRUE) ] = "L'Ulivo" # coalition
dep$party[ grepl("(RIFONDAZIONE )?COMUNISTA", dep$party, ignore.case = TRUE) ] = "P. Rifondazione Comunista" # PRC
dep$party[ grepl("DEMOCRATICI CRISTIANI", dep$party, ignore.case = TRUE) ] = "Unione di Centro"
dep$party[ grepl("SINISTRA DEMOCRATICA", dep$party, ignore.case = TRUE) ] = "Sinistra Democratica" # coalition
dep$party[ grepl("^MISTO|^DCA(.*)PSI$", dep$party, ignore.case = TRUE) ] = "Misto" # residuals -- leave at end

# print(table(dep$party, gsub("(.*)&leg=(\\d+)&(.*)", "\\2", dep$url), exclude = NULL))

# number of groups per legislature
# tapply(dep$party, gsub("(.*)leg=(\\d+)(.*)", "\\2", dep$url), dplyr::n_distinct)

write.csv(dep, "data/deputati.csv", row.names = FALSE)
