# parse bill pages

# NOTE - the code does not update the current legislature past the date of the
# first scrape, but that should not be too complicated to implement if needed.
# Running on 2014-11-02 returned 4,025 bills for the current legislature (17),
# between 5,000 and 10,000 bills for legislatures 13-16, 37,388 bills in total.

if(!file.exists(bills)) {
  
  b = data.frame()
  
  for(i in 17:13) {
    
    # first page
    h = htmlParse(paste0(root, "/ric/sddl/risultati.do?params.commissioneBoolOp=AND&params.statoDiv=0,0,0,1,0,0,0&des=&params.parlamentareIniziativaBoolOp=AND&selmode=&params.interventiBoolOp=AND&params.campoOrdinamento=dataPresentazione&params.tipoIniziativa=1&params.gruppoTrattazioneBoolOp=AND&params.rows=100&livelloTeseo=&params.teseoTuttiTermini=T&params.relatoriBoolOp=AND&params.start=0&params.legislatura=", i, "&params.tipoFirmatari=1&teseo=&sel=&params.ordinamento=desc"))
    
    # number of pages
    p = xpathSApply(h, "//a[contains(@href, 'params.start')]/@href")
    p = gsub("\\D", "", str_extract(p, "params.start=[0-9]+"))
    p = seq(0, max(as.numeric(p)), 100)
    
    for(j in rev(p)) {
      
      cat("Legislature", i, "page", sprintf("%3.0f", which(p == j)))
      h = htmlParse(paste0(root, "/ric/sddl/risultati.do?params.commissioneBoolOp=AND&params.statoDiv=0,0,0,1,0,0,0&des=&params.parlamentareIniziativaBoolOp=AND&selmode=&params.interventiBoolOp=AND&params.campoOrdinamento=dataPresentazione&params.tipoIniziativa=1&params.gruppoTrattazioneBoolOp=AND&params.rows=100&livelloTeseo=&params.teseoTuttiTermini=T&params.relatoriBoolOp=AND&params.start=", j, "&params.legislatura=", i, "&params.tipoFirmatari=1&teseo=&sel=&params.ordinamento=desc"))
      h = xpathSApply(h, "//div[@id='content']//a[contains(@href, 'sddliter')]/@href")
      
      b = rbind(b, data.frame(legislature = as.character(i), url = h, stringsAsFactors = FALSE))
      cat(":", sprintf("%5.0f", nrow(b)), "bills in total\n")
      
    }
    
  }

  # bills metadata
  b$ref = b$date = b$teseo = b$title = b$prima = b$cofirm = NA
  
  # used to further discriminate between cosponsors and merged bill authors
  # e.g. http://www.senato.it/leg/13/BGT/Schede/Ddliter/13229.htm
  #   or http://www.senato.it/leg/17/BGT/Schede/Ddliter/39637.htm
  b$cofirm_dummy = NA
  
  write.csv(b, bills, row.names = FALSE)
  
}

# parse bill details

b = read.csv(bills, stringsAsFactors = FALSE)

# exclude buggy (empty) pages

b = subset(b, !grepl("id=(19902|18800|18870|18686)", url))

# parse missing pages [ !FIX: remove sample when done ]

j = sample(b$url[ is.na(b$prima) ], 100)
for(i in rev(j)) {
  
  cat(sprintf("%4.0f", which(j == i)), str_pad(i, 47, "right"))
  h = try(htmlParse(paste0(root, i)), silent = TRUE)
  if(!"try-error" %in% class(h)) {
    
    # test whether page is empty
    hh = try(xpathSApply(h, "//title"), silent = TRUE)
    if(!"try-error" %in% class(hh)) {
      
      ref = xpathSApply(h, "//div[@id='content']//h1", xmlValue)
      title = scrubber(xpathSApply(h, "//div[@class='boxTitolo']", xmlValue))
      
      # date is the last legislative status, not introduction date
      date = scrubber(xpathSApply(h, "//div[@class='bordoNero']/table/tr/td[3]/strong", xmlValue))
      kw = scrubber(xpathSApply(h, "//h2[text()='Classificazione TESEO']/following-sibling::p[1]", xmlValue))
      
      # first authors are located in the first div
      # authors of related legislation are located in the second div
      prima = xpathSApply(h, "//div[@class='testoMedium'][1]/a/@href")
      cofirm = xpathSApply(h, "//div[@class='testoSmall']/a/@href")
      
      cofirm01 = xpathSApply(h, "//div[@id='div1Top']/span/strong/a", xmlValue)
      cofirm01 = ifelse(is.null(cofirm01), NA, cofirm01)
      cofirm01 = ifelse(is.na(cofirm01), NA, as.numeric(grepl("Cofirma", cofirm01)))
      
      cat(":", sprintf("%3.0f", length(prima)), "author(s)",
          sprintf("%3.0f", length(cofirm)), "cosponsor(s)\n")
      
      b$ref[ b$url == i ] = ref
      b$title[ b$url == i ] = title
      b$date[ b$url == i ] = date
      b$teseo[ b$url == i ] = ifelse(length(kw), kw, NA)
      b$prima[ b$url == i ] = paste0(prima, collapse = ";")
      b$cofirm[ b$url == i ] = ifelse(is.null(cofirm), NA, paste0(cofirm, collapse = ";"))
      b$cofirm_dummy[ b$url == i ] = cofirm01
      
    } else {
      
      cat(": failed (empty page)\n")
      
    }
    
  } else {
    
    cat(": failed (network error)\n")
    
  }
  
}

# all scraped bills, by legislature
print(table(subset(b, !is.na(prima))$legislature))

cat("\n", sum(!is.na(b$prima)), "bills parsed",
    round(100 * sum(!is.na(b$prima)) / nrow(b), 1), "% of total",
    round(100 * sum(!is.na(b$prima) & !is.na(b$cofirm))/sum(!is.na(b$prima)), 1), "% cosponsored",
    sum(is.na(b$prima)), "left to parse\n\n")

b$n_au = 1 + str_count(b$prima, ";")
b$n_au[ is.na(b$prima) ] = 0
table(b$n_au, exclude = NULL)

b$n_co = 1 + str_count(b$cofirm, ";")
b$n_co[ is.na(b$cofirm) ] = 0
table(b$n_co, exclude = NULL)

table(b$n_au + b$n_co, exclude = NULL)

# less than 1% of bills are ambiguous regarding cosponsors: (38 + 0) / nrow(b)
# all others are either single-authored (FALSE/0 cell), or cosponsored (TRUE/1 cell)
table(b$n_au + b$n_co > 1, b$cofirm_dummy, exclude = NULL)

# only a handful of bills mix senators and MPs in their signatories
b$chamber_dummy = 1
b[ grepl("SATTSEN", b$prima) & grepl("CAM", b$prima), "chamber_dummy" ] = 0   # n = 25
b[ grepl("SATTSEN", b$cofirm) & grepl("CAM", b$cofirm), "chamber_dummy" ] = 0 # n = 1
b[ grepl("SATTSEN", b$prima) & grepl("CAM", b$cofirm), "chamber_dummy" ] = 0  # n = 11
b[ grepl("SATTSEN", b$cofirm) & grepl("CAM", b$prima), "chamber_dummy" ] = 0  # n = 9
table(b$chamber_dummy, exclude = NULL)

# only a handful of bills include members of the government
b$notgov_dummy = 1
b[ grepl("COMPGOV", b$prima), "notgov_dummy" ] = 0  # n = 107
b[ grepl("COMPGOV", b$cofirm), "notgov_dummy" ] = 0 # n = 0
table(b$notgov_dummy, exclude = NULL)

# parliamentary-only bills, sponsored in a single chamber, where cosponsors are 'cofirmatari'
b$sample = b$chamber_dummy & b$cofirm_dummy & b$notgov_dummy
table(b$sample, exclude = NULL)

# final sample: cosponsored bills where all three dummies are true
table((b$n_au + b$n_co > 1) & b$sample, b$legislature, exclude = NULL)

# leave commented out to count percentage of cosponsored bills later
# b$sample = b$sample & (b$n_au + b$n_co > 1)

# proportion of bills cosponsored in each legislature (50-70%)
prop.table(table(b$n_au + b$n_co > 1 & b$sample, b$legislature), 2)

write.csv(b, bills, row.names = FALSE)
