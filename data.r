# in Italy, everything is both old and new, and more complex than elsewhere

root = "http://www.senato.it"
sponsors = "data/parlamentari.csv"
bills = "data/ddl.csv"

# legislatures
rom = c("I" = 1, "II" = 2, "III" = 3, "IV" = 4, "V" = 5, "VI" = 6, "VII" = 7,
        "VIII" = 8, "IX" = 9, "X" = 10, "XI" = 11, "XII" = 12, "XIII" = 13,
        "XIV" = 14, "XV" = 15, "XVI" = 16)

source("data-bills.r")    # scrapes ~ 37,000 bills
source("data-se.r") # scrapes ~ 1,600 senator details (sponsors only)
source("data-ca-old.r") # scrapes ~ 1,900 MP details from legislatures 13-15 (all of them)
source("data-ca-new.r") # scrapes ~ 1,200 MP details from legislatures 16-17 (sponsors only)

# reload and finalize bills dataset

b = read.csv(bills, stringsAsFactors = FALSE)

# removes bothing once all bills are scraped
b = subset(b, !is.na(prima))

# remove 41 bills (out of over 37,000) without correct first author identification
b = subset(b, !prima %in% paste0("/loc/link.asp?tipodoc=CAM.DEP&leg=", 13:17, "&id="))

# remove 15 bills from a first author without a profile page
b = subset(b, prima != "/loc/link.asp?tipodoc=CAM.DEP&leg=15&id=50433")

s = rbind(read.csv("data/deputati-old.csv", stringsAsFactors = FALSE),
          read.csv("data/deputati-new.csv", stringsAsFactors = FALSE))

# assign party abbreviations
s$partyname = s$party
s$party = as.character(parties[ s$party ])

# legislature minus previous mandates, times mandate length
s$nyears = as.numeric(gsub("(.*)&leg=(\\d+)(.*)", "\\2", s$url))
for(i in 1:nrow(s)) {
  ii = as.numeric(unlist(strsplit(s$mandate[i], ";")))
  s$nyears[ i ] = 5 * sum(ii < s$nyears[i])
}

write.csv(s, "data/deputati.csv", row.names = FALSE)

# check all sponsors are recognized
a = na.omit(unique(c(unlist(strsplit(b$prima, ";")), unlist(strsplit(b$cofirm, ";")))))
table(a %in% c(sen$url, s$url)) # FALSE should be below 10

# cosponsors that failed to scrape (first authors debugged a few lines above)
buggy = a[ !a %in% c(sen$url, s$url) ]
buggy = buggy[ !grepl("id=(;|$)", buggy) ] # only a handful of cases
buggy = subset(b, grepl("id=(;|$)", prima) | grepl("id=(;|$)", cofirm))

# number of bills with issues < 1% after all debugging steps
nrow(buggy) / nrow(b[ !is.na(b$prima), ])

b$n_a = b$n_au + b$n_co
b$date = unlist(str_extract(b$date, "[0-9]{4}"))

# finally done