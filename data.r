# http://stackoverflow.com/a/6364905/635806
simpleCap <- function(x) {
  s = strsplit(x, "\\s")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

yrs = c(
  "1" = "1948-1953",
  "2" = "1953-1958",
  "3" = "1958-1963",
  "4" = "1963-1968",
  "5" = "1968-1972",
  "6" = "1972-1976",
  "7" = "1976-1979",
  "8" = "1979-1983",
  "9" = "1983-1987",
  "10" = "1987-1992",
  "11" = "1992-1994",
  "12" = "1994-1996",
  "13" = "1996-2001",
  "14" = "2001-2006",
  "15" = "2006-2008",
  "16" = "2008-2013",
  "17" = "2013-2018"
)

cat("Parsing data for the Camera...\n")
source("data-ca.r")

a_ca = a
s_ca = s

cat("Parsing data for the Senato...\n")
source("data-se.r")

a_se = a
s_se = s

a = bind_rows(
  cbind(chamber = "ca", a_ca, stringsAsFactors = FALSE),
  cbind(chamber = "se", a_se, stringsAsFactors = FALSE)
)

# table(a$legislature, a$chamber)

s = bind_rows(
  cbind(chamber = "ca", s_ca, stringsAsFactors = FALSE),
  cbind(chamber = "se", s_se, stringsAsFactors = FALSE)
)

# table(s$legislature, s$chamber)
