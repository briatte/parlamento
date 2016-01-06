# hi Italy

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

if (file.exists("photos_ca.zip"))
  unzip("photos_ca.zip")

dir.create("photos_ca", showWarnings = FALSE)

if (file.exists("photos_se.zip"))
  unzip("photos_se.zip")

dir.create("photos_se", showWarnings = FALSE)

if (file.exists("raw_ca.zip"))
  unzip("raw_ca.zip")

if (file.exists("raw_se.zip"))
  unzip("raw_se.zip")

dir.create("raw_ca"          , showWarnings = FALSE)
dir.create("raw_ca/mp-bills" , showWarnings = FALSE)
dir.create("raw_ca/mp-lists" , showWarnings = FALSE)

dir.create("raw_se"          , showWarnings = FALSE)
dir.create("raw_se/mp-bills" , showWarnings = FALSE)
dir.create("raw_se/mp-lists" , showWarnings = FALSE)
dir.create("raw_se/mp-pages" , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Italy",
  "lang" = "it", # Wikipedia language for chamber and constituencies
  "ca" = "Camera dei Deputati",
  "se" = "Senato della Repubblica",
  "type-ca" = "Lower",
  "type-se" = "Upper",
  "ipu-ca" = 2157,
  "ipu-se" = 2158,
  "seats-ca" = 630,
  "seats-se" = 322
)

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

save(list = ls(pattern = "^(co)?(net|edges|bills)_it_(ca|se)\\d{4}$"),
     file = "data/net_it.rda")

# have a nice day
