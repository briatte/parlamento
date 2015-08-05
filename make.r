# hi Italy

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"      , showWarnings = FALSE)
dir.create("photos_ca" , showWarnings = FALSE)
dir.create("photos_se" , showWarnings = FALSE)
dir.create("plots"     , showWarnings = FALSE)

dir.create("raw"            , showWarnings = FALSE)
dir.create("raw/bill-lists" , showWarnings = FALSE)
dir.create("raw/bill-pages" , showWarnings = FALSE)
dir.create("raw/mp-lists"   , showWarnings = FALSE)
dir.create("raw/mp-pages"   , showWarnings = FALSE)

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

# have a nice day
