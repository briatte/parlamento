# hi Italy

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"      , showWarnings = FALSE)
dir.create("photos_ca" , showWarnings = FALSE)
dir.create("photos_se" , showWarnings = FALSE)
dir.create("plots"     , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

# have a nice day
