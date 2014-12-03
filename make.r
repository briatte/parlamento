# hi Italy

# WARNING - this script will get your IP address banned from accessing the
# Italian Senate website. You will need to use a VPN to run without issues.

# SOURCES
# http://www.senato.it/  : all bills and senators
# http://www.camera.it/  : used for MPs only
# http://dati.camera.it/ : not used (additional RDF/XML dumps)

# networks
library(ggplot2)
library(GGally)
library(grid)
library(igraph)
library(network)
library(plyr)
library(qdap)
library(rgexf)
library(sna)
library(stringr)
library(tnet)
library(XML)

dir.create("data", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)
dir.create("photos_ca", showWarnings = FALSE)
dir.create("photos_se", showWarnings = FALSE)

root = "http://www.senato.it"
sponsors = "data/parlamentari.csv"
bills = "data/ddl.csv"

plot = TRUE
gexf = TRUE

# let it be said, Italian political parties are a fucking mess
colors = c(
  "P. Rifondazione Comunista" = "#E41A1C", # red
  "P. Comunisti Italiani" = "#FB8072", # light red, Camera only
  "Verdi e Communisti" = "#FB8072", # Red-Green coalition, light red, Senato only
  "Partito Democratico" = "#F781BF", # PD (2007-, leg. 16-17), pink
  "Verdi" = "#4DAF4A", # Greens, green, Camera l. 13-15 only
  "Sinistra Ecologia Libertà" = "#4DAF4A", # Greens, green, Camera l. 16-17 only
  "Sinistra Democratica" = "#FCCDE5", # coalition, Camera only, light pink (l. 15 only)
  "L'Ulivo" = "#B3DE69", # light green
  "Rinnovamento Italiano" = "#FCCDE5", # Senato only, allied to L'Ulivo, light pink (l. 13 only)
  "Socialisti e Radicali" = "#F781BF", # Camera only, pink
  "Margherita" = "#FDB462", # Senato only, centre-left, light orange
  "Centro Democratico" = "#FDB462", # Camera l. 16-17 only, centre-left, light orange
  "Unione di Centro" = "#80B1D3", # light blue
  "Popolari-UDEUR" = "#8DD3C7", # teal, Camera l. 13-15 only
  "Scelta Civica con Monti" = "#053061", # Monti, both chambers l. 17 but small in Senate, dark blue
  "Italia dei Valori" = "#FF7F00", # orange, centrist (Mani Pulite)
  "Nuovo Centrodestra" = "#BEBADA", # NCD, Senato only, light purple
  "Partito Popolare Italiano" = "#FFFFB3", # PPI, Senato only, light yellow (l. 13 only)
  "Per l'Italia" = "#8DD3C7", # PI, teal, Senato only
  "Fratelli d'Italia" = "#8DD3C7", # teal, Camera 16-17 only
  "Grandi Autonomie e Libertà" = "#E5D8BD", # GAL coalition, Senato only, very light brown
  "Forza Italia" = "#377EB8", # blue, Camera l. 13-15 and Senate
  "Il Popolo della Libertà" = "#377EB8", # blue, Camera l. 16-17
  "Alleanza Nazionale" = "#A65628", # brown, post-fascist
  "Lega Nord" = "#984EA3", # purple
  "M5S" = "#FFFF33", # yellow, Camera and Senato l. 17
  "Misto" = "#AAAAAA" # mixed, light grey
)
order = names(colors)

source("ddl.r")    # scrapes ~ 37,000 bills
source("senato.r") # scrapes ~ 1,600 senator details (sponsors only)
source("camera-old.r") # scrapes ~ 1,900 MP details from legislatures 13-15 (all of them)
source("camera-new.r") # scrapes ~ 1,200 MP details from legislatures 16-17 (sponsors only)
source("build.r")  # build the networks

# have a nice day
