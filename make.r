# hi Italy

# WARNING - the scripts might get your IP address banned from accessing the
# Italian Senate website. You might need to use a VPN to run without issues.

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
  "Partito Democratico" = "#F781BF", # PD (2007-, leg. 16-17), Senato only (for now), pink
  "Verdi" = "#4DAF4A", # Greens, green, Camera only
  "Sinistra Democratica" = "#FCCDE5", # coalition, Camera only, light pink (1. 15 only)
  "L'Ulivo" = "#B3DE69", # light green
  "Rinnovamento Italiano" = "#FCCDE5", # Senato only, allied to L'Ulivo, light pink (l. 13 only)
  "Socialisti e Radicali" = "#F781BF", # Camera only, pink
  "Margherita" = "#FDB462", # Senato only, centre-left, light orange
  "Unione di Centro" = "#80B1D3", # light blue
  "Popolari-UDEUR" = "#8DD3C7", # teal, Camera only
  "Italia dei Valori" = "#FF7F00", # orange, centrist (Mani Pulite)
  "Nuovo Centrodestra" = "#BEBADA", # NCD, Senato only, light purple
  "Partito Popolare Italiano" = "#FFFFB3", # PPI, Senato only, light yellow (l. 13 only)
  "Per l'Italia" = "#8DD3C7", # PI, teal, Senato only
  "Grandi Autonomie e Libertà" = "#E5D8BD", # GAL coalition, Senato only, very light brown
  "Forza Italia" = "#377EB8", # blue
  "Alleanza Nazionale" = "#A65628", # brown, post-fascist
  "Lega Nord" = "#984EA3", # purple
  "M5S" = "#FFFF33", # yellow, Senato only (because of missing MP legislatures)
  "Misto" = "#AAAAAA" # mixed, light grey
)
order = names(colors)

# so far, 25% of all MP details (those from legislatures 16-17) are missing

source("ddl.r")    # scrapes ~ 37,000 bills
source("senato.r") # scrapes ~ 1,600 senator details
source("camera.r") # scrapes ~ 1,800 MP details
source("build.r")  # build the networks

# have a nice day
