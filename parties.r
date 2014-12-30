# party colors

colors = c(
  "PRC" = "#E41A1C", # red
  "SEL" = "#4DAF4A", # Greens, green, Camera l. 16-17 only
  "PCI" = "#FB8072", # light red, Camera only
  "VERD/COM" = "#FB8072", # Red-Green coalition, light red, Senato only
  "VERD" = "#4DAF4A", # Greens, green, Camera l. 13-15 only
  "SINDEM" = "#FCCDE5", # coalition, Camera only, light pink (l. 15 only)
  "M5S" = "#FFFF33", # yellow, Camera and Senato l. 17
  "ULIV-PD" = "#B3DE69", # light green/olive
  # "Partito Democratico" = "#F781BF", # PD (2007-, leg. 16-17), pink
  "SOC/RAD" = "#F781BF", # Camera only, pink
  "MARGH" = "#FDB462", # Senato only, centre-left, light orange
  "PPI" = "#FFFFB3", # PPI, Senato only, light yellow (l. 13 only)
  "IDV" = "#FF7F00", # orange, centrist (Mani Pulite)
  "RINNOV" = "#FCCDE5", # Senato only, allied to L'Ulivo, light pink (l. 13 only)
  "UDEUR" = "#8DD3C7", # teal, Camera l. 13-15 only
  "PERLIT" = "#8DD3C7", # PI, teal, Senato only
  "SC" = "#053061", # Monti, both chambers l. 17 but small in Senate, dark blue
  "CD" = "#FDB462", # Camera l. 16-17 only, centre-left, light orange
  "UDC" = "#80B1D3", # light blue
  "GAL" = "#E5D8BD", # GAL coalition, Senato only, very light brown
  "FI-PDL" = "#377EB8", # blue, Camera l. 13-15 and Senate
  # "Il Popolo della Libertà" = "#377EB8", # blue, Camera l. 16-17
  "NCD" = "#BEBADA", # NCD, Senato only, light purple
  "FRAT" = "#8DD3C7", # teal, Camera 16-17 only
  "LN" = "#984EA3", # purple
  "AN" = "#A65628", # brown, post-fascist
  "IND" = "#AAAAAA" # mixed, light grey
)

# party abbreviations

parties = c(
  "Alleanza Nazionale" = "AN",         # both chambers
  "Centro Democratico" = "CD",         # Camera
  "Forza Italia" = "FI-PDL",           # both chambers -- grouped with PdL
  "Fratelli d'Italia" = "FRAT",        # Camera
  "Grandi Autonomie e Libertà" = "GAL", # Senato
  "Il Popolo della Libertà" = "FI-PDL", # Camera -- grouped with FI
  "Italia dei Valori" = "IDV",         # both chambers
  "L'Ulivo" = "ULIV-PD",               # both chambers -- grouped with PD
  "Lega Nord" = "LN",                  # both chambers
  "M5S" = "M5S",                       # both chambers
  "Margherita" = "MARGH",              # Senato
  "Misto" = "IND",                     # both chambers
  "Nuovo Centrodestra" = "NCD",        # Senato
  "P. Comunisti Italiani" = "PCI",     # Camera
  "P. Rifondazione Comunista" = "PRC", # both chambers
  "Partito Democratico" = "ULIV-PD",   # both chambers -- grouped with PD
  "Partito Popolare Italiano" = "PPI", # Senato
  "Popolari-UDEUR" = "UDEUR",          # Camera
  "Per l'Italia" = "PERLIT",           # Senato
  "Rinnovamento Italiano" = "RINNOV",  # Senato
  "Scelta Civica con Monti" = "SC",    # both chambers
  "Sinistra Democratica" = "SINDEM",   # Camera
  "Sinistra Ecologia Libertà" = "SEL", # Camera
  "Socialisti e Radicali" = "SOC/RAD", # Camera
  "Unione di Centro" = "UDC",          # both chambers
  "Verdi" = "VERD",                    # Camera
  "Verdi e Communisti" = "VERD/COM"    # Senato
)

# ParlGov Left/Right scores

scores = c(
  "PRC" = 0.9,
  "SEL" = 1.3,
  "PCI" = 1.6,
  "VERD/COM" = 2,
  "VERD" = 2.6,
  "SINDEM" = 2.6,
  "M5S" = 2.6,
  "ULIV-PD" = 2.75,
  "SOC/RAD" = 4, 
  "MARGH" = 4,
  "PPI" = 4.6, 
  "IDV" = 4.9,
  "RINNOV" = 5,
  "UDEUR" = 5.3,
  "PERLIT" = 5.76,
  "SC" = 6,
  "CD" = 6,
  "UDC" = 6.1,
  "GAL" = 7,
  "FI-PDL" = 7.1,
  "NCD" = 7.4,
  "FRAT" = 7.4,
  "LN" = 7.8,
  "AN" = 8.1,
  "IND" = Inf
)

stopifnot(names(colors) == names(scores))
order = names(colors)[ order(scores) ]
