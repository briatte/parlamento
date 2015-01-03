# party colors

colors = c(
  "PRC" = "#E41A1C", # red
  "PCI" = "#FB8072", # light red, Camera only
  "SINDEM" = "#FCCDE5", # coalition, Camera only, light pink (l. 15 only)
  "SEL" = "#4DAF4A", # Greens, green, Camera l. 16-17 only
  "VERD/PCI" = "#FB8072", # FdV + PdCI, light red, Senato only
  "VERD" = "#4DAF4A", # Greens, green, Camera l. 13-15 only
  "DEMSIN" = "#B3DE69", # Senato l. 13-14 only, light green/olive
  "M5S" = "#FFFF33", # yellow, Camera and Senato l. 17
  "ULIV-PD" = "#B3DE69", # Senato l. 15 (L'Ulivo), light green/olive
  # "Partito Democratico" = "#F781BF", # PD (2007-, leg. 16-17), pink
  "AUT-PSI-MAIE" = "#F781BF", # Senato only, pink
  "RNP" = "#F781BF", # Camera only, pink
  "MARGH" = "#FDB462", # Senato only, centre-left, light orange
  "ID" = "#80B1D3", # Camera l. 13 only, light blue
  "PPI" = "#FFFFB3", # PPI, Senato only, light yellow (l. 13 only)
  "IDV" = "#FF7F00", # orange, centrist (Mani Pulite)
  "RINNOV" = "#FCCDE5", # Senato only, allied to L'Ulivo, light pink (l. 13 only)
  "UDEUR" = "#8DD3C7", # teal, Camera l. 13-15 only
  "SVP" = "#E5D8BD", # very light brown, Senato l. 16 only
  "PERLIT" = "#8DD3C7", # PI, teal, Senato only
  "UDC-SVP-MPA" = "#E5D8BD", # very light brown, Senato l. 16 only
  "CCD" = "#80B1D3",     # Senato l. 13                     -- light blue
  "CCD-CDU" = "#80B1D3", # Senato l. 14, followed by UDC    -- light blue
  "SC" = "#053061",      # Monti, both chambers l. 17       -- dark blue
  "CD" = "#FDB462",      # Camera l. 16-17 only             -- light orange
  "UDC" = "#80B1D3",     # Senato l. 15, follows CCD-CDU    -- light blue
  "CDU" = "#80B1D3",     # Senato l. 13                     -- teal
  "MPA" = "#E5D8BD", # Camera l. 15 and Senato l. 15, very light brown
  "GAL" = "#E5D8BD", # GAL coalition, Senato l. 17 only, very light brown
  "FI-PDL" = "#377EB8", # blue, Camera l. 13-15 and Senate
  # "Il Popolo della Libertà" = "#377EB8", # blue, Camera l. 16-17
  "NCD" = "#BEBADA", # NCD, Senato only, light purple
  "FRAT" = "#8DD3C7", # teal, Camera 16-17 only
  "LN" = "#984EA3", # purple
  "AN" = "#A65628", # brown, post-fascist
  "MIN" = "#444444",# Camera l. 14-15 only, dark grey
  "IND" = "#AAAAAA" # mixed, light grey
)

# party abbreviations

parties = c(
  "Alleanza Nazionale" = "AN",         # both chambers
  "Autonomie, PSI e MAIE" = "AUT-PSI-MAIE",# Senato l. 17
  "Centro Democratico" = "CD",         # Camera
  "Centro Cristiano Democratico" = "CCD", # Senato l. 13
  "Cristiani Democratici Uniti" = "CDU",  # Senato l. 13
  "CCD-CDU: Biancofiore" = "CCD-CDU",     # Senato l. 14
  "Democratici di Sinistra" = "DEMSIN",   # Senato l. 13-14
  "Forza Italia" = "FI-PDL",           # both chambers -- grouped with PdL
  "Fratelli d'Italia" = "FRAT",        # Camera
  "Grandi Autonomie e Libertà" = "GAL", # Senato
  "I Democratici" = "ID",               # Camera l. 13
  "Il Popolo della Libertà" = "FI-PDL", # Camera -- grouped with FI
  "Italia dei Valori" = "IDV",         # both chambers
  "linguistic minorities" = "MIN",     # Camera l. 14-15
  "L'Ulivo" = "ULIV-PD",               # both chambers -- grouped with PD
  "Lega Nord" = "LN",                  # both chambers
  "Movimento 5 Stelle" = "M5S",        # both chambers
  "Movimento per l'Autonomia" = "MPA", # both chambers
  "Movimento per le Autonomie" = "MPA",# both chambers
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
  "Rosa nel Pugno" = "RNP",            # Camera 14-15
  "Scelta Civica" = "SC",              # both chambers
  "Sinistra Democratica" = "SINDEM",   # Camera
  "Sinistra Ecologia Libertà" = "SEL", # Camera
  "Südtiroler Volkspartei" = "SVP",    # Camera l. 16-17
  "Unione di Centro" = "UDC",          # both chambers
  "UDC, SVP e Autonomie" = "UDC-SVP-MPA",# Senato l. 16
  "Verdi" = "VERD",                    # Camera
  "Verdi e Communisti" = "VERD/PCI"    # Senato
)

# ParlGov Left/Right scores

scores = c(
  "PRC" = 0.9,
  "PCI" = 1, # PdCI
  "SINDEM" = 1.3,
  "SEL" = 1.3,
  "VERD/PCI" = 1.7, # FdV + PdCI / 2
  "VERD" = 2.4, # FdV
  "DEMSIN" = 2.6,
  "M5S" = 2.6,
  "ULIV-PD" = 3,
  "AUT-PSI-MAIE" = 3.4,
  "RNP" = 4, 
  "MARGH" = 4, # Senato l. 14
  "ID" = 4.1,  # Camera l. 13 only
  "PPI" = 4.6, 
  "IDV" = 4.9,
  "RINNOV" = 5,
  "UDEUR" = 5.3,
  "SVP" = 5.4,
  "PERLIT" = 5.6, # Senato l. 17
  "UDC-SVP-MPA" = 5.9,
  "CCD" = 5.9, # Senato l. 13
  "CCD-CDU" = 5.9, # Senato l. 13-14
  "SC" = 6,
  "CD" = 6,
  "UDC" = 6.1,
  "CDU" = 6.2,
  "MPA" = 6.2,
  "GAL" = 6.5,
  "FI-PDL" = 7.1,
  "NCD" = 7.4,
  "FRAT" = 7.4,
  "LN" = 7.8,
  "AN" = 8.1,
  "MIN" = Inf, # missing
  "IND" = Inf
)

stopifnot(parties %in% names(colors))
stopifnot(parties %in% names(scores))

stopifnot(names(colors) == names(scores))
order = names(colors)[ order(scores) ]
