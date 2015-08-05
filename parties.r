# party colors

colors = c(
  "PRC" = "#E41A1C",          # red
  "PCI" = "#FB8072",          # light red, Camera only
  "SINDEM" = "#FCCDE5",       # coalition, Camera only, light pink
  "SEL" = "#4DAF4A",          # Greens, green, Camera l. 16-17 only
  "VERD-PCI" = "#FB8072",     # FdV + PdCI, light red, Senato only
  "VERD" = "#4DAF4A",         # Greens, green, Camera l. 13-15 only
  "DEMSIN" = "#B3DE69",       # Senato l. 13-14 only, light green/olive
  "M5S" = "#FFFF33",          # Camera and Senato l. 17     -- yellow
  "PD" = "#F781BF",           # Camera and Senato l. 16-17  -- pink
  "ULIV" = "#B3DE69",         # Camera and Senato l. 15     -- light green/olive
  "AUT-PSI-MAIE" = "#FB8072", # Senato l. 17 only, light red
  "RNP" = "#F781BF",          # Camera only, pink
  "MARGH" = "#FDB462",        # Senato only, centre-left, light orange
  "ID" = "#80B1D3",           # Camera l. 13 only, light blue
  "PPI" = "#FFFFB3",          # PPI, Senato only, light yellow (l. 13 only)
  "IDV" = "#FF7F00",          # orange, centrist (Mani Pulite)
  "RINNOV" = "#8DD3C7",       # Senato only, allied to L'Ulivo, teal
  "UDEUR" = "#8DD3C7",        # teal, Camera l. 13-15 only
  "SVP" = "#444444",          # dark grey
  "LD-PRI-NPSI" = "#053061", # dark blue, Camera l. 14 only
  "DCA-NPSI" = "#053061",     # dark blue, Camera l. 15 only
  "UDC-SVP-MPA" = "#E5D8BD",  # very light brown, Senato l. 16 only
  "CCD" = "#80B1D3",          # Senato l. 13                     -- light blue
  "CCD-CDU" = "#80B1D3",      # Senato l. 14, followed by UDC    -- light blue
  "SC" = "#053061",           # Monti, both chambers l. 17       -- dark blue
  "CD" = "#FDB462",           # Camera l. 16-17 only             -- light orange
  "UDC" = "#80B1D3",          # Senato l. 15, follows CCD-CDU    -- light blue
  "CDU" = "#053061",          # Senato l. 13                     -- dark blue
  "MPA" = "#E5D8BD",          # Camera l. 15 and Senato l. 15, very light brown
  "GAL" = "#E5D8BD",          # GAL coalition, Senato l. 17 only, very light brown
  "AL-A" = "#8DD3C7",         # teal
  "NCD-UDC" = "#BEBADA",      # NCD = teal, UDC = light blue -- light purple
  "CRI" = "#053061",          # blue -- dark blue
  "FI-PDL" = "#377EB8",       # blue, FI in l. 13-15 and PdL in l. 16-17
  "FRAT" = "#8DD3C7",         # teal, Camera 16-17 only
  "LN" = "#984EA3",           # purple
  "AN" = "#A65628",           # brown
  "MIN" = "#444444",          # Camera l. 14-15 only, dark grey
  "IND" = "#AAAAAA"           # independent/mixed/minor, light grey
)

# party abbreviations

groups = c(
  "PRC" = "P. Rifondazione Comunista", # C14-15, S13, S15
  "PCI" = "P. Comunisti Italiani",     # C14-15
  "SINDEM" = "Sinistra Democratica",   # C15, S13
  "SEL" = "Sinistra Ecologia Libertà", # C17
  "VERD-PCI" = "Verdi e Communisti",   # S15
  "VERD" = "Verdi",                    # C14-15, S13-14
  "DEMSIN" = "Democratici di Sinistra",   # C13-14, S13-14
  "M5S" = "Movimento 5 Stelle",        # both chambers
  "PD" = "Partito Democratico",        # C16-17, S16-17
  "ULIV" = "L'Ulivo",                  # C15, S15
  "AUT-PSI-MAIE" = "Autonomie, PSI e MAIE", # S17
  "RNP" = "Rosa nel Pugno",            # C14-15
  "MARGH" = "Margherita",              # C14, S14 -- part of L'Ulivo
  "ID" = "I Democratici",               # C13
  "PPI" = "Partito Popolare Italiano", # Senato
  "IDV" = "Italia dei Valori",         # C15, C16, S16
  "RINNOV" = "Rinnovamento Italiano",  # S13
  "UDEUR" = "Popolari-UDEUR",          # C13-15
  "SVP" = "Südtiroler Volkspartei",    # C16-17
  "LD-PRI-NPSI" = "Liberal-Democratici, Repubblicani e Nuovo PSI", # C14 -- allied to FI
  "DCA-NPSI" = "Democrazia Cristiana per le Autonomie e Nuovo PSI", # C15 -- allied to FI
  "UDC-SVP-MPA" = "UDC, SVP e Autonomie", # S16
  "CCD" = "Centro Cristiano Democratico", # S13
  "CCD-CDU" = "CCD-CDU: Biancofiore",     # C14, S14
  "SC" = "Scelta Civica",              # both chambers
  "CD" = "Centro Democratico",              # C17
  "UDC" = "Unione di Centro",          # C15, C16-17, S15
  "CDU" = "Cristiani Democratici Uniti",  # Senato l. 13
  "MPA" = "Movimento per le Autonomie", # C15, S14
  "GAL" = "Grandi Autonomie e Libertà", # Senato
  "AL-A" = "Alleanza Liberalpopolare-Autonomie", # S17 -- split FI
  "NCD-UDC" = "Area Popolare", # S.17 -- mostly NCD-UDC, plus a few SC and M5S
  "CRI" = "Conservatori, Riformisti italiani",   # S17 -- split from FI/PPI, some GAL
  "FI-PDL" = "Forza Italia (Il Popolo della Liberta')", # C14-15, C16-17, S13-17
  "FRAT" = "Fratelli d'Italia",        # C17
  "LN" = "Lega Nord",                  # C13-15, S13-17
  "AN" = "Alleanza Nazionale",         # C15-16, S13-S15
  "MIN" = "linguistic minorities",     # C14-15
  "IND" = "mixed or minor group"      # both chambers
)

# ParlGov Left/Right scores

scores = c(
  "PRC" = 0.9,
  "PCI" = 1,        # PdCI
  "SINDEM" = 1.3,
  "SEL" = 1.3,
  "VERD-PCI" = 1.7, # FdV + PdCI / 2
  "VERD" = 2.4,     # FdV
  "DEMSIN" = 2.6,
  "M5S" = 2.6,
  "PD" = 2.6,
  "ULIV" = 3.3,
  "AUT-PSI-MAIE" = 3.4, # SVP + PSI / 2
  "RNP" = 4, 
  "MARGH" = 4, # Senato l. 14
  "ID" = 4.1,  # Camera l. 13
  "PPI" = 4.6, 
  "IDV" = 4.9,
  "RINNOV" = 5,
  "UDEUR" = 5.3,
  "SVP" = 5.4,
  "LD-PRI-NPSI" = 5.5, # Camera l. 14, LD + PRI + NPSI / 3
  "DCA-NPSI" = 5.8,    # Camera l. 15, DCA + NPSI / 2
  "UDC-SVP-MPA" = 5.9, # UDC + SVP + MPA / 3
  "CCD" = 5.9,         # Senato l. 13
  "CCD-CDU" = 5.9,     # Senato l. 13-14
  "SC" = 6,
  "CD" = 6,
  "UDC" = 6.1,
  "CDU" = 6.2,
  "MPA" = 6.2,
  "GAL" = 6.5,
  "AL-A" = 6.8, # GAL + CRI + FI = (GAL + FI-PDL) / 2
  "NCD-UDC" = 6.8, # NCD (7.4) + UDC (6.1) / 2
  "CRI" = 7.1, # split from FI-PDL, scored identically
  "FI-PDL" = 7.1,
  "FRAT" = 7.4,
  "LN" = 7.8,
  "AN" = 8.1,
  "MIN" = Inf, # missing
  "IND" = Inf
)

stopifnot(names(colors) == names(groups))
stopifnot(names(colors) == names(scores))
