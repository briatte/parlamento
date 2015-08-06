# add committee co-memberships: Camera

raw = data_frame()
sponsors = list.files("raw/mp-pages", pattern = "^dep-", full.names = TRUE)
regex = "//a[contains(@href, 'organiparlamentarism') or contains(@href, 'organiparlamentariSM') or contains(@href, 'shadow_organo_parlamentare')]"

# find unique committees

cat("Parsing Camera committees")
for (i in sponsors) {
  
  h = htmlParse(i)
  n = xpathSApply(h, regex, xmlValue)
  l = xpathSApply(h, paste0(regex, "/@href"))
  n = str_clean(n)
  if (length(l)) # not saving links (vary per legislature)
    raw = rbind(raw, unique(data_frame(i, n, NA)))
  
}
raw = subset(raw, grepl("commi(ssione|tato)|consiglio|giunta|collegio", n, ignore.case = TRUE))

cat(":", nrow(unique(raw[, -1 ])), "unique categories\n")

# save flat list
write.csv(raw[, -1 ] %>%
            arrange(n) %>%
            group_by(n) %>%
            mutate(members = n()) %>%
            unique, "data/committees-ca.csv", row.names = FALSE)

# unique committees, using names
comm = data_frame(n = unique(raw$n))

# add sponsor columns
for (i in sponsors)
  comm[, gsub("raw/mp-pages/dep-|\\.html", "", i) ] = 0

raw$i = gsub("raw/mp-pages/dep-|\\.html", "", raw$i)

for (i in colnames(comm)[ -1 ])
  comm[ , i ] = as.numeric(comm$n %in% raw$n[ raw$i == i ])

# missing sponsor (Raffaele VALENSISE)
comm[, "13-d00583" ] = 0
comm[ comm$n == "V Commissione permanente Bilancio", "13_d00583" ] = 1

# assign co-memberships to networks
for (i in ls(pattern = "^net_it_ca")) {
  
  n = get(i)
  cat(i, ":", network.size(n), "nodes")
  
  sp = network.vertex.names(n)
  names(sp) = urls[ n %v% "url" ]
  names(sp) = paste0(gsub("leg=", "", str_extract(names(sp), "leg=\\d+")), 
                     "-", gsub("(.*)id=(.*)", "\\2", names(sp))) # URL to id
  stopifnot(names(sp) %in% colnames(comm))
  
  m = comm[ , names(sp) ]
  
  cat(" :", nrow(m), "committees", ncol(m), "MPs")
  M = m
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  stopifnot(ncol(m) == network.size(n))
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  e = data_frame(i = n %e% "source", j = n %e% "target")
  e$committee = NA
  
  for (j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]
  
  cat(" co-memberships:",
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"),
      sum(e$committee == 0), "null,",
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")
  
  nn = network(e[, 1:2], directed = FALSE)
  nn %e% "committee" = e$committee
  
  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(!is.na(nn %e% "committee"))
  
  n %e% "committee" = e$committee
  assign(i, n)
  
  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("co", i), nn)
  
}
