# add committee co-memberships

for (k in c("se", "ca")) {
  
  cat("\nParsing", ifelse(k == "ca", "Camera", "Senato"), "committees")
  x = s[ s$chamber == k, ]

  comm = data_frame()
  
  # find unique committees
  for (i in unique(x$legislature) %>% sort) {
    
    n = x$committee[ x$legislature == i ] %>%
      str_split(";") %>%
      sapply(unique) %>% # remove sponsor-level duplicate memberships
      unlist
    
    comm = rbind(comm, cbind(i, data.frame(table(n))))
    
  }
  
  names(comm) = c("legislature", "committee", "members")
  
  # filter out non-committees (e.g. delegations, presidential office, "", etc.)
  comm = filter(comm, grepl("commissione", committee, ignore.case = TRUE))
  
  cat(":", nrow(comm), "unique committees\n")
  write.csv(comm, paste0("data/committees-", k, ".csv"), row.names = FALSE)
  
  for (i in 1:nrow(x)) {
    
    l = str_split(x$committee[ i ], ";") %>%
      sapply(unique) %>% # remove sponsor-level duplicate memberships
      unlist
    
    l = comm$legislature == x$legislature[ i ] &
      comm$committee %in% l
    
    comm[, paste0(x$legislature[ i ], "_", x$name[ i ]) ] = as.numeric(l)
    
  }
  
  # totals should match
  stopifnot(!length(comm$committee[ rowSums(comm[, -1:-3 ]) != comm$members ]))
  
  # assign co-memberships to networks
  for (i in unique(comm$legislature) %>% sort) {
    
    cat("\nLegislature", i)
    
    sp = x$name[ x$legislature == i ]
    names(sp) = paste0(i, "_", x$name[ x$legislature == i ])
    
    m = comm[ comm$legislature == i, names(comm) %in% names(sp) ]
    cat(":", nrow(m), "committees", ncol(m), "MPs")
    M = m
    
    m = t(as.matrix(m)) # sponsors in rows, committees in columns
    m = m %*% t(m) # adjacency matrix
    
    colnames(m) = sp[ colnames(m) ]
    rownames(m) = sp[ rownames(m) ]
    
    n = get(paste0("net_it_", k, substr(yrs[ as.character(i) ], 1, 4)))
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
    set.edge.attribute(nn, "committee", e$committee)
    
    print(table(nn %e% "committee", exclude = NULL))
    stopifnot(!is.na(nn %e% "committee"))
    
    set.edge.attribute(n, "committee", e$committee)
    assign(paste0("net_it_", k, substr(yrs[ as.character(i) ], 1, 4)), n)
    
    nn %n% "committees" = as.table(rowSums(M))
    assign(paste0("conet_it_", k, substr(yrs[ as.character(i) ], 1, 4)), nn)
    
  }
  
}
