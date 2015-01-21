meta = c("Italy", "Parlamento")
mode = "fruchtermanreingold"

# loop over chambers and legislatures
for(jj in c("ca", "se")) {
  
  if(jj == "ca") {

    meta = c(meta[1], "Camera")
    a = subset(b, grepl("CAM\\.DEP", prima) & sample) # Camera 13-17
    s = "deputati"
    
  } else {
    
    meta = c(meta[1], "Senato")
    a = subset(b, grepl("SATTSEN", prima) & sample) # Senato 13-17
    s = "senatori"
    
  }

  s = read.csv(paste0("data/", s, ".csv"), stringsAsFactors = FALSE)
    
  for(ii in unique(a$legislature)) {
    
    cat("\n", jj, ii)
    data = subset(a, legislature == ii & n_a > 1)
    sp = subset(s, grepl(paste0("leg=", ii), url))
        
    cat(":", nrow(data), "cosponsored documents, ")
    
    #
    # directed edge list
    #
    
    edges = bind_rows(lapply((1:nrow(data)), function(i) {
      
      # first authors
      w = unlist(strsplit(data$prima[ i ], ";"))
      d = s$name[ s$url %in% w ] # eliminates buggy URLs
      
      # cosponsors
      v = unlist(strsplit(data$cofirm[ i ], ";"))
      v = v[ !v %in% w ] # remove redundant first authors
      e = s$name[ s$url %in% v ] # eliminates buggy URLs
      
      d = expand.grid(i = c(d, e), j = d[1], stringsAsFactors = FALSE)
      
      return(data.frame(d, w = length(c(v, w)) - 1)) # number of cosponsors
      
    }))

    #
    # edge weights
    #
    
    # first author self-loops, with counts of cosponsors
    self = subset(edges, i == j)
    
    # count number of bills per first author
    n_au = table(self$j)
    
    # remove self-loops from directed edge list
    edges = subset(edges, i != j)
    
    # count number of bills cosponsored per sponsor
    n_co = table(edges$i)
    
    # identify directed ties
    edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
    
    # raw edge counts
    raw = table(edges$ij)
    
    # Newman-Fowler weights (weighted quantity of bills cosponsored)
    edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
    
    # expand to edge list
    edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                       j = gsub("(.*)///(.*)", "\\2", edges$ij),
                       raw = as.vector(raw[ edges$ij ]), # raw edge counts
                       nfw = edges$w, stringsAsFactors = FALSE)
    
    # Gross-Shalizi weights (weighted propensity to cosponsor)
    edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
    edges$gsw = edges$nfw / edges$w
    
    # final edge set: cosponsor, first author, weights
    edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]
    
    cat(nrow(edges), "edges, ")
    
    #
    # directed network
    #
    
    n = network(edges[, 1:2 ], directed = TRUE)
    
    n %n% "country" = meta[1]
    n %n% "title" = paste(meta[2], paste0(range(unique(substr(data$date, 1, 4))),
                                          collapse = " to "))
        
    n %n% "n_bills" = nrow(data)
    n %n% "n_sponsors" = table(subset(a, legislature == ii)$n_a)
    
    n_au = as.vector(n_au[ network.vertex.names(n) ])
    n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
    
    n_co = as.vector(n_co[ network.vertex.names(n) ])
    n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
    
    n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
    
    cat(network.size(n), "nodes\n")
    
    rownames(sp) = sp$name
    
    n %v% "url" = as.character(gsub("&", "&amp;", gsub("/loc/link.asp\\?tipodoc=", "",
                                                       sp[ network.vertex.names(n), "url" ])))
    n %v% "sex" = as.character(sp[ network.vertex.names(n), "sex" ])
    n %v% "born" = as.numeric(substr(sp[ network.vertex.names(n), "born" ], 1, 4))
    n %v% "party" = as.character(sp[ network.vertex.names(n), "party" ])
    n %v% "constituency" = as.character(sp[ network.vertex.names(n), "constituency" ])
    n %v% "partyname" = as.character(sp[ network.vertex.names(n), "partyname" ])
    n %v% "lr" = as.numeric(scores[ n %v% "party" ])
    n %v% "photo" = as.character(sp[ network.vertex.names(n), "photo" ])
    n %v% "nyears" = as.numeric(sp[ network.vertex.names(n), "nyears" ]) # pre-computed
    
    set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
    set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
    
    set.edge.attribute(n, "raw", edges$raw) # raw edge counts
    set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
    set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights

    #
    # weighted measures
    #
    
    n = get_modularity(n, weights = "raw")
    n = get_modularity(n, weights = "nfw")
    n = get_modularity(n, weights = "gsw")
    
    n = get_centrality(n, weights = "raw")
    n = get_centrality(n, weights = "nfw")
    n = get_centrality(n, weights = "gsw")
    
    #
    # network plot
    #
    
    if(plot) {
      
      q = n %v% "degree"
      q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
      
      ggnet_save(n, file = paste0("plots/net_it_", jj, ii),
                 i = colors[ sp[ n %e% "source", "party" ] ],
                 j = colors[ sp[ n %e% "target", "party" ] ],
                 q, colors, order)
      
    }

    #
    # save objects
    #
    
    assign(paste0("net_it_", jj, ii), n)
    assign(paste0("bills_it_", jj, ii), data)
    assign(paste0("edges_it_", jj, ii), edges)
    
    #
    # export gexf
    #
    
    if(gexf)
      get_gexf(paste0("net_it_", jj, ii), n, meta, mode, colors, extra = "constituency")

  }

  if(gexf)
    zip(paste0("net_it_", jj, ".zip"), dir(pattern = paste0("^net_it_", jj, "\\d{2}\\.gexf$")))

}

save(list = ls(pattern = "^(net|edges|bills)_it_(ca|se)\\d{2}$"),
     file = "data/net_it.rda")

# kthxbye
