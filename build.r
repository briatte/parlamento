# reload and complete bills dataset

b = read.csv(bills, stringsAsFactors = FALSE)
b = subset(b, !is.na(prima))

s = rbind(read.csv("data/deputati-old.csv", stringsAsFactors = FALSE),
          read.csv("data/deputati-new.csv", stringsAsFactors = FALSE))

# impute seniority since 1996 (five-year mandates)
s = ddply(s, .(name), transform, nyears = 5 * 1:length(name))

write.csv(s, "data/deputati.csv", row.names = FALSE)

# check all sponsors are recognized
a = na.omit(unique(c(unlist(strsplit(b$prima, ";")), unlist(strsplit(b$cofirm, ";")))))
table(a %in% c(sen$url, s$url)) # should be below 10

# sponsors that failed to scrape
buggy = a[ !a %in% c(sen$url, s$url) ]
buggy = buggy[ !grepl("id=(;|$)", buggy) ] # only a handful of cases
buggy = subset(b, grepl("id=(;|$)", prima) | grepl("id=(;|$)", cofirm))
nrow(buggy) / nrow(b[ !is.na(b$prima), ]) # number of bills with issues < 2%

b$n_a = b$n_au + b$n_co

# loop over chambers and legislatures
for(jj in c("ca", "se")) {
  
  if(jj == "ca") {

    a = subset(b, grepl("CAM\\.DEP", prima) & sample) # Camera 13-17
    s = "deputati"
    
  } else {
    
    a = subset(b, grepl("SATTSEN", prima) & sample) # Senato 13-17
    s = "senatori"
    
  }

  s = read.csv(paste0("data/", s, ".csv"), stringsAsFactors = FALSE)
    
  for(ii in unique(a$legislature)) {
    
    cat("\n", jj, ii)
    data = subset(a, legislature == ii & n_a > 1)
    sp = subset(s, grepl(paste0("leg=", ii), url))
    
    cat(":", nrow(data), "cosponsored bills, ")
    
    edges = rbind.fill(lapply((1:nrow(data)), function(i) {
      
      # first authors
      w = unlist(strsplit(data$prima[ i ], ";"))
      d = s$name[ s$url %in% w ] # eliminates buggy URLs
      
      # cosponsors
      v = unlist(strsplit(data$cofirm[ i ], ";"))
      v = v[ !v %in% w ] # remove redundant first authors
      e = s$name[ s$url %in% v ] # eliminates buggy URLs

      if(length(c(d, e)) != length(unique(c(d, e)))) {
        cat("\nERROR: redundant sponsor(s) on", data$url[ i ], "\n")
      }
      
      # first authors connected to each other, and then to cosponsors
      d = subset(rbind(expand.grid(d, d, stringsAsFactors = FALSE),
                       expand.grid(d, e, stringsAsFactors = FALSE)),
                 Var1 != Var2)
      d = unique(apply(d, 1, function(x) paste0(sort(x), collapse = "_")))
      
      if(length(d))
        return(data.frame(d, w = length(c(v, w)) - 1)) # number of cosponsors
      else
        return(data.frame())
      
    }))
    
    # raw edge counts
    count = table(edges$d)
    
    # Newman-Fowler weights (weighted quantity of bills cosponsored)
    edges = aggregate(w ~ d, function(x) sum(1 / x), data = edges)
    
    # raw counts
    edges$count = as.vector(count[ edges$d ])
    
    edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$d),
                       j = gsub("(.*)_(.*)", "\\2", edges$d),
                       w = edges$w, n = edges[, 3])
    
    cat(nrow(edges), "edges, ")
    
    # network
    n = network(edges[, 1:2 ], directed = FALSE)
    n %n% "title" = paste(ifelse(jj == "ca", "Camera", "Senato"), ii)
    n %n% "n_bills" = nrow(data)
    
    n %n% "n_sponsors" = table(subset(a, legislature == ii)$n_a)
    
    cat(network.size(n), "nodes")
    
    rownames(sp) = sp$name
    n %v% "url" = sp[ network.vertex.names(n), "url" ]
    n %v% "name" = sp[ network.vertex.names(n), "name" ]
    n %v% "sex" = sp[ network.vertex.names(n), "sex" ]
    n %v% "born" = as.numeric(sp[ network.vertex.names(n), "born" ])
    n %v% "nyears" = sp[ network.vertex.names(n), "nyears" ]
    n %v% "party" = sp[ network.vertex.names(n), "party" ]
    # missing, not properly scraped: party_url, county
    n %v% "photo" = sp[ network.vertex.names(n), "photo" ]
    
    network::set.edge.attribute(n, "source", as.character(edges[, 1]))
    network::set.edge.attribute(n, "target", as.character(edges[, 2]))
    
    network::set.edge.attribute(n, "weight", edges[, 3])
    network::set.edge.attribute(n, "count", edges[, 4])
    network::set.edge.attribute(n, "alpha",
                                as.numeric(cut(n %e% "count", c(1:4, Inf),
                                               include.lowest = TRUE)) / 5)
    
    # modularity
    
    nn = graph.edgelist(as.matrix(edges[, 1:2 ]), directed = FALSE)
    E(nn)$weight = edges[, 3]
    
    i = sp[ V(nn)$name, "party" ]
    i[ i %in% c("Misto") ] = NA # mixed groups

    nn = nn - which(is.na(i))
    i = as.numeric(factor(i[ !is.na(i) ]))
    
    n %n% "modularity" = modularity(nn, membership = i, weights = E(nn)$weight)
    cat("\nModularity:", round(n %n% "modularity", 2))
    
    walktrap = lapply(1:50, function(x) walktrap.community(nn, steps = x))
    
    # max. partition
    maxwalks = order(sapply(walktrap, modularity), decreasing = TRUE)[1]
    walktrap = walktrap[[ maxwalks ]]
    
    n %n% "modularity_walktrap" = modularity(walktrap)
    cat(" Walktrap:", round(n %n% "modularity_walktrap", 2))
    
    louvain = multilevel.community(nn)
    
    n %n% "modularity_louvain" = modularity(louvain)
    cat(" Louvain:", round(n %n% "modularity_louvain", 2))

    # weighted adjacency matrix to tnet
    tnet = as.tnet(as.sociomatrix(n, attrname = "weight"), type = "weighted one-mode tnet")
    
    # weighted degree and distance
    wdeg = as.data.frame(degree_w(tnet, measure = "degree"))
    dist = distance_w(tnet)
    wdeg$distance = NA
    wdeg[ attr(dist, "nodes"), ]$distance = colMeans(dist, na.rm = TRUE)
    wdeg = cbind(wdeg, clustering_local_w(tnet)[, 2])
    names(wdeg) = c("node", "degree", "distance", "clustering")
    
    n %v% "degree" = wdeg$degree
    n %n% "degree" = mean(wdeg$degree, na.rm = TRUE)
    cat("\nDegree:", round(n %n% "degree", 2))
    
    n %v% "distance" = wdeg$distance
    n %n% "distance" = mean(wdeg$distance, na.rm = TRUE)
    cat(" Distance:", round(n %n% "distance", 2))
    
    n %v% "clustering" = wdeg$clustering    # local
    n %n% "clustering" = clustering_w(tnet) # global
    cat(" Clustering:", round(n %n% "clustering", 2))

    # edge colors
    
    i = colors[ sp[ n %e% "source", "party" ] ]
    j = colors[ sp[ n %e% "target", "party" ] ]
    
    party = as.vector(i)
    party[ i != j ] = "#AAAAAA"
    
    # print(table(n %v% "party", exclude = NULL))
    
    # number of bills cosponsored
    nb = c( unlist(strsplit(data$prima, ";")), unlist(strsplit(data$cofirm, ";")) )
    nb = sapply(n %v% "url", function(x) {
      sum(nb == x) # ids are unique URLs
    })
    n %v% "n_bills" = as.vector(nb)
    
    if(plot) {
      
      q = unique(quantile(n %v% "degree")) # safer
      n %v% "size" = as.numeric(cut(n %v% "degree", q, include.lowest = TRUE))
      
      g = suppressWarnings(ggnet(n, size = 0, segment.alpha = 1/2, # mode = "kamadakawai",
                                 segment.color = party) +
                             geom_point(alpha = 1/3, aes(size = n %v% "size", color = n %v% "party")) +
                             geom_point(alpha = 1/2, aes(size = min(n %v% "size"), color = n %v% "party")) +
                             scale_size_continuous(range = c(6, 12)) +
                             scale_color_manual("", values = colors, breaks = order) +
                             theme(legend.key.size = unit(1, "cm"),
                                   legend.text = element_text(size = 16)) +
                             guides(size = FALSE, color = guide_legend(override.aes = list(alpha = 1/3, size = 6))))
      
      print(g)
      
      ggsave(paste0("plots/net_", jj, ii, ".pdf"),
             g + theme(legend.key = element_blank()), width = 12, height = 9)
      ggsave(paste0("plots/net_", jj, ii, ".jpg"),
             g + theme(legend.position = "none"), width = 9, height = 9)
      
    }
    
    assign(paste0("net_it_", jj, ii), n)
    assign(paste0("bills_it_", jj, ii), data)
    assign(paste0("edges_it_", jj, ii), edges)
    
    if(gexf) {
      
      rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
      mode = "fruchtermanreingold"
      meta = list(creator = "rgexf",
                  description = paste(mode, "placement", nrow(data), "bills"),
                  keywords = "parliament, italy")
      
      node.att = data.frame(url = n %v% "url",
                            party = n %v% "party",
                            bills = n %v% "n_bills",
                            distance = round(n %v% "distance", 1),
                            photo = n %v% "photo",
                            stringsAsFactors = FALSE)
      
      people = data.frame(id = as.numeric(factor(network.vertex.names(n))),
                          label = network.vertex.names(n),
                          stringsAsFactors = FALSE)
      
      relations = data.frame(
        source = as.numeric(factor(n %e% "source", levels = levels(factor(people$label)))),
        target = as.numeric(factor(n %e% "target", levels = levels(factor(people$label)))),
        weight = round(n %e% "weight", 3), count = n %e% "count")
      relations = na.omit(relations)
      
      # check all weights are positive after rounding
      stopifnot(all(relations$weight > 0))
      
      nodecolors = lapply(n %v% "party", function(x)
        data.frame(r = rgb[x, 1], g = rgb[x, 2], b = rgb[x, 3], a = .5))
      nodecolors = as.matrix(rbind.fill(nodecolors))
      
      # node placement
      position = do.call(paste0("gplot.layout.", mode),
                         list(as.matrix.network.adjacency(n), NULL))
      position = as.matrix(cbind(round(position, 1), 1))
      colnames(position) = c("x", "y", "z")
      
      # convert URLs
      node.att$url = gsub("&", "&amp;", node.att$url)
      node.att$url = gsub("/loc/link.asp\\?tipodoc=", "", node.att$url)
      
      # save with compressed floats
      write.gexf(nodes = people, nodesAtt = node.att,
                 edges = relations[, 1:2 ], edgesWeight = relations[, 3],
                 nodesVizAtt = list(position = position, color = nodecolors,
                                    size = round(n %v% "degree", 1)),
                 # edgesVizAtt = list(size = relations[, 4]),
                 defaultedgetype = "undirected", meta = meta,
                 output = paste0("net_it_", jj, ii, ".gexf"))
      
    }
        
  }
  
  if(gexf)
    zip(paste0("net_it_", jj, ".zip"), dir(pattern = paste0("^net_it_", jj, "\\d{2}\\.gexf$")))
    
}

save(list = ls(pattern = "^(net|edges|bills)_it_(ca|se)\\d{2}$"),
     file = "data/net_it.rda")

# kthxbye
