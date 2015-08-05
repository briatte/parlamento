# add committee co-memberships

load("data/net_it.rda")

urls = s$url
names(urls) = s$url_chamber

source("comm-se.r")
source("comm-ca.r")

save(list = ls(pattern = "^((co)?net|edges|bills)_it_(ca|se)\\d{4}$"),
     file = "data/net_it.rda")
