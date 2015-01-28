# add committee co-memberships

load("data/net_it.rda")

source("comm-se.r")
source("comm-ca.r")

save(list = ls(pattern = "^((co)?net|edges|bills)_it_(ca|se)\\d{2}$"),
     file = "data/net_it.rda")
