This repository contains code to build cosponsorship networks from bills passed in the [lower][ca] and [upper][se] chambers of the Italian Parliament. 

- [interactive demo](http://briatte.org/parlamento/)
- [static plots](http://briatte.org/parlamento/plots.html)

[ca]: http://www.camera.it/
[se]: http://www.senato.it/

# HOWTO

Replicate by running `make.r` in R.

Due to network issues with redirect links from the _Senato_ website, the scripts are not obvious to run at all, and require to use a VPN to avoid getting banned from `senato.it` after a few thousand requests. This is obviously not the easiest solution, which would be to use the XML/RDF data provided by [dati.camera.it/](http://dati.camera.it/) (if you know how to write [SPARQL](http://www.w3.org/TR/sparql11-protocol/) queries), or to scrape the [storia.camera.it/](http://storia.camera.it/) portal, if it could be linked to bill signatures.

The order in which `data.r` runs the scripts is as follows:

* `data-bills.r` scrapes bills (_disegni di legge_) and sponsor URLs from legislatures 13--17 (1996-2014) to `ddl.csv`, and exports bill keywords to `keywords.csv`
* `data-se.r` scrapes sponsor details to `parlamentari.csv` (all sponsor URLs, plus senator sponsor details) and `senatori.csv` (Senate sponsor details only)
* `data-ca-old.r` scrapes MP details for legislatures 13--15 to `camera-old.csv` (details for all MPs) and `deputati-old.csv` (MP sponsor details only)
* `data-ca-new.r` scrapes MP details for legislatures 16--17 to `deputati-new.csv`(details for sponsors only) -- and finally, all sponsors are in.

You will have to re-run the scripts (and possibly to subsample from the complete list of bills) to fix network errors and get enough data. The complete data collection process takes several days to run, but can be easily assigned to an idle machine running the download loop over and over again:

```{r}
# run make.r first
for(jj in 1:1000)
  try(source("ddl.r"), silent = TRUE)
```

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

The `ddl.csv` dataset holds both Camera and Senate bills and contains the following details, all in Italian (including dates):

* `legislature` -- legislature number
* `url` -- link to details
* `prima` -- first authors (connected to each other when building the networks)
* `cofirm` -- cosponsors (connected to all first authors when building the networks)
* `title` -- short description
* `date` -- date of introduction
* `ref` -- a reference of the form "Atto [Chamber: Camera/Senato] n. [number]"
* `teseo` -- bill keywords (roughly 4,000 of them, none missing)
* `n_au` -- number of first authors
* `n_co` -- number of cosponsors

When building the networks, `n_au` and `n_co` are summed to identify bills with more than one sponsor, first authors are connected to each other though undirected ties, and cosponsors are connected to each first author through directed ties. All ties are alphabetically sorted before being summed for weighting, so the resulting networks are undirected overall.

The additional dummies in the `ddl.csv` dataset are:

* `cofirm_dummy` -- the cosponsors on the bill are confirmed to be "cofirmatari" of that bill, and not related sponsors from related bills
* `chamber_dummy` -- the bill is sponsored by members of a single chamber, either the Camera (`CAM.DEP` sponsors) or the Senate (`SATTSEN` sponsors)
* `notgov_dummy` -- the bill is _not_ (co)sponsored by a member of the executive branch of government (`COMPGOV` sponsors), and should be considered
* `sample` -- final sample: the sum of first authors and cosponsors on the bill is superior to 1, and the three previous dummies are true

The `sample` dummy ends up excluding only a very small fraction of the private bills: see the comments in `ddl.r` for counts of excluded bills and further details.

## Sponsors

Common variables to both MPs and senators:

- `url` -- sponsor URL, as used in the bills data
- `name` -- sponsor name
- `sex` -- gender (F/M)
- `born` -- year of birth
- `mandate`	-- past mandates, used to compute the `nyears` seniority variable
- `photo` -- photo URL, shortened to its filename
- `partyname` -- simplified political party name
- `party` -- abbreviated political party
- `constituency` -- constituency, stored as the string to its Wikipedia Italiano entry

# Note

Italy has a strict distinction between first author(s) (_prima firmatori_) and cosponsor(s) (_cofirmatori_), and there is, in a few cases, multiple first authors, with or without cosponsors. This means that the cosponsorship graphs might contain two kinds of directed ties:

- mutual ties between first authors, and
- ties from cosponsors to first authors.

The confusion seems reasonable: all "first authors" are connected as if they had cosponsored a separate bill by each of the other "first authors", and each cosponsor is connected to each first author as if they had cosponsored a separate bill by each of them.

This method, however, makes the edge lists a bit longer, because cosponsors are connected to each "first author", not just to the first of them. This "exhaustive" method for tie construction would preserve the maximum number of directed ties between sponsor(s).

The alternative is to use a "restricted" rule for edge list construction, where all first authors beyond the first one are treated as cosponsors. The method is also reasonable because the order of the first authors is not random, i.e. alphabetical; see, e.g., [this bill](http://www.senato.it/leg/13/BGT/Schede/Ddliter/13114.htm).

The "restricted" construction rule makes the graphs more comparable with others. It does not affect the number of nodes per graph and has trivial effects on centrality computations, even though it sometimes loses many graph edges. The following logs show how much:

```
ca 17: 1497 cosponsored bills, 12769 edges, 621 nodes # exhaustive
ca 17: 1497 cosponsored bills, 12761 edges, 621 nodes # restricted; <1% loss
ca 16: 4069 cosponsored bills, 28942 edges, 658 nodes # exhaustive
ca 16: 4069 cosponsored bills, 25456 edges, 658 nodes # restricted; 12% loss
ca 15: 1737 cosponsored bills, 14599 edges, 618 nodes # exhaustive
ca 15: 1737 cosponsored bills, 14204 edges, 618 nodes # restricted; 3% loss
ca 14: 2857 cosponsored bills, 26230 edges, 609 nodes # exhaustive
ca 14: 2857 cosponsored bills, 24812 edges, 609 nodes # restricted; 5% loss
ca 13: 3997 cosponsored bills, 23954 edges, 638 nodes # exhaustive
ca 13: 3997 cosponsored bills, 23953 edges, 638 nodes # restricted: <1% loss
se 17: 880  cosponsored bills,  6005 edges, 325 nodes # exhaustive
se 17: 880  cosponsored bills,  6005 edges, 325 nodes # restricted: identical
se 16: 2012 cosponsored bills, 10223 edges, 338 nodes # exhaustive
se 16: 2012 cosponsored bills, 10155 edges, 338 nodes # restricted: <1% loss 
se 15: 877  cosponsored bills,  5481 edges, 326 nodes # exhaustive
se 15: 877  cosponsored bills,  5481 edges, 326 nodes # restricted: identical
se 14: 1744 cosponsored bills, 13247 edges, 323 nodes # exhaustive
se 14: 1744 cosponsored bills, 13196 edges, 323 nodes # restricted: <1% loss
se 13: 2737 cosponsored bills, 10624 edges, 332 nodes # exhaustive
se 13: 2737 cosponsored bills, 10624 edges, 332 nodes # restricted: identical
```

After running both methods and reading the logs, we opted to code the restricted rule. To try out the "exhaustive" method, just change remove the part of the edge list constructor that restricts the `j` column to the first of all first authors:

```{r}
d = expand.grid(i = c(d, e), j = d, stringsAsFactors = FALSE)
```
