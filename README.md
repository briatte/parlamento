This repository contains code to build cosponsorship networks from bills passed in the [lower][ca] and [upper][se] chambers of the Italian Parliament. 

- [interactive demo](http://f.briatte.org/parlviz/parlamento/)
- [static plots](http://f.briatte.org/parlviz/parlamento/plots.html)
- [more countries](https://github.com/briatte/parlnet)

[ca]: http://www.camera.it/
[se]: http://www.senato.it/

# HOWTO

Replicate by running `make.r` in R. The `data.r` script runs several scripts in sequence to download the bills (_disegni di legge_) from both chambers, and then the sponsor details from both chambers, dealing with the different HTML structure of the sponsor profiles depending on the legislature.

The order in which the data are collected is as follows:

* `data.r` scrapes all bills to `bills.csv` and all sponsor URLs to `sponsors.csv`
* `data-se.r` scrapes senator details for legislatures 13--17 to `sponsors-se.csv`
* `data-ca-old.r` scrapes MP details for legislatures 13--15 to `sponsors-ca-old.csv`
* `data-ca-new.r` scrapes MP details for legislatures 16--17 to `sponsors-ca-new.csv`

This workflow covers both chambers simultaneously, but it is very slow and will produce a large amount of network errors, which can yet be solved by running the scripts several times. For the lower chamber, it should be easier to use the XML/RDF data provided by [dati.camera.it/](http://dati.camera.it/), or even to scrape the [storia.camera.it/](http://storia.camera.it/) portal, which is [incredibly extensive](http://storia.camera.it/documenti/progetti-legge) in coverage. For the upper chamber, the open data portal is [dati.senato.it](http://dati.senato.it/).

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

The `bills.csv` dataset holds both Camera and Senate bills and contains the following details, all in Italian (including dates):

- `legislature` -- legislature number
- `url` -- link to details
- `url_chamber` -- the real link, after redirection
- `ref` -- a reference of the form "Atto [Chamber: Camera/Senato] n. [number]"
- `date` -- date of introduction
- `title` -- short description
- `teseo` -- bill keywords (roughly 4,000 of them, none missing)
- `authors` -- first authors ([see note below](#note))
- `cosponsors` -- cosponsors ([see note below](#note))
- `n_au` -- number of first authors
- `n_co` -- number of cosponsors
- `cosponsors_dummy` -- whether all cosponsors on the bill are confirmed to be _cofirmatari_ of that bill, and not related sponsors from related bills (0/1)

## Sponsors

Common variables to both MPs and senators:

- `chamber` -- chamber of origin
- `legislature` -- legislature number
- `url` -- sponsor URL, as used in the bills data
- `url_chamber` -- sponsor URL to the parliamentary chamber
- `name` -- sponsor name
- `sex` -- gender (F/M)
- `born` -- year of birth
- `constituency` -- constituency, stored as the string to its Wikipedia Italiano entry
- `party` -- abbreviated political party or parliamentary coalition (there are _many_ factions)
- `nyears` -- time in office (approximate, computed over intervals of 5 years)
- `photo` -- file path to sponsor photo

# NOTE

Italy has a strict distinction between first author(s) (_prima firmatori_) and cosponsor(s) (_cofirmatori_), and there is, in a few cases, multiple first authors, with or without cosponsors. This means that the cosponsorship graphs might contain two kinds of directed ties:

- mutual ties between first authors, and
- ties from cosponsors to first authors.

The confusion seems reasonable: all "first authors" are connected as if they had cosponsored a separate bill by each of the other "first authors", and each cosponsor is connected to each first author as if they had cosponsored a separate bill by each of them.

This method, however, makes the edge lists a bit longer, because cosponsors are connected to each "first author", not just to the first of them. This "exhaustive" method for tie construction would preserve the maximum number of directed ties between sponsor(s).

The alternative is to use a "restricted" rule for edge list construction, where all first authors beyond the first one are treated as cosponsors. The method is also reasonable because the order of the first authors is not random (i.e. alphabetical; see, e.g., [this bill](http://www.senato.it/leg/13/BGT/Schede/Ddliter/13114.htm)).

The "restricted" construction rule makes the graphs more comparable with cosponsorship graphs from other countries. It does not affect the number of vertices per graph and has trivial effects on centrality computations, even if it sometimes loses up to 5% of all ties.

The code currently enforces the restricted rule. To try out the "exhaustive" method, just change the edge list constructor that restricts the `j` column to the first of all first authors. Edit the `expand.grid` function found in `build.r` as follows:

```{r}
d = expand.grid(i = c(d, e), j = d, stringsAsFactors = FALSE)
```

Note that using the "exhaustive" construction rule will not solve issues that tend to blur the distinction between authors and cosponsors, such as the fact that some sponsors appear listed as both. The code handles this issue by removing authors from cosponsors.

# THANKS

Thanks to [Jeroen Ooms](https://github.com/jeroenooms/) for [finding the trick](https://github.com/jeroenooms/curl/issues/35) to work with the misconfigured cache server of the _Senato_ website.
