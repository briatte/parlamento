This repository contains code to build cosponsorship networks from bills passed in the [lower][ca] and [upper][se] chambers of the Italian Parliament. 

- [interactive demo](http://f.briatte.org/parlviz/parlamento/)
- [static plots](http://f.briatte.org/parlviz/parlamento/plots.html)
- [more countries](https://github.com/briatte/parlnet)

[ca]: http://www.camera.it/
[se]: http://www.senato.it/

# HOWTO

Replicate by running `make.r` in R. The `data.r` script will run two different scripts to download the sponsors from both chambers, followed by the bills (_disegni di legge_) that they have cosponsored in each legislature. The data for the lower chamber are collected via the [SPARQL endpoint](http://dati.camera.it/sparql) of its open data portal.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `legislature` -- legislature number
- `ref` -- a reference of the form "Atto [Chamber: Camera/Senato] n. [number]"
- `date` -- date of introduction
- `title` -- short description
- `authors` -- first authors
- `cosponsors` -- cosponsors
- `n_a` -- number of first authors ([see note below](#note))
- `n_c` -- number of cosponsors

## Sponsors

- `legislature` -- legislature number
- `url` -- sponsor URL, as used in the bills data
- `name` -- sponsor name
- `sex` -- gender (F/M)
- `born` -- year of birth
- `constituency` -- constituency, stored as the string to its Wikipedia Italiano entry
- `party` -- political party or parliamentary coalition
- `committee` - committee memberships, semicolon-separated
- `nyears` -- time in office before start of legislature (in years)
- `photo` -- file path to sponsor photo

# NOTE

Italy has a strict distinction between first author(s) (_prima firmatori_) and cosponsor(s) (_cofirmatori_), and there is, in a limited number of cases from the Senate, multiple first authors, with or without cosponsors. This means that the cosponsorship graphs might contain two kinds of directed ties:

- mutual ties between first authors, and
- ties from cosponsors to first authors.

In order to keep things comparable across countries, it is probably best to treat only the first "first author" as such, and then treat all other sponsors as cosponsors. Adding mutual ties between first authors only marginally increases the density of the graphs.

# THANKS

Thanks to [Jeroen Ooms](https://github.com/jeroenooms/), who helped with a previous version of the code by [providing a trick](https://github.com/jeroenooms/curl/issues/35) to work with the misconfigured cache server of the _Senato_ website.
