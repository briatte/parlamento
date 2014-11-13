Cosponsorship networks from bills passed in the [lower][ca] and [upper][se] chambers of the Italian Parliament. 
Replicate the [plots][plots] by running `make.r` in R, or see the [interactive visualization][viz].

[ca]: http://www.camera.it/
[se]: http://www.senato.it/
[plots]: http://briatte.org/parlamento/plots.html
[viz]: http://briatte.org/parlamento/

# HOWTO

Due to network issues with redirect links from the _Senato_ website, the scripts are not obvious to run at all. I recommend using a VPN to avoid getting banned from `senato.it` after a few thousand requests.

The order in which `make.r` runs the scripts is as follows:

* `ddl.r` scrapes bills (_disegni di legge_) and sponsor URLs from legislatures 13--17 (1996-2014) to `ddl.csv`, and exports bill keywords to `keywords.csv`
* `senato.r` scrapes sponsor details to `parlamentari.csv` (all sponsor URLs, plus senator sponsor details) and `senatori.csv` (Senate sponsor details only)
* `camera-old.r` scrapes MP details for legislatures 13--15 to `camera-old.csv` (details for all MPs) and `deputati-old.csv` (MP sponsor details only)
* `camera-new.r` scrapes MP details for legislatures 16--17 to `deputati-new.csv`(details for sponsors only) -- and finally, all sponsors are in

You will have to re-run the scripts (and possibly to subsample from the complete list of bills) to fix network errors and get enough data. And that is not to say anything about the complexity of Italian political parties.

# DATA

The `ddl.csv` dataset contains the following details for bills, all in Italian (including dates):

* `legislature` -- legislature number
* `url` -- link to details
* `prima` -- first authors (connected to each other when building the networks)
* `cofirm` -- cosponsors (connected to all first authors when building the networks)
* `title` -- short description
* `date` -- date of introduction
* `ref` -- a reference of the form "Atto [Chamber: Camera/Senato] n. [number]"
* `teseo` -- bill keywords ~ roughly 4,000 of them
* `n_au` -- number of first authors
* `n_co` -- number of cosponsors

When building the networks, `n_au` and `n_co` are summed to identify bills with more than one sponsor, first authors are connected to each other though undirected ties, and cosponsors are connected to each first author through directed ties. All ties are alphabetically sorted before being summed for weighting, so the resulting networks are undirected overall.

The additional dummies in the `ddl.csv` dataset are:

* `cofirm_dummy` -- the cosponsors on the bill are confirmed to be "cofirmatari" of that bill, and not related sponsors from related bills
* `chamber_dummy` -- the bill is sponsored by members of a single chamber, either the Camera (`CAM.DEP` sponsors) or the Senate (`SATTSEN` sponsors)
* `notgov_dummy` -- the bill is _not_ (co)sponsored by a member of the executive branch of government (`COMPGOV` sponsors), and should be considered
* `sample` -- final sample: the sum of first authors and cosponsors on the bill is superior to 1, and the three previous dummies are true

The `sample` dummy ends up excluding only a very small fraction of the private bills: see the comments in `ddl.r` for counts of excluded bills and further details.

# TODO

* [x] fix bill details
* [x] fix MP last/first names
* [x] export keywords
