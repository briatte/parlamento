Cosponsorship networks from bills passed in the [lower](http://www.camera.it/) and [upper](http://www.senato.it/) chambers of the Italian Parliament. Replicate the [plots](http://briatte.org/parlamento/plots.html) by running `make.r` in R, or see the [interactive visualization](http://briatte.org/parlamento/).

# HOWTO

* `ddl.r` -- scrapes bills (_disegni di legge_) and sponsors from legislatures 13--17 (1996-2014)
* `senato.r` -- scrapes senator details to `parlamentari.csv` (all sponsors) and `senatori.csv`
* `camera.r` -- scrapes MP details to `camera.csv` (all MPs) and `deputati.csv`

Due to weird issues with redirect links, the scraper currently ignores MPs from legislatures 16-17.

# DATA

The dummies in the `ddl.csv` dataset are:

* `cofirm_dummy` -- the cosponsors on the bill are confirmed to be "cofirmari" of that bill, and not related sponsors from identical or related bills
* `chamber_dummy` -- the bill is sponsored by members of a single chamber, either the Camera (`CAM.DEP` sponsors) or the Senate (`SATTSEN` sponsors)
* `notgov_dummy` -- the bill is _not_ sponsored by a member of the executive branch of government (`COMPGOV` sponsors), and should be considered
* `sample` -- finalized sample: the sum of first authors and cosponsors on the bill is superior to 1, and the three previous dummies are true

# TODO

* [ ] reduce size of sponsor photos
* [ ] reduce size of GEXF networks
* [ ] reduce size of sponsor details
* [ ] fix MP last/first names
* [ ] reweight edges to Gross-Shalizi?
