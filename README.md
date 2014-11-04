Cosponsorship networks from bills passed in the [lower](http://www.camera.it/) and [upper](http://www.senato.it/) chambers of the Italian Parliament. Replicate the [plots](http://briatte.org/parlamento/plots.html) by running `make.r` in R, or see the [interactive visualization](http://briatte.org/parlamento/).

# HOWTO

Due to network issues with redirect links from the Senate, the scripts are not obvious to run at all. The order in which `make.r` runs them is as follows:

* `ddl.r` -- scrapes bills (_disegni di legge_) and sponsors from legislatures 13--17 (1996-2014)
* `senato.r` -- scrapes sponsor details to `parlamentari.csv` (all sponsors) and `senatori.csv` (Senate sponsors only)
* `camera-old.r` -- scrapes MP details for legislatures 13--15 to `camera-old.csv` (all MPs) and `deputati-old.csv` (sponsors only)
* `camera-new.r` -- scrapes MP details for legislatures 16--17 to `deputati-new.csv`(sponsors only)

And that is not to say anything about the complexity of Italian political parties.

# DATA

The dummies in the `ddl.csv` dataset are:

* `cofirm_dummy` -- the cosponsors on the bill are confirmed to be "cofirmatari" of that bill, and not related sponsors from related bills
* `chamber_dummy` -- the bill is sponsored by members of a single chamber, either the Camera (`CAM.DEP` sponsors) or the Senate (`SATTSEN` sponsors)
* `notgov_dummy` -- the bill is _not_ sponsored by a member of the executive branch of government (`COMPGOV` sponsors), and should be considered
* `sample` -- finalized sample: the sum of first authors and cosponsors on the bill is superior to 1, and the three previous dummies are true

The `sample` dummy ends up excluding only a very small fraction of the bills.

# TODO

* [x] fix bill details
* [x] fix MP last/first names
* [ ] reweight edges to Gross-Shalizi?
