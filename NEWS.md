# mccca 2.5

* `decideK()` now returns an object of class `"decideK"` (previously
  `"calcIndex"`). Objects created with earlier versions need to be recomputed.

* `MCCCA()` now returns the correct cluster sizes in `csize` and `summary()`
  when the class labels are not in alphabetical order.

* `create.MCCCAdata()` now gives informative errors for missing values and
  for active variables with only one category.

* New data set `young` and new vignette, `vignette("mccca")`.
