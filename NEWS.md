epicontacts 1.1.4 (2024-04-29)
==================

### BUG FIXES

- Fix `thin` option for `plot.epicontacts`
- Deal with duplicated IDs

epicontacts 1.1.3 (2023-03-28)
==================

### BUG FIXES

- Small fix for CRAN warnings


epicontacts 1.1.2 (2021-10-19)
==================

### BUG FIXES

- plotting bug (blank plot) induced by changes to `visNetwork` in `vis_epicontacts()` has been 
  fixed


epicontacts 1.1.1 (2018-9-18)
==================

### NEW FEATURES

* `vis_epicontacts()` gains an `x_axis` argument to specify a column to use for
  displaying dates

### BUG FIXES

- `vis_epicontacts()` and `plot()` now both use same default thinning

epicontacts 1.1.0 (2017-11-20)
==================

### NEW FEATURES

- `vis_epicontacts()` now accepts fontawesome icon codes for node shapes

- `graph3D()` will now plot an **igraph** object direct thanks to updates in **threejs** API


epicontacts 1.0.1 (2017-05-11)
==================

### BUG FIXES

- `make_epicontacts` now handles contacts provided as factors

- added URL of website and issue system to DESCRIPTION

- replaced javascript graphics in README.md with screenshots - these were not
  rendering on github



epicontacts 1.0.0 (2017-04-27)
==================

- *epicontacts* now available on CRAN!

