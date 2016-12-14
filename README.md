

```
## Loading required package: printr
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'printr'
```


Welcome to the *epicontacts* package!
---------------------------------------

<br>

[![Travis-CI Build Status](https://travis-ci.org/reconhub/epicontacts.svg?branch=master)](https://travis-ci.org/reconhub/epicontacts)

[![Coverage Status](https://img.shields.io/codecov/c/github/reconhub/epicontacts/master.svg)](https://codecov.io/github/reconhub/epicontacts?branch=master)

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/epicontacts)](https://cran.r-project.org/package=epicontacts)




<br>
<br>

# Installing the package

To install the current stable, CRAN version of the package, type:

```r
install.packages("epicontacts")
```

To benefit from the latest features and bug fixes, install the development, *github* version of the package using:

```r
devtools::install_github("reconhub/epicontacts")
```

Note that this requires the package *devtools* installed.



<br>
<br>



# What does it do?

The main features of the package include:

* **`epicontacts`:** a new S3 class for storing linelists and contacts data

* **`make_epicontacts`:** a constructor for the new `epicontacts` class

* **`get_id`:** access unique IDs in an `epicontacts` with various options

* **`get_pairwise`**:  extract attributes of record(s) in contacts database using information provided in the linelist data of an `epicontacts` object.

* **`get_degree`:** access degree of cases in `epicontacts` with various options

* **`x[i,j,contacts]`:** subset an `epicontacts` object by retaining specified cases

* **`thin`:** retains matching cases in linelist / contacts

* **`summary`:** summary for  `epicontacts` objects

* **`plot`:** plot for  `epicontacts` objects; various types of plot are available; default to `vis_epicontacts`

* **`vis_epicontacts`:** plot an `epicontacts` object using `visNetwork
`
* **`as.igraph.epicontacts`:** create an `igraph` object from a epicontacts object

* **`clusters_epicontacts`:** assign clusters and corresponding cluster sizes to linelist of an epicontacts object (clusters being groups of connected individuals/nodes).

* **`subset_clusters_by_id`**: subset an `epicontacts` object based on a IDs of cases of interest.

* **`subset_clusters_by_size`**:  subset an `epicontacts` object based on size(s) of clusters (clusters being groups of connected individuals/nodes).

* **`graph3D`**: 3D graph from an `epicontacts` object.

* **`epicontacts_server`**: run the epicontacts Shiny app on a local host




<br>
<br>

# Resources

<br>

## Vignettes

An overview of *epicontacts* is provided below in the worked example below.
More detailed tutorials are distributed as vignettes with the package:

```r
vignette(package="epicontacts")
#> Error in find.package(package, lib.loc): there is no package called 'epicontacts'
```

To open these, type:

```r
vignette("overview", package="epicontacts")
vignette("customize_plot", package="epicontacts")
vignette("epicontacts_class", package="epicontacts")
```

<br>
<br>

## Websites

The following websites are available:

- The official *epicontacts* website, providing an overview of the package's functionalities, up-to-date tutorials and documentation: <br>
[http://www.repidemicsconsortium.org/epicontacts/](http://www.repidemicsconsortium.org/epicontacts/)

- The *epicontacts* project on *github*, useful for developers, contributors, and users wanting to post issues, bug reports and feature requests: <br>
[http://github.com/reconhub/epicontacts](http://github.com/reconhub/epicontacts)

- The *epicontacts* page on CRAN: <br>
[https://CRAN.R-project.org/package=epicontacts](https://CRAN.R-project.org/package=epicontacts)


<br>
<br>

## Getting help online

Bug reports and feature requests should be posted on *github* using the [*issue*](http://github.com/reconhub/epicontacts/issues) system. All other questions should be posted on the **RECON forum**: <br>
[http://www.repidemicsconsortium.org/forum/](http://www.repidemicsconsortium.org/forum/)





<br>
<br>

# A quick overview

The following worked example provides a brief overview of the package's
functionalities. See the [*vignettes section*](#vignettes) for more detailed tutorials.

## Loading the data

... to be continued!






<br>
<br>

# Contributors (by alphabetic order):
- [Thomas Crellen](https://github.com/tc13)
- [Finlay Campbell](https://github.com/finlaycampbell)
- [Thibaut Jombart](https://github.com/thibautjombart)
- [Nistara Randhawa](https://github.com/nistara)
- Bertrand Sudre


See details of contributions on: <br>
https://github.com/reconhub/epicontacts/graphs/contributors



Contributions are welcome via **pull requests**.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

**Maintainer:** VP Nagraj (vpnagraj@virginia.edu)
