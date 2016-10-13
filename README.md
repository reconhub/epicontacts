[![Travis-CI Build Status](https://travis-ci.org/reconhub/epicontacts.svg?branch=master)](https://travis-ci.org/reconhub/epicontacts)

[![Coverage Status](https://img.shields.io/codecov/c/github/reconhub/epicontacts/master.svg)](https://codecov.io/github/Reconhub/epicontacts?branch=master)


Welcome to the *epicontacts* package!
---------------------------------------

# Installation

To install the devel version of the package, type:

```r
devtools::install_github("reconhub/epicontacts")
```

Note that this requires the package *devtools* installed.


# Main features
Main features include:
* **`epi_contacts`:** a new S3 class for storing linelists and contacts data
* **`make_epi_contacts`:** a constructor for the new `epi_contacts` class
* **`get_id`:** access unique IDs in an `epi_contacts` with various options
* **`get_pairwise`**:  extract attributes of record(s) in contacts database using information provided in the linelist data of an `epi_contacts` object.
* **`get_degree`:** access degree of cases in `epi_contacts` with various options
* **`x[i,j,contacts]`:** subset an `epi_contacts` object by retaining specified cases
* **`summary`:** summary for  `epi_contacts` objects
* **`plot`:** plot for  `epi_contacts` objects; various types of plot are available; default to `vis_epi_contacts`
* **`vis_epi_contacts`:** plot an `epi_contacts` object using `visNetwork`
* **`igraph.epi_contacts`:** create an `igraph` object from a epi_contacts object
* **`clusters_epi_contacts`:** assign clusters and corresponding cluster sizes to linelist of an epi_contacts object (clusters being groups of connected individuals/nodes).
* **`subset_clusters_by_id`**: subset an `epi_contacts` object based on a IDs of cases of interest.
* **`subset_clusters_by_size`**:  subset an `epi_contacts` object based on size(s) of clusters (clusters being groups of connected individuals/nodes).
* **`graph3D`**: 3D graph from an `epi_contacts` object.
* **`epicontacts_server`**: run the epicontacts Shiny app on a local host

 
