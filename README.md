[![Travis-CI Build Status](https://travis-ci.org/Hackout3/contacts.svg?branch=master)](https://travis-ci.org/Hackout3/contacts)

[![Coverage Status](https://img.shields.io/codecov/c/github/Hackout3/contacts/master.svg)](https://codecov.io/github/Hackout3/contacts?branch=master)



Welcome to the contacts package!
---------------------------------------
(title is work in progress ;) )

For ongoing tasks and various information, check out our [**wiki**](http://github.com/hackout3/contacts/wiki).


# Installation

To install the devel version of the package, type:

```r
devtools::install_github("Hackout3/contacts")
```

Note that this requires the package *devtools* installed.


# Main features
Main features include:
* **`epi_contacts`:** a new S3 class for storing linelists and contacts data
* **`make_epi_contacts`:** a constructor for the new `epi_contacts` class
* **`get_id`:** access unique IDs in an `epi_contacts` with various options
* **`get_degree`:** access degree of cases in `epi_contacts` with various options
* **`x[i,j,contacts]`:** susbset an `epi_contacts` object by retaining specified cases
* **`summary`:** summary for  `epi_contacts` objects
 