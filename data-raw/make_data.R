# load ebola.sim object from outbreaks and write to data
ebola.sim <- outbreaks::ebola.sim

devtools::use_data(ebola.sim, overwrite = TRUE, compress = "gzip")