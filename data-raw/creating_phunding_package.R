# on terminal move to dir where you wanna store your package, then type:
# Rscript -e 'devtools::create("phunding")'
# cd phunding/
# git init .
# git add .
# git commit -m "initial commit"
# Alternatively, from within R you can:
setwd("/Users/luna/GoogleDrive/lunasare.com")
devtools::create("phunding")
setwd("/Users/luna/GoogleDrive/lunasare.com/phunding")
# devtools::use_git() # unnecessary if git was already initialized from terminal

devtools::use_readme_md()
devtools::use_data_raw()
# move this file to data-raw dir
# download data from rnsf package to data-raw dir for now
load("data-raw/grants.rda")
devtools::load_all()
# write get_funded_taxa function in R dir
nsf_relevant_grants_raw <- get_funded_taxa()
# save data in data dir
devtools::use_data(nsf_relevant_grants_raw)
# run a round of checking with no documentation
devtools::check(document=FALSE)
# write nsf_relevant_grants_raw data description in R dir
write(x = paste0("#'   \\item{", names(nsf_relevant_gran_raw), "}{A ", gsub("character", "character vector", sapply(relevant.grants, class)), " of ", "}"), file = "data-raw/nsf_relevant_grants_names.txt", sep = "\n")
# write documentation of functions and data sets:
devtools::document() # is the same as running roxygen2::roxygenise()
# check with documentation now
devtools::check()
# formalize unit testing
devtools::use_testthat()
# create some tests, then run them
library(testthat)
devtools::test() # it is not working because grants object is in data-raw dir
# data.rda has to be in data dir so it can be used inside any function and testhat
# so moving it there and generating a description for it
devtools::document()
devtools::test() # this one passed successfully
# add documentation for the package as a whole
devtools::use_package_doc() # this creates a dummy that need to be edited by hand
# once it is edited, let's run document() again
devtools::document()
# create a vignette for the nsf funded tree of life
devtools::use_vignette("NSF-funded-Tree-of-Life")
