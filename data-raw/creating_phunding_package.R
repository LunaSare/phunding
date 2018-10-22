setwd("/Users/luna/GoogleDrive/lunasare.com")
devtools::create("phunding")
setwd("/Users/luna/GoogleDrive/lunasare.com/phunding")
devtools::use_readme_md()
devtools::use_data_raw()
# get data from rnsf package and download to data-raw
load("data-raw/grants.rda")
devtools::load_all()
nsf_relevant_grants_raw <- get_funded_taxa()
devtools::check(document=FALSE)
devtools::document() # is the same as running roxygen2::roxygenise()
# devtools::use_git() # unnecessary to use this since git was already initialized with
# Rscript -e 'devtools::create("phunding")'
# cd phunding/
# git init .
# git add .
# git commit -m "initial commit"
# devtools::use_data(nsf_relevant_grants_raw)
# did not use previous since it was stored with document() already
