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
# move this R file to data-raw dir
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
devtools::check() # passed with 1 warning and 3 notes
devtools::test() # this one is not passing for some reason
# curate nsf_relevant_grants_raw with clean_funded_taxa
devtools::load_all()
nsf_relevant_grants_raw <- get_funded_taxa()
use_data("nsf_relevant_grants_raw")
utils::data(nsf_relevant_grants_raw)
names(nsf_relevant_grants_raw)
nsf_relevant_grants_c1 <- clean_unmapped_taxa()
unique(unlist(nsf_relevant_grants_c1$taxa_correct))
names(nsf_relevant_grants_c1)
nsf_relevant_grants_c2 <- clean_approximated_taxa(nsf_relevant_grants_c1)
# cottoidea is not in ott taxonomy
rotl::tnrs_match_names("cottoidea")
# according to the title of the grant it is a superfamily of sculpins
# so we have to get the families from a taxonomic database:
taxize::downstream("Cottoidea", downto = "family", db = "ncbi")
# Cottoidea is not found on any taxonomy (tried itis, col, gbif and ncbi)
# so I gathered the info available in wikipedia. It referenced Nelson 2006 fishes of the world
# so I compilated families from two editions and matched to tnrs
cott <- clean_tnrs(taxa = tolower(c("Abyssocottidae", "Comephoridae", "Cottocomephoridae",
  "Ereuniidae", "Hemitripteridae", "JORDANIIDAE", "RHAMPHOCOTTIDAE",
  "SCORPAENICHTHYIDAE", "AGONIDAE", "COTTIDAE", "PSYCHROLUTIDAE", "BATHYLUTICHTHYIDAE")))
paste(c("Abyssocottidae", "Comephoridae", "Cottocomephoridae",
  "Ereuniidae", "Hemitripteridae", "JORDANIIDAE", "RHAMPHOCOTTIDAE",
  "SCORPAENICHTHYIDAE", "AGONIDAE", "COTTIDAE", "PSYCHROLUTIDAE", "BATHYLUTICHTHYIDAE"), collapse = ",")
#Cottidae,Ereuniidae,Jordaniidae,Rhamphocottidae,Agonidae,Psychrolutidae
#Abyssocottidae,Comephoridae,Cottocomephoridae,Ereuniidae,Hemitripteridae,JORDANIIDAE,RHAMPHOCOTTIDAE,SCORPAENICHTHYIDAE,AGONIDAE,COTTIDAE,PSYCHROLUTIDAE,BATHYLUTICHTHYIDAE
nsf_relevant_grants_raw <- nsf_relevant_grants_c1
unique(unlist(nsf_relevant_grants_c2$taxa_correct))
nsf_relevant_grants_c3 <- clean_synonym_taxa(nsf_relevant_grants_c2)
unique(unlist(nsf_relevant_grants_c3$taxa_ott))
unique(unlist(nsf_relevant_grants_c3$taxa_correct))
nsf_relevant_grants_raw <- nsf_relevant_grants_c3
nsf_relevant_grants_c4 <- clean_suspicious_taxa(nsf_relevant_grants_c3, taxa=c("medium", "pegasus"))
unique(unlist(nsf_relevant_grants_c4$taxa_ott))
unique(unlist(nsf_relevant_grants_raw$taxa_correct))
head(tax_map_tnrs)
nsf_relevant_grants_raw <- nsf_relevant_grants_c4
# add_ott_families tests
tax_info[[i]]$lineage[grep("^family$", sapply(tax_info[[i]]$lineage, "[", "rank"))][[1]]$unique_name
taxa <- tolower(c("Abyssocottidae", "Comephoridae", "Cottocomephoridae",
"Ereuniidae", "Hemitripteridae", "JORDANIIDAE", "RHAMPHOCOTTIDAE",
"SCORPAENICHTHYIDAE", "AGONIDAE", "COTTIDAE", "PSYCHROLUTIDAE", "BATHYLUTICHTHYIDAE"))
external[[1]] <- "Cottidae,Ereuniidae,Jordaniidae,Rhamphocottidae,Agonidae,Psychrolutidae"
external <- "Cottidae,Ereuniidae,Jordaniidae,Rhamphocottidae,Agonidae,Psychrolutidae"
nsf_relevant_grants <- get_ott_families(nsf_relevant_grants_c4)
unlist(nsf_relevant_grants$fam_ott_ids)
usethis::use_data(nsf_relevant_grants, overwrite = TRUE)
# get money per family
fam_funds <- get_funds()
sapply("Nileidae", grep, nsf_relevant_grants$fams)
grep("Nileid", unlist(nsf_relevant_grants$fams))
usethis::use_data(fam_funds)

# get the dated family tree of Life
# devtools::use_package("datelife")
usethis::use_package("datelife")


# map tips to the Tree

# # add documentation for the package as a whole
# devtools::use_package_doc() # this creates a dummy that needs to be edited by hand
# # once it is edited, let's run document() again
# devtools::document()
# # create a vignette for the nsf funded tree of life
# devtools::use_vignette("NSF-funded-Tree-of-Life")
