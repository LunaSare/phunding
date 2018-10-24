#' Get taxa from awarded grant proposals within Systematics and Phylogenetics
#'
#' @param database A character vector with the database to get taxa from. Default to rnsf package grants database
#' @return A data frame with grant data
#' @export
# Inspired by bomeara/hexapods/R/functions.R get_funding and ExtractTaxonNames functions
# required packages
# rnsf
# rphylotastic
# devtools::install_github("bomeara/rnsf")  # installing package is not working so downloaded data file for now
# load("grants.rda")
get_funded_taxa <- function(database = "grants.rda") {
  # utils::data(grants, package = "rnsf") #from rnsf
  # installing package is not working so downloaded data to this package for now
  # load("data-raw/grants.rda") # cannot do this inside a function
  utils::data(grants)
  if(database == "grants.rda"){
    # utils::data("grants.rda")
    good.grant.indices <- which(grepl("systematics|phylo|bioinfor|taxonom|revision|peet", grants$fundProgramName, ignore.case=TRUE))
    good.grant.indices <- c(good.grant.indices,which(grepl("systematics|phylo|bioinfor|taxonom|revision|peet", grants$abstractText, ignore.case=TRUE)))
    good.grant.indices <- c(good.grant.indices,which(grepl("systematics|phylo|bioinfor|taxonom|revision|peet", grants$title, ignore.case=TRUE)))


    relevant.grants <- grants[unique(good.grant.indices),]
    taxa <- lapply(seq(relevant.grants$title), function(i){
        rphylotastic::text_get_scientific_names(paste(#as.character(relevant.grants$abstractText[i]),
        as.character(relevant.grants$title[i])))
        })
    relevant.grants$taxa <- taxa
    #this is too slow, maybe for loop is faster???
    # taxa <- vector(mode="list", length = length(relevant.grants$title))
    # for (i in seq(relevant.grants$title)){
    #     taxa[[i]] <- rphylotastic::text_get_scientific_names(paste(#as.character(relevant.grants$abstractText[i]),
    #     as.character(relevant.grants$title[i])))
    #     print(paste(i, ":", taxa[[i]]))
    # }
  }
  # list(nsf = relevant.grants)
  return(relevant.grants)
}


#' Systematics and Phylogenetics NSF awards information and studied taxa
#'
#' It includes grants from its (NSF) start until the package was last updated.
#' @name nsf_relevant_grants_raw
#' @docType data
#' @format A list of 49 elements, with infromstion on NSF awarded grants in Systematics and Phylogenetics.
#' \describe{
#'   \item{abstractText}{A character vector of grant abstracts}
#'   \item{agency}{A character vector of }
#'   \item{awardAgencyCode}{A character vector of }
#'   \item{awardeeAddress}{A character vector of }
#'   \item{awardeeCity}{A character vector of }
#'   \item{awardeeCountryCode}{A character vector of }
#'   \item{awardeeCounty}{A character vector of }
#'   \item{awardeeDistrictCode}{A character vector of }
#'   \item{awardeeName}{A character vector of }
#'   \item{awardeeStateCode}{A character vector of }
#'   \item{awardeeZipCode}{A character vector of }
#'   \item{cfdaNumber}{A character vector of }
#'   \item{dunsNumber}{A character vector of }
#'   \item{estimatedTotalAmt}{A character vector of }
#'   \item{fundsObligatedAmt}{A character vector of }
#'   \item{fundAgencyCode}{A character vector of }
#'   \item{fundProgramName}{A character vector of }
#'   \item{id}{A character vector of }
#'   \item{parentDunsNumber}{A character vector of }
#'   \item{pdPIName}{A character vector of }
#'   \item{perfAddress}{A character vector of }
#'   \item{perfCity}{A character vector of }
#'   \item{perfCountryCode}{A character vector of }
#'   \item{perfCounty}{A character vector of }
#'   \item{perfDistrictCode}{A character vector of }
#'   \item{perfLocation}{A character vector of }
#'   \item{perfStateCode}{A character vector of }
#'   \item{perfZipCode}{A character vector of }
#'   \item{piFirstName}{A character vector of }
#'   \item{piLastName}{A character vector of }
#'   \item{poName}{A character vector of }
#'   \item{primaryProgram}{A character vector of }
#'   \item{date}{A character vector of }
#'   \item{startDate}{A character vector of }
#'   \item{expDate}{A character vector of }
#'   \item{title}{A character vector of }
#'   \item{transType}{A character vector of }
#'   \item{awardee}{A character vector of }
#'   \item{coPDPI}{A list of }
#'   \item{piMiddeInitial}{A character vector of Principal Investigator middle initials}
#'   \item{publicationResearch}{A list of }
#'   \item{projectOutComesReport}{A character vector of }
#'   \item{response.serviceNotification.notificationType}{A character vector of }
#'   \item{response.serviceNotification.notificationCode}{A character vector of }
#'   \item{response.serviceNotification.notificationMessage}{A character vector of }
#'   \item{taxa}{A list of }
#'   \item{taxa_correct}{A list of }
#'   \item{name_tnrs}{A list of }
#'   \item{fam_tnrs}{A list of }
#' }
#' @source \url{http://www.nsf.org}
#' @keywords nsf grant taxa systematics phylogenetics
#' @details
#'
#' Generated with nsf_relevant_grants_raw <- get_funded_taxa()
#'
"nsf_relevant_grants_raw"

#' NSF awards information database from rnsf package
#'
#' It includes grants from its start until the databse was last copied in 2018.10.15.
#' @name grants
#' @docType data
#' @format A list of 45 elements, with information on NSF awarded grants.
#' @source \url{http://www.nsf.org}
#' @keywords nsf grant
"grants"
