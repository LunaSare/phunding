# functions to get funds by family
get_funds <- function(data = nsf_relevant_grants){
    fam_ott_ids <- vector_names(data$fam_ott_ids)
    fams <- vector_names(data$fams)
    fam_ott_ids_index <- sapply(fam_ott_ids, grep, data$fam_ott_ids)
    # fams_index <- sapply(fams, grep, data$fams)
    # fam_names_funds <- sapply(fams_index, function(x) sum(as.numeric(data$estimatedTotalAmt[x])))
    fam_names_funds[!unname(fam_names_funds) == fam_ott_ids_funds]
    fam_ott_ids_funds <- sapply(fam_ott_ids_index, function(x) sum(as.numeric(data$estimatedTotalAmt[x])))
    names(fam_ott_ids_funds) <- paste0("ott", fam_ott_ids)
    return(list(funds = fam_ott_ids_funds, ott_names = fams))
}
