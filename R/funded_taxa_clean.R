clean_funded_taxa <- function(){
    utils::data("nsf_relevant_grants_raw")
    nsf_relevant_grants_raw$taxa <- sapply(nsf_relevant_grants_raw$taxa, tolower)
    nsf_relevant_grants_raw$fam_tnrs <- nsf_relevant_grants_raw$taxa_correct <- nsf_relevant_grants_raw$taxa
    vector_names <- function(all){
      index <- sapply(all, function(x) length(x) > 0)
      funded_names <- all[index]
      unique_names <- unique(unlist(funded_names))
      unique_names <- unique_names[!unique_names == "NA"]
      unique_names <- tolower(unique_names[!is.na(unique_names)])
      return(unique_names)
    }
    unique_names <- vector_names(nsf_relevant_grants_raw$taxa)
    # I. map names to ott with tnrs:
    tax_map_tnrs <- suppressWarnings(rotl::tnrs_match_names(unique_names))
    # II. find, check and correct tnrs unmapped names (NA in unique)
    unmapped <- unique_names[is.na(tax_map_tnrs$unique)]
    cat("The following taxa in grants did not match ott.\n", paste0("'", unmapped, "' "), "\n",
        "The grants they were extracted from will be displayed, please submit a corrected name or NA if it is not a taxon name.")
    unmapped_index <- sapply(unmapped, grep, nsf_relevant_grants_raw$taxa)
    unmapped_correct <- vector(mode = "list", length = length(unmapped))
    for(i in seq(unmapped)){
        interactive()
        print(nsf_relevant_grants_raw$title[unmapped_index[[i]]])
        print(nsf_relevant_grants_raw$taxa[unmapped_index[[i]]])
        unmapped_correct[i] <- readline("Review the award title/titles where the taxon appears and write down the correct name (separated by comma if multiple): ")
    }
    unmapped_correct <- sapply(unmapped_correct, strsplit, ",")
    unmapped_correct <- sapply(unmapped_correct, trimws)
    for(i in seq(unmapped_index)){
        if(length(unmapped_index[[i]]) == 1){
            nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]]]] <- unmapped_correct[[i]]
        } else {
            for(j in seq(length(unmapped_index[[i]]))){
                nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]] <- unmapped_correct[[i]]
            }
        }
    }
    # III. determine, check and correct names approximated to ott
    correct_names <- vector_names(nsf_relevant_grants_raw$taxa_correct)
    approxed <- tax_map_tnrs$search[tax_map_tnrs$approx]
    approxed <- approxed[!is.na(approxed)]
    approxed_index <- sapply(approxed, grep, nsf_relevant_grants_raw$taxa)
    for(i in seq(approxed)){
        print(nsf_relevant_grants_raw$title[approxed_index[[i]]])
        print(nsf_relevant_grants_raw$taxa[approxed_index[[i]]])
        print(nsf_relevant_grants_raw$abstract[approxed_index[[i]]])
    }


}
