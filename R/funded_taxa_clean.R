vector_names <- function(all){ # change to vector_grant_taxa
  index <- sapply(all, function(x) length(x) > 0)
  funded_names <- all[index]
  unique_names <- unique(unlist(funded_names))
  unique_names <- unique_names[!unique_names == "NA"]
  unique_names <- unique_names[!unique_names == ""]
  unique_names <- unique_names[!is.na(unique_names)]
  # unique_names <- tolower(unique_names)
  return(unique_names)
}
clean_unmapped_taxa <- function(){
    utils::data("nsf_relevant_grants_raw")
    nsf_relevant_grants_raw$taxa_correct <- nsf_relevant_grants_raw$taxa_low <- sapply(nsf_relevant_grants_raw$taxa, tolower)
    # replace_names <- function(i,j){
    #     if(length(nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]]) > 1){
    #         unmapped_i <- unmapped[i] == nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]]
    #         nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]][unmapped_i] <- unmapped_correct[[i]]
    #     } else {
    #         nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]] <- unmapped_correct[[i]]
    #     }
    # }
    unique_names1 <- vector_names(nsf_relevant_grants_raw$taxa)
    unique_names <- vector_names(nsf_relevant_grants_raw$taxa_low)
    # 0. map names to ott with tnrs:
    tax_map_tnrs <- suppressWarnings(rotl::tnrs_match_names(unique_names))
    # I. find, check and correct tnrs unmapped names (NA in unique)
    # unmapped1 <- unique_names1[is.na(tax_map_tnrs$unique)]
    unmapped <- unique_names[is.na(tax_map_tnrs$unique)]
    while(length(unmapped)>0){
        cat("The following taxa extracted from grants do not match ott.\n", paste0("'", unmapped, "' "), "\n\n",
            "Review the award information and submit a valid ott name or NA if it is not a taxon name.\n\n")
        unmapped_index <- sapply(unmapped, grep, nsf_relevant_grants_raw$taxa_low)
        unmapped_correct <- vector(mode = "list", length = length(unmapped))
        for(i in seq(unmapped)){
            cat("OTT unmatched name:\n", paste0("'", unmapped[i], "'"), "\n")
            cat("Award Title:\n", unique(unlist(nsf_relevant_grants_raw$title[unmapped_index[[i]]])), "\n")
            cat("All names found in award:\n", paste0("'", unique(unlist(nsf_relevant_grants_raw$taxa[unmapped_index[[i]]])), "' "), "\n")
            cat("Award Abstract:\n", unique(unlist(nsf_relevant_grants_raw$abstract[unmapped_index[[i]]])), "\n")
            unmapped_correct[i] <- readline(paste0("\nReview the award and give a valid ott name for' ", unmapped[i], "' (NA if it is not a taxon name; separated by comma if multiple names; uppercases are ignored): "))
        }
        # unmapped_correct <- c(rep(list("NA"),4), list("ploceus"), list("chrysochus, apocynaceae"))
        if(any(sapply(unmapped_correct, function(x)any(x == "")))){
          unmapped <- unmapped[sapply(unmapped_correct, function(x)any(x != ""))]
          unmapped_index <- sapply(unmapped, grep, nsf_relevant_grants_raw$taxa_correct)
          unmapped_correct <- unmapped_correct[sapply(unmapped_correct, function(x) any(x != ""))]
        }
        unmapped_correct <- sapply(unmapped_correct, strsplit, ",")
        unmapped_correct <- sapply(unmapped_correct, trimws)
        for(i in seq(unmapped_index)){
          for(j in seq(length(unmapped_index[[i]]))){
              # if(length(nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]]) > 1){
                  unmapped_i <- unmapped[i] == nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]]
                  nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]][unmapped_i] <- NA
                  all_correct <- unique(c(nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]], unmapped_correct[[i]]))
                  all_correct <- all_correct[all_correct != "NA"]
                  nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]] <- all_correct
          }
            # if(length(unmapped_index[[i]]) == 1){
            #     # replace_names(i, j =1)
            #     j <- 1
            #     if(length(nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]]) > 1){
            #         unmapped_i <- unmapped[i] == nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]]
            #         nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]][unmapped_i] <- unmapped_correct[[i]]
            #     } else {
            #         nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]] <- unmapped_correct[[i]]
            #     }
            # } else {
            #     for(j in seq(length(unmapped_index[[i]]))){
            #         # replace_names(i, j)
            #         if(length(nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]]) > 1){
            #             unmapped_i <- unmapped[i] == nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]]
            #             nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]][unmapped_i] <- unmapped_correct[[i]]
            #         } else {
            #             nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]] <- unmapped_correct[[i]]
            #         }
            #     }
            # }
        }
        unmapped_correct <- unique(unlist(unmapped_correct))
        unmapped_correct <- unmapped_correct[!is.na(unmapped_correct)]
        unmapped_correct <- unmapped_correct[unmapped_correct != "NA"]
        unmapped_tnrs <- suppressWarnings(rotl::tnrs_match_names(unmapped_correct))
        # II. find, check and correct tnrs unmapped names (NA in unique)
        unmapped <- unmapped[is.na(unmapped_tnrs$unique)]
    }
    return(nsf_relevant_grants_raw)
}
clean_approximated_taxa <- function(nsf_relevant_grants_raw){
    # II. determine, check and correct names approximated to ott
    correct_names <- vector_names(nsf_relevant_grants_raw$taxa_correct)
    tax_map_tnrs <- suppressWarnings(rotl::tnrs_match_names(correct_names))
    approxed <- tax_map_tnrs$search[tax_map_tnrs$approx]
    approxed_index <- sapply(approxed, grep, tolower(nsf_relevant_grants_raw$taxa_correct))
    approxed2 <- tax_map_tnrs$unique[tax_map_tnrs$approx]
    # approxed <- approxed[!is.na(approxed)]
    cat("\n\n\nThe following taxa in grants match ott approximately:\n", paste0("'", unique(unlist(approxed)), "' "), "\n",
        "Review the grant on screen and submit a valid ott name, NA if it is not a taxon name, or hit enter if the original name is correct.\n\n")
    approxed_correct <- vector(mode = "list", length = length(approxed))
    for(i in seq(approxed)){
        # if(i==0) i <- 1
        cat("OTT approximated name:\n", approxed[i], " to ", approxed2[i], "\n")
        cat("Award Title:\n", nsf_relevant_grants_raw$title[approxed_index[[i]]], "\n")
        cat("All names found in award:\n", paste0("'", unique(unlist(nsf_relevant_grants_raw$taxa_correct[approxed_index[[i]]])), "' "), "\n")
        cat("Award Abstract:\n", nsf_relevant_grants_raw$abstract[approxed_index[[i]]], "\n")
        approxed_correct[i] <- readline(paste0("\nReview the award and give a valid ott name for '", approxed[i], "' (hit enter if the name is correct; NA if it is not a taxon name; separated by comma if multiple names; uppercases are ignored): "))
    }
    # approxed_correct <- c(list(""), list("salvia"), rep(list("NA"),3))
    if(any(sapply(approxed_correct, function(x)any(x == "")))){
        approxed <- approxed[sapply(approxed_correct, function(x)any(x != ""))]
        approxed_index <- sapply(approxed, grep, nsf_relevant_grants_raw$taxa_correct)
        approxed_correct <- approxed_correct[sapply(approxed_correct, function(x)any(x != ""))]
    }
    approxed_correct <- sapply(approxed_correct, strsplit, ",")
    approxed_correct <- lapply(approxed_correct, trimws)
    # sapply(test, function(x)any(x == "")) & sapply(test, function(x) length(x) == 1)
    for(i in seq(approxed)){
        # if(length(approxed_index[[i]]) == 1){
        #     j <- 1
        #     if(length(nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]]) > 1){
        #         approxed_i <- approxed[i] == nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]]
        #         nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]][approxed_i] <- approxed_correct[[i]]
        #     } else {
        #         nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]]]] <- NA
        #         nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]] <- approxed_correct[[i]]
        #     }
        # } else {
            for(j in seq(length(approxed_index[[i]]))){
                # if(length(nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]]) > 1){
                    approxed_i <- approxed[i] == nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]]
                    nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]][approxed_i] <- NA
                    all_correct <- unique(c(nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]], approxed_correct[[i]]))
                    all_correct <- all_correct[all_correct != "NA"]
                    nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]] <- all_correct
                # } else {
                #     nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]] <- approxed_correct[[i]]
                # }
            }
        # }
    }
    return(nsf_relevant_grants_raw)
}

clean_synonym_taxa <- function(nsf_relevant_grants_raw){
    # III. determine, check and correct names that are synonyms in tnrs taxonomy
    nsf_relevant_grants_raw$taxa_ott <- nsf_relevant_grants_raw$taxa_correct
    correct_names <- vector_names(nsf_relevant_grants_raw$taxa_correct)
    tax_map_tnrs <- rotl::tnrs_match_names(correct_names)
    syn <- tax_map_tnrs$search[tax_map_tnrs$is_syn]
    syn2 <- tax_map_tnrs$unique[tax_map_tnrs$is_syn]
    syn_index <- sapply(tolower(syn), grep, tolower(nsf_relevant_grants_raw$taxa_correct))
    cat("\n\n\nThe following taxa in grants are ott synonyms:\n", paste0("'", unique(unlist(syn)), "' "), "\n",
        "Review the grant on screen and and decide if the synonym is ok by hitting enter, NA if it is not a taxon name.\n\n")
    syn_correct <- vector(mode = "list", length = length(syn))
    for(i in seq(syn)){
        cat("OTT synonyms name:\n", paste0("'", syn[i], "'"), " to ", paste0("'", syn2[i], "'"), "\n")
        cat("Award Title:\n", nsf_relevant_grants_raw$title[syn_index[[i]]], "\n")
        cat("All names found in award:\n", paste0("'", unique(unlist(nsf_relevant_grants_raw$taxa_correct[syn_index[[i]]])), "' "), "\n")
        cat("Award Abstract:\n", nsf_relevant_grants_raw$abstract[syn_index[[i]]], "\n")
        syn_correct[i] <- readline(paste0("\nReview the award and hit enter if the OTT synonym '", syn2[i], "' is ok (NA if it is not a taxon name; uppercases are ignored): "))
    }
    syn_correct[sapply(syn_correct, function(x)any(x == ""))] <- syn2[sapply(syn_correct, function(x)any(x == ""))]
    syn_index <- sapply(syn, grep, nsf_relevant_grants_raw$taxa_correct)
    for(i in seq(syn_index)){
        # if(length(syn_index[[i]]) == 1){
        #     # replace_names(i, j =1)
        #     j <- 1
        #     if(length(nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]]) > 1){
        #         syn_i <- syn[i] == nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]]
        #         if(syn_correct[[i]] == "NA"){
        #             nsf_relevant_grants_raw$taxa_correct[[syn_index[[i]][j]]][syn_i] <- syn_correct[[i]]
        #         }
        #         nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]][syn_i] <- syn_correct[[i]]
        #     } else {
        #         if(syn_correct[[i]] == "NA"){
        #             nsf_relevant_grants_raw$taxa_correct[[syn_index[[i]][j]]] <- syn_correct[[i]]
        #         }
        #         nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]] <- syn_correct[[i]]
        #     }
        # } else {
            for(j in seq(length(syn_index[[i]]))){
                # replace_names(i, j)
                # if(length(nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]]) > 1){
                    syn_i <- syn[i] == nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]]
                    if(syn_correct[[i]] == "NA"){
                        nsf_relevant_grants_raw$taxa_correct[[syn_index[[i]][j]]][syn_i] <- NA
                        nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]][syn_i] <- NA
                    } else {
                        nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]][syn_i] <- syn_correct[[i]]
                    }
                # } else {
                #     if(syn_correct[[i]] == "NA"){
                #         nsf_relevant_grants_raw$taxa_correct[[syn_index[[i]][j]]] <- syn_correct[[i]]
                #     }
                #     nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]] <- syn_correct[[i]]
                # }
            }
        # }
    }
    return(nsf_relevant_grants_raw)
}

clean_suspicious_taxa <- function(nsf_relevant_grants_raw, taxa = NULL){
    ott_names <- vector_names(nsf_relevant_grants_raw$taxa_ott)
    if(!is.null(taxa)){
        if(!all(taxa %in% ott_names)) {
            stop("taxa are not in database")
        }
        ott_names <- tolower(as.character(taxa))
    } else {
        taxa <- tolower(ott_names)
    }
  tax_map_tnrs <- suppressWarnings(rotl::tnrs_match_names(ott_names))
  taxa_index <- sapply(taxa, grep, tolower(nsf_relevant_grants_raw$taxa_ott))
  taxa2 <- tax_map_tnrs$unique[unique(unlist(sapply(taxa, grep, tax_map_tnrs$search)))]
  cat("\n\n\nThe following taxa in grants will be checked:\n", paste0("'", unique(taxa), "' "), "\n",
      "Review the grant on screen and submit a valid ott name, NA if it is not a taxon name, or hit enter if the name is correct.\n\n")
  taxa_correct <- vector(mode = "list", length = length(taxa))
  for(i in seq(taxa)){
      cat("\n\nName to check:\n", taxa[i], " matches ", taxa2[i], "in OTT\n")
      cat("Award Title:\n", unique(unlist(nsf_relevant_grants_raw$title[taxa_index[[i]]])), "\n")
      cat("All names found in award:\n", paste0("'", unique(unlist(nsf_relevant_grants_raw$taxa_correct[taxa_index[[i]]])), "' "), "\n")
      cat("Award Abstract:\n", unique(unlist(nsf_relevant_grants_raw$abstract[taxa_index[[i]]])), "\n")
      taxa_correct[i] <- readline(paste0("\nReview the award and give a valid ott name for '", taxa[i], "' (hit enter if the name is correct; NA if it is not a taxon name; separated by comma if multiple names; uppercases are ignored): "))
  }
  if(any(sapply(taxa_correct, function(x)any(x == "")))){
      taxa <- taxa[sapply(taxa_correct, function(x)any(x != ""))]
      taxa_index <- sapply(taxa, grep, nsf_relevant_grants_raw$taxa_ott)
      taxa_correct <- taxa_correct[sapply(taxa_correct, function(x)any(x != ""))]
  }
  taxa_correct <- sapply(taxa_correct, strsplit, ",")
  taxa_correct <- lapply(approxed_correct, trimws)

  # sapply(test, function(x)any(x == "")) & sapply(test, function(x) length(x) == 1)
  for(i in seq(taxa)){
      # if(length(taxa_index[[i]]) == 1){
      #     j <- 1
      #     if(length(nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]]) > 1){
      #         taxa_i <- taxa[i] == nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]]
      #         nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]][taxa_i] <- taxa_correct[[i]]
      #         nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]][taxa_i] <- taxa_correct[[i]]
      #     } else {
      #         nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]] <- taxa_correct[[i]]
      #         nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]] <- taxa_correct[[i]]
      #     }
      # } else {
          for(j in seq(length(taxa_index[[i]]))){
              # if(length(nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]]) > 1){
                  taxa_i <- taxa[i] == nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]]
                  nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]][taxa_i] <- NA
                  nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]][taxa_i] <- NA
                  all_correct <- unique(c(nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]], taxa_correct[[i]]))
                  all_correct <- all_correct[all_correct != "NA"]
                  nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]] <- all_correct
                  nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]] <- all_correct
              # } else {
              #     nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]] <- taxa_correct[[i]]
              #     nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]] <- taxa_correct[[i]]
              # }
          }
      # }
  }
  return(nsf_relevant_grants_raw)
}
get_ott_families <- function(nsf_relevant_grants_raw = NULL, taxa = NULL){
  if(is.null(nsf_relevant_grants_raw)){
      ott_names <- taxa
  } else {
      nsf_relevant_grants_raw$taxa_ott_fams <- nsf_relevant_grants_raw$taxa_ott
      ott_names <- vector_names(nsf_relevant_grants_raw$taxa_ott)
  }
  # tax_map_tnrs <- rotl::tnrs_match_names(ott_names)
  tax_map_tnrs <- datelife::input_tnrs(input = ott_names)
  fams <- vector(mode = "list", length = length(ott_names))
  names(fams) <- ott_names

  tax_info <- rotl::taxonomy_taxon_info(tax_map_tnrs$ott_id, include_lineage = TRUE)
  # tax_info <- rotl::taxonomy_taxon_info(tax_map_tnrs$ott_id[!is.na(tax_map_tnrs$ott_id)])

  tax_map_tnrs$rank <- tax_map_tnrs$ott_id
  tax_map_tnrs$rank[!is.na(tax_map_tnrs$ott_id)] <- unlist(sapply(tax_info, "[", "rank"))

  fams_index <- grep("^family$", tax_map_tnrs$rank)
  fams[fams_index] <- tax_map_tnrs$unique[fams_index]

  subfams_index <- sort(unname(unlist(sapply(c("species", "genus", "subfamily"), grep, tax_map_tnrs$rank))))
  fams[subfams_index] <- sapply(tax_info[subfams_index], function(x) x$lineage[grep("^family$", sapply(x$lineage, "[", "rank"))][[1]]$unique_name)
  # x <- tax_info[[1]]
  superfams_index <- sort(unname(unlist(sapply(c("phylum", "domain", "class", "order", "superfamily"), grep, tax_map_tnrs$rank))))
  tax_info2 <- rotl::taxonomy_taxon_info(tax_map_tnrs$ott_id[superfams_index], include_children = TRUE)
  # length(tax_info2[[1]]$children)
  # tax_info2[[1]]$children[[1]]
  # length(tax_info[[1]]$children[grep("^family$", sapply(tax_info2[[1]]$children, "[", "rank"))][[1]]$unique_name)
  ranks <- sapply(tax_info2, function(x) sapply(x$children, "[", "rank"))
  ranks_index <- sapply(ranks, function(x) grep("^family$", x))
  # sapply(ranks_index, length) # number of actual families
  # sapply(tax_info2[4], function(x) sapply(x$children, "[", "rank"))
  superfams <- sapply(tax_info2, function(x) sapply(x$children, "[", "unique_name"))
  # length(superfams)
  # superfams[[1]][ranks_index[[1]]]
  # superfams[unlist(ranks_index)]
  # fams[superfams_index]

  for(i in seq(superfams)[sapply(ranks_index, length)>0]){
      fams[superfams_index[i]][[1]] <- unname(unlist(superfams[[i]][ranks_index[[i]]]))
  }
  all_index <- sapply(nsf_relevant_grants_raw$taxa_ott, match, ott_names)
  # sapply(all_index, length)
  # nsf_relevant_grants_raw$taxa_ott[253]
  if(any(sapply(fams, length) ==0)){
    empty <- ott_names[sapply(fams, length) ==0]
    get_ott <- readline(paste0("\nThe following taxa retrieved no OTT families: ", paste0("'", empty, "'"), "\nDo you want to add some: "))
    if(grepl("y", tolower(get_ott))){
      # external <- vector(mode = "list", length = length(empty))
      empty_index <- which(sapply(fams, length) ==0)
      for (i in seq(empty)){
        # external[[i]] <- readline(paste0("\nReview the award and provide family names from external data for taxon '", empty[i], "' to match to OTT (hit enter or NA if you have no external data; separate names by comma if multiple; uppercases are ignored): "))
        external <- readline(paste0("\nReview the award and provide family names from external data for taxon '", empty[i], "' to match to OTT (hit enter or NA if you have no external data; separate names by comma if multiple; uppercases are ignored): "))
        external <- unlist(strsplit(external, ","))
        external <- trimws(external)
        # fams[empty_index[i]] <- NA
        fams[[empty_index[i]]] <- get_ott_taxa(taxa = external)
      }
      # external <- sapply(external, strsplit, ",") # need to be sapply in here and lapply in the next one
      # external <- lapply(external, trimws)
      # ott_fams <- lapply(external, get_ott_taxa)
    }
    # fams[sapply(fams, length) ==0] <- ott_fams
  }
  nsf_relevant_grants_raw$fams <- vector(mode = "list", length = length(nsf_relevant_grants_raw$taxa_ott))
  for (i in seq(all_index)){
    if(length(all_index[[i]][!is.na(all_index[[i]])]) == 0){
      next
    } else {
      ottnames_index <- all_index[[i]][!is.na(all_index[[i]])]
      nsf_relevant_grants_raw$fams[[i]] <- unique(unlist(fams[ottnames_index]))
    }
  }
  return(nsf_relevant_grants_raw)
}

get_ott_taxa <- function(taxa){
  # add possibility to get families from other taxonomies
  # taxize::downstream("Cottoidea", downto = "family", db = "ncbi")
  if(!is.character(taxa)){
    stop("taxa must be a character vector")
  }
  # taxa_tnrs <- suppressWarnings(taxatnrs <- rotl::tnrs_match_names(taxa))
  # taxa_tnrs <- taxa_tnrs[!is.na(taxa_tnrs$unique),]
  taxa_tnrs <- datelife::input_tnrs(input = taxa)
  unvalid <- c("BARREN", "EXTINCT", "UNCULTURED", "CONFLICT", "INCERTAE", "UNPLACED")
  x <- sapply(unvalid, grepl, taxa_tnrs$flags)
  if(nrow(taxa_tnrs)==1){
    x <- matrix(x, ncol = 6, dimnames = list(NULL, names(x)))
  }
  x <- sapply(1:nrow(x), function(z) sum(x[z,]))
  taxa_ott <- unique(taxa_tnrs[x==0, ]$unique_name)
  taxa_ott <- taxa_ott[!is.na(taxa_ott)]
  return(unname(taxa_ott))
}
