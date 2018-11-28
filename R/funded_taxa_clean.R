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

# add ott_ids on all these functions too
clean_unmapped_taxa <- function(){
    utils::data("nsf_relevant_grants_raw")
    nsf_relevant_grants_raw$taxa_correct <- nsf_relevant_grants_raw$taxa_low <- sapply(nsf_relevant_grants_raw$taxa, tolower)
    unique_names1 <- vector_names(nsf_relevant_grants_raw$taxa)
    unique_names <- vector_names(nsf_relevant_grants_raw$taxa_low)
    # 0. map names to ott with tnrs:
    tax_map_tnrs <- suppressWarnings(rotl::tnrs_match_names(unique_names))
    # I. find, check and correct tnrs unmapped names (NA in unique)
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
        if(any(sapply(unmapped_correct, function(x)any(x == "")))){
          unmapped <- unmapped[sapply(unmapped_correct, function(x)any(x != ""))]
          unmapped_index <- sapply(unmapped, grep, nsf_relevant_grants_raw$taxa_correct)
          unmapped_correct <- unmapped_correct[sapply(unmapped_correct, function(x) any(x != ""))]
        }
        unmapped_correct <- sapply(unmapped_correct, strsplit, ",")
        unmapped_correct <- sapply(unmapped_correct, trimws)
        for(i in seq(unmapped_index)){
          for(j in seq(length(unmapped_index[[i]]))){
                  unmapped_i <- unmapped[i] == nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]]
                  nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]][unmapped_i] <- NA
                  all_correct <- unique(c(nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]], unmapped_correct[[i]]))
                  all_correct <- all_correct[all_correct != "NA"]
                  nsf_relevant_grants_raw$taxa_correct[[unmapped_index[[i]][j]]] <- all_correct
          }
        }
        unmapped_correct <- unique(unlist(unmapped_correct))
        unmapped_correct <- unmapped_correct[!is.na(unmapped_correct)]
        unmapped_correct <- unmapped_correct[unmapped_correct != "NA"]
        unmapped_tnrs <- suppressWarnings(rotl::tnrs_match_names(unmapped_correct))
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
    cat("\n\n\nThe following taxa in grants match ott approximately:\n", paste0("'", unique(unlist(approxed)), "' "), "\n",
        "Review the grant on screen and submit a valid ott name, NA if it is not a taxon name, or hit enter if the original name is correct.\n\n")
    approxed_correct <- vector(mode = "list", length = length(approxed))
    for(i in seq(approxed)){
        cat("OTT approximated name:\n", approxed[i], " to ", approxed2[i], "\n")
        cat("Award Title:\n", nsf_relevant_grants_raw$title[approxed_index[[i]]], "\n")
        cat("All names found in award:\n", paste0("'", unique(unlist(nsf_relevant_grants_raw$taxa_correct[approxed_index[[i]]])), "' "), "\n")
        cat("Award Abstract:\n", nsf_relevant_grants_raw$abstract[approxed_index[[i]]], "\n")
        approxed_correct[i] <- readline(paste0("\nReview the award and give a valid ott name for '", approxed[i], "' (hit enter if the name is correct; NA if it is not a taxon name; separated by comma if multiple names; uppercases are ignored): "))
    }
    if(any(sapply(approxed_correct, function(x)any(x == "")))){
        approxed <- approxed[sapply(approxed_correct, function(x)any(x != ""))]
        approxed_index <- sapply(approxed, grep, nsf_relevant_grants_raw$taxa_correct)
        approxed_correct <- approxed_correct[sapply(approxed_correct, function(x)any(x != ""))]
    }
    approxed_correct <- sapply(approxed_correct, strsplit, ",")
    approxed_correct <- lapply(approxed_correct, trimws)
    for(i in seq(approxed)){
            for(j in seq(length(approxed_index[[i]]))){
                    approxed_i <- approxed[i] == nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]]
                    nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]][approxed_i] <- NA
                    all_correct <- unique(c(nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]], approxed_correct[[i]]))
                    all_correct <- all_correct[all_correct != "NA"]
                    nsf_relevant_grants_raw$taxa_correct[[approxed_index[[i]][j]]] <- all_correct
            }
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

            for(j in seq(length(syn_index[[i]]))){
                    syn_i <- syn[i] == nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]]
                    if(syn_correct[[i]] == "NA"){
                        nsf_relevant_grants_raw$taxa_correct[[syn_index[[i]][j]]][syn_i] <- NA
                        nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]][syn_i] <- NA
                    } else {
                        nsf_relevant_grants_raw$taxa_ott[[syn_index[[i]][j]]][syn_i] <- syn_correct[[i]]
                    }
            }
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
  taxa_correct <- lapply(taxa_correct, trimws)
  for(i in seq(taxa)){
          for(j in seq(length(taxa_index[[i]]))){
                  taxa_i <- taxa[i] == nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]]
                  nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]][taxa_i] <- NA
                  nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]][taxa_i] <- NA
                  all_correct <- unique(c(nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]], taxa_correct[[i]]))
                  all_correct <- all_correct[all_correct != "NA"]
                  nsf_relevant_grants_raw$taxa_correct[[taxa_index[[i]][j]]] <- all_correct
                  nsf_relevant_grants_raw$taxa_ott[[taxa_index[[i]][j]]] <- all_correct
          }
  }
  return(nsf_relevant_grants_raw)
}
get_ott_families <- function(nsf_relevant_grants_raw = NULL, taxa = NULL, tree = NULL){
    # enhancement: add possibility to get families from other taxonomies
    # e.g., taxize::downstream("Cottoidea", downto = "family", db = "ncbi")
  if(is.null(nsf_relevant_grants_raw)){
      ott_names <- tolower(taxa)
  } else {
      ott_names <- tolower(vector_names(nsf_relevant_grants_raw$taxa_ott))
  }
  if(is.null(tree)){
      utils::data(fam_tree)
  }
  # tax_map_tnrs <- rotl::tnrs_match_names(ott_names)
  tax_map_tnrs <- datelife::batch_tnrs_match_names(names = ott_names)
  tax_info <- fam_ids <- fams <- vector(mode = "list", length = length(ott_names))
  names(tax_info) <- names(fams) <- names(fam_ids) <- ott_names
  progression <- utils::txtProgressBar(min = 0, max = length(tax_info), style = 3)
  for (i in seq(tax_info)){
      tax_info[i] <- tryCatch(rotl::taxonomy_taxon_info(tax_map_tnrs$ott_id[i], include_lineage = TRUE),
        error = function(e) NA)
      utils::setTxtProgressBar(progression, i)
  }

  tax_map_tnrs$rank <- tax_map_tnrs$ott_id
  tax_map_tnrs$rank[!is.na(tax_map_tnrs$ott_id)] <- unlist(sapply(tax_info, "[", "rank"))

  fams_index <- grep("^family$", tax_map_tnrs$rank)
  fams[fams_index] <- tax_map_tnrs$unique[fams_index]
  fam_ids[fams_index] <- paste0("ott", tax_map_tnrs$ott_id[fams_index])

  subfams_index <- sort(unname(unlist(sapply(c("species", "genus", "subfamily"), grep, tax_map_tnrs$rank))))
  fams[subfams_index] <- sapply(tax_info[subfams_index], function(x) {
      lin <- tryCatch(x$lineage[grep("^family$", sapply(x$lineage, "[", "rank"))][[1]]$unique_name,
      error = function(e) NA)
      return(lin)
  })
  fam_ids[subfams_index] <- sapply(tax_info[subfams_index], function(x) {
      lin <- tryCatch(paste0("ott", x$lineage[grep("^family$", sapply(x$lineage, "[", "rank"))][[1]]$ott_id),
      error = function(e) NA)
      return(lin)
  })
  superfams_index <- sort(unname(unlist(sapply(c("phylum", "domain", "class", "order", "superfamily"), grep, tax_map_tnrs$rank))))
  # tax_info2 <- vector(mode = "list", length = length(superfams_index))
  # progression <- utils::txtProgressBar(min = 0, max = length(superfams_index), style = 3)
  # for (i in seq(superfams_index)){
  #     tax_info2[i] <- tryCatch(rotl::taxonomy_taxon_info(tax_map_tnrs$ott_id[superfams_index[i]], include_children = TRUE),
  #       error = function(e) NA)
  #     utils::setTxtProgressBar(progression, i)
  # }
  # clean_superfams <- get_valid_children_names(tax_info2)
  # fam_rank_index <- sapply(clean_superfams$ranks, function(x) grep("^family$", x))
  #
  # for(i in seq(superfams)[sapply(fam_rank_index, length)>0]){
  #     fams[superfams_index[i]][[1]] <- unname(unlist(clean_superfams$unique_names[[i]][fam_rank_index[[i]]]))
  #     fam_ids[superfams_index[i]][[1]] <- unname(unlist(clean_superfams$ott_ids[[i]][fam_rank_index[[i]]]))
  # }
  nodes <- sapply(as.character(tax_map_tnrs$unique_name[superfams_index]), grep, fam_tree$node.label)
  ff <- sapply(unlist(nodes)+ape::Ntip(fam_tree), function(x) {
      tt <- phytools::getDescendants(fam_tree, x)
      ii <- fam_tree$ott_ids[tt[tt <= ape::Ntip(fam_tree)]]
      names(ii) <- fam_tree$ott_names[tt[tt <= ape::Ntip(fam_tree)]]
      ii
  })
  names(ff) <- tolower(names(ff))
  for(i in tolower(names(ff))){
      fams[[i]] <- names(ff[[i]])
      fam_ids[[i]] <- unname(ff[[i]])
  }
  all_index <- sapply(nsf_relevant_grants_raw$taxa_ott, match, ott_names)
  if(any(sapply(fams, length) ==0)){
    empty <- ott_names[sapply(fams, length) ==0]
    empties <- paste0("'", empty, "'", collapse = " ")
    get_ott <- readline(paste0("\nThe following taxa retrieved no OTT families: ", empties, "\nDo you want to add some: "))
    if(grepl("y", tolower(get_ott))){
      empty_index <- which(sapply(fams, length) ==0)
      for (i in seq(empty)){
        external <- readline(paste0("\nProvide family names from external data for taxon '", empty[i], "' to match to OTT (hit enter or NA if you have no external data; separate names by comma if multiple; uppercases are ignored): "))
        external <- unlist(strsplit(external, ","))
        external <- trimws(external)
        tax_map_tnrs <- datelife::batch_tnrs_match_names(names = external)
        tax_map_tnrs <- clean_tnrs(tnrs = tax_map_tnrs)
        fams[[empty_index[i]]] <- tax_map_tnrs$unique
        fam_ids[[empty_index[i]]] <- paste0("ott", tax_map_tnrs$ott_id)
      }
    }
  }
  ## now add new data to the grants object:
  nsf_relevant_grants_raw$fams <- nsf_relevant_grants_raw$fam_ott_ids <- vector(mode = "list", length = length(nsf_relevant_grants_raw$taxa_ott))
  for (i in seq(all_index)){
    if(length(all_index[[i]][!is.na(all_index[[i]])]) == 0){
      next
    } else {
      ottnames_index <- all_index[[i]][!is.na(all_index[[i]])]
      nsf_relevant_grants_raw$fams[[i]] <- unique(unlist(fams[ottnames_index]))
      nsf_relevant_grants_raw$fam_ott_ids[[i]] <- unique(unlist(fam_ids[ottnames_index]))
    }
  }
  return(nsf_relevant_grants_raw)
}


#' identifies valid children names, ott_ids and ranks from a taxonomy_taxon_info output
#' returns a list with valid children unique OTT names, ott_ids and ranks
# get_valid_children_names <-
clean_taxon_info_children <- function(taxon_info, invalid = c("barren", "extinct", "uncultured", "major_rank_conflict", "incertae_sedis", "unplaced", "conflict", "environmental", "not_otu")){
    # invalid <- c("BARREN", "EXTINCT", "UNCULTURED", "MAJOR_RANK_CONFLICT", "INCERTAE", "UNPLACED", "CONFLICT")
    # names(taxon_info[[2]])
    for (i in seq(taxon_info)){
      # length(tax_info[[i]][[1]]$children)
      # sapply(tax_info[[i]][[1]]$children, length)
      # length(sapply(sapply(tax_info[[i]][[1]]$children, "[", "flags"), function(y) unlist(tolower(y))))
      # sapply(sapply(tax_info[[i]][[1]]$children, "[", "flags"), function(y) unlist(tolower(y)))[1]
      ii <- lapply(sapply(sapply(taxon_info[[i]]$children, "[", "flags"), unlist), function(y) any(toupper(invalid) %in% y))
      # ii <- lapply(sapply(sapply(taxon_info[[i]][[1]]$children, "[", "flags"), unlist), function(y) any(toupper(invalid) %in% y))
      if(length(ii)>0){
        taxon_info[[i]]$children <- taxon_info[[i]]$children[!unlist(ii)]
      }
      # now clean names with sp. or environmental
      ii <- unlist(sapply(c("sp\\.","environmental", "unclassified", "incertae"), function(x) grep(x, sapply(sapply(taxon_info[[i]]$children, "[", "unique_name"), unlist))))

      if(length(ii)>0){
        taxon_info[[i]]$children <- taxon_info[[i]]$children[-unique(ii)]
      }
      # taxon_info[[i]][[1]]$children <- taxon_info[[i]][[1]]$children[!unlist(ii)]
    }
    return(taxon_info)
}

#' checks input for get_ott_clade and get_ott_children functions
#' returns a numeric vector of ott ids
check_ott_input <- function(input, ott_id){
    if(is.null(ott_id)){
          # input <- datelife::datelife_query_check(input)$cleaned_names
          input_tnrs <- datelife::batch_tnrs_match_names(names = input)
          # should we clean tnrs from invalid? what about NA's?
          input_ott_match <- suppressWarnings(as.numeric(input_tnrs$ott_id))
          names(input_ott_match) <- input_tnrs$unique_name
      if(any(is.na(input_ott_match))){
          message(paste0("Input '", paste(input[which(is.na(input_ott_match))], collapse = "', '"), "', not found in Open Tree of Life Taxonomy."))
          input_ott_match <- input_ott_match[which(!is.na(input_ott_match))]
      }
    } else {
          input_ott_match <- suppressWarnings(as.numeric(ott_id))
          # add a check for existing/valid ott ids??
      if(any(is.na(input_ott_match))){
          message(paste0("Ott id '", paste(ott_id[which(is.na(input_ott_match))], collapse = "', '"), "', not numeric and will be excluded from the search."))
          input_ott_match <- input_ott_match[which(!is.na(input_ott_match))]
      }
      names(input_ott_match) <- rotl::tax_name(rotl::taxonomy_taxon_info(ott_ids = input_ott_match))
    }
    if(length(input_ott_match) < 1){
      message("At least one valid input name or numeric ott_id are needed to get any information")
      return(NA)
    }
    return(input_ott_match)
}
#' gets the ott id and name of one or several given taxonomic rank from one or more input taxa
#' returns a list of named numeric vectors of ott ids from input and all corresponding requested ranks
get_ott_clade <- function(input = c("Felis", "Homo"), ott_id = NULL, rank = "family"){
    # ott_id= c('649007', '782239', '782231', '1053057', '372826', '766272', '36015', '914245', '873016', '684051')
    # ott_id = c('431493', '431493', '431493', '431493', '431493', '431493', '431493', '429482', '429482', '429482')
  input_ott_match <- check_ott_input(input, ott_id)
  tax_info <- vector(mode = "list", length = length(input_ott_match))
  progression <- utils::txtProgressBar(min = 0, max = length(tax_info), style = 3)
  for (i in seq(input_ott_match)){
      tax_info[i] <- tryCatch(rotl::taxonomy_taxon_info(input_ott_match[i], include_lineage = TRUE),
        error = function(e) NA)
      utils::setTxtProgressBar(progression, i)
  }
  # names(tax_info[[10]])
  # sapply(tax_info[[10]]$lineage, "[", "rank")
  # length(tax_info[[10]]$lineage)
  # names(tax_info[[10]]$lineage[[1]])
  # tax_info[[10]]$lineage[[1]]$flags
  input_ott_names <- unlist(sapply(tax_info, "[", "unique_name"))
  rank_ott_ids <- rank_names <- vector(mode = "list", length = length(rank))
  # I still need to drop all invalid lineages first here!!!
  for (i in seq(rank)){
      rank_names[[i]] <- sapply(tax_info, function(x) {
          lin <- tryCatch(x$lineage[grep(paste0("^", rank[i], "$"), sapply(x$lineage, "[", "rank"))][[1]]$unique_name,
          error = function(e) NA)
          return(lin)
      })
      rank_ott_ids[[i]] <- sapply(tax_info, function(x) {
          lin <- tryCatch(x$lineage[grep(paste0("^", rank[i], "$"), sapply(x$lineage, "[", "rank"))][[1]]$ott_id,
          error = function(e) NA)
          return(lin)
      })
      names(rank_ott_ids[[i]]) <- rank_names[[i]]
      # length(rank_ott_ids[i])
      # stop()
  }
  names(input_ott_match) <-  input_ott_names
  res <- c(list(input_ott_match), rank_ott_ids)
  names(res) <- c("input", rank)
  return(res)
}
#'
#' extracts valid children from a taxon_info object from rotl
get_valid_children <- function(input = c("Felis", "Homo", "Malvaceae"), ott_id = NULL){
    input_ott_match <- check_ott_input(input, ott_id)
    all_children <- vector(mode = "list", length = length(input_ott_match))
    # monotypic <- vector(mode = "logical", length = length(input_ott_match))
    progression <- utils::txtProgressBar(min = 0, max = length(all_children), style = 3)
    for (i in seq(length(input_ott_match))){
        tt <- tryCatch(rotl::taxonomy_taxon_info(input_ott_match[i], include_children = TRUE),
          error = function(e) NA)
        tt <- clean_taxon_info_children(tt) # removes all invalid children
        if(length(tt[[1]]$children) > 0){
          # sapply(tt[[1]]$children, "[", "flags")
          # sapply(tt[[1]]$children, "[", "unique_name")
          # which(unlist(sapply(tt[[1]]$children, "[", "unique_name")) == "Mesangiospermae")
          # tt[[1]]$children[108]
          rr <- unname(unlist(sapply(tt[[1]]$children, "[", "rank")))
          # ii <- grep(paste0("^", ott_rank, "$"), unname(unlist(rr)))  # need to unlist rr
          child <- unname(unlist(sapply(tt[[1]]$children, "[", "ott_id")))
          # if(length(child)>0){
              names(child) <- names(rr) <- unname(unlist(sapply(tt[[1]]$children, "[", "unique_name")))
          # }
          monotypic <- FALSE
        } else {
          child <- tt[[1]]$ott_id
          rr <- tt[[1]]$rank
          names(child) <- names(rr) <- tt[[1]]$unique_name
          monotypic <- TRUE
        }
        all_children[[i]] <- list(children = data.frame(ott_id = child, rank = rr), is_monotypic = monotypic)
        utils::setTxtProgressBar(progression, i)
    }
    names(all_children) <- names(input_ott_match)
    return(all_children)
}
get_ott_children <- function(input = c("Felis", "Homo", "Malvaceae"), ott_id = NULL, ott_rank = "species"){
    input_ott_match <- check_ott_input(input, ott_id)
    all_children <- vector(mode = "list", length = length(input_ott_match))
    # progression <- utils::txtProgressBar(min = 0, max = length(all_children), style = 3)
    for (i in seq(length(input_ott_match))){
        mm <- data.frame(ott_id = vector(mode = "numeric", length = 0), rank = vector(mode = "logical", length = 0))
        vv <- get_valid_children(ott_id = input_ott_match[i])
        success <- vv[[1]]$children$rank == ott_rank | vv[[1]]$is_monotypic
        if(any(success)){
          mm <- rbind(mm, vv[[1]]$children[success,])
        }
        while(!all(success)){
          vv <- get_valid_children(ott_id = unlist(sapply(sapply(vv, "[", "children"), "[", "ott_id"))[!success])
          if(any(unlist(sapply(vv, "[", "is_monotypic")))){
            mm <- rbind(mm, do.call("rbind", sapply(vv[unlist(sapply(vv, "[", "is_monotypic"))], "[", "children")))
            vv <- vv[!unlist(sapply(vv, "[", "is_monotypic"))]
          }
          success <- unlist(sapply(sapply(vv, "[", "children"), "[", "rank")) == ott_rank
          if(any(success)){
            mm <- rbind(mm, do.call("rbind", sapply(vv, "[", "children"))[success,])
          }
        }
        # utils::setTxtProgressBar(progression, i)
        all_children[[i]] <- mm
    }
    names(all_children) <- names(input_ott_match)
    return(all_children)
}
