# function to get funds by family from grants object
get_funds <- function(data = nsf_relevant_grants){
    fam_ott_ids <- vector_names(data$fam_ott_ids)
    fams <- vector_names(data$fams)
    fam_ott_ids_index <- vector(mode = "list", length = length(fam_ott_ids))
    progression <- utils::txtProgressBar(min = 0, max = length(fam_ott_ids), style = 3)
    for(i in seq(fam_ott_ids)){
      fam_ott_ids_index[[i]] <- grep(fam_ott_ids[i], data$fam_ott_ids)
      utils::setTxtProgressBar(progression, i)
    }
    cat("\n")
    # fams_index <- sapply(fams, grep, data$fams)
    # fam_names_funds <- sapply(fams_index, function(x) sum(as.numeric(data$estimatedTotalAmt[x])))
    # fam_names_funds[!unname(fam_names_funds) == fam_ott_ids_funds]
    fam_ott_ids_funds <- sapply(fam_ott_ids_index, function(x) sum(as.numeric(data$estimatedTotalAmt[x])))
    names(fam_ott_ids_funds) <- fam_ott_ids
    return(list(funds = fam_ott_ids_funds, ott_names = fams))
}

#' Sum up values assigned to tips of a tree down its nodes.
#' It is used to get total fundings for each node, but can be used to sum up any tip value up to all nodes
#' @param tree A phylo object
#' @param values A numeric vector named with the tip labels of the tree. Names must match tip labels from tree.
#' @details
#' For now you must use tip labels to name the values vector. We can later just take values in the order of tips, in case users do not want to name their values vector.
#' @examples
#' # generate a random tree:
#' library(ape)
#' t1 <- rcoal(10)
#' # generate some fake richness data
#' r1 <- c(rep(1,5), 400, 20, 30, 5, 200)
#' names(r1) <- t1$tip.label
#' # get the node richness values
#' x <- sum_tips(tree = t1, values = r1)
#' # plot them:
#' plot(t1, label.offset = 0.1)
#' tiplabels(x[1:Ntip(t1)], cex = 0.5)
#' nodelabels(x[-c(1:Ntip(t1))], cex = 0.5)
sum_tips <-  function(tree, values) {
    # tree <- ape::reorder.phylo(tree, "postorder")  # no need to reorder the whole tree
    res <- numeric(max(tree$edge))
    res[1:ape::Ntip(tree)] <- values[match(names(values), tree$tip.label)]
    for (i in ape::postorder(tree))  { # ape postorder doesn't include root
         tmp <- tree$edge[i,1]
         # print(i)
         res[tmp] <- res[tmp] + res[tree$edge[i, 2]]
         # print(res)
   }
   res
}
