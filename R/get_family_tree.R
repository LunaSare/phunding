#' function to get a family tree level from Open Tree of Life synthetic tree
#'

get_otol_family_tree <- function(){
    utils::data(all_ott_fams)
    fam_tree <- suppressWarnings(rotl::tol_induced_subtree(ott_id=all_ott_fams$ott_id, label_format = "name"))
    fam_tree$ott_ids <- suppressWarnings(rotl::tol_induced_subtree(ott_id=all_ott_fams$ott_id, label_format = "id")$tip.label)
    # mrca <- grepl("mrcaott", fam_tree$tip.label)
    # fam_tree <- ape::drop.tip(fam_tree, fam_tree$tip.label[mrca])
    fam_tree$ott_names <- fam_tree$tip.label
    return(fam_tree)
}

#' Open Tree of Life Family Tree
#'
#' Induced OTL subtree of family rank taxa from OTT
#' @name fam_tree
#' @docType data
#' @format A phylo object.
#' @source \url{http://www.otol.org}
#' @keywords otol tree families
#' @details
#'
#' Generated with fam_tree <- get_otol_family_tree()
#'
"fam_tree"
