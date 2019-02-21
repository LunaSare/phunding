#' Open Tree of Life Land Plant Orders Chronogram
#'
#' Dated induced OToL subtree of OTT order rank taxa of Land Plants
#' @name fam_tree
#' @docType data
#' @format A phylo object.
#' @source \url{http://www.otol.org}
#' @keywords otol tree orders chronogram time land plants
#' @details
#'
#' Generated with code in data-raw/plant_family_tree-2dateA.R
#'
"plant_tree_orders"


#' Open Tree of Life Orders
#'
#' Data Frame of all order in Open Tree of Life Taxonomy
#' @name all_ott_orders
#' @docType data
#' @format data frame
#' @source \url{}
#' @keywords otol orders ott
#' @details
#' Generated with
#' grep -i "order" taxonomy.tsv > orders.tsv
#' grep -i "order" taxonomy.tsv | grep -v "genus" | grep -v "no rank - terminal" | grep -v "merged" | grep -v "barren" | grep -v "extinct" | grep -v "major_rank_conflict_inherited" | grep -v "no rank" | grep -v "family" | grep -v "suborder" | grep -v "species" | grep -v "infraorder" | grep -v "unplaced" | grep -v "superorder" | grep -v "tribe" | grep -v "parvorder" > orders.tsv
#' all_ott_orders <- read.table("/Users/luna/GoogleDrive/datelife/otol/taxonomy/ott/orders.tsv", sep = "|", header = FALSE, fill = TRUE, stringsAsFactors=FALSE)
#' names(all_ott_orders) <- c("ott_id", "parent_ott_id", "name", "rank", "source_info", "unique_name", "flags", "?")
#' for (i in c("name", "rank", "source_info", "unique_name", "flags")){
#'   all_ott_orders[,i] <- gsub("\\t", "", all_ott_orders[,i])
#' }
#' all_ott_orders[,"source_info"] <- gsub("/", "", all_ott_orders[,"source_info"])
#' unique(all_ott_orders[,"flags"])
#' usethis::use_data(all_ott_orders, overwrite = TRUE)
"all_ott_orders"

#' Open Tree of Life Orders
#'
#' Data Frame of all order in Open Tree of Life Taxonomy
#' @name all_orders_chrono
#' @docType data
#' @format data frame
#' @source \url{}
#' @keywords otol orders chronogram
#' @details
#' Generated with
#' data(all_ott_orders)
#' all_orders_chrono <- datelife::get_dated_otol_induced_subtree(ott_id = all_ott_orders$ott_id)
#' missing_index <- is.na(match(all_ott_orders$name, all_orders_chrono$tip.label))
#' missing_ott_orders <- all_ott_orders$ott_id[missing_index]
#' names(missing_ott_orders) <- all_ott_orders$name[missing_index]
#' all_orders_chrono$missing_ott_orders <- missing_ott_orders
#' usethis::use_data(all_orders_chrono, overwrite = TRUE)
"all_orders_chrono"
