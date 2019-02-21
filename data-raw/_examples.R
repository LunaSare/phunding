library(devtools)
data(fam_tree)
load_all("~/Desktop/datelife")
# II. use dates from blackrim service for now
data(all_orders_chrono)
ape::Ntip(all_orders_chrono)
missing_from_chrono <-
# using get_ott_children just takes too long
# ee <- get_ott_children(input=NULL, ott_ids = 93302, ott_rank = "order")
ee_tree <- datelife::get_dated_otol_induced_subtree(input = final) #
# get order tree
data(all_ott_orders)
head(all_ott_orders, 10)
tail(all_ott_orders)
library(rotl)
taxnames <- c("Alligator mississipiensis", "Allium ameloprasum", "Allium sativum", "Artemisia dracunculus", "Citrus limon", "Thymus vulgaris", "Piper nigrum", "Salvia officinalis", "Triticum aestivum", "Vitis vinifera", "Zea mays", "Bos taurus")
taxon_search <- tnrs_match_names(names = taxnames)
taxon_lin <- get_ott_lineage(ott_id = taxon_search$ott_id)
# x <- taxon_lin[[1]]
ingredients_orders <- unname(unique(sapply(taxon_lin, function(x) rownames(x)[grepl("\\border\\b", x[,"ott_ranks"])][1])))
match(ingredients_orders, all_orders_chrono$tip.label)

# paste(taxon_search$ott_id, collapse = ", ")
# taxnames <- cbind(taxnames, unique_name(taxon_search), taxon_search$ott_id)
# ott_in_tree <- ott_id(taxon_search)[is_in_tree(ott_id(taxon_search))] # get id's
# tr <- tol_induced_subtree(ott_ids = ott_in_tree) # get published and taxonomy
# tree <- tr
# tree$tip.label <- c("Citrus_limon (Lemon)", "Vitis_vinifera (White Wine)", "Salvia_officinalis (Sage)", "Thymus_vulgaris (Thymus)", "Artemisia_dracunculus (Terragon)", "Triticum_aestivum (Wheat Flour)", "Zea_mays (Corn Starch)", "Allium_eremoprasum (Leek)", "Allium_sativum (Garlic)", "Piper_nigrum (White Pepper)", "Bos_taurus (Cream)", "Alligator_mississippiensis (Alligator)")

phytools::plotSimmap(all_orders_chrono,type="fan",part=0.5,fsize=0.3, ftype="i", colors = cols)


