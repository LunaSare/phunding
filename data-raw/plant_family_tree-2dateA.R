# get all ottids needed for the tree; we want a representation of:
# (1) all land plant orders, (2) all families from the Arnold Arboretum and (3) all NSF funded families

# get all plant orders:
ee <- get_ott_children(input= "embryophyta", ott_rank = "order")
# names(ee)
# rownames(ee$Embryophyta)
# drop unwanted taxon "core leptosporangiate ferns" because it is just an unplaced species:
final <- ee$Embryophyta[-grep("core leptosporangiate ferns", rownames(ee$Embryophyta)),]
#check that all taxa can be dated:
ee_tree <- datelife::get_dated_otol_induced_subtree(input = final) # 141 tips, order level tree
m1 <- match(gsub("_", " ", ee_tree$tip.label), rownames(final)) # all tip labels are in rownames
m2 <- match(rownames(final), gsub("_", " ", ee_tree$tip.label)) # but not all rownames are in tip tiplabels
# let's check that
unm <- rownames(final)[which(is.na(m2))]
# Taxa absent from dated tree (such as Gleicheniales and Polypodiales) are in the
# otol taxonomy but not in the tree, because, as stated in the
# message from Otol website: This taxon is in our taxonomy but not in our tree
# synthesis database. This can happen for a variety of reasons, but the most
# probable is that is has a taxon flag (e.g. incertae sedis) that causes it to be
# pruned from the synthetic tree. See the taxonomy browser for more information about this taxon.
# To solve their absence:
# a) Get their species children:
goc <- get_ott_children(input = unm)
sapply(goc, nrow)
# b) Now get the dated tree of the species within:
goc_tree <- lapply(goc, datelife::get_dated_otol_induced_subtree)
# still 3 clades are absent from the synthetic tree because all their children are in conflict
# these clades are:
names(goc[which(is.na(goc_tree))]) # "Psilotopsida" "Ginkgoopsida" "Gnetopsida"
# manual inspection of otol website reveals that these clades are represented now with another name Psilotales, Ginkgoales, Gnetales
# THIS IS AN EXAMPLE OF A GOOD CASE IN WHICH I SHOULD DROP MONOTYPIC TAXA FROM get_ott_children, bc they could be invalid \o/
# Weel, for now, just drop them from teh final ist of names:
todrop <- unlist(sapply(names(goc[which(is.na(goc_tree))]), grep, rownames(final)))
final <- final[-todrop,]
goc_tree <- goc_tree[-todrop]
# c) Now get one species that works for each of the other good taxa and doublec heck you get the tree:
miss <- sapply(goc_tree, function(x) x$tip.label[1])
misstree <- datelife::get_dated_otol_induced_subtree(input = miss) # 20 tips
# d) Now add it to the df of final names
final[match(names(miss), rownames(final)), "ott_id"] <- as.numeric(gsub("ott", "", sapply(goc_tree, function(x) x$ott_id[1])))
levels(final$rank) <- c(levels(final$rank), "search")
final[match(names(miss), rownames(final)), "rank"] <- as.factor("search")
# test again that they work:
ee_tree <- datelife::get_dated_otol_induced_subtree(input = final) # 160 tips
m1 <- match(gsub("ott", "", ee_tree$ott_id), final$ott_id) # all tip labels are in rownames
m2 <- match(final$ott_id, gsub("ott", "", ee_tree$ott_id)) # and visceversa, yayyy
# Now, leave just one of the following taxa with no order:
unw <- c("Phyllites", "Dasypogon", "Kingia", "Calectasia", "Vahlia")
unw1 <- sapply(unw, grep, rownames(final))
unw1 <- sapply(unw1, "[", -1)
final <- final[-unlist(unw1),]
rownames(final)[unlist(sapply(unw, grep, rownames(final)))] <- unw
# drop "Baxteria australis" because it is family Dasypogonaceae:
final <- final[-grep("Baxteria australis", rownames(final)),]
# Get and save the order level land plant tree:
plant_tree_orders <- datelife::get_dated_otol_induced_subtree(input = final) # 139 tips
# names(plant_tree_orders)
plant_tree_orders$ott_names <- plant_tree_orders$tip.label
data.frame(tips=plant_tree_orders$tip.label[match(final$ott_id, gsub("ott", "", plant_tree_orders$ott_id))], df=rownames(final))
plant_tree_orders$tip.label[match(final$ott_id, gsub("ott", "", plant_tree_orders$ott_id))] <- rownames(final)
usethis::use_data(plant_tree_orders)

#################################### *
#################################### *
#################################### *
#################################### *
# Now, map AA families to Funded Tree
# tree of AA families
xx <- read.csv(file = "~/GoogleDrive/getting_a_job/2018-fall/ArnoldArboretum/arnold_arboretum_living_plants.csv",
    header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
length(unique(xx$FAMILY))  # 108 families in AA
fams_aa <- unique(xx$FAMILY)
fams_aa_tnrs <- datelife:::clean_tnrs(datelife::batch_tnrs_match_names(fams_aa))
length(unique(fams_aa_tnrs$unique)) #103 valid families identified
names(fams_aa_tnrs)
aa_fam_tree <- datelife::get_dated_otol_induced_subtree(ott_ids = gsub("(.*)ott", "", unique(fams_aa_tnrs$ott_id)))
