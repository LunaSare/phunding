
gg <- match(tolower(fam_tree_brlen$ott_ids), tolower(names(fam_funds$funds)))
# just to check which ott_ids are not in the output dated tree
gg <- gg[!is.na(gg)]
funds <- fam_funds$funds[gg]  # the tips that have funding
length(funds) == length(fam_funds$funds) # if TRUE all funded taxa are in the tips of the tree
no_funds_names <- fam_tree_brlen$tip.label[!fam_tree_brlen$ott_ids %in% names(funds)]
no_funds <- rep(0, length(no_funds_names))
names(no_funds) <- no_funds_names
funds_names <- fam_tree_brlen$tip.label[match(names(funds), fam_tree_brlen$ott_ids)]
names(funds) <- funds_names
all_funds <- c(funds, no_funds)
length(all_funds) == length(fam_tree_brlen$tip.label) #should be TRUE
