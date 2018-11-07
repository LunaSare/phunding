#
# I. loading the data
plot_otol_fam_tree_funds(){

}
utils::data(fam_funds)
utils::data(fam_tree)
fam_tree2 <- fam_tree
fam_tree2$tip.label <- fam_tree$ott_ids
fam_tree <- ape::ladderize(fam_tree)
fam_tree2 <- ape::ladderize(fam_tree2)
head(fam_tree$tip.label)
head(fam_tree$ott_id)
head(fam_tree$ott_name)
# fam_tree$tip.label <- fam_tree$ott_id
names(fam_tree)
names(fam_funds)
head(names(fam_funds$funds))
# II. making sure all funded family names (ott ids) are in the tips of the tree:
gg <- sapply(tolower(names(fam_funds$funds)), grep, tolower(fam_tree$tip.label))
length(gg) == length(gg[sapply(gg, length) >0])  # if TRUE all fams in funds are in the family tree
fam_funds$ott_names[which(sapply(gg, length) == 0)] # these are the families that are in funds bc they where extracted from grants by matching to ott with tnrs, and yet, are not on the ott family induced subtree
# testing unfound taxa:
pp <- as.numeric(gsub("ott", "", names(gg[!sapply(gg, length) >0])))
fam_tree2 <- rotl::tol_induced_subtree(ott_id=as.numeric(gsub("ott", "", names(fam_funds$funds))), label_format = "id")
prob_info <- rotl::taxonomy_taxon_info(pp)
sapply(prob_info, "[", "flags")
# IIb. nudibranchia has no families obtained with get_ott_families in fam_tree, wha???
rotl::tnrs_match_names("nudibranchia")
# grep("76753", fam_tree$node.label) # node labels are full names usually, so don't use this
fam_tree$node.label[grep("nudibranchia", tolower(fam_tree$node.lab))]
# so, nudibranchia is there, it just dies not get any families with taxonomy_taxon_info
# then we will get them from the descendant tips in the tree:
tt <- phytools::getDescendants(fam_tree, 1037+ape::Ntip(fam_tree))
fam_tree$tip.label[tt[tt<ape::Ntip(fam_tree)]]
# this one worked to get all tips from a node, like that we make sure we are taking all phylogenetic diversity into account
# changed get_ott_families so we get descendant tips when dealing with a taxon name above family level
# IIb. still having some unfound taxa: [1] "Funariaceae"   "Tenebrionidae" "Cottidae"      "Agonidae"
#  [5] "Amaranthaceae" "Sphagnaceae"   "Ploceidae"     "Nectriaceae"
#  [9] "Hypocreaceae"  "Orobanchaceae" "Chrysomelidae"
# they have no flags, not sure why they are not in the tree...
# we will just leave them outside the reconstruction for now:
funds <- fam_funds$funds[sapply(gg, length) >0]
length(funds)  # must be the same length as fam_funds$funds, but not yet bc some taxa do not match tip labels
gg2 <- sapply(tolower(names(funds)), grep, tolower(fam_tree$tip.label))
all(sapply(gg2, length) >0)  # it is TRUE when all names are in the labels of the tree
# add all unmatched tips that either have no funding or no data on it:
no_funds_names <- fam_tree$tip.label[!fam_tree$tip.label %in% names(funds)]
no_funds <- rep(0, length(no_funds_names))
names(no_funds) <- no_funds_names
all_funds <- c(funds, no_funds)
head(all_funds)
length(all_funds) == length(fam_tree$tip.label) #should be TRUE
# III. get edge funds:
edge_values <- sum_tips(fam_tree, all_funds)
# generate a color scheme for them:
max(edge_values)
gray2green <- colorRampPalette(c("gray", "green"))
cols <- gray2green(length(unique(edge_values)))
edge_cols <- cols[match(edge_values, sort(unique(edge_values)))]
# use dates from blackrim service for now
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = gsub("(.*)ott", "", unique(c(fam_tree$ott_ids, names(fam_funds$funds)))))
names(fam_tree_brlen)
head(fam_tree_brlen$ott_ids)
head(names(fam_funds$funds))
gg <- sapply(tolower(names(fam_funds$funds)), grep, tolower(fam_tree_brlen$ott_ids))
length(gg)
length(fam_funds$funds)  #
ape::Ntip(fam_tree_brlen)
funds <- fam_funds$funds[sapply(gg, length) >0]
length(funds) # not all funded taxa are in the tips of the tree
no_funds_names <- fam_tree_brlen$tip.label[!fam_tree_brlen$ott_ids %in% names(funds)]
length(no_funds_names)
no_funds <- rep(0, length(no_funds_names))
funds_names <- fam_tree_brlen$tip.label[match(names(funds), fam_tree_brlen$ott_ids)]
head(funds_names)
names(funds) <- funds_names
length(funds)
names(no_funds) <- no_funds_names
all_funds <- c(funds, no_funds)
head(all_funds, 50)
length(all_funds) == length(fam_tree_brlen$tip.label) #should be TRUE
all_values <- sum_tips(fam_tree_brlen, all_funds)
# match(names(all_funds), fam_tree_brlen$tip.label) # it works
length(which(all_values[seq(ape::Ntip(fam_tree_brlen))] > 0))
all_values[which(all_values[seq(ape::Ntip(fam_tree_brlen))] > 0)]
names(all_values)
# generate a color scheme for them:
max(all_values)
length(all_values)
length(fam_tree_brlen$edge.length)
# gray2green <- colorRampPalette(c("gray", "green"))
yell2green <- colorRampPalette(c('yellow',"green", 'forestgreen'))
cols <- yell2green(1000)
cols <- c("red", cols)
yy <- round(1000*all_values/max(all_values))
all_cols <- cols[1+yy]
all_cols[8740:8750]
tip_cols <- all_cols[seq(ape::Ntip(fam_tree_brlen))]
length(tip_cols) == ape::Ntip(fam_tree_brlen)
sum(tip_cols !="red")
edge_cols <- all_cols[fam_tree_brlen$edge[,2]]
length(edge_cols) == length(fam_tree_brlen$edge.length)

pdf("/Users/luna/GoogleDrive/lunasare.com/phunding/data-raw/fam_tree_colored1.pdf", height = 50, width = 50)
plot(x= fam_tree_brlen, edge.color = edge_cols, type = "fan", cex= 0.1, edge.width = 0.1,
 label.offset = 0.005, tip.color = tip_cols)
 # edge_cols[seq(ape::Ntip(fam_tree_brlen))]
# ape::edgelabels(text = as.character(seq(ape::Ntip(fam_tree))), edge=seq(ape::Ntip(fam_tree)), cex = 0.5, frame = "none")
# ape::nodelabels(text = as.character(seq(ape::Ntip(fam_tree))), edge=seq(ape::Ntip(fam_tree)), cex = 0.5, frame = "none")
dev.off()
