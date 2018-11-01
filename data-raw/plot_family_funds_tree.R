# match family names to tip.labels

funded_tips <- match(unique(unlist(nsf_relevant_grants$fams)), fam_tree$tip.label)
funded_tips <- funded_tips[!is.na(funded_tips)]
fam_tree_brlen$tip.label[funded_tips]
tip_color <- rep("gray", ape::Ntip(fam_tree_brlen))
tip_color[funded_tips] <- "green"
pdf("/Users/luna/GoogleDrive/lunasare.com/tol_funding/fam_tree_colored.pdf", height = 50, width = 50)
plot(x= fam_tree_brlen, edge.color = "gray", type = "fan", cex= 0.1, edge.width = 0.1,
 label.offset = 0.005, tip.color = tip_color)
dev.off()

# ancestral reconstruction
names(grants)
utils::data(fams_funds)
utils::data(fam_tree)
fam_tree$tip.label
fam_tree$ott_id
fam_tree$ott_name
fam_tree$tip.label <- fam_tree$ott_id
names(fam_tree)
names(fam_funds$funds)
gg <- sapply(tolower(names(fam_funds$funds)), grep, tolower(fam_tree$tip.label))
length(gg)
length(gg[sapply(gg, length) >0])
prob <- as.numeric(gsub("ott", "", names(gg[!sapply(gg, length) >0])))
funded_tips <- match(unique(unlist(nsf_relevant_grants$fams)), fam_tree$tip.label)
fam_tree2 <- rotl::tol_induced_subtree(ott_id=as.numeric(gsub("ott", "", names(fam_funds$funds))), label_format = "id")

prob_info <- rotl::taxonomy_taxon_info(prob)
sapply(prob_info, "[", "flags")
rotl::tnrs_match_names("nudbranchia")
grep("76753", fam_tree$node.lab)
fam_tree$node.label[grep("nudibranchia", tolower(fam_tree$node.lab))]
phylobase::descendants(fam_tree, 1037+ape::Ntip(fam_tree), type = "tips")
# the above did not work
head(fam_tree$edge)
# but the node has descendants alright:
any(fam_tree$edge[,1] == 1037+ape::Ntip(fam_tree))
# so trying another function
tt <- phytools::getDescendants(fam_tree, 1037+ape::Ntip(fam_tree))
fam_tree$tip.label[tt[tt<ape::Ntip(fam_tree)]]


all_fams_funds <- rep(0, length(fam_tree$tip.label))
names(all_fams_funds) <- fam_tree$tip.label

phytools::fastAnc(fam_tree, )
