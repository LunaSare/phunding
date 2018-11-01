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
names(fams_funds)
gg <- sapply(paste0("^", tolower(names(fams_funds))), grep, tolower(fam_tree$tip.label))
length(gg[sapply(gg, length) >0])
gg[sapply(gg, length) >0]
funded_tips <- match(unique(unlist(nsf_relevant_grants$fams)), fam_tree$tip.label)


all_fams_funds <- rep(0, length(fam_tree$tip.label))
names(all_fams_funds) <- fam_tree$tip.label

phytools::fastAnc(fam_tree, )
