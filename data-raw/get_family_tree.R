# get a family level Tree of Life:
# Brian helped me extract families from the open tree of life taxonomy data table using pipeline:
grep -i "family" taxonomy.tsv | grep -v "Incertae" \ grep -v "no rank" \ grep - v "major rank conflict" | grep -v "uncultured" | grep -v "barren" | grep -v "extinct" | grep -v "incertae" | grep -v "unplaced" | grep -v "hidden" | grep -v "inconsistent"  | grep -v "subfamily" > families.tsv
# excluded "barren" taxa after problems arrising from trying to get a tree containing "Nitrosopumilaceae" among others that did worked, e.g., "Felidae", "Cactaceae", "Iamiaceae", "Hominidae"
# this left us with a dataset of 16951 families
# get_otol_synthetic_tree(c("Nitrosopumilaceae", "Iamiaceae"))
# rotl::tol_induced_subtree(ott_id=c(189165,517016))
# we can use the ott ids firectly and skip teh step of matching taxonomy within get_otol_synthetic_tree
# so we'll use rotl::tol_induced_subtree function:
fam <- read.table("/Users/luna/GoogleDrive/datelife/otol/taxonomy/ott/families.tsv", sep = "|", header = FALSE, fill = TRUE, stringsAsFactors=FALSE)
# fam <- read.table("data-raw/families.tsv", sep = "|", header = FALSE, fill = TRUE, stringsAsFactors=FALSE)
# fam[grep(, fam[,1]),],] #it is an extinct one, so taking those out too
# fam[grep(395198, fam[,1]),] # incertae
# fam[grep(5584927, fam[,1]),] # unplaced
# fam[grep(174869, fam[,1]),] # hidden
# fam[grep(185517,fam[,1]),] # inconsistent

fam_tree <- rotl::tol_induced_subtree(ott_id=fam[,1], label_format = "name")
# singletons dropped
ape::Ntip(fam_tree) #8840
fam_tree$tip.label[match("felidae", tolower(fam_tree$tip.label))]
length(fam_tree$tip.label)
# after excluding all undesired flags, we have 13700 families
# after excluding subfamilies, we have 10364 families
# exclude names that are ott ids
mrca <- grepl("mrcaott", fam_tree$tip.label)
fam_tree <- ape::drop.tip(fam_tree, fam_tree$tip.label[mrca])
save(fam_tree, file = "data-raw/fam_tree.RData")

# now plot the tree
load("/Users/luna/GoogleDrive/lunasare.com/tol_funding/fam_tree.RData")
fam_tree <- ape::ladderize(fam_tree)
fam_tree_brlen <- ape::compute.brlen(fam_tree)
match("felidae", tolower(fam_tree_brlen$tip.label))
save(fam_tree_brlen, file = "fam_tree_brlen.RData")
pdf("/Users/luna/GoogleDrive/lunasare.com/tol_funding/fam_tree.pdf", height = 50, width = 50)
plot(x= fam_tree_brlen, edge.color = "gray", type = "fan", cex= 0.1, edge.width = 0.1, label.offset = 0.005, tip.color = "blue")
dev.off()

# now date the tree with datelife
utils::data("opentree_chronograms", package = "datelife")
length(opentree_chronograms)
names(opentree_chronograms)
trees <- opentree_chronograms$trees
trees[[1]]
tips <- unlist(sapply(trees, "[", "tip.label"))
length(tips) # 208905 tips

# match corrected_names to tip.labels
funded_tips <- match(fams, tolower(fam_tree_brlen$tip.label))
funded_tips <- funded_tips[!is.na(funded_tips)]
fam_tree_brlen$tip.label[funded_tips]
tip_color <- rep("gray", ape::Ntip(fam_tree_brlen))
tip_color[funded_tips] <- "green"
pdf("/Users/luna/GoogleDrive/lunasare.com/tol_funding/fam_tree_colored.pdf", height = 50, width = 50)
plot(x= fam_tree_brlen, edge.color = "gray", type = "fan", cex= 0.1, edge.width = 0.1,
 label.offset = 0.005, tip.color = tip_color)
dev.off()

# ancestral reconstruction
