# get a family level Tree of Life:
# Brian helped me extract families from the open tree of life taxonomy data table with pipeline:
grep -i "family" taxonomy.tsv | grep -v "Incertae" \ grep -v "no rank" \ grep - v "major rank conflict" | grep -v "uncultured" | grep -v "barren" | grep -v "extinct" | grep -v "incertae" | grep -v "unplaced" | grep -v "hidden" | grep -v "inconsistent"  | grep -v "subfamily" > families.tsv
# excluded "barren" taxa after problems arrising from trying to get a tree containing "Nitrosopumilaceae" among others that did worked, e.g., "Felidae", "Cactaceae", "Iamiaceae", "Hominidae"
# this left us with a dataset of 16951 families
# get_otol_synthetic_tree(c("Nitrosopumilaceae", "Iamiaceae"))
# rotl::tol_induced_subtree(ott_id=c(189165,517016))
# we can use the ott ids firectly and skip teh step of matching taxonomy within get_otol_synthetic_tree
# so we'll use rotl::tol_induced_subtree function:
all_ott_fams <- read.table("/Users/luna/GoogleDrive/datelife/otol/taxonomy/ott/families.tsv", sep = "|", header = FALSE, fill = TRUE, stringsAsFactors=FALSE)
# all_ott_fams <- read.table("data-raw/families.tsv", sep = "|", header = FALSE, fill = TRUE, stringsAsFactors=FALSE)
# all_ott_fams[grep(, all_ott_fams[,1]),],] #it is an extinct one, so taking those out too
# all_ott_fams[grep(395198, all_ott_fams[,1]),] # incertae
# all_ott_fams[grep(5584927, all_ott_fams[,1]),] # unplaced
# all_ott_fams[grep(174869, all_ott_fams[,1]),] # hidden
# all_ott_fams[grep(185517,all_ott_fams[,1]),] # inconsistent
head(all_ott_fams, 10)
names(all_ott_fams) <- c("ott_id", "parent_ott_id", "name", "rank", "source_info", "unique_name", "flags", "?")
usethis::use_data(all_ott_fams, overwrite = TRUE)
fam_tree <- rotl::tol_induced_subtree(ott_id=all_ott_fams$ott_id, label_format = "name")
# singletons dropped
ape::Ntip(fam_tree) #8840
fam_tree$tip.label[match("felidae", tolower(fam_tree$tip.label))] # it is there
length(fam_tree$tip.label)
# after excluding all undesired flags, we have 13700 families
# after excluding subfamilies, we have 10364 families
# now exclude names that are ott ids:
mrca <- grepl("mrcaott", fam_tree$tip.label)
fam_tree <- ape::drop.tip(fam_tree, fam_tree$tip.label[mrca]) # 8840 final tips
# final function wrapping all above:
fam_tree <- get_otol_family_tree()
usethis::use_data(fam_tree, overwrite = TRUE)
any(grepl("_", fam_tree$ott_ids))
fam_tree$ott_ids[grepl("_", fam_tree$ott_ids)]
# look at the tree
load("/Users/luna/GoogleDrive/lunasare.com/phunding/data-raw/fam_tree.RData")
fam_tree <- ape::ladderize(fam_tree)
fam_tree_brlen <- ape::compute.brlen(fam_tree)
match("felidae", tolower(fam_tree_brlen$tip.label))
save(fam_tree_brlen, file = "fam_tree_brlen.RData")
pdf("/Users/luna/GoogleDrive/lunasare.com/tol_funding/fam_tree.pdf", height = 50, width = 50)
plot(x= fam_tree_brlen, edge.color = "gray", type = "fan", cex= 0.1, edge.width = 0.1, label.offset = 0.005, tip.color = "blue")
dev.off()
