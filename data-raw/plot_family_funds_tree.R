#
# I. loading the data
utils::data(fam_funds)
names(fam_funds)
length(fam_funds$ott_names)
utils::data(fam_tree)
names(fam_tree)
# II. use dates from blackrim service for now
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = gsub("(.*)ott", "", unique(c(fam_tree$ott_ids, names(fam_funds$funds)))))
# For the vignette, map AA families to Funded Tree
xx <- read.csv(file = "~/GoogleDrive/getting_a_job/2018-fall/ArnoldArboretum/arnold_arboretum_living_plants.csv",
    header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
length(unique(xx$FAMILY))  # 108 families in AA
fams_aa <- unique(xx$FAMILY)
fams_aa_tnrs <- datelife:::clean_tnrs(datelife::batch_tnrs_match_names(fams_aa))
length(unique(fams_aa_tnrs$unique)) #103 valid families identified
names(fams_aa_tnrs)
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = gsub("(.*)ott", "", unique(c(fams_aa_tnrs$ott_id, fam_tree$ott_id, names(fam_funds$funds)))))
# tree of vascular plant orders and families of funded taxa
# get ott id for embryophyta and then all its children
rotl::tnrs_match_names("embryophyta")$ott_id
child <- rotl::taxonomy_taxon_info(56610, include_children = TRUE)
input_ott_match <- 56610
rotl::tnrs_match_names("Vahlia digyna")$ott_id
child <- rotl::taxonomy_taxon_info(5144418, include_children = TRUE)
# input_ott_match <- 769681
get_valid_children(ott_id = 769681) # Psilotopsida
get_valid_children(ott_id = 56601)  # Marchantiophyta
tt <- child
taxon_info <- tt
gg <- get_ott_children(ott_id= 56601, ott_rank = "genus")
names(gg)
oo <- get_ott_children(ott_id= 56610, ott_rank = "order")
oo <- get_ott_children(input= "magnoliophyta", ott_rank = "order")
rownames(oo$Magnoliophyta)
sum(oo$Magnoliophyta$rank == "order")
rotl::taxonomy_taxon_info(56610)
ee <- get_ott_children(input= "embryophyta", ott_rank = "order")
names(ee)
rownames(ee$Embryophyta)
famsaa_ottid <- unique(fams_aa_tnrs$ott_id)
famsaa_orders <- get_ott_clade(ott_id = famsaa_ottid, rank = "order")
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = ee$Embryophyta$ott_id) # 141 tips, order level tree
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = famsaa_ottid) # 94 tips
all_plants <- c(famsaa_ottid, unique(ee$Embryophyta$ott_id))
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = all_plants)
# Error in curl::curl_fetch_memory(url, handle = handle) :
#   Timeout was reached: Operation timed out after 10000 milliseconds with 0 bytes received
all_plants <- c(famsaa_ottid, unique(ee$Embryophyta$ott_id)[-match(unique(famsaa_orders$order), unique(ee$Embryophyta$ott_id))]) # AA fams mapped to ott taxonomy plus all_other_plant_fams
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = all_plants) #194 tips, because AA fams belong to 45 orders

names(fam_tree_brlen)
head(fam_tree_brlen$ott_ids)
head(names(fam_funds$funds))

ef <- get_ott_children(input= "embryophyta", ott_rank = "family")
fam_all_chrono <- datelife::get_dated_otol_induced_subtree(ott_ids = ef$Embryophyta$ott_id)
length(ef$Embryophyta$ott_id)- ape::Ntip(fam_all_chrono) # 62 families where dropped from the tree
# get funded families that belong to "embryophyta":
fpf <- match(ef$Embryophyta$ott_id, as.numeric(gsub("(.*)ott", "", unique(names(fam_funds$funds)))))
fpf <- fpf[!is.na(fpf)]
fam_funds$funds[fpf]
fam_funds$ott_names[fpf]
names(fam_funds$funds[fpf]) %in% fam_tree_brlen$ott_ids
ff_ottid <- as.numeric(gsub("(.*)ott", "", unique(names(fam_funds$funds[fpf]))))
ffo <- get_ott_clade(ott_id = ff_ottid, rank = "order")
all_plants <- c(famsaa_ottid, ff_ottid, unique(ee$Embryophyta$ott_id)[-match(unique(c(famsaa_orders$order, ffo$order)), unique(ee$Embryophyta$ott_id))])
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = all_plants) #230 tips, because funded fams belong to 9 orders and there's 47 of them

aa_index <- match(paste0("ott", famsaa_ottid), fam_tree_brlen$ott)
famsaa_chrono <- datelife::get_dated_otol_induced_subtree(ott_ids = famsaa_ottid[which(is.na(aa_index))])
write(paste(famsaa_ottid[which(is.na(aa_index))], collapse = ', '), file = "somestring.txt")
yy <- c(737324, 853767, 614459, 43847, 1035588, 17704, 99242, 978709, 147029)
zz <- unlist(sapply(rotl::taxonomy_taxon_info(yy), "[", "unique_name"))
write(paste(paste(zz, collapse = '", "'), "with respective ott ids", paste(yy, collapse = ", ")), file = "somestring2.txt")
# "Hamamelidaceae", "Altingiaceae", "Zamiaceae", "Rutaceae", "Saxifragaceae",
# "Asparagaceae", "Cycadaceae", "Smilacaceae", "Boraginaceae"
# with respective ott ids 737324, 853767, 614459, 43847, 1035588, 17704, 99242, 978709, 147029
# are dropped from tree.
yyspp <- get_ott_children(ott_id = yy)
length(yyspp)
lapply(yyspp, function(x) length(x[[1]]))
yyspp[[1]]
yychrono <- vector(mode = 'list', length(yyspp))
for(i in seq(length(yyspp))){
  yychrono[[i]] <- datelife::get_dated_otol_induced_subtree(ott_ids = yyspp[[i]]$ott_id)
}
names(yychrono[[1]])
yyottids <- as.numeric(gsub("ott", "", sapply(yychrono, function(x) x$ott_ids[1])))
datelife::get_dated_otol_induced_subtree(ott_ids = yyottids) # it works by species, so I'll just add these to the whole fam_tree_brlen
all_plants <- c(yyottids, famsaa_ottid, ff_ottid, unique(ee$Embryophyta$ott_id)[-match(unique(c(famsaa_orders$order, ffo$order)), unique(ee$Embryophyta$ott_id))])
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = all_plants) #239 tips, because funded fams belong to 9 orders and there's 47 of them
# it is still dropping length(all_plants)-239 37 lineages
yyind <- match(sapply(yychrono, function(x) x$ott_ids[1]), fam_tree_brlen$ott_ids)
fam_tree_brlen$tip.label[yyind] <- zz

# get funds present in tips and edges:
gg <- match(tolower(fam_tree_brlen$ott_ids), tolower(names(fam_funds$funds))) # just to check which ott_ids are not in the output dated tree
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

all_values <- sum_tips(fam_tree_brlen, all_funds)
# generating an informative color scheme has proven tricky, I'm just using a dichotomous money no money for now
all_cols <- rep("red", length(all_values))
all_cols[all_values > 0] <- "green"
tip_cols <- all_cols[seq(ape::Ntip(fam_tree_brlen))]
edge_cols <- all_cols[fam_tree_brlen$edge[,2]]
pdf("/Users/luna/GoogleDrive/lunasare.com/phunding/data-raw/plant_fam_tree_colored1.pdf", height = 50, width = 50)
plot(x= fam_tree_brlen, edge.color = edge_cols, type = "fan", cex= 1.5, edge.width = 0.5,
 label.offset = 5,
 tip.color = tip_cols)
ape::tiplabels(tip = aa_index, pch = 24, cex = 1.5, col = "blue")
 # edge_cols[seq(ape::Ntip(fam_tree_brlen))]
# ape::edgelabels(text = as.character(seq(ape::Ntip(fam_tree))), edge=seq(ape::Ntip(fam_tree)), cex = 0.5, frame = "none")
# ape::nodelabels(text = as.character(seq(ape::Ntip(fam_tree))), edge=seq(ape::Ntip(fam_tree)), cex = 0.5, frame = "none")
dev.off()

# get order and other groupings to label the tree
paste(gsub("(.*)ott", "", fam_tree_brlen$ott_ids[1:10]), collapse = "', '")
orders <- get_ott_clade(ott_ids = gsub("(.*)ott", "", fam_tree_brlen$ott_ids[1:10]), rank = c("order", "class"))
paste(orders$order, collapse = "', '")
# We can get orders but not classes
xx <- rotl::taxonomy_taxon_info(gsub("(.*)ott", "", fam_tree_brlen$ott_ids[1:3]))
xx <- c(xx, NA)
names(xx[[1]])
xx1 <- sapply(xx, "[", "unique_name")
class(xx1)
aa_index <- match(fams_aa_tnrs, fam_tree_brlen$tip.label)
length(fams_aa_tnrs) == length(aa_index)  # TRUE, so we have all fams from AA that matched ott in the tree
aa_index <- sort(aa_index[!is.na(aa_index)]) # 93 fams from 104 tnrs matched AA fams are in tree
# tip_cols[aa_index] <- "blue"
pdf("/Users/luna/GoogleDrive/lunasare.com/phunding/data-raw/fam_tree_colored2.pdf", height = 50, width = 50)
plot(x= fam_tree_brlen, edge.color = edge_cols, type = "fan", cex= 0.1, edge.width = 0.1,
 label.offset = 0.005, tip.color = tip_cols)
 ape::tiplabels(tip = aa_index, pch = 24, cex = 0.5, col = "blue")
 # edge_cols[seq(ape::Ntip(fam_tree_brlen))]
# ape::edgelabels(text = as.character(seq(ape::Ntip(fam_tree))), edge=seq(ape::Ntip(fam_tree)), cex = 0.5, frame = "none")
# ape::nodelabels(text = as.character(seq(ape::Ntip(fam_tree))), edge=seq(ape::Ntip(fam_tree)), cex = 0.5, frame = "none")
dev.off()
# use phytools for a prettier plot:
