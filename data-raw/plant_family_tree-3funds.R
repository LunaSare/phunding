# I. loading the data
utils::data(fam_funds)
names(fam_funds)
length(fam_funds$ott_names)
utils::data(fam_tree)
names(fam_tree)
# II. use dates from blackrim service for now
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_id = gsub("(.*)ott", "", unique(c(fam_tree$ott_ids, names(fam_funds$funds)))))



# fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_id = famsaa_ottid) # 94 tips
# all_plants <- c(famsaa_ottid, unique(ee$Embryophyta$ott_id))
# fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_id = all_plants)
# Error in curl::curl_fetch_memory(url, handle = handle) :
#   Timeout was reached: Operation timed out after 10000 milliseconds with 0 bytes received
# all_plants <- c(famsaa_ottid, unique(ee$Embryophyta$ott_id)[-match(unique(famsaa_orders$order), unique(ee$Embryophyta$ott_id))]) # AA fams mapped to ott taxonomy plus all_other_plant_fams
# fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_id = all_plants) #194 tips, because AA fams belong to 45 orders
#
# names(fam_tree_brlen)
# head(fam_tree_brlen$ott_ids)
# head(names(fam_funds$funds))
library(drake)
library(devtools)
load_all("~/Desktop/datelife")
# drake functions
make_yychrono <- function(yyspp){
  yychrono <- vector(mode = 'list', length(yyspp))
  for(i in seq(length(yyspp))){
    yychrono[[i]] <- datelife::get_dated_otol_induced_subtree(ott_id = yyspp[[i]]$ott_id)
  }
  yychrono
}
make_plot1 <- function(fam_tree_brlen, edge_cols, tip_cols, aa_index){
  pdf("/Users/luna/GoogleDrive/lunasare.com/phunding/data-raw/plant_fam_tree_colored1.pdf", height = 50, width = 50)
  plot(x= fam_tree_brlen, edge.color = edge_cols, type = "fan", cex= 1.5, edge.width = 0.5,
   label.offset = 5, tip.color = tip_cols)
  ape::tiplabels(tip = aa_index, pch = 24, cex = 1.5, col = "blue")
   # edge_cols[seq(ape::Ntip(fam_tree_brlen))]
  # ape::edgelabels(text = as.character(seq(ape::Ntip(fam_tree))), edge=seq(ape::Ntip(fam_tree)), cex = 0.5, frame = "none")
  # ape::nodelabels(text = as.character(seq(ape::Ntip(fam_tree))), edge=seq(ape::Ntip(fam_tree)), cex = 0.5, frame = "none")
  dev.off()
}



read_csv <- function(file){
  xx <- read.csv(file = file, header = TRUE, fill = TRUE, stringsAsFactors=FALSE)
  xx
}
plan <- drake_plan(
  # get all plant orders:
    ee = get_ott_children(input= "embryophyta", ott_rank = "order"),
    ef = get_ott_children(input= "embryophyta", ott_rank = "family"),
    fam_all_chrono = datelife::get_dated_otol_induced_subtree(ott_id = ef$Embryophyta$ott_id),
# length(ef$Embryophyta$ott_id)- ape::Ntip(fam_all_chrono) # 62 families where dropped from the tree
# get funded families that belong to "embryophyta":
    fpf = match(ef$Embryophyta$ott_id, as.numeric(gsub("(.*)ott", "", unique(names(fam_funds$funds))))),
# fpf <- fpf[!is.na(fpf)]
# fam_funds$funds[fpf]
# fam_funds$ott_names[fpf]
# names(fam_funds$funds[fpf]) %in% fam_tree_brlen$ott_ids
    ff_ottid = as.numeric(gsub("(.*)ott", "", unique(names(fam_funds$funds[fpf])))),
    ffo = get_ott_clade(ott_id = ff_ottid, ott_rank = "order"),
    xx = read_csv1(file = "~/GoogleDrive/getting_a_job/2018-fall/ArnoldArboretum/arnold_arboretum_living_plants.csv"),
    length(unique(xx$FAMILY)), # 108 families in AA
    fams_aa = unique(xx$FAMILY),
    fams_aa_tnrs = datelife:::clean_tnrs(datelife::tnrs_match(fams_aa)),
    famsaa_ottid = unique(fams_aa_tnrs$ott_id),
    famsaa_orders = get_ott_clade(ott_id = famsaa_ottid, ott_rank = "order")
)
# still need to run the following into the drake plan:
    all_plants = c(famsaa_ottid, ff_ottid, unique(ee$Embryophyta$ott_id)[-match(unique(c(famsaa_orders$order, ffo$order)), unique(ee$Embryophyta$ott_id))])
    fam_tree_brlen = datelife::get_dated_otol_induced_subtree(ott_id = all_plants) #230 tips, because funded fams belong to 9 orders and there's 47 of them

    aa_index <- match(paste0("ott", famsaa_ottid), fam_tree_brlen$ott)
# famsaa_chrono <- datelife::get_dated_otol_induced_subtree(ott_ids = famsaa_ottid[which(is.na(aa_index))])
# Error in curl::curl_fetch_memory(url, handle = handle) :
  # Empty reply from server
# write(paste(famsaa_ottid[which(is.na(aa_index))], collapse = ', '), file = "data-raw/somestring.txt")
    yy <- c(737324, 853767, 614459, 43847, 1035588, 17704, 99242, 978709, 147029)
    zz <- unlist(sapply(rotl::taxonomy_taxon_info(yy), "[", "unique_name"))
# write(paste(paste(zz, collapse = '", "'), "with respective ott ids", paste(yy, collapse = ", ")), file = "data-raw/somestring2.txt")
# "Hamamelidaceae", "Altingiaceae", "Zamiaceae", "Rutaceae", "Saxifragaceae",
# "Asparagaceae", "Cycadaceae", "Smilacaceae", "Boraginaceae"
# with respective ott ids 737324, 853767, 614459, 43847, 1035588, 17704, 99242, 978709, 147029
# are dropped from tree. Trying to get them with rotl::tol_subtree reveals that they are marked as broken taxa
# Error: HTTP failure: 400
# [/v3/tree_of_life/subtree] Error: node_id 'ott73724' was not found (broken taxon).
    yyspp <- get_ott_children(ott_id = yy)
# length(yyspp)
# lapply(yyspp, function(x) length(x[[1]]))
# yyspp[[1]]
    yychrono = make_yychrono(yyspp)
# names(yychrono[[1]])
    yyottids <- as.numeric(gsub("ott", "", sapply(yychrono, function(x) x$ott_ids[1])))
# datelife::get_dated_otol_induced_subtree(ott_ids = yyottids) # it works by species, so I'll just add these to the whole fam_tree_brlen
    all_plants <- c(yyottids, famsaa_ottid, ff_ottid, unique(ee$Embryophyta$ott_id)[-match(unique(c(famsaa_orders$order, ffo$order)), unique(ee$Embryophyta$ott_id))])
    fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids = all_plants) #239 tips, because funded fams belong to 9 orders and there's 47 of them
# it is still dropping length(all_plants)-239 = 37 lineages
    yyind <- match(sapply(yychrono, function(x) x$ott_ids[1]), fam_tree_brlen$ott_ids)

    fam_tree_brlen2 <- fam_tree_brlen
    fam_tree_brlen$tip.label[yyind] <- zz
    fam_tree_brlen$ott_ids[yyind] <- paste0("ott", yy)
# match(yy, famsaa_ottid)
    plant_tree1 <- fam_tree_brlen
# usethis::use_data(plant_tree1)
# get funds present in tips and edges:
    gg <- match(tolower(fam_tree_brlen$ott_ids), tolower(names(fam_funds$funds)))
# just to check which ott_ids are not in the output dated tree
    gg <- gg[!is.na(gg)]
    funds <- fam_funds$funds[gg]  # the tips that have funding
# length(funds) == length(fam_funds$funds) # if TRUE all funded taxa are in the tips of the tree
    no_funds_names <- fam_tree_brlen$tip.label[!fam_tree_brlen$ott_ids %in% names(funds)]
    no_funds <- rep(0, length(no_funds_names))
    names(no_funds) <- no_funds_names
    funds_names <- fam_tree_brlen$tip.label[match(names(funds), fam_tree_brlen$ott_ids)]
    names(funds) <- funds_names
    all_funds <- c(funds, no_funds)
    length(all_funds) == length(fam_tree_brlen$tip.label) #should be TRUE
    aa_index <- match(paste0("ott", famsaa_ottid), fam_tree_brlen$ott)

    all_values <- sum_tips(fam_tree_brlen, all_funds)
# generating an informative color scheme has proven tricky, I'm just using a dichotomous money no money for now
  all_cols <- rep("red", length(all_values))
  all_cols[all_values > 0] <- "green"
  edge_cols <- all_cols[fam_tree_brlen$edge[,2]]
  length(edge_cols) ==nrow(fam_tree_brlen$edge)
  tip_cols <- all_cols[seq(ape::Ntip(fam_tree_brlen))]
  plot1 = make_plot1()
# use phytools for a prettier plot:
library(phytools)
# data(anoletree)
# str(anoletree)
# names(anoletree)
# plotSimmap(anoletree,type="fan",part=0.5,fsize=0.9, ftype="i")
# # no colors provided. using the following legend:
# #     CG     GB      Non-     TC     TG        Tr        TW
# # "black"  "red"  "green3" "blue" "cyan" "magenta"  "yellow"
# ss<-sort(unique(getStates(anoletree,"tips")))
# add.simmap.legend(colors=setNames(palette()[1:length(ss)],ss))
utils::data(plant_tree1)

fam_tree_brlen3 <- plant_tree1
# class(anoletree$maps)
# str(anoletree$maps)
# anoletree$maps[1]
# length(anoletree$maps) == nrow(anoletree$edge)#162
namess <- rep("No funds", length(edge_cols))
namess[which(edge_cols == "green")] <- "NSF funds"
fam_tree_brlen3$maps <- as.list(rep(1, length(edge_cols)))
for (i in seq(length(edge_cols))){
  names(fam_tree_brlen3$maps[[i]]) <- namess[i]
}
# fam_tree_brlen3$maps[1]
# str(fam_tree_brlen3$maps)
cols <- setNames(c("red","green"), c("No funds", "NSF funds"))

# plot clades, start with angios:
# the tips are orders, so I just need to get th eMRCA of tips from 1 to Amborellales or one before Araucariaceae:
which(plant_tree1$tip.label == "Araucariaceae")
clades <- list(x1 = c("Oxalidales", "Amborellales"),
          x2 = c("Araucariaceae", "Ginkgoaceae"),
          x3 = c("Cyatheales", "Psilotales"),
          x4 = c("Notothyladales", "Leiosporocerotales"))
nodes <- sapply(clades, function(x) phytools::findMRCA(tree = plant_tree1, tips = x, type= "node"))
clades <- c("Flowering plants", "Gymnosperms", "Ferns", "Anthoceros")
orient <- c("curved", rep("horizontal",3))
pdf("/Users/luna/GoogleDrive/lunasare.com/phunding/data-raw/plant_fam_tree_colored2.pdf", height = 7.5, width = 10)
plotSimmap(fam_tree_brlen3,type="fan",part=0.5,fsize=0.3, ftype="i", colors = cols)
# ape::tiplabels(cex = 0.3, frame = "none")
# ape::nodelabels(cex = 0.3, frame = "none")
add.simmap.legend(colors= cols, prompt = FALSE, x = par("usr")[3]*2.5, y = par("usr")[4]*0.75)
text(labels = "NSF Funded tree of Land Plants", x = par("usr")[3]*0.1, y = par("usr")[4]*0.85)
for(i in 1:length(nodes)){
    phytools::arc.cladelabels(tree = fam_tree_brlen3, clades[i], nodes[i], orientation = orient[i],
                              lab.offset = 1.2,ln.offset=1.16)
}
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
