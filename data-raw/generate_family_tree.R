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
# exclude names that are ott ids
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

# ****************************************************** #
# ****************************************************** #
# ****************************************************** #
# ****************************************************** #
# ****************************************************** #
# now date the tree with datelife
utils::data("opentree_chronograms", package = "datelife")
length(opentree_chronograms)
names(opentree_chronograms)
length(opentree_chronograms$trees)
tips <- unlist(sapply(opentree_chronograms$trees, "[", "tip.label"))
length(tips) # 208905 tips
sample(unname(tips), 100)
tips2 <- unique(tips[!grepl("Original label", tips)])
length(tips2) # 169581, 86164
sample(unname(tips2), 100)
input <- ott_names <- tips2
# batch_tnrs_match_names has to call OT api 345 times
tax_map_tnrs <- datelife::batch_tnrs_match_names(names = ott_names[1:750])
tax_map_tnrs <- datelife::batch_tnrs_match_names(names = ott_names)
# getting an error, so running batch_tnrs_match_names here:
dd <- suppressWarnings(rotl::tnrs_match_names(names = "Apis mellifera"))
dd <- dd[nrow(dd)+1, ]
dd[nrow(dd)+length(input), ] <- NA
xx <- seq(1, length(input), 250)
yy <- xx+249
yy[length(xx)] <- length(input)
for (i in seq(length(xx))){
    tem <- tryCatch(suppressWarnings(rotl::tnrs_match_names(names = input[xx[i]:yy[i]])), error = function(e){NA})
    if(!is.data.frame(tem)){
        next
    } else {
        dd[xx[i]:yy[i],] <- tem
    }
    print(i)
}
i=113
i=300
input[xx[1]:yy[112]]
tem <- rotl::tnrs_match_names(names = input[xx[i]:yy[i]]) # i = 113 and i = 114 return the following  error
# Error: HTTP failure: 502
# <!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
# <html><head>
# <title>502 Proxy Error</title>
# </head><body>
# <h1>Proxy Error</h1>
# <p>The proxy server received an invalid
# response from an upstream server.<br />
# The proxy server could not handle the request <em><a href="/v3/tnrs/match_names">POST&nbsp;/v3/tnrs/match_names</a></em>.<p>
# Reason: <strong>Error reading from remote server</strong></p></p>
# <hr>
# <address>Apache/2.4.10 (Debian) Server at api.opentreeoflife.org Port 443</address>
# </body></html>
i = 113
input_genera <- unique(sapply(strsplit(input[xx[i]:yy[i]], " "), "[", 1))
tem <- rotl::tnrs_match_names(names = input_genera)

# get families by genus
tips3 <- unique(sapply(strsplit(tips2, " "), "[", 1))
length(tips3) # 22558
dd <- suppressWarnings(rotl::tnrs_match_names(names = "Apis mellifera"))
dd <- dd[nrow(dd)+1, ]
nrow(dd)
head(dd)
tail(dd)
dd[nrow(dd)+length(tips3)-1, ] <- NA
xx <- seq(1, length(tips3), 250)
yy <- xx+249
yy[length(xx)] <- length(tips3)
for (i in seq(length(xx))){
    dd[xx[i]:yy[i],] <- suppressWarnings(rotl::tnrs_match_names(names = tips3[xx[i]:yy[i]]))
    print(i)
}
tax_map_tnrs_genus <- dd
nrow(tax_map_tnrs_genus)
class(tax_map_tnrs_genus)
head(tax_map_tnrs_genus)
tail(tax_map_tnrs_genus)
unique(tax_map_tnrs_genus$flags[tax_map_tnrs_genus$flags !=""])
tax_map_tnrs_genus <- clean_tnrs(tax_map_tnrs_genus)
nrow(tax_map_tnrs_genus)
save(tax_map_tnrs_genus, file = "data-raw/tax_map_tnrs_genus.RData")
# does taxonomy_taxon_info has a limit too???
tax_info <- rotl::taxonomy_taxon_info(tax_map_tnrs_genus$ott_id, include_lineage = TRUE)
# it can, but it takes too long
# do it in batches too?
length(tax_info)  # 21923
tax_info_genus <- tax_info
save(tax_info_genus, file = "data-raw/tax_info_genus.RData")
tax_map_tnrs_genus$rank <- tax_map_tnrs_genus$ott_id
tax_map_tnrs_genus$rank[!is.na(tax_map_tnrs_genus$ott_id)] <- unlist(sapply(tax_info_genus, "[", "rank"))
fams <- vector(mode = "list", length = length(tax_map_tnrs_genus$search))
names(fams) <- tax_map_tnrs_genus$search

fams_index <- grep("^family$", tax_map_tnrs_genus$rank)
fams[fams_index] <- tax_map_tnrs_genus$unique[fams_index]
head(fams)
subfams_index <- sort(unname(unlist(sapply(c("species", "genus", "subfamily"), grep, tax_map_tnrs_genus$rank))))
fams[subfams_index] <- sapply(tax_info_genus[subfams_index], function(x) x$lineage[grep("^family$", sapply(x$lineage, "[", "rank"))][[1]]$unique_name)
for(i in seq(subfams_index)){
    x <- tax_info_genus[subfams_index][[i]]
    x$lineage[grep("^family$", sapply(x$lineage, "[", "rank"))][[1]]$unique_name
    print(i)
}
tax_map_tnrs_genus[subfams_index[i],]
sapply(x$lineage, "[", "rank")
x <- tax_info_genus[subfams_index][[1]]
# there was a problem with a taxon with no family (Myrmecia)
# so I realized the best option is downloading otl chronograms with ott_ids
# like that we won't have to match again all names.
#################################################
#################################################
#################################################
#################################################
# so, I generated an opentree_chronograms2 data set
# in it we have the same chronograms as in opentree_chronograms but with an extra element called $ott_ids
ott_ids <- unique(unlist(sapply(opentree_chronograms2$trees, "[", "ott_ids")))
length(opentree_chronograms2$trees)
length(ott_ids)
ott_ids <- ott_ids[!grepl("not_mapped", ott_ids)]
fams <- tax_info <- vector(mode = "list", length(ott_ids))
xx <- seq(1, length(ott_ids), 250)
yy <- xx+249
yy[length(xx)] <- length(ott_ids)
for (i in seq(xx)){
    tax_info[xx[i]:yy[i]] <- rotl::taxonomy_taxon_info(ott_ids[xx[i]:yy[i]], include_lineage = TRUE)
    print(i)
}
# tried to do it in batches and got two errors with unmatching ott ids:
ii <- sapply("148378_1", grepl, sapply(opentree_chronograms2$trees, "[", "ott_ids"))
ii <- sapply("5784010", grepl, sapply(opentree_chronograms2$trees, "[", "ott_ids"))
sapply(opentree_chronograms2, "[", ii)
iii <- grepl("5784010", opentree_chronograms2$trees[ii][[1]]$ott_ids)
opentree_chronograms2$trees[ii][[1]]$ott_ids[iii]
opentree_chronograms2$trees[ii][[1]]$tip.label[iii]
rotl::taxonomy_taxon_info("5784010")
ott_ids <- unique(gsub("_.", "", ott_ids))
rotl::tnrs_match_names(opentree_chronograms2$trees[ii][[1]]$tip.label[iii])
bla <- try(rotl::taxonomy_taxon_info("5784010", include_lineage = TRUE))
bla <- tryCatch(rotl::taxonomy_taxon_info("5784010", include_lineage = TRUE),
                error = function(e) NA)
# so now we are running once per ott id, to catch possible errors and get a progress bar
# the problem with particular id "5784010" is that it does not exist, I was just
# not removing everything after the underscore, but just the first number
# (because that's what "_." does), so ids of duplicated taxa with more than one digit
# after "_" would mess up the searches
# any ways there still are some ott ids npt recognised anymore
gsub("_.", "", ott_ids[grepl("_", ott_ids)][356:358])
gsub("_.*", "", ott_ids[grepl("_", ott_ids)][356:358])
ott_ids <- unique(gsub("_.*", "", ott_ids))
tax_info <- fams <- vector(mode = "list", length = length(ott_ids))
names(tax_info) <- names(fams) <- ott_ids
for (i in seq(tax_info)){
    tax_info[i] <- tryCatch(rotl::taxonomy_taxon_info(ott_ids[i], include_lineage = TRUE),
                    error = function(e) NA)
    print(i)
}
save(tax_info, file = "data-raw/tax_info.RData")
succ <- sapply(tax_info, length) > 1
ott_ids[!succ]
tax_info[1]
tax_info[length(tax_info)]
ott_id_ranks <- unlist(sapply(tax_info, "[", "rank"))
length(fams)
length(ott_ids)
length(tax_info)
ott_id_names <- names(fams) <- unlist(sapply(tax_info, "[", "unique_name"))
names(fams) <- as.character(ott_ids)
fams_index <- grep("^family$", ott_id_ranks)
fams[fams_index] <- unname(ott_id_names[fams_index])
subfams_index <- unname(unlist(sapply(c("no rank", "varietas", "species", "genus", "subfamily"), grep, ott_id_ranks)))
fams[subfams_index] <- sapply(tax_info[subfams_index], function(x) {
    lin <- tryCatch(x$lineage[grep("^family$", sapply(x$lineage, "[", "rank"))][[1]]$unique_name,
    error = function(e) NA)
    return(lin)
})
fams[sapply(fams, length) ==0]
length(fams[sapply(fams, length) ==0])  # there are 43 names with no families yet assigned
ott_id_ranks[sapply(fams, length) ==0] # we have NAs and things above family
unname(unlist(sapply(c("no rank"), grep, ott_id_ranks)))
fams[c(130,14557)]
superfams_index <- sort(unname(unlist(sapply(c("phylum", "domain", "class", "order", "superfamily"), grep, ott_id_ranks))))
fams[superfams_index]
tax_info2 <- rotl::taxonomy_taxon_info(ott_ids[superfams_index], include_children = TRUE)
ranks <- sapply(tax_info2, function(x) sapply(x$children, "[", "rank"))
ranks_index <- sapply(ranks, function(x) grep("^family$", x))
# sapply(ranks_index, length) # number of actual families
fams[superfams_index[4]]
tax_info2[[4]]
# sapply(tax_info2[4], function(x) sapply(x$children, "[", "rank"))
superfams <- sapply(tax_info2, function(x) sapply(x$children, "[", "unique_name"))
# there are not that many superfams, and I'm sot sure how to use this information to date a family level tree of Life
# I'll leave this info out for now
# Oh! I thought of a way to use them. Just store the family names as a list, and grep them when looking for a calibration point
which(sapply(ranks_index, length)==0)
superfams[which(sapply(ranks_index, length)==0)[1]]
superfams[which(sapply(ranks_index, length)==0)[2]]
superfams[2]
ranks[2]
for(i in seq(superfams)[sapply(ranks_index, length)>0]){
    fams[superfams_index[i]][[1]] <- unname(unlist(superfams[[i]][ranks_index[[i]]]))
}
length(fams)
fams[superfams_index]
save(fams, file = "data-raw/fams.RData")
# there are 2 scarab suborders that retrieve no families Adephaga and Polyphaga, check this out later
dupl <- unname(sapply(opentree_chronograms2$trees, function(x) any(grepl("_", x$ott_id))))
opentree_chronograms2$trees[which(dupl)[3]]
sapply(opentree_chronograms2$trees[which(dupl)[1:5]], "[", "ott_ids")
poly <- rotl::taxonomy_taxon_info(684689)
ade <- rotl::taxonomy_taxon_info(413458)
ade_tax_tree <- rotl::taxonomy_subtree(413458)
names(ade_tax_tree)
length(ade_tax_tree$tip_label) # 45222

# example of a tree with duplicated names:
opentree_chronograms2$trees[[9]]$ott_ids
opentree_chronograms2$studies[[9]]
tree <- rotl::get_study_tree(study_id="ot_103", tree_id= "tree4", tip_label="ott_id")
tree$tip.label
tree <- rotl::get_study_tree(study_id="ot_103", tree_id= "tree4", tip_label="ott_taxon_name")
tree$tip.label

# check that the workflow can be repeated from the function:
tip_fams <- get_ott_families(taxa = unname(tips2))
