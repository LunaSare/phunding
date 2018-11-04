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

# ancestral-ish reconstruction
# I. loading the data
names(grants)
utils::data(fam_funds)
utils::data(fam_tree)
head(fam_tree$tip.label)
head(fam_tree$ott_id)
head(fam_tree$ott_name)
fam_tree$tip.label <- fam_tree$ott_id
names(fam_tree)
names(fam_funds)
head(names(fam_funds$funds))
# II. making sure all funded family names (ott ids) are in the tips of the tree:
gg <- sapply(tolower(names(fam_funds$funds)), grep, tolower(fam_tree$tip.label))
length(gg) == length(gg[sapply(gg, length) >0])  # if TRUE all fams in funds are in the family tree
fam_funds$ott_names[which(sapply(gg, length) == 0)] # there are the families that are in funds (and where extracted by matching to ott with tnrs, and yet, are not on the ott family induced subtree)
# testing unfound taxa:
pp <- as.numeric(gsub("ott", "", names(gg[!sapply(gg, length) >0])))
fam_tree2 <- rotl::tol_induced_subtree(ott_id=as.numeric(gsub("ott", "", names(fam_funds$funds))), label_format = "id")
prob_info <- rotl::taxonomy_taxon_info(pp)
sapply(prob_info, "[", "flags")
# IIb. nudibranchia has no families obtained with get_ott_families in fam_tree, wha???
rotl::tnrs_match_names("nudibranchia")
grep("76753", fam_tree$node.label) # is empty, bc node labels are full names usually
fam_tree$node.label[grep("nudibranchia", tolower(fam_tree$node.lab))]
# so, nudibranchia is there, it just not gets any families with taxonomy_taxon_info
# then we will get them from the descendant tips in the tree:
phylobase::descendants(fam_tree, 1037+ape::Ntip(fam_tree), type = "tips")
# the above did not work
head(fam_tree$edge)
# but the node has descendants alright:
any(fam_tree$edge[,1] == 1037 + ape::Ntip(fam_tree))
# so trying another function
tt <- phytools::getDescendants(fam_tree, 1037+ape::Ntip(fam_tree))
fam_tree$tip.label[tt[tt<ape::Ntip(fam_tree)]]
# this one worked to get all tips from a node, like that we make sure we are taking all phylogenetic diversity into account
# changed get_ott_families so we get descendant tips when dealing with a taxon name above family level
# IIb. unfound taxa: [1] "Funariaceae"   "Tenebrionidae" "Cottidae"      "Agonidae"
#  [5] "Amaranthaceae" "Sphagnaceae"   "Ploceidae"     "Nectriaceae"
#  [9] "Hypocreaceae"  "Orobanchaceae" "Chrysomelidae"
# they have no flags
# we will just leave them outside the reconstruction for nrow
funds <- fam_funds$funds[sapply(gg, length) >0]
length(funds)
gg2 <- sapply(tolower(names(funds)), grep, tolower(fam_tree$tip.label))
all(sapply(gg2, length) >0)  # it is TRUE when all names are in the labels of the tree
# III. run the reconstruction
fam_tree_brlen <- ape::compute.brlen(fam_tree)
class(funds)
no_funds_names <- fam_tree$tip.label[!fam_tree$tip.label %in% names(funds)]
no_funds <- rep(0, length(no_funds_names))
names(no_funds) <- no_funds_names
all_funds <- c(funds, no_funds)
head(all_funds)
aa <- phytools::fastAnc(fam_tree_brlen, all_funds)
#takes too long to run
# lets try my algorithm
head(ape::reorder.phylo(fam_tree)$edge, 100)
tail(ape::reorder.phylo(fam_tree)$edge, 100)
ape::plot.phylo(ape::reorder.phylo(fam_tree_brlen), type = "fan", cex= 0.1)
edge_funds <- rep(0, nrow(fam_tree$edge))
edge_check <- rep(NA, nrow(fam_tree$edge))
# head(edge_funds)
# length(fam_tree_brlen$edge.length)
# head(fam_tree_brlen$edge.length)
# head(ape::reorder.phylo(fam_tree_brlen)$edge.length)
length(fam_tree$tip.label)
fam_tree$Nnode
#there are less nodes than tips, so there are politomies
numeric(max(fam_tree$edge))
length(ape::postorder(fam_tree))
values <- all_funds
head(values)
length(values)
head(fam_tree$tip.label, 10)
head(values[match(fam_tree$tip.label, names(values))], 10)
node_funding <-  function(tree, values) {
    res <- numeric(max(tree$edge))
    res[1:ape::Ntip(tree)] <- values[match(names(values), tree$tip.label)]
    for (i in ape::postorder(tree)) {
       tmp <- tree$edge[i,1]
       res[tmp] <- res[tmp] + res[tree$edge[i, 2]]
   }
   res
}
nf <- node_funding(fam_tree, all_funds)
# called the function sum_tips at the Encoding

edge_values <- sum_tips(fam_tree, all_funds)

# some previous embarrassing attempts
mm <- match(seq(ape::Ntip(fam_tree)), fam_tree$edge[,2])
# length(mm)
# head(mm)
# head(fam_tree$edge)
edge_check[mm] <- edge_funds[mm] <- unname(all_funds[match(fam_tree$tip.label, names(all_funds))])
# head(edge_funds)
# edge_funds[20:30]
# sum(is.na(edge_funds))
# length(fam_tree$node.label)
# length(fam_tree_brlen$edge.length)
# ape::is.binary(fam_tree_brlen)
# nrow(fam_tree$edge)-ape::Ntip(fam_tree)
# which(is.na(edge_funds))
# fam_tree$edge[mm,1]
# fam_tree$edge[j,]
      # for(i in seq(fam_tree$edge.length)){
      #   jj <- which(fam_tree$edge[,2]==i)
      #   rr <- match(fam_tree$edge[jj,1], fam_tree$edge[,2])
      #   # jj <-  which(fam_tree$edge[,1]==i)
      #   edge_funds[rr] <- edge_funds[rr] + edge_funds[jj]
      #   edge_check[rr] <- edge_check[rr] + edge_check[jj]
      # }
      # fam_tree$edge[which(is.na(edge_check)),]
      # i=12992
      # # rows that have info first
      # for(i in match(unique(fam_tree$edge[mm,1]),fam_tree$edge[,2])){
      #     jj <- which(i == match(fam_tree$edge[mm,1],fam_tree$edge[,2]))
      #     edge_funds[i] <- sum(edge_funds[mm][jj])
      # }
      # head(fam_tree$edge[mm,1])
for (i in 1:ape::Ntip(fam_tree)){
  jj <- fam_tree$edge[which(fam_tree$edge[,2]==i),1]
  ee <- which(fam_tree$edge[,2]==jj)
  edge_funds[ee] <- edge_funds[ee] + edge_funds[mm[i]]
  edge_check[ee] <- edge_funds[ee] + edge_funds[mm[i]]
}
      # fam_tree$edge[ee,]
      # nn <- match((ape::Ntip(fam_tree)+3):max(fam_tree$edge), fam_tree$edge[,2])
      which(is.na(edge_check))
      # length(which(!is.na(edge_check)))
      # length(which(is.na(edge_check)))
      # fam_tree$Nnode
      # fam_tree$edge[7511,]
      # count <- 1
      # for (i in (ape::Ntip(fam_tree)+3):max(fam_tree$edge)){
      #   jj <- fam_tree$edge[which(fam_tree$edge[,2]==i),1]
      #   ee <- which(fam_tree$edge[,2]==jj)
      #   edge_funds[ee] <- edge_funds[ee] + edge_funds[nn[count]]
      #   edge_check[ee] <- edge_check[ee] + edge_check[nn[count]]
      #   count <- count + 1
      # }
while(any(is.na(edge_check))){
  nn <- fam_tree$edge[which(!is.na(edge_check)),2]
  count <- 1
  for (i in nn[nn>min(which(is.na(edge_check)))){
    jj <- fam_tree$edge[which(fam_tree$edge[,2]==i),1]
    ee <- which(fam_tree$edge[,2]==jj)
    edge_funds[ee] <- edge_funds[ee] + edge_funds[nn[i]]
    edge_check[ee] <- edge_funds[ee] + edge_funds[nn[i]]
    count <- count + 1
  }
}
      # oo <- match(nn, fam_tree$edge[,1])
      # for (i in oo){
      #   ee <- which(fam_tree$edge[,2]==oo)
      # }
mm2 <- unique(match(mm, fam_tree$edge[,2]))
c(mm, fam_tree$edge[mm2,2])
