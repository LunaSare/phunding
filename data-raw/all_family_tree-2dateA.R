# I. loading the tree and funds data
utils::data(fam_funds)
names(fam_funds)
length(fam_funds$ott_names)
utils::data(fam_tree)
names(fam_tree)
# we need the funded families to make sure they are all in the final tree

# II. using blackrim service to date otol for now
fam_tree_brlen <- datelife::get_dated_otol_induced_subtree(ott_ids =
  gsub("(.*)ott", "", unique(c(fam_tree$ott_ids, names(fam_funds$funds))))
)
