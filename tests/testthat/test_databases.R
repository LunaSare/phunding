test_that("get_funded_taxa works", {
    # load("data-raw/grants.rda")  # cannot do load here either
    skip("it is working but it is very slow, so skip it for now")
    test1 <- get_funded_taxa()
})

test_that("get valid children works", {
    # length(all_flags) == length(taxon_info)
    # lapply(taxon_info[[2]]$children, "[", "flags")
    # names(taxon_info[[2]])
    # length(all_flags[[2]]) == length(all_names[[2]])
    # invalid %in% all_flags[[2]][[1]]
    # sapply(invalid, grepl, all_flags[[2]][[1]])
})
