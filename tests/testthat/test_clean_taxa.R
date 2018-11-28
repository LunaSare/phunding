test_that("get_ott_clade works", {
    xx <- get_ott_clade(input = c("random", "Homo"))
    xx <- get_ott_clade(input = c("random", "Homo"), rank = c("family", "order", "class"))
    xx <- get_ott_clade(input = c("perro", "canis", "Homo"), rank = c("family", "order", "class"))
    xx <- get_ott_clade(input = c("Lamiaceae", "Campanulaceae", "Fabaceae"), rank = "family")
    expect_false(all(is.na(xx$family))) # this shoudl return the same lineages since they are all family level already
})
