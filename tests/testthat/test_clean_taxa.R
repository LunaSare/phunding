test_that("get_ott_clade works", {
    xx <- get_ott_clade(input = c("random", "Homo"))
    xx <- get_ott_clade(input = c("random", "Homo"), rank = c("family", "order", "class"))
    xx <- get_ott_clade(input = c("perro", "canis", "Homo"), rank = c("family", "order", "class"))
})
