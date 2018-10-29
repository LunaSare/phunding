# functions to get funds by family
get_funds <- function(data = nsf_relevant_grants){
    fams <- vector_names(data$fams)
    # lapply(" ", grepl, data$fams)
    # grepl(" ", data$fams[531])
    # fams <- strsplit(fams, " ")
    # fams <- sapply(fams, "[", 1)
    fams_index <- sapply(fams, grep, data$fams)
    fams <- unique(c(fams, sapply(strsplit(names(fams_index[sapply(fams_index, length)==0]), " "), "[", 1)))
    fams_index <- sapply(fams, grep, data$fams)
    fams <- fams[sapply(fams_index, length) > 0]
    fams_index <- fams_index[sapply(fams_index, length) > 0]
    fams_funds <- sapply(fams_index, function(x) sum(as.numeric(data$estimatedTotalAmt[x])))
    return(fams_funds)
}
