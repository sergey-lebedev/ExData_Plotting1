splitter <- function(time_sequence, glue_interval){
    n <- length(time_sequence)
    subsequences <- list()
    subsequence <- NULL
    idx <- diff(time_sequence) > glue_interval
    breaks <- c(1:n)[idx]
    left_border <- 1
    counter <- 1
    for (right_border in breaks){
        subsequence <- c(left_border:right_border)
        left_border <- right_border + 1
        subsequences[counter] <- list(subsequence)
        counter <- counter + 1
    }
    subsequence <- c(left_border:n)
    subsequences[counter] <- list(subsequence)
    return(subsequences)
}
