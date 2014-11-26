divisor <- function(values){
    h <- hist(values) #, breaks=100)
    s <- sort(h$intensities, index.return=TRUE, decreasing=TRUE)
    first_max_idx <- s$ix[1] 
    second_max_idx <- s$ix[2] 
    s <- sort(h$intensities, index.return=TRUE)
    indexes <- ((s$ix - first_max_idx)*(s$ix - second_max_idx) < 0)
    min_idx <- s$ix[indexes][1]
    border <- h$mids[min_idx]
    return(border)
}
