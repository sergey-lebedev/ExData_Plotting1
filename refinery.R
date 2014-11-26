refinery <- function(a,b){
    n <- min(length(a), length(b))
    a <- a[1:n]
    b <- b[1:n]
    c <- NULL
    a_not_is_na_idx <- !is.na(a)
    b_not_is_na_idx <- !is.na(b)
    idx <- a_not_is_na_idx & b_not_is_na_idx
    c[idx] <- (a[idx] + b[idx])/2
    c[!a_not_is_na_idx] <- b[!a_not_is_na_idx]
    c[!b_not_is_na_idx] <- a[!b_not_is_na_idx]
    return(c)
}
