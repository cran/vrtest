`stat.plot` <-
function(x,k)
{
    x <- as.matrix(x)
    n <- nrow(x)
    index <- 1:k
    summ <- 0

    for (i in k:n)
    {
    summ <- summ + sum(x[index])^2
    index <- index+1
    }

    vr1 <- sum(x^2)/n
    vr2 <- summ/(n*k)

    vr <- vr2/vr1
    return(vr)
}

