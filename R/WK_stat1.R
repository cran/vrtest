WK_stat1 <-
function(y,k)
{
    y <- as.matrix(y)
    n <- nrow(y) 
    m <- mean(y)
    vr1 <- sum( (y-m)^2 )/n

    index <- 1:k
    summ <- 0

    for (i in k:n)
    {
    summ <- summ + (sum(y[index]) -k*m)^2
    index <- index+1
    }

    vr2 <- summ/(n*k)
    vr <- vr2/vr1

    m1 <- sqrt(n)*(vr-1)
return(m1)
}

