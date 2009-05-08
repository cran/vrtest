stat.plot <-
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
    tem1 <- 2*(2*k-1)*(k-1)
    tem2 <- 3*k*n

    se <- sqrt( tem1/tem2 )
    return(list(vr=vr,se=se))
}

