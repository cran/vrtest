`LM_stat` <-
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
    tem2 <- 3*k

    m1 <- sqrt(n)*(vr-1)/sqrt( tem1/tem2 )

    y1 <- y-m
    dvec <- matrix(NA, nrow=(k-1), ncol=1)

    for (j in 1:(k-1))
    {
    summ <- 0
    i <- j+1
    for (i in (j+1):n)
    {
    summ <- summ + y1[i]^2*y1[i-j]^2
    }
    dvec[j] <- summ/( sum(y1^2)^2 )
    }

    summ <- 0
    for (j in 1:(k-1))
    {
    summ <- summ + ((2*(k-j)/k)^2*dvec[j])
    }

    m2 <- sqrt(n)*(vr-1)*((n*summ)^(-.5) )
return(list(LM1=m1,LM2=m2))
}

