`VR.minus.1` <-
function(y,kvec)
{
    coe <- AR1(y)$ALPHA
    T <- length(y)
    lq <- ABEL1Q(T,coe)    
    vrsum <- 1
    for (i in 1:(T-1))
    {
        sum1 <- 0
        for (t in 1:(T-i))
        {
        sum1 <- sum1 + y[t]*y[t+i]
        }
    sum1 <- sum1/(sum(y^2))
    vrsum <- vrsum + 2*kfunc(i/lq)*sum1
    }
vr.auto <- abs(vrsum - 1)

    y <- as.matrix(y)
    n <- nrow(y) 
    m <- mean(y)
    vr1 <- sum( (y-m)^2 )/n
    
    mq <- numeric()
    for (i in 1:length(kvec))
    {
    k <- kvec[i]
    
    index <- 1:k
    summ <- 0
    for (j in k:n)
    {
    summ <- summ + (sum(y[index]) -k*m)^2
    index <- index+1
    }

    vr2 <- summ/(n*k)
    vr <- vr2/vr1
    
    mq <- c(mq,abs(vr-1))
    }  

return(list(VR.auto=vr.auto,Holding.Periods=kvec,VR.kvec=mq))
}

