`Auto.VR` <-
function(y)
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
vr <- sqrt(T/lq)*(vrsum - 1)/sqrt(2)
return(list(AutoVR=vr))
}

