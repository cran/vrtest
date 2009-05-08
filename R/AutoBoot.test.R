AutoBoot.test <-
function(y,nboot,wild,prob=c(0.025,0.975))
{
    set.seed(12345)
    y <- as.matrix(y)
    LC <- Auto.VR(y)
    
    statmat <- matrix(NA, nrow=nboot, ncol=1)
    if (wild == "Normal")
    {
        for (i in 1:nboot)
        {
        ys <- y * rnorm(nrow(y))
        statmat[i,] <- Auto.VR(ys)
        }
    }
    
    if (wild == "Mammen")
    {
        for (i in 1:nboot)
        {
        ys <- y * Mammen(nrow(y))
        statmat[i,] <- Auto.VR(ys)
        }
    }
    
    if (wild == "Rademacher")
    {
        for (i in 1:nboot)
        {
        ys <- y * Rademacher(nrow(y))
        statmat[i,] <- Auto.VR(ys)
        }
    }
     tem <- abs(statmat) > abs(LC)
     tem[tem == "TRUE"] <- 1
      p <- mean(tem)
      CI <- quantile(statmat,prob)
return(list(test.stat=LC,pval=p,CI=CI))
}

