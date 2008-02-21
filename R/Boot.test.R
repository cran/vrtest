`Boot.test` <-
function(y,kvec,nboot,wild)
{
    set.seed(12345)
    y <- as.matrix(y)
    LC <- LMCD(y,kvec)
    
    statmat <- matrix(NA, nrow=nboot, ncol=length(kvec)+1)
    if (wild == "Normal")
    {
        stat <- matrix(c(LC$M2,LC$CD2))
        for (i in 1:nboot)
        {
        ys <- y * rnorm(nrow(y))
        LCs <- LMCD(ys,kvec)
        statmat[i,] <- c(LCs$M2,LCs$CD2)
        }

    p <- matrix(NA,nrow = col(statmat), ncol=1)
    for (i in 1:ncol(statmat))
        {
        tem <- abs(statmat[,i]) > abs(stat[i])
        tem[tem == "TRUE"] <- 1
        p[i] <- mean(tem) 
        }
    }

    if (wild == "No")
    {
        stat <- matrix(c(LC$M1,LC$CD1))
        for (i in 1:nboot)
        {
        index <- as.integer(runif(nrow(y), min=1, max=nrow(y)))
        ys <- as.matrix(y[index])
        LCs <- LMCD(ys,kvec)
        statmat[i,] <- c(LCs$LM1,LCs$CD1)
        } 
    
    p <- matrix(NA,nrow = col(statmat), ncol=1)
    for (i in 1:ncol(statmat))
        {
        tem <- abs(statmat[,i]) > abs(stat[i])
        tem[tem == "TRUE"] <- 1
        p[i] <- mean(tem) 
        }
    }
    
    if (wild == "Mammen")
    {
        stat <- matrix(c(LC$M1,LC$CD1))
        for (i in 1:nboot)
        {
        ys <- y * Mammen(nrow(y))
        LCs <- LMCD(ys,kvec)
        statmat[i,] <- c(LCs$LM1,LCs$CD1)
        } 
    
    p <- matrix(NA,nrow = col(statmat), ncol=1)
    for (i in 1:ncol(statmat))
        {
        tem <- abs(statmat[,i]) > abs(stat[i])
        tem[tem == "TRUE"] <- 1
        p[i] <- mean(tem) 
        }
    }
    
    if (wild == "Rademacher")
    {
        stat <- matrix(c(LC$M1,LC$CD1))
        for (i in 1:nboot)
        {
        ys <- y * Rademacher(nrow(y))
        LCs <- LMCD(ys,kvec)
        statmat[i,] <- c(LCs$LM1,LCs$CD1)
        } 
    
    p <- matrix(NA,nrow = col(statmat), ncol=1)
    for (i in 1:ncol(statmat))
        {
        tem <- abs(statmat[,i]) > abs(stat[i])
        tem[tem == "TRUE"] <- 1
        p[i] <- mean(tem) 
        }
    }
return(list(Holding.Period=kvec,LM.pval=as.numeric(p[1:length(kvec)]),CD.pval=as.numeric(p[length(kvec)+1])))
}

