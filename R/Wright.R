`Wright` <-
function(y,kvec)
{
    n <- nrow(y)
    W_mat <- matrix(NA, nrow=length(kvec), ncol=3)
    for (i in 1:length(kvec))
    {
    k <- kvec[i]
    W <- Wright_stat(y,k)
    W_mat[i,] <- cbind(W$WR1,W$WR2,W$WS1)
    }

return(list(Holding.Period=kvec,R1.test=W_mat[,1],R2.test=W_mat[,2],S1.test=W_mat[,3]))
}

