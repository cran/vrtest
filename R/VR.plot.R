`VR.plot` <-
function(y,kvec)
{
val <- numeric()
for( i in 2:max(kvec))
{val <- c(val,stat.plot(y,i))}

plot(2:max(kvec),val,type="l",col="red",xlab="holding period",ylab="variance ratio",lwd=5)
abline(h=1)
grid(nx=max(kvec),lwd=1)
title(main = "Variance Ratios")
return(list(VR=cbind(2:max(kvec),val)))
}

