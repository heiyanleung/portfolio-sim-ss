port.sim.ss <- function(data=ret.mon, idx=dji.mon, rf=0.001, lint=FALSE){
  if(length(data[,1])!=length(idx[,1]))
    stop("The number of Portfolio and Index returns must equal.")
  num.stock <- ncol(data)
  mean.ret <- apply(data, 2, mean)
  mean.idx <- apply(idx, 2, mean)
  var.idx <- var(idx)
  vcov.ret <- cov(data)
  sigma.ret <- rep(0,num.stock)
  for (i in 1:ncol(vcov.ret)){
    sigma.ret[i]<-sqrt(vcov.ret[i,i])
    }
  a <- rep(0,num.stock)
  b <- rep(0,num.stock)
  for(i in 1:num.stock){
    q <- lm(data[,i]~idx)
    a[i]<-q$coef[1]
    b[i]<-q$coef[2]
    }
  stock <- rep(0,num.stock); mse <- rep(0,num.stock); Rbar <- rep(0,num.stock)
  Ratio <- rep(0,num.stock); col1 <- rep(0,num.stock); col2 <- rep(0,num.stock)
  col3 <- rep(0,num.stock); col4 <- rep(0,num.stock); col5 <- rep(0,num.stock)
  for(i in 1:num.stock){
    Rbar[i] <- a[i]+b[i]*mean.idx
    mse[i] <- sum(lm(data[,i]~idx)$residuals^2)/(nrow(data)-2)
    Ratio[i] <- (Rbar[i]-rf)/b[i]
    stock[i] <- i
    }
  xx <- (cbind(stock, a, b, Rbar, mse, Ratio))
  aaa <- xx[order(-Ratio),]
  col1 <- (aaa[,4]-rf)*aaa[,3]/aaa[,5]
  col3 <- aaa[,3]^2/aaa[,5]
  for(i in(1:num.stock)) {
    col2[i] <- sum(col1[1:i])
    col4[i] <- sum(col3[1:i])
    }
  for(i in (1:num.stock)) {
    col5[i] <- var.idx*col2[i]/(1+var.idx*col4[i])
    }
  z.short <- (aaa[,3]/aaa[,5])*(aaa[,6]-col5[length(col5)])
  lint <- lint
  ifelse(lint, x.short <- (z.short)/sum(abs(z.short)), x.short <- z.short/sum(z.short))
  a.single.short <- t(aaa[,2]) %*% x.short
  b.single.short <- t(aaa[,3]) %*% x.short
  ret.single.short <- a.single.short + b.single.short * mean.idx
  sd.single.short <- sqrt(b.single.short^2 * var.idx +(t(x.short^2)) %*% aaa[,5])
  table <- cbind(aaa,z.short,x.short)
  return(list(table=table,stock=names(data[(aaa[,1])]),
    weights=x.short,return=ret.single.short,risk=sd.single.short))
}
