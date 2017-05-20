#Alli Kapocius
#GSE 520
#Problem Set 5

load("C:/Temp/lawsch85.RData")
data = data[complete.cases(data),]

#creating the matrices
Y = as.matrix(data$salary)
X = as.matrix(cbind(1, data$LSAT, data$GPA, data$lcost, data$llibvol, data$rank))

#creating beta-hat
bh = solve(t(X)%*%X)%*%(t(X)%*%Y)

#description stats of residuals
resid = Y - X%*%bh
summary(resid)

#coefficient estimates
bh

#standard error estimates
n = nrow(data)
k = ncol(X)
vcv = 1/(n-k)*as.numeric(t(resid)%*%resid) * solve(t(X)%*%X)
stderr = sqrt(diag(vcv))

#t-values
t = (bh-0)/stderr

#p-values
p = 2*pt(abs(t), df=n-k,lower.tail= FALSE)

#residual standard error
var = cov(X,X)
rse = sqrt(var*(t(X)%*%X))

#R sqaured
SSE = sum(((X%*%bh) - mean(Y))^2)
SST = sum((Y - mean(Y))^2)
R.sq = SSE/SST


