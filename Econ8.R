options(digits = 4)
library(mvtnorm )
library(systemfit )
load("C:/Users/HP/Downloads/nbasal.RData")

mod1 = lm(points ~ age + exper + expersq + coll + guard + forward + black + marr, data = data )
coeftest(mod1)

mod2 = lm(rebounds ~ age + exper + expersq + coll + guard + forward + black + marr, data = data )
coeftest(mod2)

mod3 = lm(assists ~ age + exper + expersq + coll + guard + forward + black + marr, data = data )
coeftest(mod3)

X = as.matrix(cbind(rep(1, nrow(data)), data$age, data$exper,
                    data$expersq, data$coll, data$guard,
                    data$forward, data$black, data$marr))

Y = as.matrix(cbind(data$points, data$rebounds, data$assists))

n = dim(X)[1]
K = dim(X)[2]
J = dim(Y)[2]

solve(t(X)%*%X)%*%(t(X)%*%Y)

sv = c(rep(0, K*J), rep(1,(J^2 - J)/2+J))
vp = function(x){
    b = matrix(x[1:(K*J)], ncol = J)
    cholS = matrix(0, J, J);
    cholS[upper.tri(matrix(1, J, J), diag = TRUE)] = x[(K*J+1):length(x)];
    S = t(cholS)%*%cholS
    S = .5*(S+t(S));
    list(b=b, S=S)
}

negll <- function(parms){
    p = vp(parms)
    like = dmvnorm(Y - X %*% p$b, rep(0, J), p$S)
    return(-sum(log(like)))
}

res1 = optim(par=sv, negll, gr = NULL, method = "BFGS", hessian = TRUE, control = list(maxit= 1e6))
vp(res1$par)

cov2cor(vp(res1$par)$S)

r1 = points ~ age + exper + expersq + coll + guard + forward + black + marr
r2 = rebounds ~ age + exper + expersq + coll + guard + forward + black + marr
r3 = assists ~ age + exper + expersq + coll + guard + forward + black + marr
fitsur <- systemfit(list(pointsreg = r1, reboundsreg = r2, assistsreg = r3), data = data)
summary ( fitsur )

