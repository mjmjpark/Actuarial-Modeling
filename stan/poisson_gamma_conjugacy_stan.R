set.seed(123)
theta = rgamma(1, shape=1, rate=1)
N=20 #데이터가 적어서 MLE와 큰 차이
x = rpois(N, lambda=theta)
theta


library(rstan)
data = list(N=N, x=x)
stanmodel = stan_model(file="poisson_gamma_stan1.stan")
fit = sampling(stanmodel, data=data, pars=c("theta"), 
               init=function(){
                 list(theta=runif(1,0,20))
               }, seed=123, chains=2, iter=3000, warmup=200, thin=2
)

fit #0.35, MAP=1/3(mode) 
#손으로 구한거랑 거의 비슷할 것.


### 시각화 ###
ms = rstan::extract(fit, permuted=FALSE, include = TRUE)
class(ms)
dimnames(ms)
theta_s=ms[,,"theta"] #2차원 matrix

plot(theta_s[, 1], type="l") #왔다갔다 >> independence
lines(theta_s[, 2], col=2) #겹쳐져야 정상 

 
#패키지로 그려도 됨
library(bayesplot)
p <- mcmc_trace(as.array(fit),pars=c("theta"))
p

