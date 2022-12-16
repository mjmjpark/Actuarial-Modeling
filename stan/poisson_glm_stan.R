source("repeated_data_management.R")


#Inspect the data
head(data.train, 1)           #training data 
head(data.valid,1)        #test data


X_train<-model.matrix(~TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown+factor(col.Cov.Idx), data=data.train)
head(X_train)

train = data.frame(n=data.train$n,s=data.train$s,id=data.train$PolicyNum, year=data.train$Year, x=X_train)
head(train)

X_test<-model.matrix(~TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown+factor(col.Cov.Idx), data=data.valid)
test = data.frame(n=data.valid$n,s=data.valid$s,id=data.valid$PolicyNum, year=data.valid$Year, X=X_test)
  
# n:frequency, s:aggregate loss, Type: one-hot encoding, col.Cov.Idx: 0,1,2
# id: PolicyNum
mean(data.train$PolicyNum %in%  data.valid$PolicyNum) # not all training data is in test data
mean(data.valid$PolicyNum %in% data.train$PolicyNum) # all test data is in training data

head(train)

Y=train$n
X=train[, 5:12]
k=ncol(X)
n=nrow(X)
n
mu0=rep(0, k)
sigma0=rep(10, k)
library(rstan)
model_code = "
data {
int n; // number of data items 1500
int k; // number of predictors
matrix[n, k] X; //predictor matrix
int Y[n]; // outcome vector
vector[k] mu0; 
vector[k] sigma0;
}

parameters {
vector[k] beta; // coefficients for predictors
}

model {
 for (i in 1:n){
  Y[i] ~ poisson(exp(dot_product(X[i, ] , beta))); // likelihood
 }
 beta ~ multi_normal(mu0, diag_matrix(sigma0));
}
"

dat <- list(n = n, k = k, X = X, Y = Y, mu0=mu0, sigma0=sigma0)

fit <- stan(model_code = model_code, data = dat, iter = 1000, chains = 3) 
fit #3000개 뽑기

### mean보다 mode로 beta_hat fit 하는 것이 좋음 !!


### sample을 array형태로 변환
ms = rstan::extract(fit, permuted=FALSE, include = TRUE)
class(ms)
dimnames(ms)
dim(ms)
apply(ms, c(3), mean)


attributes(fit)
print(fit)
stan_diag(fit)
stan_hist(fit)
stan_trace(fit)

mm = model.matrix(~X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8])
head(mm)
class(mm)


#Comparison with MLE

### intercept(X[,1]) 제외
pois = glm(Y~X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8], family=poisson) 
pois = glm(Y~mm+0, family=poisson) 
summary(pois)
summary(pois)$coefficients[,1]
