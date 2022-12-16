source("repeated_data_management.R")


#Inspect the data
head(data.train, 1)           #training data 
head(data.valid,1)        #test data

N=dim(data.train)[1]
N
ID = unique(data.train$PolicyNum)
ID_N= length(ID)
idmap = 0
i=1

#unique값마다 ID 1, ... ,n까지로 부여해주는 complex function
for (id in 1:ID_N){
  while (data.train$PolicyNum[i] == ID[id]){
    idmap[i] = id
    i=i+1
    if( i == N+1) break;
  }
}

idmap
length(idmap)
head(idmap)



X_train<-model.matrix(~TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown+factor(col.Cov.Idx), data=data.train)
train = data.frame(n=data.train$n,s=data.train$s,id=data.train$PolicyNum, year=data.train$Year, x=X_train)
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
id_n = ID_N
mu0=rep(0, k)
sigma0=rep(10, k)
id_n
n
head(idmap)

library(rstan)
model_code = "
data {
int n; // number of data items
int k; // number of predictors
int id_n; // number of people
int idmap[n]; // 1~id_n
matrix[n,k] X; // predictor matrix
int Y[n]; // outcome vector
vector[k] mu0;
vector[k] sigma0;
}

parameters {
vector[k] beta; // coefficients for predictors
real<lower=0> alpha;
real<lower=0> R[id_n]; // random effects
}

#n:500

model {
 for (j in 1:n){
  Y[j] ~ poisson(R[idmap[j]]*exp(dot_product(X[j, ] , beta))); // likelihood
 }
 
 for (i in 1:id_n){
  R[i] ~ gamma(alpha, alpha);
 }
 
 beta ~ multi_normal(mu0, diag_matrix(sigma0));
 alpha ~ gamma(1.0/1.0, 1.0/1.0);
}
"


dat <- list(n = n, k = k, X = X, Y = Y, mu0=mu0, sigma0=sigma0, idmap=idmap, id_n=ID_N)

fit <- stan(model_code = model_code, data = dat, iter = 500, chains = 3)


# Too many variables. Collect only related variables.
print(fit)

stan_diag(fit)
stan_hist(fit)
stan_trace(fit)
