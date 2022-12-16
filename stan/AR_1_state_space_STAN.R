source("repeated_data_management.R")

#Inspect the data
head(mydata, 1)           #training data 
head(data.valid,1)        #test data


X_train<-model.matrix(~TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown+factor(col.Cov.Idx), data=mydata)
train = data.frame(n=mydata$n,s=mydata$s,id=mydata$PolicyNum, year=mydata$Year, x=X_train)
X_test<-model.matrix(~TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown+factor(col.Cov.Idx), data=data.valid)
test = data.frame(n=data.valid$n,s=data.valid$s,id=data.valid$PolicyNum, year=data.valid$Year, X=X_test)
  
# n:frequency, s:aggregate loss, Type: one-hot encoding, col.Cov.Idx: 0,1,2
# id: PolicyNum
mean(mydata$PolicyNum %in%  data.valid$PolicyNum) # not all training data is in test data
mean(data.valid$PolicyNum %in% mydata$PolicyNum) # all test data is in training data

###################################################################################
# Exercise: transform "train" and "test" to longitudinal form
library(tidyverse)
# Answer: Preparing Data
###################################################################################
# data
head(train,1)
dim(train)
IDs = unique(train$id)
train_time = matrix(NA, nrow=length(IDs), ncol=(dim(train)[2]+10))
dim(train_time)
train$id %in% IDs[1]
obs_years = matrix(NA, nrow=length(IDs), ncol=6)
non_zero_years = matrix(NA, nrow=length(IDs), ncol=6)
idx_test=rep(0, length(IDs))
dim(obs_years)
for (i in 1:length(IDs)){

 Num_Ob =  sum(train$id %in% IDs[i])
 row_idx_i = train$id %in% IDs[i]
 observed_year_i = train$year[row_idx_i]
 train_time[i, c(observed_year_i-2005, observed_year_i-1999)]=c(train$n[row_idx_i], train$s[row_idx_i])
 train_time[i, 13:(dim(train_time)[2])]=c(unlist(train[row_idx_i, 3:12][1,]))
 train_time[i,]
 train_time[i,14]=sum(row_idx_i)
 obs_years[i, 1:length(observed_year_i)] = observed_year_i-2005
 class(train_time)
 if(IDs[i] %in% test$id){
   test_idx= (IDs[i]==test$id)
   train_time[i,c(6, 12)]=c(test$n[test_idx], test$s[test_idx])
   idx_test[i]=1
 }
 # train_time[i,]
 # for (t in 1:Num_on){
 #   train_time[i, c(observed_year_i-2005, observed_year_i-2005)]=
 # }
}

# n : claim counts
# s : claim severity

colnames(train_time) <- c("n1", "n2", "n3", "n4", "n5", "n6", 
                          "s1", "s2", "s3", "s4", "s5", "s6", "id", "num_t"
                          , "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7")
head(train_time) #data
head(obs_years)  #observed years in the training data
num_obs_year=train_time[,"num_t"] #count ot observed years in the training data
idx_test 

### 데이터 가공 Part (다시 코드 작성 추천)
#This part is for the analysis of aggregate sum (supposed to be a homework)
Num_positive_year = rowSums(train_time[, 1:6]>0,na.rm = TRUE)
Positive_people= which(Num_positive_year>0)
Indicator_positive_year = train_time[, 1:6]>0
Indicator_positive_year[is.na(Indicator_positive_year)] <- FALSE
#Num_positive_year
which_positive = matrix(FALSE, nrow=length(Num_positive_year), ncol=6)
for(i in 1:length(Num_positive_year)){
  if (Num_positive_year[i]>0){
    which_positive[i,1:Num_positive_year[i]] =c(1,2,3,4,5,6)[Indicator_positive_year[i,]]    
  }
}
head(train_time[, 1:6])
head(which_positive)
head(Num_positive_year)
head(Positive_people)
#End of aggregate sum




#Data to be used in STAN
X=train_time[,15:22]
N=train_time[,1:5]
S=train_time[,7:11]
T=6
dim(obs_years)
num_obs_year
N_policy=dim(train_time)[1]
n=nrow(X)
k=ncol(X)
length(num_obs_year)

#auxilary parameters
mu0=rep(0, k)
sigma0 = rep(10, k)
# m.beta=rep(0,dim(X)[2]) 
# invS.beta=1*diag(dim(X)[2])
c0=0.001 
d0=0.001


is.na(N)
N[is.na(N)] <- 999
obs_years[is.na(obs_years)] <- 999

dat <- list(T=T, n = n, k = k, X = X, N = N, obs_years = obs_years[,1:5], 
            num_obs_year=num_obs_year, mu0=mu0, sigma0=sigma0, 
            c0=c0, d0=d0)# to be fixed here!!!

dim(obs_years)
#NA: N, obs_years
head(obs_years)
length(num_obs_year)



#Stan
model_code = "
data{
  int n;
  int k;
  int T;
  matrix[n, k] X;
  int N[n, T-1];
  int obs_years[n, T-1];
  int num_obs_year[n];
  vector[k] mu0;
  vector[k] sigma0;
  real c0;
  real d0;
}

parameters {
  real R[n, T];
  real<lower=-1, upper=1> phi;
  real<lower=0> sigsq;
  vector[k] beta; 
}

model {
for(i in 1:n){
  R[i,1] ~ normal(0, sigsq/(1-phi^2));
  
  for(t in 2:T){
  R[i,t] ~ normal(phi*R[i,t-1], sigsq);
  }

}

for(i in 1:n){
  for(t in 1:num_obs_year[i]){
    N[i,obs_years[i,t]] ~ poisson( 
      exp(dot_product(X[i, ] , beta)-0.5*sigsq/(1-phi^2)+R[i,obs_years[i,t]]) );
  }
}
beta ~ multi_normal(mu0, diag_matrix(sigma0));
sigsq ~ uniform(0,100);
phi ~ uniform(-1,1);
}

### 이름 다시 붙여주기 (가공)
generated quantities {
  real return_hidden_1[n];
  for (i in 1:n){
   return_hidden_1[i] = R[i,T]; #R중에 6번째 해를 다시 이름 붙여주는!
  }
}
"

#fit <- stan(model_code = model_code, data = dat, iter = 1000, chains = 3) 
stanmodel<-stan_model(model_code=model_code)
fit <-sampling(stanmodel, data=dat, pars=c("phi", "sigsq", "beta", "retrun_hidden_1"), iter=1000, warmup=200, chains=3)

### pars옵션으로 sampling할 때 돌려받고싶은 parameter 지정 가능

print(fit)
stan_diag(fit)
stan_hist(fit)
stan_trace(fit)
