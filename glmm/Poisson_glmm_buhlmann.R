source("repeated_data_management.R")

#Inspect the data
head(data.train, 1)           #training data 
head(data.valid,1)        #test data
table(data.train$col.Cov.Idx) 
# n:frequency, s:aggregate loss, Type: one-hot encoding, col.Cov.Idx: 0,1,2
# id: PolicyNum
mean(data.train$PolicyNum %in%  data.valid$PolicyNum) # not all training data is in test data
mean(data.valid$PolicyNum %in% data.train$PolicyNum) # all test data is in training data


# Do the glm and predict for test data
poi1 <- glm(n ~ TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown+factor(col.Cov.Idx), data = data.train, family=poisson)
summary(poi1)
pred.poi1<-predict(poi1, type="response", newdata=data.valid)
mean((pred.poi1- data.valid$col.freq)^2)


#Prediction with Poisson GLMM and predict for test data
#install.packages("lme4")
library(lme4)
poi2 <- glmer(n ~ TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown+(1|PolicyNum), data = data.train, family=poisson)
summary(poi2)
pred.poi2<-predict(poi2, type="response", newdata=data.valid)
mean((pred.poi2- data.valid$n)^2)


# Closely understand the result for the prediction.
X<-model.matrix(~TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown, data=data.valid)
beta<-summary(poi2)$coefficients[,1]
lambda=exp(X%*%beta)
dim(data.train)
rane<-ranef(poi2, condVar=TRUE)
rane$PolicyNum[[1]]
lambda[1]*exp(rane$PolicyNum[[1]][1])

myindex<-unique(data.train$PolicyNum) %in% data.valid$PolicyNum
mean( (lambda*exp(rane$PolicyNum[[1]][myindex]) - data.valid$col.freq)^2)





#### Additional part #####
#Buhlmann Prediction 
summary(poi2)
str(summary(poi2))

beta<-summary(poi2)$coefficients[,1]
X<-model.matrix(~TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown, data=data.valid)
s.square=1.029
lambda=exp(X%*%beta)

u=exp(1/2*s.square)*lambda
v=exp(1/2*s.square)*lambda
a=lambda^2*(exp(s.square)-1)*exp(s.square)

Prem=K=Z=Xbar=0
for(i in 1:dim(data.valid)[1]){
  K[i]<-sum(data.train$PolicyNum %in% data.valid$PolicyNum[i])
  idx.buhl<-data.train$PolicyNum %in% data.valid$PolicyNum[i]
  Xbar[i]<-mean(data.train[idx.buhl,"col.freq"])
  Z[i]=(K[i]*a[i])/(v[i]+K[i]*a[i])
  Prem[i]=Z[i]*Xbar[i]+(1-Z[i])*u[i]
}


mean((Prem- data.valid$n)^2)

poi_changing_X <- glm(n ~ TypeCity+TypeCounty+TypeMisc+TypeSchool+TypeTown+col.Cov, data = data.train, family=poisson)
summary(poi_changing_X)
pred.poi_changing_X<-predict(poi1, type="response", newdata=data.valid)