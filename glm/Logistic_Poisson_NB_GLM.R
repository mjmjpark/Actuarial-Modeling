# Faraway Chapter 2.
install.packages("faraway")
library(faraway)


#################
# Binary response 
#################
data(wcgs)#Heart attack data #Farawat Ch2
wcgs=na.omit(wcgs)
wcgs$chd = ifelse(wcgs$chd=="yes", 1, 0)
head(wcgs, 2)
lmod = glm(chd~. - dibep-typechd, family=binomial, data=wcgs)
summary(lmod)
predprob=predict(lmod, type="response")
MSE_a = mean((wcgs$chd-predprob)^2)
MSE_a

MSE_b = mean((wcgs$chd-(predprob>0.5) )^2)
MSE_b

sum(predprob>0.5) # Check how many men are classified as


predout = ifelse(predprob>0.5, "Yes", "no")
mytable = table(wcgs$chd, predout)
mytable
# Question: In theory, 
# what is the most proper threshold so that predictive MSE is minimized?
# Ans: threshold = 1/2

# I will not go over the following ROC example. You can do it by yourself to solve the problems.
# Draw ROC curve

# 1> Specificity:= (chd$False & predout$False) / chd$False
#     The rate that healthy people will be classified as healthy
2843/(2843+42)

# 2> Sensitivity:= (chd$True & predout$True) / chd$True
#     the rate that sick people will be classified as sick
75/(180+75) #relatively low

# Draw Sensitivity curve for different threshold
thresholds = seq(0.01, 0.49, length=100)
sensitivity = rep(NA, 100)
specificity = rep(NA, 100)
for(i in 1:100){
 predout = ifelse(predprob>thresholds[i], "Yes", "no")
 mytable = table(wcgs$chd, predout)
 specificity[i] = mytable[1,1]/(mytable[1,1]+mytable[1,2])
 sensitivity[i] = mytable[2,2]/(mytable[2,1]+mytable[2,2])
}

# plot of sensitivity and specificity 
plot(thresholds, sensitivity, type="l")
lines(thresholds, specificity, type="l", lty=2)

#ROC curve
plot(1-specificity, sensitivity, type="l")






lmod2 = glm(chd~age+height+weight+chol+cigs+arcus, family=binomial, data=wcgs)
predprob2=predict(lmod2, type="response")
predout = ifelse(predprob2>0.5, "Yes", "no")
mytable = table(wcgs$chd, predout)
mytable


MSE_a_2 = mean((wcgs$chd-predprob2)^2)
MSE_a_2

MSE_b_2 = mean((wcgs$chd-(predprob2>0.5) )^2)
MSE_b_2



# Draw Sensitivity curve for different threshold
thresholds = seq(0.01, 0.49, length=100)
sensitivity2 = rep(NA, 100)
specificity2 = rep(NA, 100)
for(i in 1:100){
  predout = ifelse(predprob2>thresholds[i], "Yes", "no")
  mytable = table(wcgs$chd, predout)
  specificity2[i] = mytable[1,1]/(mytable[1,1]+mytable[1,2])
  sensitivity2[i] = mytable[2,2]/(mytable[2,1]+mytable[2,2])
}

# plot of sensitivity and specificity 
plot(thresholds, sensitivity2, type="l")
lines(thresholds, specificity2, type="l", lty=2)

#ROC curve
plot(1-specificity, sensitivity, type="l")
lines(1-specificity2, sensitivity2, type="l", lty=2)



###################
# Binomial response 
###################
data(orings)#Faraway Ch3
mod1 = glm(cbind(damage, 6-damage)~temp, family=binomial, data = orings)
summary(mod1)
dim(orings)
pred_prob1 = predict(mod1, type="response")
pred_prob1
MSE1 = mean((orings$damage-pred_prob1*6)^2)
MSE1


mod2 = glm(cbind(damage, 6-damage)~temp+I(temp^2), family=binomial, data = orings)
summary(mod2)
pred_prob2 = predict(mod2, type="response")
pred_prob2
MSE2 = mean((orings$damage-pred_prob2*6)^2)
MSE2




#################
# Poisson GLM
#################
data(dicentric) #Faraway Ch5
dicentric$dosef = factor(dicentric$doseamt)
mod1 = glm(ca~offset(log(cells))+log(doserate)*dosef, family=poisson, data=dicentric)
mod1
mean(dicentric$ca)
pred=predict(mod1, type="response")
mean(pred)
MSE_0 = mean((dicentric$ca-mean(dicentric$ca))^2)
MSE_0
MSE_1 = mean((dicentric$ca-pred)^2)
MSE_1



library(MASS)
data(solder)
solder
mod_nb = glm.nb(skips~., data=solder)
mod_poisson = glm(skips~.,family=poisson, data=solder)

pred_nb=predict(mod_nb, type="response")
summary(mod_nb)


MSE_2 = mean((solder$skips-pred_nb)^2)
MSE_2


