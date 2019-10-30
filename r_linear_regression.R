df=mtcars
head(df)
summary(mtcars)
#step1: To fit the model
lmfit = lm(df$mpg ~ df$wt)
#step2 : Analysing the model
summary(lmfit)

#estimate if intercept = 37.28
# y intercept: value of y, when x=0.
# In this case, it is the mileage at wt = 0.
#intercept it as it as maximum poissble mileage of any car within this data set

#estimate of beta coefficient = -5.35
#for each unit increase in wt, mpg decresses by 5.35

#plot the graph 
predmpg = predict(lmfit,df)
head(df)

head(lmfit)
#overlaying the predicted va;ues of mpg opn actual values of mpg.
plot(df$wt,df$mpg)
points(df$wt, predmpg, col= "red" )
df
df$mpg
predmpg
class(df$mpg)#to know the class
class(predmpg)
#mutiple R square interpretation
cor(df$mpg, predmpg)^2
summary(lmfit)
#It is the square of the correlation betwen actual mpg and prediceted mpg

#interpreting standard errors
#lower the standard error,higher tge reliabity

#interpreting the p values [pr(>|t|)]
#atleast one star means the p is less than 0.05
#alteast one star means statistical siginficance of relationship in population

#no star imply the value of the coefficient in population is zero
# no star imply that with cahnge in x, there is no change in y.

#interpret Adjusted R square
#adjusted R square is always <= R square
#r square 
#if the prediction is very good, then the r square value should be close to one.(strong correlation)


#Methoids to Improve R square
# 1. Increase sample size
#2. increase the number of independent of varaiables

#adjusted R square is tampered proof estimate of R suare that is oblivious(does not change) w r t increase in
#sample size or independent variables

lmfit = lm(mpg ~wt, data =df)

#step3: 
carwt = data.frame(wt = c(2,3,4))
predict(lmfit,carwt, interval = "confidence")

#mutiple linear regression\\

plot(df$hp, df$mpg)

#multiple linear regression
lmrfit <- lm(mpg ~ hp + wt, data= df)
summary(lmrfit)

#interpretation 
#mpg = 37.227 - 0.03*hp - 3.87*wt
#wt = 2 tonne, hp = 250
#mpg = 37.227 - 0.03*250 - 3.87*2

#Predcition
newcars = data.frame(hp = c(100,150), wt=c(3,2))
summary(newcars)
predict(lmrfit, newcars)
predict(lmrfit, newcars, interval = "confidence")#getting the range for the confidence level.. by default it is 95%

#determining the variable importance
summary(lmrfit)

mean(df$wt)
mean(df$hp)

sd(df$wt)
sd(df$hp)

# if(sd(df$wt) == sd(df$hp))print("fine")
# else
#   print("not fine")

slmfit = lm(mpg ~ scale(wt)+scale(hp),data =df)
#scale(df$wt[1:2])
summary(slmfit)

#interpretation slmfit
 
#for one sd increase in wt, mpg decereases by 3.8 units
#for one sd increase in hp, mpg decreases by 2.2 units