#Replace An Assignment: 

#STEP 1: READ IN DATA
library(tidyverse)


data1 <- read.csv("~/Desktop/Econometrics/dataset_hsk.csv")
names(data1)
sapply(data1, is.numeric)


#Heteroskedasticity Visuals and Breuch-Pagan Test

#linear model: 
mod1 <- lm(monthly_food_expenditure.y. ~ monthly_income.x., 
           data = data1)
summary(mod1)

preds_df <- data.frame(preds = predict(mod1, newdata = data1),
                       resids = data1$monthly_food_expenditure.y. - predict(mod1))
data1 <- cbind.data.frame(data1,preds_df)

# Built-in plots:
p2 <- plot(mod1)
plot(p2)
#Looks big time heteroskedastic...

#plots predicted vs residual values
p1 <- ggplot(data1, aes(x = preds, y = resids)) + geom_point() + labs(x = "Fitted Values", y = "Residuals", title = "Data1 Plot")
plot(p1)
#Also looks big time heteroskedastic...non-constant variance in the error term

#A "True-Plot" Is a plot of true values vs predicted values

#Breuch-Pagan Test
install.packages("lmtest")
library(lmtest)
#Sig. Threshold: p<.05
#Ho (Null): Our model is Homoskedastic
#Ha (Alternative): Our model is Heteroskedastic
bptest(mod1)

#Our p-value of 0.00688 < .05...since our p is low we reject Ho (null hypothesis)
#our model is indeed heteroskedastic

#Corrections to heteroskedastic model:
#We will perform some transformations...

#Transformation 1 (most common)...logarithmic...transform variable y (log(y)) and regress it against our x-variable
data1 <- data1 %>% mutate(log(monthly_food_expenditure.y.)) %>% rename(log_y = "log(monthly_food_expenditure.y.)")

mod2 <- lm(log_y ~ monthly_income.x., 
           data = data1)
summary(mod2)

plot(mod2)
#Slightly better in regards to variance of error terms
bptest(mod2)
#p-value is much larger indicating that we cannot reject the null hypothesis that our model is homoskedastic



