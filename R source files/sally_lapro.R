## MEng Project
## First Modelling Attempt
## Eeshan Wagh

## Last Modified: February 2nd, 2014
library(car)
library(hydroGOF)
full_data   <- read.csv("~/Dropbox/MEng Project/R Stuff/lapro-training_new_no30.csv")
test <-  read.csv("~/Dropbox/MEng Project/R Stuff/lapro-testing_new_no30.csv")

 #change this to own directory
data        <- full_data
## Constructing the Model
#data$ASA.f = factor(data$ASA) #convert into categorical
data$ASA_Level.f = factor(data$ASA_Level)
data$Room_Levels.f = factor(data$Room_Levels)
data$Surgeon_Level.f = factor(data$Surgeon_Level)

test$ASA_Level.f = factor(test$ASA_Level)
test$Room_Levels.f = factor(test$Room_Levels)
test$Surgeon_Level.f = factor(test$Surgeon_Level)


model <- lm(log(Actual_Length) ~ Predicted_Length + Age + ASA_Level.f + Surgeon_Level.f + Emergency + Room_Levels.f + Facility + Anes_Level, data) 

sum <- summary(model)
std_res = rstandard(model)
plot(model$fitted.values, std_res, ylab = 'Standardized Residuals', xlab = 'Fitted Vaues', main =  "Log Model Residuals for Laproscopic Assisted C/H")

qqPlot(model$residuals,distribution="norm",main= "Log Transform QQ Plot for Laproscopic Assisted C/H", ylab="Sample Quantiles" )

## Plot Actual Vs Predicted Length
#plot(log(data$Actual_Length), model$fitted.values, main = "Actual Length Vs Fitted Length", xlab = 'Actual Length', ylab = 'Fitted Length')



## Changing the Baseline to Michelassi
#data$Surgeon_Level.f = relevel(data$Surgeon_Level.f, "MICHELASSI")
#model <- lm(log(Actual_Length) ~ Predicted_Length +  Age + ASA_Level.f + Surgeon_Level.f + Emergency + Room_Levels.f + Facility + Anes_Level, data) 

#summary(model)

## Boxplot
#boxplot(Actual_Length ~ Predicted_Length, data = data, xlab = 'Predicted Length', ylab = 'Actual Length', main = 'Boxplot of Actual Length by Predicted Length')

### MSE Comparison
#Diff <- log(data$Actual_Length) - log(data$Predicted_Length)
#Diff2<- log(data$Actual_Length) - (model$fitted.values)



#RMSE and MSE of our predictions
actual <- log(test$Actual_Length)
predicted <- predict(model,test)
print(paste("RMSE of ours: ",rmse(predicted,actual)))



#RMSE and MSE of their predictions
actual <- log(test$Actual_Length)
predicted <- log(test$Predicted_Length)
print(paste("RMSE of theirs: ",rmse(predicted,actual)))


#Bias-Corrected Version of their predictions
actual <- log(test$Actual_Length)
bias_mean = mean(data$Actual_Length - data$Predicted_Length)
predicted <- log(test$Predicted_Length + bias_mean)
print(paste("RMSE of theirs bias-corrected: ",rmse(predicted,actual)))
