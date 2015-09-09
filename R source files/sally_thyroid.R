
library(car)
library(hydroGOF)
full_data   <- read.csv("~/Dropbox/MEng Project/R Stuff/thyroid-training.csv")
test <-  read.csv("~/Dropbox/MEng Project/R Stuff/thyroid-testing.csv")

 #change this to own directory
data        <- full_data

data$Room_Level.f = factor(data$Room_Level)
data$Surgeon_Level.f = factor(data$Surgeon_Level)


test$Room_Level.f = factor(test$Room_Level)
test$Surgeon_Level.f = factor(test$Surgeon_Level)


model <- lm(log(Actual_Length) ~ Predicted_Length + Age + ASA_Level + Surgeon_Level.f + Emergency + Room_Level.f + Facility + Anes_Type_Level, data) 

#sum <- summary(model)
std_res = rstandard(model)
plot(model$fitted.values, std_res, ylab = 'Std. Residuals', xlab = 'Fitted Vaues', main =  "Log Model Residuals for Thyroidectomy")

qqPlot(model$residuals,distribution="norm",main= "Log Transform QQ Plot for Thyroidectomy", ylab="Sample Quantiles" )

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
print(paste("RMSE of ours: ",mae(predicted,actual)))



#RMSE and MSE of their predictions
actual <- log(test$Actual_Length)
predicted <- log(test$Predicted_Length)
print(paste("RMSE of theirs: ",mae(predicted,actual)))


#Bias-Corrected Version of their predictions
actual <- log(test$Actual_Length)
bias_mean = mean(data$Actual_Length - data$Predicted_Length)
predicted <- log(test$Predicted_Length + bias_mean)
print(paste("RMSE of theirs bias-corrected: ",mae(predicted,actual)))
