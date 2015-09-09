## MEng Project
## First Modelling Attempt
## Eeshan Wagh

## Last Modified: March 3rd 2014
library(car)
library(hydroGOF)
data   <-read.csv("~/Dropbox/MEng Project/R Stuff/vat-training.csv")
test_vat <- read.csv("~/Dropbox/MEng Project/R Stuff/vat-testing.csv")

## Constructing the Model
data$ASA.f = factor(data$ASA) #convert into categorical
test_vat$ASA.f = factor(test_vat$ASA)


model_1     <- lm((Actual_Length^(1/2)) ~ Predicted_Length + Age + ASA_Level + Surgeon_Level + Emergency + Room_Level + Pt_Type, data) 
qqPlot(model_1$residuals,distribution="norm",main= "QQ Plot of Residuals for Vat", ylab="Sample Quantiles" )



## Diagnostics
std_res = rstandard(model_1)
plot(model_1$fitted.values, std_res,ylab = 'Std. Residuals',xlab = "Fitted Values", main= " Standardized Residual Plot for Vat")





# SQRT RMSE and MSE of our predictions on training data
actual <- (data$Actual_Length)^(1/2)
predicted <- predict(model_1, data)
print(paste("SQRT RMSE of ours on training: ",rmse(predicted,actual)))



#SQRT RMSE and MSE of our predictions on testing data
actual <- (test_vat$Actual_Length)^(1/2)
predicted <- predict(model_1, test_vat)
print(paste("SQRT RMSE of ours on testing: ",rmse(predicted,actual)))





#LOG SCALE RMSE and MSE of our predictions on training data
actual <- log((data$Actual_Length))
predicted <- log(predict(model_1, data)^2)
print(paste("LOG SCALE RMSE of ours on training: ",rmse(predicted,actual)))



#LOG SCALE RMSE and MSE of our predictions on testing data
actual <- log((test_vat$Actual_Length))
predicted <- log(predict(model_1, test_vat)^2)
print(paste("LOG SCALE RMSE of ours on testing: ",rmse(predicted,actual)))


#LOG SCALE MAE of our predictions on testing data
actual <- log((test_vat$Actual_Length))
predicted <- log(predict(model_1, test_vat)^2)
print(paste("LOG SCALE MAE of ours on testing: ",mae(predicted,actual)))



#LOG SCALE MAE of their predictions
actual <- log(test_vat$Actual_Length)
predicted <- log(test_vat$Predicted_Length)
print(paste("MAE of theirs: ",mae(predicted,actual)))




#MAE of Bias-Corrected Version of their predictions
actual <- log(test_vat$Actual_Length)
bias_mean_c = mean(data$Actual_Length - data$Predicted_Length)
predicted <- log(test_vat$Predicted_Length + bias_mean_c)
print(paste("MAE of bias: ",mae(predicted,actual)))





