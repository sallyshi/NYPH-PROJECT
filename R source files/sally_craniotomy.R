## Last Modified: February 22nd, 2014
library(car)
library(hydroGOF)
data_cran   <-  read.csv("~/Dropbox/MEng Project/R Stuff/Craniotomy_No30_Training.csv")
test_cran <- read.csv("~/Dropbox/MEng Project/R Stuff/Craniotomy_No30_Testing.csv")

## Constructing the Model
data_cran$ASA_Level.f = factor(data_cran$ASA_Level)
data_cran$Room_Level.f = factor(data_cran$Room_Level)
data_cran$Surgeon_Level.f = factor(data_cran$Surgeon_Level)
data_cran$Anes_Type.f = factor(data_cran$Anes_Type)
data_cran$Anesthesiologist_Level.f = factor(data_cran$Anesthesiologist_Level)


test_cran$ASA_Level.f = factor(test_cran$ASA_Level)
test_cran$Room_Level.f = factor(test_cran$Room_Level)
test_cran$Surgeon_Level.f = factor(test_cran$Surgeon_Level)
test_cran$Anes_Type.f = factor(test_cran$Anes_Type)
test_cran$Anesthesiologist_Level.f = factor(test_cran$Anesthesiologist_Level)


model_2     <- lm(log(Actual_Length) ~ Predicted_Length + Age + ASA_Level.f + Surgeon_Level.f + Emergency + Room_Level.f + Anes_Type + Anesthesiologist_Level , data_cran) 




## Diagnostics
#plot(model_2$fitted.values, model_1$residuals,xlab = 'Fitted Values', ylab = 'Residuals',main= 'Residual Plot with Predicted Length',xlim=c(150,350))
##hist(model_2$residuals, main = "Residual Histogram")
qqnorm(model_2$residuals)
qqline(model_2$residuals)
qqPlot(model_2$residuals,distribution="norm",main= "QQ Plot of Residuals for Craniotomy", ylab="Sample Quantiles" )
## standardized residuals
std_res = rstandard(model_2)
plot(model_2$fitted.values, std_res,ylab = 'Std. Residuals',xlab = "Fitted Values", main= " Standardized Residual Plot for Craniotomy")



#RMSE and MSE of our predictions
actual <- log(test_cran$Actual_Length)
predicted <- predict(model_2, test_cran)
print(paste("RMSE of ours: ",rmse(predicted,actual)))


#RMSE and MSE of their predictions
actual <- log(test_cran$Actual_Length)
predicted <- log(test_cran$Predicted_Length)
print(paste("RMSE of theirs: ",rmse(predicted,actual)))




#Bias-Corrected Version of their predictions
actual <- log(test_cran$Actual_Length)
bias_mean_c = mean(data_cran$Actual_Length - data_cran$Predicted_Length)
predicted <- log(test_cran$Predicted_Length + bias_mean_c)
print(paste("RMSE of ours: ",rmse(predicted,actual)))



#MAE of our predictions
actual <- log(test_cran$Actual_Length)
predicted <- predict(model_2, test_cran)
print(paste("MAE of ours: ",mae(predicted,actual)))


#MAE of their predictions
actual <- log(test_cran$Actual_Length)
predicted <- log(test_cran$Predicted_Length)
print(paste("MAE of theirs: ",mae(predicted,actual)))




#MAE of Bias-Corrected Version of their predictions
actual <- log(test_cran$Actual_Length)
bias_mean_c = mean(data_cran$Actual_Length - data_cran$Predicted_Length)
predicted <- log(test_cran$Predicted_Length + bias_mean_c)
print(paste("MAE of bias: ",mae(predicted,actual)))



