install.packages(c("car", "MASS","pscl"))
library(car)
library(MASS)
library(pscl)

#USES DATA CLEANING 8 BUT WITH THE COMMENTED OUT GROUPINGS I SET  
#FREQTIDIED5 TO THE GROUPINGS INSTEAD OF FREQTIDIEDĖ AND USE THAT FOR TRAIN TEST SPLIT
#FREQTIDIED4 IS USED AS IS FOR THE FIRST COUPLE MODELS TO TEST UNGROUPED
# As long as Test Train split is defined models should all come out the same with very
# minor differences which will need to be changed in the report, fine for draft.
modeltest = function(model) {
  # AIC and BIC
  model_aic = AIC(model)
  model_bic = BIC(model)
  
  #Residuals
  residuals = deviance(model)
  
  # Residual Degrees of freedom
  df = model$df.residual
  
  # Return results as a list
  results = list(
    AIC = model_aic,
    BIC = model_bic,
    Residual_Deviance = residuals,
    Residual_df = df
  )
  return(results)
}

poisson_glm = glm(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + Area + Density + Region
                  +Gender + Transmission + Employment + Fines + offset(log(Exposure)), data = Freqtidied4, family = poisson(link = 'log'))
modeltest(poisson_glm)
negbin_glm <- glm.nb(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + Area + Density + Region
                     +Gender + Transmission + Employment + Fines + offset(log(Exposure)),
                     data = Freqtidied4)
modeltest(negbin_glm)
##Zero inflation models need to set inflationary predictors this model requires a lot of testing so for now to upload something 
## im going to focus on the negbin and poisson
# zip_model <- zeroinfl(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + Area + Density + Region
#                       +Gender + Transmission + Employment + Fines + offset(log(Exposure)) |
#                         VehAge + Gender + Region,
#                       data = Train_data, dist = "poisson")

##Basically Quasi requires a load of work to compute AIC and BIC I could do it if we want it for the model but
##for now I'm going to write it off 
# quasi_poisson_glm <- glm(ClaimNb ~ VehPower + VehAge + DrivAge + BonusMalus + VehBrand + Area + Density + Region
#                          +Gender + Transmission + Employment + Fines + offset(log(Exposure)),
#                          family = quasipoisson(link = "identify"),data = Freqtidied4)
# dfun <- function(object) {
#   with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
# }



#Poisson Testing 
poisson_glm1 = glm(ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + Fines + Employment + Gender +
                      BonusMalusGLM + VehBrand + VehGas + DensityGLM + Transmission +Region +offset(log(Exposure)),
                    family = poisson(link = "log"),
                    data = Train_data_freq)

modeltest(poisson_glm1)

step(poisson_glm1)

poisson_glm2 = glm(ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + Fines + 
                     Employment + BonusMalusGLM + VehBrand + VehGas + DensityGLM  + Transmission
                   +Region + offset(log(Exposure)),family = poisson(link = "log"),
                   data = Train_data_freq)
modeltest(poisson_glm2)

poisson_glm3 = glm(ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + Fines + 
                     Employment + BonusMalusGLM + VehBrand + VehGas + Region+ DensityGLM + 
                     offset(log(Exposure)),family = poisson(link = "log"),
                   data = Train_data_freq)
modeltest(poisson_glm3)

poisson_glm4 = glm(formula = ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + 
                     Fines + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region
                   +offset(log(Exposure)), family = poisson(link = "log"),  
                   data = Train_data_freq)
modeltest(poisson_glm4)

poisson_glm5 = glm(formula = ClaimNb ~VehPowerGLM + VehAgeGLM + DrivAgeGLM + 
                     Fines + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region
                   +offset(log(Exposure)), family = poisson(link = "log"),  
                   data = Train_data_freq)
modeltest(poisson_glm5)
#Negative Binomial Testing 

negbin_glm1 = glm.nb(ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + Fines + Employment + Gender +
                       BonusMalusGLM + VehBrand + VehGas + DensityGLM
                     +Transmission + Region +offset(log(Exposure)),
                     data = Train_data_freq)
modeltest(negbin_glm1)
step(negbin_glm1)

negbin_glm2 = glm.nb(ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + Fines + 
                       Employment + Gender + BonusMalusGLM + VehBrand + VehGas + 
                       DensityGLM + Region + offset(log(Exposure)),data = Train_data_freq)
modeltest(negbin_glm2)

negbin_glm3 = glm.nb(ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + Fines + 
                       +Employment + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region + 
                       offset(log(Exposure)),data = Train_data_freq)
modeltest(negbin_glm3)

negbin_glm4 = glm.nb(ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + Fines + 
                       BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region + 
                       offset(log(Exposure)),data = Train_data_freq)
modeltest(negbin_glm4)

negbin_glm5 = glm.nb(ClaimNb ~ VehPowerGLM + VehAgeGLM + DrivAgeGLM + Fines + BonusMalusGLM + 
                       VehBrand + VehGas + DensityGLM + Region + offset(log(Exposure))
                     ,data = Train_data_freq)
modeltest(negbin_glm5)


# this is a function based on what wuthrich does in his paper we can use it or not
# I was talking to another group and they said they arent using any 'machine learning'
# type tests.
Sampletest <- function(model) {

  # Calculate fitted values (in-sample prediction) for the model
  fitted_values = fitted(model)
  
  # In-sample loss calculation directly using fitted values from the model
  in_sample_loss = 2 * (sum(fitted_values) - sum(Train_data_freq$ClaimNb) + 
                           sum(log((Train_data_freq$ClaimNb / fitted_values)^(Train_data_freq$ClaimNb))))
  
  # Average in-sample loss
  average_in_sample_loss = in_sample_loss / nrow(Train_data_freq)
  
  # Calculate in-sample loss (MSE and RMSE)
  mse = mean((Train_data_freq$ClaimNb - fitted_values)^2)
  rmse = sqrt(mse)
                 
  # Out-of-sample prediction using the model and a new dataset for predictions
  predicted_values = predict(model, newdata = Test_data_freq, type = "response")
  
  # Out-of-sample loss calculation
  out_of_sample_loss = 2 * (sum(predicted_values) - sum(Test_data_freq$ClaimNb) + 
                               sum(log((Test_data_freq$ClaimNb / predicted_values)^(Test_data_freq$ClaimNb))))
  
  # Average out-of-sample loss
  average_out_of_sample_loss = out_of_sample_loss / nrow(Test_data_freq)
  
  # Calculate out-of-sample loss (MSE and RMSE)
  mseoos = mean((Test_data_freq$ClaimNb - predicted_values)^2)
  rmseoos = sqrt(mseoos)
  
  # Listing results
  results <- list(
    In_sample_loss = in_sample_loss,
    average_in_sample_loss = average_in_sample_loss,
    MSE = mse,
    RMSE = rmse, 
    out_of_sample_loss = out_of_sample_loss,
    average_out_of_sample_loss = average_out_of_sample_loss,
    MSEoos = mseoos,
    RMSEoos = rmseoos
  )
  
  return(results)
}
# Testing which model performs best on unknown data
Sampletest(poisson_glm1)
Sampletest(poisson_glm2)
Sampletest(poisson_glm3)                                      
Sampletest(poisson_glm4)
Sampletest(poisson_glm5)

Sampletest(negbin_glm1)
Sampletest(negbin_glm2)
Sampletest(negbin_glm3)
Sampletest(negbin_glm4)
Sampletest(negbin_glm5)

# Best model currently will be used as basis for comparison
# Interaction can only be verified using a correlation matrix or improvements to the model
# Choosing to go brute force method and seeing if model improves should it i will keep the 
# interaction and add another
poisson_glm4 = glm(formula = ClaimNb ~ AreaGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + 
                     Fines + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region
                   +offset(log(Exposure)), family = poisson(link = "log"),  
                   data = Train_data_freq)
AIC(poisson_glm4)
Sampletest(poisson_glm4)
summary(poisson_glm4)
interaction1 = glm(formula = ClaimNb ~ AreaGLM*DensityGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + 
                     Fines + BonusMalusGLM + VehBrand + VehGas + Region
                   +offset(log(Exposure)), family = poisson(link = "log"),  
                   data = Train_data_freq)
AIC(interaction1)
Sampletest(interaction1)
summary(interaction1)

interaction2a = glm(formula = ClaimNb ~ AreaGLM*DensityGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + 
                     Fines + BonusMalusGLM + VehBrand + VehGas +AreaGLM*Region
                   +offset(log(Exposure)), family = poisson(link = "log"),  
                   data = Train_data_freq)
AIC(interaction2)
Sampletest(interaction2)

interaction2b = glm(formula = ClaimNb ~ Region*DensityGLM + VehPowerGLM + VehAgeGLM + DrivAgeGLM + 
                     Fines + BonusMalusGLM + VehBrand + VehGas +AreaGLM*Region
                   +offset(log(Exposure)), family = poisson(link = "log"),  
                   data = Train_data_freq)
AIC(interaction3)
Sampletest(interaction3)

# Testing both as AIC better for region oos better for Area
interaction3a = glm(formula = ClaimNb ~ AreaGLM*DensityGLM + VehPowerGLM + VehAgeGLM 
                    + DrivAgeGLM*BonusMalusGLM + Fines
                    + VehBrand + VehGas +AreaGLM*Region
                   +offset(log(Exposure)), family = poisson(link = "log"),  
                   data = Train_data_freq)
interaction3b = glm(formula = ClaimNb ~ Region*DensityGLM + VehPowerGLM + VehAgeGLM +
                     DrivAgeGLM*BonusMalusGLM + Fines+ 
                     VehBrand + VehGas +AreaGLM*Region
                   +offset(log(Exposure)), family = poisson(link = "log"),  
                   data = Train_data_freq)
AIC(interaction3a)
AIC(interaction3b)
Sampletest(interaction3a)
Sampletest(interaction3b)
summary(interaction3a)
summary(interaction3b)
# 3b wins in the end lower AIC can be visualised looking at significance also 
# more significant terms in the summary than with Area
interaction4 = glm(formula = ClaimNb ~ AreaGLM*DensityGLM + VehPowerGLM + VehAgeGLM +
                      DrivAgeGLM*BonusMalusGLM + Fines*BonusMalusGLM+ 
                      VehBrand + VehGas +AreaGLM*Region
                    +offset(log(Exposure)), family = poisson(link = "log"),  
                    data = Train_data_freq)
AIC(interaction4)
Sampletest(interaction4)
summary(interaction4)
# Prefer model interaction4 to interaction5, beginning to increase runtime issues and
# model complexity more so than wanted and interaction5 is just looking for an interaction
# rather than thinking of what is a meaningful interaction.
interaction5 = glm(formula = ClaimNb ~ AreaGLM*DensityGLM + VehPowerGLM*VehBrand
                   + VehAgeGLM + DrivAgeGLM*BonusMalusGLM + Fines*BonusMalusGLM+ 
                     + VehGas +AreaGLM*Region
                   +offset(log(Exposure)), family = poisson(link = "log"),  
                   data = Train_data_freq)
AIC(interaction5)
Sampletest(interaction5)
summary(interaction5)
