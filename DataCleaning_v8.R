

##to run
getwd()



#if necessary, change this to your desired working directory
setwd(getwd())

CONTRACTS=read.csv("Motor Liability Claims Data - Frequency.csv",header=TRUE,stringsAsFactors=T)
CLAIMS=read.csv("Motor Liability Claims Data - Severity.csv",header=TRUE,stringsAsFactors=T)


#Merged DF:
merged_df <- merge(CLAIMS, CONTRACTS, by = "IDpol", all = TRUE)


merged_df

#filter out where claim number = NA
claimno <- merged_df[is.na(merged_df$ClaimNb) == TRUE, ]
#this is 195 lines where there is a claim amount, but no claim numbers or other factors. 
#Do we remove these from the model all together or try to predict the other factors and include them in severity
#I think remove from the model all together:


Freqtidied <- merged_df[is.na(merged_df$ClaimNb) == FALSE, ]
#checking there was 195 rows removed
(679708 - 679513)



#I want to remove where Claim amount = NA and Claim mo >= 1 , and I want to make claim no = 0 where claim no = 0


#getting rid of claim amounts = NA where claim No > 0
(653069 - 643953)
Freqtidied2 <- Freqtidied[!(is.na(Freqtidied$ClaimAmount) & Freqtidied$ClaimNb >= 1), ]
#check:
sum(is.na(Freqtidied$ClaimAmount) & Freqtidied$ClaimNb >= 1)
(653069 - 643953)

#setting claim amount = 0 if claim amount = Na and Claim number = 0
Freqtidied2$ClaimAmount[is.na(Freqtidied2$ClaimAmount) & Freqtidied2$ClaimNb == 0] <- 0
Freqtidied2
##seems ok!!!


#checking for NA
rows_with_na <- apply(Freqtidied2, 1, function(row) any(is.na(row)))
rows_with_na <- Freqtidied2[rowSums(is.na(Freqtidied2)) > 0, ]
rows_with_na



# not dealing with spikes for severity


##Spikes in the data, I want to keep them in for frequency as we are assuming these are claims that have occurred but are not
##settled (IBNR)


##Outliers: for frequency, I want to cap the Claim.no at 4 and claim amounts at 400,000
AmountCap = 400000  ## since we are capping and not removing this should be ok to do for freq df also
FreqCap = 4
Freqtidied3 <- Freqtidied2
#capping freq:
Freqtidied3$ClaimNb[Freqtidied3$ClaimNb >= FreqCap] <- FreqCap


#capping claim amount:
Freqtidied3$ClaimAmount[Freqtidied3$ClaimAmount >= AmountCap] <- AmountCap
##Question: If, for example I am capping freq at 4 - do I need to change the "amount" cell for that row, I presume
##not for now, while working in frequency. 
##If capping the amount do I need to change frequency - again I presume not since severity isn't really a factor here

Freqtidied3





#Severity

#filter out where claim number = NA
claimno <- merged_df[is.na(merged_df$ClaimNb) == TRUE, ]
#this is 195 lines where there is a claim amount, but no claim numbers or other factors. 
#Do we remove these from the model all together or try to predict the other factors and include them in severity
#I think remove from the model all together:


Sevtidied <- merged_df[is.na(merged_df$ClaimNb) == FALSE, ]
#checking there was 195 rows removed
(679708 - 679513)



#I want to remove where Claim amount = NA and Claim mo >= 1 , and I want to make claim no = 0 where claim no = 0


#getting rid of claim amounts = NA where claim No = 0
(653069 - 643953)
Sevtidied2 <- Sevtidied[!(is.na(Sevtidied$ClaimAmount) & Sevtidied$ClaimNb >= 1), ]
#check:
sum(is.na(Sevtidied$ClaimAmount) & Sevtidied$ClaimNb >= 1)
(653069 - 643953)

#setting claim amount = 0 if claim amount = Na and Claim number = 0
Sevtidied2$ClaimAmount[is.na(Sevtidied2$ClaimAmount) & Sevtidied2$ClaimNb == 0] <- 0
Sevtidied2
##seems ok!!!


#checking for NA
rows_with_na <- apply(Sevtidied2, 1, function(row) any(is.na(row)))
rows_with_na <- Sevtidied2[rowSums(is.na(Sevtidied2)) > 0, ]
rows_with_na



#Spikes
#I want to remove the claim amounts that have high frequency

#this table shows claim amounts and the frequency of that particular claim amount:
# Step 1: Create a frequency table of claim amounts
freq <- table(Sevtidied2$ClaimAmount)

# Step 2: Convert the table to a dataframe
freq_df <- as.data.frame(freq)

# Step 3: Rename the columns for clarity
colnames(freq_df) <- c("Claim_Amount", "Frequency")


#Set the frequency you want i.e. pick a significant frequency so if there are that many claims of the same number 
#they will be excluded
amountfreqcap = 2000
freq_df$Claim_Amount <- as.numeric(as.character(freq_df$Claim_Amount))
spikes = freq_df[freq_df$Freq > amountfreqcap & freq_df$Claim_Amount >0 ,]
spikes



#Now I want to remove entries in 'Sevtidied2' where the claim amount is present in spikes
# Filter out rows in Sevtidied2 where 'Claim Amount' is in 'spikes$Claim Amount'
Sevtidied3 <- Sevtidied2[!Sevtidied2$`ClaimAmount` %in% spikes$`ClaimAmount`, ]

#Plot
par(mfrow = c(1, 2))
boxplot(Sevtidied2$ClaimAmount, main = "Claim Amount")
boxplot(Sevtidied2$ClaimNb, main = "Claim Frequency")


##Outliers: for frequency, I want to cap the Claim.no at 4 and claim amounts at 400,000
AmountCap = 400000  ## since we are capping and not removing this should be ok to do for freq df also
FreqCap = 4

#capping freq:
Sevtidied3$ClaimNb[Sevtidied3$ClaimNb >= FreqCap] <- FreqCap
#capping claim amount:
Sevtidied3$ClaimAmount[Sevtidied3$ClaimAmount >= AmountCap] <- AmountCap
##Question: If, for example I am capping freq at 4 - do I need to change the "amount" cell for that row, I presume
##not for now, while working in frequency. 
##If capping the amount do I need to change frequency - again I presume not since severity isn't really a factor here







#Extra Cleaning:
#severity
#exposure: 


plot(Sevtidied3$Exposure)
max(Sevtidied3$Exposure)
min(Sevtidied3$Exposure)
quantile(Sevtidied3$Exposure, probs = c(.05 , .99))
##need to get rid of anything above 1 (top 1% as data error)

#Looking at if there is anything weird about exposure > 1
Sevtidied3$ExposureFlag <- ifelse(Sevtidied3$Exposure>1 , 1 ,0)
glm_exposure_model <- glm(ExposureFlag ~ ClaimAmount+ClaimNb+VehPower+VehAge+DrivAge+BonusMalus+VehBrand+Area+Region+Gender+Transmission+Employment+Fines, data = Sevtidied3, family = binomial(link = "logit"))
summary(glm_exposure_model)



ExposureData = Sevtidied3[Sevtidied3$Exposure > 1,]
par(mfrow=c(2,1))
plot(ExposureData$Region , ExposureData$Exposure)
plot(Sevtidied3$Region , Sevtidied3$Exposure)

# Fit the GLM
exposureglm <- glm(Exposure ~ ., data = ExposureData, family = gaussian())

# Check results
summary(exposureglm)




Sevtidied4 = Sevtidied3[Sevtidied3$Exposure <= 1,]
Freqtidied4 = Freqtidied3[Freqtidied3$Exposure <= 1,]


#grouping
Sevtidied4
par(mfrow = c(1, 2))
par(mar = c(5, 4, 4, 2) + 0.1)
plot(Sevtidied4$VehAge, Sevtidied4$ClaimAmount)
plot(Sevtidied4$DrivAge,Sevtidied4$ClaimNb)



plot(Freqtidied4$DrivAge, Freqtidied4$ClaimAmount)
plot(Freqtidied4$DrivAge,Freqtidied4$ClaimNb)

length(Freqtidied4$ClaimNb)


##getting rid of all zero values:
Sevtidied4 = Sevtidied4[Sevtidied4$ClaimAmount > 0,]

#Sev tided data is much smaller because we took out all values where
#claim amount  = 0









#final tables:

Sevtidied4
Freqtidied4

#Variable Groupings

#install.packages('dplyr')
library(dplyr)

Sevtidied5 <- Sevtidied4 %>%
  mutate(
    DriverAge = cut(DrivAge, breaks = c(18, 21, 26, 31, 41, 51, 61, 71, Inf),
                    labels = c("18-21", "21-26", "26-31", "31-41", "41-51", "51-61", "61-71", "71+"),
                    right = TRUE, include.lowest = TRUE),
    VehicleAge = cut(VehAge, breaks = c(0,1,10,Inf),
                     labels = c("0-1", "1-10", "10+"),
                     right = TRUE, include.lowest = TRUE),
    VehiclePower = cut(VehPower, breaks = c(4,5,6,7,8,9,Inf),
                       labels = c("4", "5", "6", "7", "8", "9+"),
                       right = TRUE, include.lowest = TRUE)
  ) %>%
  select(-c(DrivAge, VehAge, VehPower))


Freqtidied5 <- Freqtidied4 %>%
  mutate(
    DriverAge = cut(DrivAge, breaks = c(18, 21, 26, 31, 41, 51, 61, 71, Inf),
                    labels = c("18-21", "21-26", "26-31", "31-41", "41-51", "51-61", "61-71", "71+"),
                    right = TRUE, include.lowest = TRUE),
    VehicleAge = cut(VehAge, breaks = c(0,1,10,Inf),
                     labels = c("0-1", "1-10", "10+"),
                     right = TRUE, include.lowest = TRUE),
    VehiclePower = cut(VehPower, breaks = c(4,5,6,7,8,9,Inf),
                       labels = c("4", "5", "6", "7", "8", "9+"),
                       right = TRUE, include.lowest = TRUE)
  ) %>%
  select(-c(DrivAge, VehAge, VehPower))

Freqtidied5

# Grouping stuff needs to be checked!!!!!!
# 
# 
# # 1. Convert Area to integer
# Freqtidied4$AreaGLM <- as.factor(Freqtidied4$Area)
# 
# # 2. VehPower: Categorical, merging vehicle power groups >= 9 into one class
# Freqtidied4$VehPowerGLM <- as.factor(pmin(Freqtidied4$VehPower, 9))
# 
# # 3. VehAge: Creating 3 categorical classes [0, 1), [1, 10], (10, ???)
# VehAgeGLM <- cbind(0:110, c(1, rep(2, 10), rep(3, 100)))
# Freqtidied4$VehAgeGLM <- as.factor(VehAgeGLM[Freqtidied4$VehAge + 1, 2])
# Freqtidied4$VehAgeGLM <- relevel(Freqtidied4$VehAgeGLM, ref = '2')  # Reference level: [1, 10]
# 
# # 4. DrivAge: Creating 7 categorical classes
# DrivAgeGLM <- cbind(18:100, c(rep(1, 3),   # [18, 21)
#                               rep(2, 5),   # [21, 26)
#                               rep(3, 5),   # [26, 31)
#                               rep(4, 10),  # [31, 41)
#                               rep(5, 10),  # [41, 51)
#                               rep(6, 20),  # [51, 71)
#                               rep(7, 30))) # [71, ???)
# 
# # Assign the category to DrivAge
# Freqtidied4$DrivAgeGLM <- as.factor(DrivAgeGLM[Freqtidied4$DrivAge - 17, 2])
# 
# # Setting reference level for DrivAge to [41, 51) (category 5)
# Freqtidied4$DrivAgeGLM <- relevel(Freqtidied4$DrivAgeGLM, ref = "5")
# 
# # 5. BonusMalus: Continuous log-linear, capped at 150
# Freqtidied4$BonusMalusGLM <- as.integer(pmin(Freqtidied4$BonusMalus, 150))
# 
# # 6. VehBrand: Categorical with 11 classes
# Freqtidied4$VehBrand <- as.factor(Freqtidied4$VehBrand)
# 
# # 7. VehGas: Binary feature component (already binary assumed)
# Freqtidied4$VehGas <- as.factor(Freqtidied4$VehGas)
# 
# # 8. Density: Log-density as continuous log-linear
# Freqtidied4$DensityGLM <- as.numeric(log(Freqtidied4$Density))
# 
# # 9. Region: Categorical with 22 classes, setting reference level to "Centre"
# Freqtidied4$Region <- as.factor(Freqtidied4$Region)
# Freqtidied4$Region <- relevel(Freqtidied4$Region, ref = "Centre")
# #10
# Freqtidied4$Gender <- as.factor(Freqtidied4$Gender)
# 
# #11 employment??
# 
# #12
# Freqtidied4$Fines <- as.numeric(Freqtidied4$Fines)


#Subset: Freq
subset_size <- 0.6 * nrow(Freqtidied5)  # 60% of the total rows

# Randomly sample row indices
set.seed(1)  # Set a seed for reproducibility
random_indices <- sample(1:nrow(Freqtidied5), size = subset_size)

# Create the random subset
Train_data_freq <- Freqtidied5[random_indices, ]
Test_data_freq <- Freqtidied5[-random_indices, ]

#Subset: Freq
subset_size <- 0.6 * nrow(Sevtidied5)  # 60% of the total rows

# Randomly sample row indices
set.seed(1)  # Set a seed for reproducibility
random_indices <- sample(1:nrow(Sevtidied5), size = subset_size)

# Create the random subset
Train_data_sev <- Sevtidied5[random_indices, ]
Test_data_sev <- Sevtidied5[-random_indices, ]

Train_data_sev

#All possible models code:
#Working with Train_data_sev
#Getting error
X=names(Train_data_sev)
X

drop <- c("PolicyID","ClaimNb","Exposure")
X = X[!X %in% drop]
X

out <- unlist(lapply(1:7, function(n) {
  # get combinations
  combinations <- t(combn(X,n))
  # collapse them into usable formulas:
  formulas <- apply(combinations, 1, 
                    function(row) paste0("ClaimNb ~ offset(log(Exposure))+", paste0(row, collapse = "+")))
  
}))

out

models <- lapply(out, function(formula) {
  glm(as.formula(formula), family = poisson, data=Train_data_sev)
})

AIC_values<-rep(0,length(out))

for(i in 1:length(out)){
  AIC_values[i]=models[[i]]$aic
}

AIC_Table<-cbind(out,AIC_values)

AIC_Table[which.min(AIC_values),]


# 
# 
# quasi_poisson_glm <- glm(ClaimNb ~ AreaGLM+ VehPowerGLM + VehAgeGLM + DrivAgeGLM + Fines + Employment + Gender +
#                            BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region+ offset(log(Exposure)),
#                          family = quasipoisson(link = "log"),
#                          data = Train_data)
# summary(quasi_poisson_glm)
# PredictedQPFREQ = predict(quasi_poisson_glm, newdata = Test_data, Type = 'response')
# summary(PredictedQPFREQ)
# 
# 
# 




Train_data_sev

#Applying gamma:
reg.gamma <- glm(ClaimAmount~VehAge+VehGas,family=Gamma(link="log"), data=Train_data_sev)
summary(reg.gamma)





##Applying log normal:

#Taking out where claim number =0
Freqtidied3_nozeros <- Freqtidied3[Freqtidied3$ClaimNb != 0,]
Freqtidied3_nozeros

##Taking out non numeric factors - not sure how to deal with these
numeric_logfreq1 <- Freqtidied3_nozeros[sapply(Freqtidied3_nozeros, is.numeric)]

# Apply log(x + 1) transformation to the numeric data - as ln 0 does not exist
numeric_logfreq2 <- log(numeric_logfreq1 + 1)

library(MASS)

# Fit a log-normal distribution to each numeric column
log_normal_fits <- lapply(numeric_logfreq2, function(x) fitdistr(x, "log-normal"))



# Install and load the required package if needed
rm(list = ls())  # Clears the global environment
getwd()  # Check the current working directory
setwd(getwd())  # Set the working directory


install.packages("MASS")
library(MASS)

# Fit a log-normal distribution to each numeric column
log_normal_fits <- lapply(log_numeric_data, function(x) fitdistr(x, "log-normal"))

# View the fit for the first numeric column
log_normal_fits[[1]]







# Extract only numeric columns
numeric_data_freq <- Freqtidied3_subset[sapply(Freqtidied3_subset, is.numeric)]
numeric_log_freq <- log(numeric_data_logfreq)







# Convert the non-numeric columns to factors
# Convert categorical variables to numeric using factor
Freqtidied3_subset$VehBrand <- as.numeric(factor(Freqtidied3_subset$VehBrand))
Freqtidied3_subset$VehGas <- as.numeric(factor(Freqtidied3_subset$VehGas))
Freqtidied3_subset$Area <- as.numeric(factor(Freqtidied3_subset$Area))
Freqtidied3_subset$Region <- as.numeric(factor(Freqtidied3_subset$Region))
Freqtidied3_subset$Gender <- as.numeric(factor(Freqtidied3_subset$Gender))
Freqtidied3_subset$Transmission <- as.numeric(factor(Freqtidied3_subset$Transmission))
Freqtidied3_subset$Employment <- as.numeric(factor(Freqtidied3_subset$Employment))



# Now you can apply the math function to the entire dataframe
log_freq_data <- log(Freqtidied3_subset)

# Extract only numeric columns
numeric_data <- Freqtidied3_subset[sapply(Freqtidied3_subset, is.numeric)]


# Extract only numeric columns
numeric_data <- Freqtidied3_subset[sapply(Freqtidied3_subset, is.numeric)]



