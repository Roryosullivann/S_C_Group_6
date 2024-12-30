#Data:
setwd(getwd())

CONTRACTS=read.csv("Motor Liability Claims Data - Frequency.csv",header=TRUE,stringsAsFactors=T)
CLAIMS=read.csv("Motor Liability Claims Data - Severity.csv",header=TRUE,stringsAsFactors=T)


#Step 1: Merged DF:
merged_df <- merge(CLAIMS, CONTRACTS, by = "IDpol", all = TRUE)


#Step2: Missing Values

#i. Claim Amount present but no claim number - Remove
claimno <- merged_df[is.na(merged_df$ClaimNb) == TRUE, ]
merged_df2 <- merged_df[is.na(merged_df$ClaimNb) == FALSE, ]  #(should be 195 rows gone)


#ii.  Claim Nb > 1 , but Claim amount = NA - Remove
merged_df3 <- merged_df2[!(is.na(merged_df2$ClaimAmount) & merged_df2$ClaimNb >= 1), ]   #(should be 9116 rows gone)

#iii. Setting Claim Amount = 0 if Claim nb = 0 (for simplicity)
merged_df3$ClaimAmount[is.na(merged_df3$ClaimAmount) & merged_df3$ClaimNb == 0] <- 0


#iv. Check if there are any missing values remaining:
#checking for NA
rows_with_na <- apply(merged_df3, 1, function(row) any(is.na(row)))
rows_with_na <- merged_df3[rowSums(is.na(merged_df3)) > 0, ]
rows_with_na



#Step 3: Outliers
AmountCap = 40000  
FreqCap = 4
merged_df3$ClaimNb[merged_df3$ClaimNb >= FreqCap] <- FreqCap
merged_df3$ClaimAmount[merged_df3$ClaimAmount >= AmountCap] <- AmountCap

#Step 4: Grouping
#install.packages('dplyr')
library(dplyr)

merged_df4 <- merged_df3 %>%
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

#Step 5: Data Errors (Exposure)

#i. Getting rid of high exposure:
merged_df5 = merged_df4[merged_df4$Exposure <= 1,]

#i. Getting rid of low exposure: (check this)
#Also getting rid of low exposure values  (check this with people)
merged_df6 = merged_df5[merged_df5$Exposure >= 0.01,]


#Step 6: Split into Frequency and Severity tables
Severity = merged_df6
Frequency = merged_df6

#Step 7: Removing entries of no claims in Severity
Severity2 = Severity[Severity$ClaimAmount > 0,]


#Step 8: Spikes for Severity
#this table shows claim amounts and the frequency of that particular claim amount:

#To observe the amount of claims reported for each claim amount
freq <- table(Severity2$ClaimAmount)
freq_df <- as.data.frame(freq)
colnames(freq_df) <- c("Claim_Amount", "Frequency")

#Picking out 'Spikes'
amountfreqcap = 2000
freq_df$Claim_Amount <- as.numeric(as.character(freq_df$Claim_Amount))
spikes = freq_df[freq_df$Freq > amountfreqcap & freq_df$Claim_Amount >0 ,]
spikes

#Removing entries if the claim amount of one of the spikes:
Severity3 <- Severity2[!(Severity2$ClaimAmount %in% spikes$Claim_Amount), ]

#Check:
nrow(Severity2) - nrow(Severity3)
#n of rows that should be deleted:
sum(spikes$Frequency) #9908




#Step 9: splitting into train and test data:
#i. Frequency

#a/ Train and Test data: (60:40)
subset_size <- 0.6 * nrow(Frequency)  # 60% of the total rows
set.seed(1)  # Set a seed for reproducibility
random_indices <- sample(1:nrow(Frequency), size = subset_size)
Train_data_freq <- Frequency[random_indices, ]
Test_data_freq <- Frequency[-random_indices, ]

#b/ Spliting Test data in half: (result: 60:20:20 split)
random_indices2 <- sample(1:nrow(Test_data_freq), 0.2 * nrow(Frequency))
FreqTest1 = Test_data_freq[random_indices2, ]
FreqTest2 = Test_data_freq[-random_indices2, ]


#ii. Severity
#a/ Train and Test data: (60:40)
subset_size <- 0.6 * nrow(Severity3)  # 60% of the total rows
set.seed(1)  # Set a seed for reproducibility
random_indices <- sample(1:nrow(Severity3), size = subset_size)
Train_data_sev <- Severity3[random_indices, ]
Test_data_sev <- Severity3[-random_indices, ]

#b/ Spliting Test data in half: (result: 60:20:20 split)
random_indices2 <- sample(1:nrow(Test_data_sev), 0.2 * nrow(Severity3))
SevTest1 = Test_data_sev[random_indices2, ]
SevTest2 = Test_data_sev[-random_indices2, ]
