# Q1 and Q2 of SMDE Group Homework                        
# 

# A. Load dataset and convert variable types -----------------------------
file_path <- dirname(rstudioapi::getActiveDocumentContext()$path) # In R Studio
# file_path <- getSrcDirectory(function(){})[1] # as R source script
setwd(file_path)
rstudioapi::filesPaneNavigate(file_path) # In R Studio
library(readr)
airplanes_df <- read_csv("airplane_price_dataset.csv")

View(airplanes_df)
summary(airplanes_df)

airplanes_df$Model <- as.factor(airplanes_df$Model)
airplanes_df$NumberofEngines <- as.factor(airplanes_df$NumberofEngines)
airplanes_df$EngineType <- as.factor(airplanes_df$EngineType)
airplanes_df$SalesRegion <- as.factor(airplanes_df$SalesRegion)

summary(airplanes_df)

# B. Summarize Price and Fuel Consumption ----
# B.1 In all Data
print("= ALL ===================================================")
summary(airplanes_df$`Price($)`)
summary(airplanes_df$`FuelConsumption(L/h)`)
print("= ALL ===================================================")


# B.2 Per EngineType
piston_df = airplanes_df[airplanes_df$EngineType == 'Piston',]
turbofan_df = airplanes_df[airplanes_df$EngineType == 'Turbofan',]
# Sum of rows in separate dataframes should be the same as original dataframe
cat("|piston_df| + |turbofan_df| =", nrow(piston_df) + nrow(turbofan_df))
cat("|airplanes_df| =", nrow(airplanes_df))

print("= piston_df ===================================================")
summary(piston_df$`Price($)`)
summary(piston_df$`FuelConsumption(L/h)`)
print("= piston_df ===================================================")

print("= turbofan_df ===================================================")
summary(turbofan_df$`Price($)`)
summary(turbofan_df$`FuelConsumption(L/h)`)
print("= turbofan_df ===================================================")

hist()

boxplot(`Price($)`~EngineType, data=airplanes_df)
boxplot(`FuelConsumption(L/h)`~EngineType, data=airplanes_df)

# 2. Make a smaller sample for exercise and final validation -------------
# install.packages("tidyverse")
library(tidyverse)
set.seed(1)
samples_df <- airplanes_df %>%
  sample_n(100)

view(samples_df)

# 3. 

barplot(table(samples_df$NumberofEngines))

columns = names(airplanes_df)
for (col in columns) {
  if (is.factor(airplanes_df$col))
  print(col)
}

n = columns[1]
airplanes_df$n
