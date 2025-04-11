# Q1 of SMDE Group Homework                        
# 

# A. Load dataset for models Airbus A320, A350, Boeing 737, Boeing 777 -----------------------------
library(readr)
require(gridExtra)
library(ggplot2)
library(car)
library(tidyverse)
airplanes_df <- read_csv("airplane_price_dataset.csv")
airplanes_df$Model <- as.factor(airplanes_df$Model)
airplanes_df$NumberofEngines <- as.factor(airplanes_df$NumberofEngines)
airplanes_df$EngineType <- as.factor(airplanes_df$EngineType)
airplanes_df$SalesRegion <- as.factor(airplanes_df$SalesRegion)
# Transforming Price to be more legible in figures
airplanes_df$`Price($)` <- airplanes_df$`Price($)` / 1e6
colnames(airplanes_df)[colnames(airplanes_df) == "Price($)"] <- "Price($M)"

summary(airplanes_df)

airplanes_df$`log-Price($M)` <- log10(airplanes_df$`Price($M)`)

airplanes_df <- airplanes_df %>%
  filter(Model %in% c("Airbus A320", "Airbus A350", "Boeing 737", "Boeing 777"))


summary(airplanes_df)

# Check Prices for whole samples
p <- ggplot(airplanes_df, aes(x=`log-Price($M)`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = 3) +
  # geom_density(aes(y = ..count.. * 3), alpha=.2, fill="blue") +
  ggtitle("Piston") +
  theme_classic()
p

# Check Prices for Airbus 320 samples
p1 <- ggplot(airplanes_df[airplanes_df$Model == 'Airbus A320',], aes(x=`log-Price($M)`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = 3) +
  # geom_density(aes(y = ..count.. * 3), alpha=.2, fill="blue") +
  ggtitle("Airbus A320") +
  theme_classic()


# Check Prices for whole samples
p2 <- ggplot(airplanes_df[airplanes_df$Model == 'Airbus A350',], aes(x=`log-Price($M)`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = 5) +
  # geom_density(aes(y = ..count.. * 3), alpha=.2, fill="blue") +
  ggtitle("Airbus A350") +
  theme_classic()


# Check Prices for whole samples
p3 <- ggplot(airplanes_df[airplanes_df$Model == 'Boeing 737',], aes(x=`log-Price($M)`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = 3) +
  # geom_density(aes(y = ..count.. * 3), alpha=.2, fill="blue") +
  ggtitle("Boeing 737") +
  theme_classic()

# Check Prices for whole samples
p4 <- ggplot(airplanes_df[airplanes_df$Model == 'Boeing 777',], aes(x=`log-Price($M)`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = 6) +
  # geom_density(aes(y = ..count.. * 3), alpha=.2, fill="blue") +
  ggtitle("Boeing 777") +
  theme_classic()

grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)
