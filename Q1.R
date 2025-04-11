# Q1 and Q2 of SMDE Group Homework                        
# 

# A. Load dataset and convert variable types -----------------------------
file_path <- dirname(rstudioapi::getActiveDocumentContext()$path) # In R Studio
# file_path <- getSrcDirectory(function(){})[1] # as R source script
setwd(file_path)
rstudioapi::filesPaneNavigate(file_path) # In R Studio
library(readr)
airplanes_df <- read_csv("airplane_price_dataset.csv")

# Transform Attributes to Correct Types
airplanes_df$Model <- as.factor(airplanes_df$Model)
airplanes_df$NumberofEngines <- as.factor(airplanes_df$NumberofEngines)
airplanes_df$EngineType <- as.factor(airplanes_df$EngineType)
airplanes_df$SalesRegion <- as.factor(airplanes_df$SalesRegion)
# Transforming Price to be more legible in figures
airplanes_df$`Price($)` <- airplanes_df$`Price($)` / 1e6
colnames(airplanes_df)[colnames(airplanes_df) == "Price($)"] <- "Price($M)"

summary(airplanes_df)


# B. Summarize Price and Fuel Consumption ----
# B.1 In all Data
print("= ALL ===================================================")
summary(airplanes_df$`Price($M)`)
summary(airplanes_df$`FuelConsumption(L/h)`)
print("= ALL ===================================================")

# B.2 Per EngineType
piston_df = airplanes_df[airplanes_df$EngineType == 'Piston',]
turbofan_df = airplanes_df[airplanes_df$EngineType == 'Turbofan',]
# Sum of rows in separate dataframes should be the same as original dataframe
cat("|piston_df| + |turbofan_df| =", nrow(piston_df) + nrow(turbofan_df))
cat("|airplanes_df| =", nrow(airplanes_df))

print("= piston_df ===================================================")
summary(piston_df$`Price($M)`)
summary(piston_df$`FuelConsumption(L/h)`)
print("= piston_df ===================================================")

print("= turbofan_df ===================================================")
summary(turbofan_df$`Price($M)`)
summary(turbofan_df$`FuelConsumption(L/h)`)
print("= turbofan_df ===================================================")


# C. Test Fuel Consumption Affected by Engine Type ----

# take 2000 turbofan samples to do a fair comparison and shapiro tests can 
# only do up to 3000 observations
library(tidyverse)
set.seed(2)
s_turbofan_df <- turbofan_df %>%
  sample_n(2000)


# Try to transform Fuel Consumption to do Hypothesis Testing on Engine Type Effect
s_turbofan_df$`log(FuelConsumption(L/h))` <- log10(s_turbofan_df$`FuelConsumption(L/h)`)
s_turbofan_df$`sqrt(FuelConsumption(L/h))` <- sqrt(s_turbofan_df$`FuelConsumption(L/h)`)
s_turbofan_df$`recip(FuelConsumption(L/h))` <- 1.0/(s_turbofan_df$`FuelConsumption(L/h)`)

piston_df$`log(FuelConsumption(L/h))` <- log10(piston_df$`FuelConsumption(L/h)`)
piston_df$`sqrt(FuelConsumption(L/h))` <- sqrt(piston_df$`FuelConsumption(L/h)`)
piston_df$`recip(FuelConsumption(L/h))` <- 1.0/(piston_df$`FuelConsumption(L/h)`)

# Plot Fuel Consumptions
require(gridExtra)
library(ggplot2)

# op <- par(mfrow=c(1,2))
# hist(piston_df$`FuelConsumption(L/h)`)
# hist(turbofan_df$`FuelConsumption(L/h)`)
# par(op)

p <- ggplot(airplanes_df, aes(x=EngineType, y=`FuelConsumption(L/h)`)) +
  geom_boxplot(fill="#40f585", color="black") +
  theme_classic()
p

num_bins = 27
xrange = max(piston_df$`FuelConsumption(L/h)`) - min(piston_df$`FuelConsumption(L/h)`)
bin_width_1 = xrange / num_bins
piston_p <- ggplot(piston_df, aes(x=`FuelConsumption(L/h)`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = bin_width_1) +
  geom_density(aes(y = ..count.. * bin_width_1), alpha=.2, fill="blue") +
  ggtitle("Piston") +
  theme_classic()
xrange = max(s_turbofan_df$`FuelConsumption(L/h)`) - min(s_turbofan_df$`FuelConsumption(L/h)`)
bin_width_2 = xrange / num_bins
turbofan_p <- ggplot(s_turbofan_df, aes(x=`FuelConsumption(L/h)`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = bin_width_2) +
  geom_density(aes(y = ..count.. * bin_width_2), alpha=.2, fill="blue") +
  ggtitle("Turbofan") +
  theme_classic()
grid.arrange(piston_p, turbofan_p, ncol=2)

# Do Assumptions Verification

# Normality
?shapiro.test
shapiro.test(s_turbofan_df$`FuelConsumption(L/h)`)
shapiro.test(piston_df$`FuelConsumption(L/h)`)

# For piston
ggplot(piston_df, aes(sample = `FuelConsumption(L/h)`)) +
  stat_qq() + stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot: Piston Fuel Consumption")

ggplot(s_turbofan_df, aes(sample = `FuelConsumption(L/h)`)) +
  stat_qq() + stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot: s_turbofan_df Fuel Consumption")


# Homogenity of Variance
var_test <- var.test(piston_df$`FuelConsumption(L/h)`, s_turbofan_df$`FuelConsumption(L/h)`)
cat("F-test for Equal Variances:\n")
cat("F =", var_test$statistic, ", p-value =", var_test$p.value, "\n\n")
var_test

library(car)
fuel <- c(piston_df$`FuelConsumption(L/h)`, s_turbofan_df$`FuelConsumption(L/h)`)
group <- factor(c(rep("Piston", nrow(piston_df)), rep("Turbofan", nrow(s_turbofan_df))))
leveneTest(fuel ~ group)
# Do Hypothesis Testing Summary

# Welch's t-test, safer when variances are unequal and distributions are non-normal
t.test(piston_df$`FuelConsumption(L/h)`, s_turbofan_df$`FuelConsumption(L/h)`, var.equal = FALSE)
wilcox.test(piston_df$`FuelConsumption(L/h)`, s_turbofan_df$`FuelConsumption(L/h)`)
# D. Make 95% confidence intervals ----

print("-----95% mean confidence interval for Piston FuelConsumption-----")
interval <- round(t.test(piston_df$`FuelConsumption(L/h)`)$conf.int, 2)
interval
print("-----95% mean confidence interval for TurboFan FuelConsumption-----")
interval <- round(t.test(s_turbofan_df$`FuelConsumption(L/h)`)$conf.int, 2)
interval



# E. Check association Model and Sales Region ----

summary(airplanes_df)

# Check a Cross Table to see any first pattern
table <- table(airplanes_df$Model, airplanes_df$SalesRegion)
pt<-prop.table(table, margin=2)
pt

# Try running a quick chi squared test to see if there is a strong relationship
chisq.test(table)

# Plot Normal Bar Charts
model_p <- ggplot(airplanes_df, aes(x=Model)) +
  geom_bar(stat="count", fill="#40f585", color="black") +
  ggtitle("Model") +
  theme_classic()
region_p <- ggplot(airplanes_df, aes(x=SalesRegion)) +
  geom_bar(stat="count", fill="#40f585", color="black") +
  ggtitle("Region") +
  theme_classic()
grid.arrange(model_p, region_p, ncol=2)

# Plot Proportion Stacked Bar Charts of Model and Region
library(dplyr)
model_order <- airplanes_df %>%
  count(Model) %>%
  arrange(desc(n)) %>%
  pull(Model)
airplanes_df$Model <- factor(airplanes_df$Model, levels = model_order)
model_p <- ggplot(airplanes_df, aes(x = Model, fill = SalesRegion)) +
  geom_bar(position = "fill") +
  labs(title = "Model by SalesRegion", x = "Model", y = "SalesRegion")
region_order <- airplanes_df %>%
  count(SalesRegion) %>%
  arrange(desc(n)) %>%
  pull(SalesRegion)
airplanes_df$SalesRegion <- factor(airplanes_df$SalesRegion, levels = region_order)
region_p <- ggplot(airplanes_df, aes(x = SalesRegion, fill = Model)) +
  geom_bar(position = "fill") +
  labs(title = "SalesRegion by Model", x = "SalesRegion", y = "Model")
grid.arrange(model_p, region_p, ncol=2)

# F. Consider only Bombadier CRJ200 and CESSNA 172 models ----
filtered_df <- airplanes_df %>%
  filter(Model %in% c("Bombardier CRJ200", "Cessna 172"))

# G. Check different distributions of price ----
piston_df = filtered_df[filtered_df$EngineType == 'Piston',]
turbofan_df = filtered_df[filtered_df$EngineType == 'Turbofan',]

num_bins = 27
xrange = max(piston_df$`Price($M)`) - min(piston_df$`Price($M)`)
bin_width_1 = xrange / num_bins
piston_p_1 <- ggplot(piston_df, aes(x=`Price($M)`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = bin_width_1) +
  geom_density(aes(y = ..count.. * bin_width_1), alpha=.2, fill="blue") +
  ggtitle("Piston") +
  theme_classic()
xrange = max(turbofan_df$`Price($M)`) - min(turbofan_df$`Price($M)`)
bin_width_2 = xrange / num_bins
turbofan_p_1 <- ggplot(turbofan_df, aes(x=`Price($M)`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = bin_width_2) +
  geom_density(aes(y = ..count.. * bin_width_2), alpha=.2, fill="blue") +
  ggtitle("Turbofan") +
  theme_classic()
# grid.arrange(piston_p, turbofan_p, ncol=2)

turbofan_df$`log(Price($M))` <- log(turbofan_df$`Price($M)`, 2)
turbofan_df$`sqrt(Price($M))` <- sqrt(turbofan_df$`Price($M)`)
piston_df$`log(Price($M))` <- logb(piston_df$`Price($M)`, 2)
piston_df$`sqrt(Price($M))` <- sqrt(piston_df$`Price($M)`)

num_bins = 27
xrange = max(piston_df$`log(Price($M))`) - min(piston_df$`log(Price($M))`)
bin_width_3 = xrange / num_bins
piston_p_2 <- ggplot(piston_df, aes(x=`log(Price($M))`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = bin_width_3) +
  geom_density(aes(y = ..count.. * bin_width_3), alpha=.2, fill="blue") +
  ggtitle("log-Piston") +
  theme_classic()
xrange = max(turbofan_df$`log(Price($M))`) - min(turbofan_df$`log(Price($M))`)
bin_width_4 = xrange / num_bins
turbofan_p_2 <- ggplot(turbofan_df, aes(x=`log(Price($M))`)) +
  geom_histogram(fill="#40f585", color="black", aes(y=..count..), binwidth = bin_width_4) +
  geom_density(aes(y = ..count.. * bin_width_4), alpha=.2, fill="blue") +
  ggtitle("log-Turbofan") +
  theme_classic()
grid.arrange(piston_p_1, turbofan_p_1, piston_p_2, turbofan_p_2, ncol=2, nrow=2)


ggplot(filtered_df, aes(x = `Price($M)`, fill = EngineType)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Price Distribution by Engine Type", x = "Price($M)", y = "Count") +
  theme_minimal()

# H. Categorize Price into Low/High by cutting from the median ----
# Cut for turbofan
data_df <- bind_rows(turbofan_df, piston_df)

low <- 0
med <- median(data_df$`Price($M)`)
high <- max(data_df$`Price($M)`) + 1

turbofan_df$Price_cut = cut(turbofan_df$`Price($M)`, c(low, med, high))
levels(turbofan_df$Price_cut)<-c("Low","High")
# Cut for piston
low <- 0
med <- median(piston_df$`Price($M)`)
high <- max(piston_df$`Price($M)`) + 1
piston_df$Price_cut = cut(piston_df$`Price($M)`, c(low, med, high))
levels(piston_df$Price_cut)<-c("Low","High")

# I. Cross classify model and price categories ----
# Plot Proportion Stacked Bar Charts of Model and Region
# model_order <- airplanes_df %>%
#   count(Model) %>%
#   arrange(desc(n)) %>%
#   pull(Model)
# airplanes_df$Model <- factor(airplanes_df$Model, levels = model_order)



model_p <- ggplot(data_df, aes(x = Model, fill = Price_cut)) +
  geom_bar(position = "fill") +
  labs(title = "Price by Model", x = "Model", y = "Price Level")
model_p
# region_order <- airplanes_df %>%
#   count(SalesRegion) %>%
#   arrange(desc(n)) %>%
#   pull(SalesRegion)
# airplanes_df$SalesRegion <- factor(airplanes_df$SalesRegion, levels = region_order)

price_p <- ggplot(data_df, aes(x = Price_cut, fill = Model)) +
  geom_bar(position = "fill") +
  labs(title = "Model by Price Level", x = "Price Level", y = "Model")
price_p
grid.arrange(model_p, price_p, ncol=2)

data_df$Model <- droplevels(data_df$Model)
table <- table(data_df$Model, data_df$Price_cut)
pt<-prop.table(table, margin=1)
pt

# J. Association between model and price level? ----
chisq.test(table)
# K. Cross classify Model and SalesRegion. Association? ----

summary(data_df)
table <- table(data_df$Model, data_df$SalesRegion)
table

pt<-prop.table(table, margin=2)
pt

chisq.test(table)

model_p <- ggplot(data_df, aes(x = Model, fill = SalesRegion)) +
  geom_bar(position = "fill") +
  labs(title = "Sales Region by Model", x = "Model", y = "Sales Region")
model_p
region_p <- ggplot(data_df, aes(x = SalesRegion, fill = Model)) +
  geom_bar(position = "fill") +
  labs(title = "Model by SalesRegion", x = "SalesRegion", y = "Model")
region_p

grid.arrange(model_p, region_p, ncol=2)
