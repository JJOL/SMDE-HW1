library(readr)
library(dplyr)
library(ggplot2)

#NOTE: change file location accordingly

airplanes <- read_csv("/home/arixara/Downloads/airplane_price_dataset.csv", col_types = cols(
  
  `Model` = col_factor(),
  `ProductionYear` = col_integer(),
  `NumberofEngines` = col_integer(),
  `EngineType` = col_factor(),
  `Capacity` = col_integer(),
  `Range(km)` = col_integer(),
  `FuelConsumption(L/h)` = col_double(),
  `HourlyMaintenance($)` = col_double(),
  `Age` = col_integer(),
  `SalesRegion` = col_factor(),
  `Price($)` = col_double()
))

#a) Apply PCA analysis on airplane data and interpret the results of the analysis. 

#STEPS OF PCA:

#project data onto eigenvectors
#uncorrelated lower dim data obtained
#Group numeric predictors

#check correlation

anova_result <- aov(`Price($)` ~ `SalesRegion`, data = airplanes)
summary(anova_result)

anova_result <- aov(`Price($)` ~ `EngineType`, data = airplanes)
summary(anova_result)

anova_result <- aov(`Price($)` ~ `ProductionYear`, data = airplanes)
summary(anova_result)

anova_result <- aov(`Price($)` ~ `NumberofEngines`, data = airplanes)
summary(anova_result)

anova_result <- aov(`Price($)` ~ `Capacity`, data = airplanes)
summary(anova_result)

anova_result <- aov(`Price($)` ~ `Range(km)`, data = airplanes)
summary(anova_result)

anova_result <- aov(`Price($)` ~ `FuelConsumption(L/h)`, data = airplanes)
summary(anova_result)

anova_result <- aov(`Price($)` ~ `HourlyMaintenance($)`, data = airplanes)
summary(anova_result)
#just in case

boxplot(`Price($)` ~ `EngineType`, data = airplanes,
col = "lightblue", main = "Price by types of Engine",
ylab = "Price", xlab = "Engine")

numeric_data <- airplanes %>%
  select(`ProductionYear`, `NumberofEngines`, `Capacity`,
         `Range(km)`, `FuelConsumption(L/h)`, `HourlyMaintenance($)`)


# Convert Engine Type to dummy variables (one-hot encoding)
engine_dummies <- model.matrix(~ `EngineType` - 1, data = airplanes)

# Combine with numeric predictors
pca_input <- cbind(numeric_data, engine_dummies)

# 2. Scale the data
scaled_data <- scale(pca_input)

# 3. Perform PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)

# 4. View summary
summary(pca_result)

# 5. Scree plot (proportion & cumulative in one plot if needed)
variances <- pca_result$sdev^2
prop_variance <- variances / sum(variances)
cum_variance <- cumsum(prop_variance)

# Scree plot (proportional)
par(mfrow = c(1, 2))
plot(prop_variance, type = "b", pch = 19,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Scree Plot: Proportions")

#we have a knee

# Cumulative plot
plot(cum_variance, type = "b", pch = 19, col = "blue",
     xlab = "Principal Component",
     ylab = "Cumulative Variance Explained",
     main = "Cumulative Scree Plot")
par(mfrow = c(1, 1))

#surpass the threshold with 3+ variables

# 6. Get the PCA-transformed dataset
pc_df2 <- as.data.frame(pca_result$x)
print(pca_result$rotation)

pc_df2 <- as.data.frame(pca_result$x)
pc_df2$`Price($)` <- airplanes$`Price($)`

model_2pc <- lm(log(`Price($)`) ~ PC1 + PC2, data = pc_df2)
summary(model_2pc)

model_3pc <- lm(log(`Price($)`) ~ PC1 + PC2 + PC3, data = pc_df2)
summary(model_3pc)

#p-value is extra small, looks good

model_4pc <- lm(log(`Price($)`) ~ PC1 + PC2 + PC3 + PC4, data = pc_df2)
summary(model_4pc)

model_5pc <- lm(log(`Price($)`) ~ PC1 + PC2 + PC3 + PC4 + PC5, data = pc_df2)
summary(model_5pc)

residuals <- resid(model_2pc)

hist(residuals,
     breaks = 20,
     main = "Histogram of Residuals (2 PC)",
     xlab = "Residuals",
     col = "skyblue",
     border = "white")
# one peak already!! but its not symmetrical yet 

residuals <- resid(model_3pc)

hist(residuals,
     breaks = 20,
     main = "Histogram of Residuals (3 PC)",
     xlab = "Residuals",
     col = "skyblue",
     border = "white")

residuals <- resid(model_4pc)

hist(residuals,
     breaks = 20,
     main = "Histogram of Residuals (4 PC)",
     xlab = "Residuals",
     col = "skyblue",
     border = "white")

residuals <- resid(model_5pc)

hist(residuals,
     breaks = 20,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "skyblue",
     border = "white")

par(mfrow = c(2, 2))

# Plot 1: Residuals vs Fitted
plot(model_2pc$fitted.values, resid(model_2pc),
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals",
     pch = 19, col = "darkblue")
abline(h = 0, col = "red", lty = 2)

# Plot 2: Normal Q-Q Plot
qqnorm(resid(model_2pc), main = "Normal Q-Q Plot", pch = 19, col = "darkgreen")
qqline(resid(model_2pc), col = "red", lwd = 2)

# Plot 3: Scale-Location (Spread-Location)
sqrt_std_resid <- sqrt(abs(rstandard(model_2pc)))
plot(model_2pc$fitted.values, sqrt_std_resid,
     main = "Scale-Location",
     xlab = "Fitted values",
     ylab = "√|Standardized residuals|",
     pch = 19, col = "purple")
abline(h = 0, col = "red", lty = 2)

# Plot 4: Residuals vs Leverage
plot(hatvalues(model_2pc), rstandard(model_2pc),
     main = "Residuals vs Leverage",
     xlab = "Leverage",
     ylab = "Standardized Residuals",
     pch = 19, col = "brown")
abline(h = 0, col = "red", lty = 2)
# Optional: add Cook's distance lines
cook_cutoff <- 4 / nrow(pc_df2)
abline(h = c(-2, 2), col = "gray", lty = 2)
abline(v = cook_cutoff, col = "orange", lty = 2)

# Reset plotting layout
par(mfrow = c(1, 1))

