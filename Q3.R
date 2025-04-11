# Scatterplot to visualize the potential linear relationship
plot(airplanes$Range.km., airplanes$Price...,
     xlab = "Range (km)",
     ylab = "Price",
     main = "Scatterplot of Range vs Price",
     pch = 20,
     col = "steelblue")

cor(airplanes$Range.km., airplanes$Price..., use = "complete.obs")


model_range <- lm(Price... ~ Range.km., data = airplanes)
summary(model_range)


# Residuals vs Fitted values
plot(model_range$fitted.values, model_range$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted",
     pch = 20,
     col = "darkgreen")
abline(h = 0, col = "red", lty = 2)

# Q-Q plot for residuals
qqnorm(model_range$residuals,
       main = "Normal Q-Q Plot of Residuals")
qqline(model_range$residuals, col = "blue", lwd = 2)

# Shapiro-Wilk normality test
shapiro.test(sample(model_range$residuals, 5000))  # Sample due to size limit

# Fit the log-log regression model
model_log <- lm(log(Price...) ~ log(Range.km.), data = airplanes)
summary(model_log)

# Residual diagnostics for the transformed model
plot(model_log$fitted.values, model_log$residuals,
     xlab = "Fitted values (log)",
     ylab = "Residuals",
     main = "Residuals vs Fitted - Log Model",
     pch = 20,
     col = "darkgreen")
abline(h = 0, col = "red", lty = 2)

qqnorm(model_log$residuals,
       main = "Normal Q-Q Plot - Log Model")
qqline(model_log$residuals, col = "blue", lwd = 2)

# Shapiro-Wilk test for normality of residuals
shapiro.test(sample(model_log$residuals, 5000))

#####
# Seleziona le variabili numeriche
num_vars <- airplanes[, sapply(airplanes, is.numeric)]

# Calcola matrice di correlazione
cor_matrix <- cor(num_vars, use = "complete.obs")

# Visualizza le correlazioni con Price
cor_matrix["Price...", c("Range.km.", "Capacity")]



# Scatterplot: Price vs Range
plot(airplanes$Range.km., airplanes$Price..., 
     main = "Figure 1: Scatterplot of Price vs Range",
     xlab = "Range (km)", ylab = "Price", pch = 20, col = "steelblue")

# Scatterplot: Price vs Capacity
plot(airplanes$Capacity, airplanes$Price..., 
     main = "Figure 2: Scatterplot of Price vs Capacity",
     xlab = "Capacity", ylab = "Price", pch = 20, col = "darkred")

# Correlation between the two independent variables
cor(airplanes$Range.km., airplanes$Capacity, use = "complete.obs")

# Variance Inflation Factor
#install.packages("car")
library(car)
vif(model_vif)


# Fit the multivariate log-log regression model
model_multi <- lm(log(Price...) ~ log(Range.km.) + log(Capacity), data = airplanes)

# Display full summary
summary(model_multi)

# Residuals vs Fitted plot
plot(model_multi$fitted.values, model_multi$residuals,
     xlab = "Fitted values (log scale)",
     ylab = "Residuals",
     main = "Residuals vs Fitted - Multivariate Log Model",
     pch = 20, col = "darkgreen")
abline(h = 0, col = "red", lty = 2)

# Q-Q plot for residuals
qqnorm(model_multi$residuals,
       main = "Normal Q-Q Plot - Multivariate Log Model")
qqline(model_multi$residuals, col = "blue", lwd = 2)

# Shapiro-Wilk normality test (with 5000 residuals sample)
set.seed(42)
shapiro.test(sample(model_multi$residuals, 5000))


summary(model_multi)

#####


# Create a factor variable with three levels: Airbus, Boeing, Other
airplanes$ManufacturerGroup <- ifelse(grepl("Airbus", airplanes$Model), "Airbus",
                                      ifelse(grepl("Boeing", airplanes$Model), "Boeing", "Other"))

# Convert to factor (to ensure correct treatment in lm)
airplanes$ManufacturerGroup <- factor(airplanes$ManufacturerGroup)

# Fit the log-log model with the manufacturer factor included
model_factor <- lm(log(Price...) ~ log(Range.km.) + log(Capacity) + ManufacturerGroup, data = airplanes)

# View summary
summary(model_factor)


####

airplanes$ManufacturerGroup <- ifelse(grepl("Airbus", airplanes$Model), "Airbus",
                                      ifelse(grepl("Boeing", airplanes$Model), "Boeing", "Other"))
airplanes$ManufacturerGroup <- factor(airplanes$ManufacturerGroup)
summary(airplanes$ManufacturerGroup)


model_factor <- lm(log(Price...) ~ log(Range.km.) + log(Capacity) + ManufacturerGroup, data = airplanes)
summary(model_factor)


###

# Residuals vs Fitted
plot(model_factor$fitted.values, model_factor$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted Values (Final Model)",
     pch = 20, col = "darkgreen")
abline(h = 0, col = "red", lty = 2)

# Q-Q plot
qqnorm(model_factor$residuals, main = "Normal Q-Q Plot (Final Model)")
qqline(model_factor$residuals, col = "blue", lwd = 2)

# Shapiro-Wilk test
set.seed(42)
shapiro.test(sample(model_factor$residuals, 5000))

# Durbin-Watson (requires car)
library(car)
durbinWatsonTest(model_factor)
