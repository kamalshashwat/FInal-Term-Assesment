data <- read.csv("/Users/kamalshashwat/Desktop/LSA Final Assesment/brazil.csv")
View(data)
install.packages("ggplot2")
library(ggplot2)
install.packages("textreg")
library(textreg)
setwd("/Users/kamalshashwat/Desktop/LSA Midterm Assesment")
#Question 1
#a. municipalities with no data
no_data <- sum(is.na(data$council.age))
print(no_data)
#b. boxplot of the health council age
# Basic boxplot for council.age
boxplot(data$council.age,
        main = "Box Plot of Health Council Age",
        ylab = "Council Age",
        col = "lightblue")
# Optional: Add horizontal line at the median
abline(h = median(data$council.age, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
#c. Interpret the median and mean of the variable corruption
corruption <- mean(data$corruption)
print(corruption)
corruption_median <- median(data$corruption)
print(corruption_median)
#Question 2
#a. liner regression with courruption index as the outcome
install.packages("stargazer")
library(stargazer)
munc <- lm(corruption ~ council.age, data = data)
summary(munc)
#b.
#Question 3
#a. fit a linear regression model adding margin, reelected, poverty
multi <- lm(corruption ~ council.age + margin + reelected + poverty, data = data)
summary(multi)
#d. corruption index score for a municipality health council that is >10 years olrd
# Fit the multiple regression model
simple <- lm(corruption ~ poverty, data=data)
munc_model <- lm(corruption ~ council.age + margin + reelected + poverty, data = data)
summary(munc_model)
stargazer(munc, multi, type = "text",
          title = "Comparison of Linear Regression Models",
          column.labeles = c("Model 1", "Model 2"),
          dep.var.labels = "Courrption Index",
          covariate.labels = c("Council Age", "Margin", "Reelected", "Poverty"),
          out = "results.txt")
summary(multi, munc_model)
# Create a new data frame with the specified values
new_data <- data.frame(
  council.age = 10, # 10 years old council
  margin = 12, # Mayor won by 12 percentage points
  reelected = 1, # Re-elected Mayor (1 = yes)
  poverty = 50 # Poverty level is 50
)
# Predict the corruption index score
predicted_corruption <- predict(munc_model, newdata = new_data)
# Print the predicted corruption index score
predicted_corruption
#Question4
#a.
multi <- lm(corruption ~ council.age + margin + reelected + poverty, data = data)
multi_health <- lm(corruption ~ council.age * reelected + margin + reelected + poverty,
                   data = data)
# Present the models side by side
stargazer(multi, multi_health,
          type = "text",
          column.labels = c("Without Interaction", "With Interaction"),
          dep.var.labels = "Corruption Index",
          title = "Comparison of Models with and without Interaction")
#c.
multi_health <- lm(corruption ~ council.age * reelected + margin + reelected + poverty,
                   data = data)
summary(multi_health)
#e.
# Assuming 'data' is your dataset and the regression model has been fit as follows:
multi_health <- lm(corruption ~ council.age * reelected + margin + poverty, data = data)
# Create a sequence of council ages from 0 to 20 years
council_ages <- seq(0, 20, by = 1)
# Create a data frame for the fitted values, with separate rows for reelected and not
reelected mayors
fitted_values <- data.frame(
  council.age = rep(council_ages, times = 2),
  reelected = rep(c(0, 1), each = length(council_ages)),
  margin = 10,
  poverty = 50
)
# Calculate the fitted values using the predict function
fitted_values$predicted_corruption <- predict(multi_health, newdata = fitted_values)
# Load ggplot2 for visualization
install.packages("ggplot2")
library(ggplot2)
# Plot the fitted values
ggplot(fitted_values, aes(x = council.age, y = predicted_corruption, color =
                            factor(reelected))) +
  geom_line(linewidth = 1.5) +
  labs(
    title = "Fitted Values of Corruption vs. Council Age",
    x = "Council Age (years)",
    y = "Predicted Corruption",
    color = "Reelected Mayor"
  ) +
  scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
  theme_minimal()
#SECTION2:
peace <- read.csv("/Users/kamalshashwat/Downloads/trading.csv")
View(peace)
#Question1
#a.
palestine <- sum(peace$palstock)
print(palestine)
israel <- sum(peace$isrstock)
print(israel)
none <- none <- sum(peace$assettreat == 1 & peace$isrstock == 0 & peace$palstock == 0)
print(none)
subset_data <- subset(peace,!(assettreat == 1 & isrstock == 0 & palstock ==0))
total_individuals <- nrow(subset_data)
print(total_individuals)
treated_data <- subset(subset_data, assettreat == 1)
overall_uptake <- mean(treated_data$asset_comp)
print(overall_uptake)
uptake_irstock <- mean(treated_data$asset_comp[treated_data$isrstock==1])
print(uptake_irstock)
uptake_palstock <- mean(treated_data$asset_comp[treated_data$palstock==1])
print(uptake_palstock)
uptake_difference <- uptake_irstock - uptake_palstock
print(uptake_difference)
took <- mean(peace$assettreat ==1 & peace$tradestock6all==1)
print(took)
took_israel <- mean(peace$assettreat == 1 & peace$isrstock==1)
print(took_israel)
took_palestine <- mean(peace$assettreat == 1 & peace$palstock==1)
print(took_palestine)
diff <- took_palestine - took_israel
print(diff)
# Assuming your data frame is named 'peace'
# Load necessary library
library(ggplot2)
peace_long <- peace_long[is.finite(peace_long$Peace_Index)]
# Create a new data frame in long format for easier plotting
peace_long <- data.frame(
  Year = rep(c("2013", "2015"), each = nrow(peace)),
  Peace_Index = c(peace$p_index_2013, peace$e_index_2015)
)
# Create the boxplot
ggplot(peace_long, aes(x = Year, y = Peace_Index)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Distribution of Peace Index (2013 vs 2015)",
       x = "Year",
       y = "Peace Index") +
  theme_minimal()
#Question 2
peace_clean <- subset_data[!is.na(subset_data$p_index_2015),]
mean_treatment <- mean(peace_clean$p_index_2015[peace_clean$assettreat==1])
print(mean_treatment)
mean_control <- mean(peace_clean$p_index_2015[peace_clean$assettreat==0])
print(mean_control)
diff_in_means <- mean_treatment - mean_control
print(diff_in_means)
# Perform the t-test
t_test_result <- t.test(peace_clean$p_index_2015 ~ peace_clean$assettreat)
# Print the results of the t-test
print(t_test_result)
t-value <- -1.5281
SE <- diff_in_means/t-value
#c
# Perform a t-test to check the balance of political orientation between treatment and
control groups
t_test_left_2013 <- t.test(left_2013 ~ assettreat, data = subset_data)
# Display the results
print(t_test_left_2013)
# Perform a t-test to check the balance of peace index between treatment and control
groups
t_test_p_index_2013 <- t.test(p_index_2013 ~ assettreat, data = subset_data)
# Display the results
print(t_test_p_index_2013)
# Perform a t-test to check the balance of age between treatment and control groups
t_test_age <- t.test(age ~ assettreat, data = subset_data)
# Display the results
print(t_test_age)
# Perform a t-test to check the balance of family income between treatment and control
groups
t_test_faminc <- t.test(faminc ~ assettreat, data = subset_data)
# Display the results
print(t_test_faminc)
#b.
# Calculate standard deviations and sample sizes for the control and treatment groups
sd_control <- sd(peace_clean$p_index_2015[peace_clean$assettreat == 0])
sd_treatment <- sd(peace_clean$p_index_2015[peace_clean$assettreat == 1])
n_control <- sum(peace_clean$assettreat == 0)
n_treatment <- sum(peace_clean$assettreat == 1)
# Calculate the standard error
SE <- sqrt((sd_control^2 / n_control) + (sd_treatment^2 / n_treatment))
print(paste("Standard Error:", SE))
# Calculate the critical t value for 99% confidence level
alpha <- 0.01
df <- n_control + n_treatment - 2 # Degrees of freedom
t_critical <- qt(1 - alpha/2, df = df)
# Calculate the 99% confidence interval
CI_lower <- diff_in_means - t_critical * SE
CI_upper <- diff_in_means + t_critical * SE
# Display the confidence interval
cat("99% Confidence Interval: [", CI_lower, ", ", CI_upper, "]\n")
#Question 3
# Compare the balance of 'right_2013' between treatment and control groups
t_test_right_2013 <- t.test(peace$right_2013 ~ peace$assettreat)
cat("T-test for 'right_2013':\n")
print(t_test_right_2013)
# Compare the balance of 'left_2013' between treatment and control groups
t_test_left_2013 <- t.test(peace$left_2013 ~ peace$assettreat)
cat("T-test for 'left_2013':\n")
print(t_test_left_2013)
# Compare the balance of 'age' between treatment and control groups
t_test_age <- t.test(peace$age ~ peace$assettreat)
cat("T-test for 'age':\n")
print(t_test_age)
#c.
# Subset the data for those who received either Israeli or Palestinian stocks
israel_vs_palestine <- subset(peace, peace$isrstock == 1 | peace$palstock == 1)
# Create a new variable to distinguish between Israeli and Palestinian stocks
israel_vs_palestine$stock_type <- ifelse(israel_vs_palestine$isrstock == 1, "Israeli",
                                         "Palestinian")
# Perform a t-test to compare the Peace Index in 2015 between the two groups
t_test_result <- t.test(p_index_2015 ~ stock_type, data = israel_vs_palestine)
# Print the results of the t-test
print(t_test_result)
#Question 4
hypo <- lm(p_index_2015 ~ assettreat*palstock+p_index_2013, data = subset_data)
summary(hypo)
# Step 1: Create a combined treatment group variable
subset_data$treatment_group <- factor(
  ifelse(subset_data$assettreat == 0, "Control",
         ifelse(subset_data$palstock == 1, "Palestinian_Stock", "Israeli_Stock"))
)
# Step 2: Fit the linear model using the combined treatment group variable
simple_model <- lm(p_index_2015 ~ treatment_group + p_index_2013, data = subset_data)
# Step 3: Summarize the model to see the differences between groups
summary(simple_model)
install.packages("sjPlot")
library(sjPlot)
plot_model(simple_model, type = "est", show.values = TRUE, value.offset = .3)
# Save the plot to a file
ggsave("regression_plot.png", width = 8, height = 6)
confint(simple_model)
#Question5
# Compute the mean Peace Index for each group and time period
mean_pre_treatment <- mean(peace$p_index_2013[peace$assettreat == 1], na.rm = TRUE)
mean_post_treatment <- mean(peace$p_index_2015[peace$assettreat == 1], na.rm = TRUE)
mean_pre_control <- mean(peace$p_index_2013[peace$assettreat == 0], na.rm = TRUE)
mean_post_control <- mean(peace$p_index_2015[peace$assettreat == 0], na.rm = TRUE)
# Calculate the change over time for each group
change_treatment <- mean_post_treatment - mean_pre_treatment
change_control <- mean_post_control - mean_pre_control
# Compute the DiD estimate
DiD_estimate <- change_treatment - change_control
print(DiD_estimate)
# Compute the change in Peace Index for each individual
peace$change <- peace$p_index_2015 - peace$p_index_2013
# Perform a t-test to assess statistical significance of the DiD estimate
t_test_result <- t.test(change ~ assettreat, data = peace)
print(t_test_result)