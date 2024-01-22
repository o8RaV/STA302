# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming your CSV file is named 'your_data_file.csv'
file_path <- "Donaldson Paper/inputs/data/Polls Data.csv"

# Read the CSV file
data <- read.csv(file_path)

# Display structure of the data
str(data)

# Summary statistics for numeric variables
summary_stats <- data %>%
  summarize(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE))
print("Summary Statistics:")
print(summary_stats)

# Mean and standard deviation for numeric variables
mean_sd_values <- data %>%
  summarise(across(where(is.numeric), list(Mean = mean, SD = sd), na.rm = TRUE))
print("Mean and Standard Deviation:")
print(mean_sd_values)

# Ward-wise Analysis - Bar Chart
ward_turnout <- data %>%
  group_by(POLL_CD) %>%
  summarise(turnout_percentage = mean(BALLOTS_CAST / POTENTIAL_VOTERS) * 100)

ggplot(ward_turnout, aes(x = POLL_CD, y = turnout_percentage)) +
  geom_bar(stat = "identity", color = "red") +
  ggtitle("Ward-wise Voter Turnout") +
  xlab("Ward") +
  ylab("Turnout Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(ward_turnout)

# Subdivision Placement Impact - Scatter Plot
scatter_plot_subdivision <- ggplot(data, aes(x = ADDRESS, y = BALLOTS_CAST / POTENTIAL_VOTERS * 100)) +
  geom_point() +
  labs(title = "Subdivision Placement Impact on Voter Turnout", x = "Subdivision Placement", y = "Voter Turnout (%)")

print(scatter_plot_subdivision)

# Scatter plot with histograms
poll_plot_with_histograms <- ggplot(data, aes(x = BALLOTS_CAST, y = BALLOTS_RECEIVED_BY_VOTERS, color = POLL_RESULT)) +
  geom_point(shape = 19) +
  labs(title = "Scatterplot of Ballots Cast vs Received by Voters", x = "Ballots Cast", y = "Ballots Received by Voters") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("In Favour" = "blue", "Opposed" = "red")) +
  geom_bin2d(bins = 20) +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))
print(poll_plot_with_histograms)

# Scatterplot with regression line
poll_plot_with_regression <- ggplot(data, aes(x = BALLOTS_CAST, y = BALLOTS_RECEIVED_BY_VOTERS, color = POLL_RESULT)) +
  geom_point(shape = 19) +
  labs(title = "Scatterplot of Ballots Cast vs Received by Voters with Regression Line", x = "Ballots Cast", y = "Ballots Received by Voters") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("In Favour" = "blue", "Opposed" = "red")) +
  geom_smooth(method = "lm", se = FALSE, color = "green")
print(poll_plot_with_regression)

# Residual analysis using dplyr and ggplot2
linear_model <- lm(BALLOTS_RECEIVED_BY_VOTERS ~ BALLOTS_CAST, data = data)

# Create a data frame with residuals
residual_data <- data %>%
  mutate(Residuals = residuals(linear_model))

# Histogram of residuals
histogram <- ggplot(residual_data, aes(x = Residuals)) +
  geom_histogram(binwidth = 10, fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residual Values", y = "Frequency")
print(histogram)

# Boxplot of residuals
boxplot <- ggplot(residual_data, aes(y = Residuals)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Residuals", x = "", y = "Residual Values")
print(boxplot)

# QQ plot of residuals
qq_plot <- ggplot(residual_data, aes(sample = Residuals)) +
  stat_qq() +
  labs(title = "Normal Probability Plot of Residuals")
print(qq_plot)

# Residuals vs. Ballots Cast
residuals_vs_cast <- ggplot(residual_data, aes(x = BALLOTS_CAST, y = Residuals)) +
  geom_point(shape = 19) +
  labs(title = "Residuals vs. Ballots Cast", x = "Ballots Cast", y = "Residual Values") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
print(residuals_vs_cast)

# Set up a 2 by 2 layout
par(mfrow = c(2, 2))

# Plot 1: Histogram of residuals
hist(residual_data$Residuals, main = "Histogram of Residuals", xlab = "Residual Values")

# Plot 2: Boxplot of residuals
boxplot(residual_data$Residuals, main = "Boxplot of Residuals", ylab = "Residual Values")

# Plot 3: QQ plot of residuals
qqnorm(residual_data$Residuals)
qqline(residual_data$Residuals, col = "red")
title(main = "Normal Probability Plot of Residuals")

# Plot 4: Residuals vs. Ballots Cast
plot(residual_data$BALLOTS_CAST, residual_data$Residuals,
     main = "Residuals vs. Ballots Cast", xlab = "Ballots Cast", ylab = "Residual Values")
abline(h = 0, col = "red", lty = 2)

# Reset the plotting layout
par(mfrow = c(1, 1))
