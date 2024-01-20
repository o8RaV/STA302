# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming your CSV file is named 'your_data_file.csv'
file_path <- "D:/stowage/Courses/3rd year 2nd semester/STA302/STA302-1/Donaldson Paper/inputs/data/Polls Data.csv"

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

# Scatterplot
poll_plot <- ggplot(data, aes(x = BALLOTS_CAST, y = BALLOTS_RECEIVED_BY_VOTERS, color = POLL_RESULT)) +
  geom_point(shape = 19) +
  labs(title = "Scatter Plot", x = "Ballots Cast", y = "Ballots Received by Voters") +
  scale_color_manual(values = c("In Favour" = "blue", "Opposed" = "red"))
print(poll_plot)

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
