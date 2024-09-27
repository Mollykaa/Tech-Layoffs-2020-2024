# Set the working directory
setwd("C:/Users/Acer/Documents/STAT")
getwd()

# Import data tech_layoffs
tech_layoffs <- read.csv("tech_layoffs.csv", header=TRUE)
View(tech_layoffs)

# First Step Details of the Data
# Find Size and Dimensions
dim(tech_layoffs)

# Variable and Columns
str(tech_layoffs)

# Find the data type
sapply(tech_layoffs, class)

# Summary Statistics 
summary(tech_layoffs$Laid_Off)
summary(tech_layoffs$Company_Size_before_Layoffs)

# Temporal Aspect
range(tech_layoffs$Year)

# Second Step Descriptive Statistics 
# Identify the variables
variables <- colnames(tech_layoffs)

# Check data types
data_types <- sapply(tech_layoffs, class)

# Cleaning data
library(dplyr)
class(tech_layoffs$Laid_Off)
class(tech_layoffs$Industry)
class(tech_layoffs$Money_Raised_in_._mil)
class(tech_layoffs$Stage)
class(tech_layoffs$X.)
class(tech_layoffs$Company)
class(tech_layoffs$Location_HQ)
class(tech_layoffs$Country)
class(tech_layoffs$Continent)
class(tech_layoffs$Date_layoffs)
class(tech_layoffs$Percentage)
class(tech_layoffs$Company_Size_before_Layoffs)
Class(tech_layoffs$Company_Size_after_layoffs)
Class(tech_layoffs$Year)

# Describe the data
library(psych)
describe(tech_layoffs$Company_Size_before_Layoffs)

# Check the correlation between company size and layoffs
cor(tech_layoffs$Company_Size_before_Layoffs, tech_layoffs$Laid_Off)

# Pattern of Layoffs by Continents 
library(dplyr)
layoffs_by_continent <- tech_layoffs %>%
  group_by(Continent) %>%
  summarize(Total_Layoffs = sum(Laid_Off))

layoffs_by_continent_year <- tech_layoffs %>%
  group_by(Continent, Year) %>%
  summarize(Total_Layoffs = sum(Laid_Off))

# Identify the area with the highest total layoffs
library(dplyr)
layoffs_by_area <- tech_layoffs %>%
  group_by(Continent) %>%
  summarize(Total_Layoffs = sum(Laid_Off)) %>%
  arrange(desc(Total_Layoffs))

most_layoffs_area <- layoffs_by_area[1, ]

# Analyze the average number of layoffs across company size categories
tech_layoffs %>%
  mutate(Company_Size_Category = cut(Company_Size_before_Layoffs,
                                     breaks = c(0, 100, 1000, Inf),
                                     labels = c("Small", "Medium", "Large"))) %>%
  group_by(Company_Size_Category) %>%
  summarize(Avg_Layoffs = mean(Laid_Off))

# Step Three Visualization
# Identify the Area with Most Layoffs
library(ggplot2)
ggplot(layoffs_by_continent_year, aes(x = Year, y = Total_Layoffs, color = Continent, group = Continent)) +
  geom_line() +
  labs(title = "Layoffs in the Tech Industry (2020-2024) Across Continents",
       x = "Year",
       y = "Total Layoffs")

# Analyze the relationship between Company Size and Layoffs
library(ggplot2)
tech_layoffs <- tech_layoffs %>%
  mutate(Company_Size_before_Layoffs_thousands = Company_Size_before_Layoffs / 1000)

ggplot(tech_layoffs, aes(x = Company_Size_before_Layoffs_thousands, y = Laid_Off)) +
  geom_point() +
  labs(title = "Correlation Between Company Size and Layoffs",
       x = "Company Size Before Layoffs (in Thousands)",
       y = "Number of Layoffs")

# Create a scatter plot with a trend line
tech_layoffs <- tech_layoffs %>%
  mutate(Company_Size_before_Layoffs_thousands = Company_Size_before_Layoffs / 1000)

ggplot(tech_layoffs, aes(x = Company_Size_before_Layoffs_thousands, y = Laid_Off)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trend line
  labs(title = "Scatter Plot with Trend Line",
       x = "Company Size Before Layoffs (in Thousands)",
       y = "Number of Employees Laid Off")

# Use describe() and describeby() on relevant variables
library(psych)  
describe(tech_layoffs$Company_Size_before_Layoffs)

library(ggplot2)

# Create a histogram
ggplot(tech_layoffs, aes(x = Company_Size_before_Layoffs)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black", aes(y = ..density..)) +
  geom_density(alpha = 0.5, fill = "orange") +
  
  # Add skewness and kurtosis annotations
  annotate("text", x = 30000, y = 0.00007, label = paste("Skewness:", round(skewness(tech_layoffs$Company_Size_before_Layoffs), 2)), color = "blue") +
  annotate("text", x = 30000, y = 0.000065, label = paste("Kurtosis:", round(kurtosis(tech_layoffs$Company_Size_before_Layoffs), 2)), color = "red") +
  
  # Labeling
  labs(title = "Distribution of Company Sizes Before Layoffs",
       x = "Company Size Before Layoffs",
       y = "Density") +
  
  theme_minimal()

# Report #3 Methodology
# Find out the hypothesis test for correlation between company size and layoffs
cor_test_result <- cor.test(tech_layoffs$Company_Size_before_Layoffs, tech_layoffs$Laid_Off)
cor_test_result


# Confidence Intervals for Correlation Coefficient
cor_ci <- cor.test(tech_layoffs$Company_Size_before_Layoffs, tech_layoffs$Laid_Off)$conf.int
cor_ci

# T-Test for Mean Differences between Company Size Before and After Layoffs
t_test_result <- t.test(tech_layoffs$Company_Size_before_Layoffs, tech_layoffs$Company_Size_after_layoffs, paired = TRUE)
t_test_result

library(ggplot2)

# Perform paired t-test
t_test_result <- t.test(tech_layoffs$Company_Size_before_Layoffs, tech_layoffs$Company_Size_after_layoffs, paired = TRUE)

# Create a data frame for plotting
plot_data <- data.frame(
  category = c("Before vs After"),
  mean_difference = c(t_test_result$estimate),
  lower_ci = c(t_test_result$conf.int[1]),
  upper_ci = c(t_test_result$conf.int[2])
)

# Create a plot
ggplot(plot_data, aes(x = category, y = mean_difference)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "red") +
  labs(title = "Paired t-test for Mean Differences",
       y = "Mean Difference (After - Before)",
       caption = paste("p-value: <", format(t_test_result$p.value / 10000000000, scientific = TRUE, digits = 2)),
       x = "") +
  theme_minimal()

# Regression Model
tech_layoffs$Company_Size_1000 <- tech_layoffs$Company_Size_before_Layoffs/ 1000
plot(`Laid_Off` ~ Company_Size_1000, data = tech_layoffs,
     xlab = "Company Size Before Layoffs (in thousands)",
     ylab = "Laid Off",
     main = "Layoffs vs Company Size",
     pch = 20,
     cex = 2,
     col = "grey")
abline(tech_layoffs, lwd = 3, col = "darkorange")
coef(tech_layoffs)
residuals <- resid(tech_layoffs)
fitted_values <- fitted(tech_layoffs)
summary(tech_layoffs)

dummy_continent <- as.data.frame(model.matrix(~ Continent - 1, data = tech_layoffs))
tech_layoffs <- cbind(tech_layoffs, dummy_continent)
layoffs_model_continent <- lm(`Laid_Off` ~ ., data = tech_layoffs)
layoffs_model_continent