
# ─────────────────────────────────────────────────────────────────
# Tech Layoffs 2020-2024 | Statistical Analysis
# Author: Vesethmollyka VAR
# Institution: Institut Mines-Telecom Business School
# Course: Application of Statistics
# ─────────────────────────────────────────────────────────────────

# Set your working directory to the folder containing tech_layoffs.csv
# setwd("path/to/your/folder")
getwd()

# ── Import Data ───────────────────────────────────────────────────
tech_layoffs <- read.csv("tech_layoffs.csv", header = TRUE, fileEncoding = "latin1")
View(tech_layoffs)

# ─────────────────────────────────────────────────────────────────
# STEP 1 — DETAILS OF THE DATA
# ─────────────────────────────────────────────────────────────────

# Size and dimensions
dim(tech_layoffs)

# Variable names and structure
str(tech_layoffs)

# Data types for all columns
sapply(tech_layoffs, class)

# Summary statistics
summary(tech_layoffs$Laid_Off)
summary(tech_layoffs$Company_Size_before_Layoffs)

# Temporal range
range(tech_layoffs$Year)

# ─────────────────────────────────────────────────────────────────
# STEP 2 — DESCRIPTIVE STATISTICS
# ─────────────────────────────────────────────────────────────────

# Identify variables
variables  <- colnames(tech_layoffs)
data_types <- sapply(tech_layoffs, class)

# Check data types for each column
library(dplyr)
class(tech_layoffs$Laid_Off)
class(tech_layoffs$Industry)
class(tech_layoffs$Money_Raised_in_$_mil)          # Fixed: correct column name
class(tech_layoffs$Stage)
class(tech_layoffs$X.)                             # '#' column (R reads as X.)
class(tech_layoffs$Company)
class(tech_layoffs$Location_HQ)
class(tech_layoffs$Country)
class(tech_layoffs$Continent)
class(tech_layoffs$Date_layoffs)
class(tech_layoffs$Percentage)
class(tech_layoffs$Company_Size_before_Layoffs)
class(tech_layoffs$Company_Size_after_layoffs)     # Fixed: lowercase 'class'
class(tech_layoffs$Year)                           # Fixed: lowercase 'class'

# Descriptive statistics
library(psych)
describe(tech_layoffs$Company_Size_before_Layoffs)

# Correlation between company size and layoffs
cor(tech_layoffs$Company_Size_before_Layoffs, tech_layoffs$Laid_Off)

# ── Layoffs by Continent ──────────────────────────────────────────
layoffs_by_continent <- tech_layoffs %>%
  group_by(Continent) %>%
  summarize(Total_Layoffs = sum(Laid_Off))

layoffs_by_continent_year <- tech_layoffs %>%
  group_by(Continent, Year) %>%
  summarize(Total_Layoffs = sum(Laid_Off))

# Continent with the highest total layoffs
layoffs_by_area <- tech_layoffs %>%
  group_by(Continent) %>%
  summarize(Total_Layoffs = sum(Laid_Off)) %>%
  arrange(desc(Total_Layoffs))

most_layoffs_area <- layoffs_by_area[1, ]

# Average layoffs by company size category
tech_layoffs %>%
  mutate(Company_Size_Category = cut(Company_Size_before_Layoffs,
                                     breaks = c(0, 100, 1000, Inf),
                                     labels = c("Small", "Medium", "Large"))) %>%
  group_by(Company_Size_Category) %>%
  summarize(Avg_Layoffs = mean(Laid_Off))

# ─────────────────────────────────────────────────────────────────
# STEP 3 — VISUALIZATIONS
# ─────────────────────────────────────────────────────────────────
library(ggplot2)

# Chart 1: Layoff trends across continents
ggplot(layoffs_by_continent_year, aes(x = Year, y = Total_Layoffs, color = Continent, group = Continent)) +
  geom_line() +
  labs(title = "Layoffs in the Tech Industry (2020-2024) Across Continents",
       x = "Year",
       y = "Total Layoffs")

# Chart 2: Company size vs layoffs (scatter)
tech_layoffs <- tech_layoffs %>%
  mutate(Company_Size_before_Layoffs_thousands = Company_Size_before_Layoffs / 1000)

ggplot(tech_layoffs, aes(x = Company_Size_before_Layoffs_thousands, y = Laid_Off)) +
  geom_point() +
  labs(title = "Correlation Between Company Size and Layoffs",
       x = "Company Size Before Layoffs (in Thousands)",
       y = "Number of Layoffs")

# Chart 3: Scatter plot with linear trend line
ggplot(tech_layoffs, aes(x = Company_Size_before_Layoffs_thousands, y = Laid_Off)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot with Trend Line",
       x = "Company Size Before Layoffs (in Thousands)",
       y = "Number of Employees Laid Off")

# Chart 4: Histogram with density overlay
library(psych)
ggplot(tech_layoffs, aes(x = Company_Size_before_Layoffs)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black", aes(y = ..density..)) +
  geom_density(alpha = 0.5, fill = "orange") +
  annotate("text", x = 30000, y = 0.00007,
           label = paste("Skewness:", round(skewness(tech_layoffs$Company_Size_before_Layoffs), 2)),
           color = "blue") +
  annotate("text", x = 30000, y = 0.000065,
           label = paste("Kurtosis:", round(kurtosis(tech_layoffs$Company_Size_before_Layoffs), 2)),
           color = "red") +
  labs(title = "Distribution of Company Sizes Before Layoffs",
       x = "Company Size Before Layoffs",
       y = "Density") +
  theme_minimal()

# ─────────────────────────────────────────────────────────────────
# STEP 4 — HYPOTHESIS TESTING
# ─────────────────────────────────────────────────────────────────

# Correlation test: company size vs layoffs
cor_test_result <- cor.test(tech_layoffs$Company_Size_before_Layoffs, tech_layoffs$Laid_Off)
cor_test_result

# 95% Confidence interval for correlation coefficient
cor_ci <- cor.test(tech_layoffs$Company_Size_before_Layoffs, tech_layoffs$Laid_Off)$conf.int
cor_ci

# Paired t-test: company size before vs after layoffs
t_test_result <- t.test(tech_layoffs$Company_Size_before_Layoffs,
                        tech_layoffs$Company_Size_after_layoffs,
                        paired = TRUE)
t_test_result

# Chart 5: Error bar plot for paired t-test
plot_data <- data.frame(
  category       = c("Before vs After"),
  mean_difference = c(t_test_result$estimate),
  lower_ci       = c(t_test_result$conf.int[1]),
  upper_ci       = c(t_test_result$conf.int[2])
)

ggplot(plot_data, aes(x = category, y = mean_difference)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "red") +
  labs(title = "Paired t-test for Mean Differences",
       y = "Mean Difference (Before - After)",
       caption = paste("p-value:", format(t_test_result$p.value, scientific = TRUE, digits = 2)),
       x = "") +
  theme_minimal()

# ─────────────────────────────────────────────────────────────────
# STEP 5 — REGRESSION MODEL
# ─────────────────────────────────────────────────────────────────

# Prepare variable (company size in thousands)
tech_layoffs$Company_Size_1000 <- tech_layoffs$Company_Size_before_Layoffs / 1000

# Fit linear regression model                    # Fixed: lm() model stored correctly
layoffs_model <- lm(Laid_Off ~ Company_Size_1000, data = tech_layoffs)

# Base R scatter plot with regression line
plot(Laid_Off ~ Company_Size_1000, data = tech_layoffs,
     xlab = "Company Size Before Layoffs (in thousands)",
     ylab = "Laid Off",
     main = "Layoffs vs Company Size",
     pch  = 20,
     cex  = 2,
     col  = "grey")
abline(layoffs_model, lwd = 3, col = "darkorange")  # Fixed: pass model not dataframe

# Model diagnostics
coef(layoffs_model)
residuals    <- resid(layoffs_model)
fitted_values <- fitted(layoffs_model)
summary(layoffs_model)

# Regression model with continent dummy variables
dummy_continent        <- as.data.frame(model.matrix(~ Continent - 1, data = tech_layoffs))
tech_layoffs_ext       <- cbind(tech_layoffs, dummy_continent)
layoffs_model_continent <- lm(Laid_Off ~ Company_Size_1000 + ., data = dummy_continent)
summary(layoffs_model_continent)
