install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
library(ggplot2)
library(dplyr)
library(lubridate)


data<- read.csv("unemployment rates.csv")

# Inspect the data structure
str(data)
head(data)

##DATA Exploration
# Line plot of unemployment rate over time
ggplot(data, aes(x = DATE, y = CAUR)) +
  geom_line(color = "blue") +
  labs(title = "Unemployment Rate in California (1976 - 2020)",
       x = "DATE",
       y = "CAUR (%)") +
  theme_minimal()



# Convert DATE column to Date type
data$DATE <- as.Date(data$DATE, format="%Y-%m-%d")

# Extract Year from the DATE column
data$Year <- year(data$DATE)

# Check the result

head(data)


# Summary statistics
summary_stats <- data %>%
  summarize(
    Mean = mean(CAUR),
    Median = median(CAUR),
    SD = sd(CAUR),
    Min = min(CAUR),
    Max = max(CAUR)
  )
print(summary_stats)

##Distribution of Unemployment Rates

# Histogram of unemployment rates
ggplot(data, aes(x = CAUR)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Unemployment Rates in California",
       x = "CAUR (%)",
       y = "Frequency") +
  theme_minimal()

##Identify Peaks and Troughs
# Highlighting periods of economic crisis (e.g., 2008 recession  and 2020 covid-19 effects)
ggplot(data, aes(x = DATE, y = CAUR, group = 1)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = as.numeric(as.Date(c("2008-01-01", "2020-01-01"))), 
             linetype = "dashed", color = "red") +
  labs(title = "Unemployment Rate with Highlighted Crises (1976 - 2020)",
       x = "DATE",
       y = "CAUR (%)") +
  theme_minimal()

# Decompose time series to extract trend, seasonality, and residuals
unemployment_ts <- ts(data$CAUR, start = c(1976, 1), frequency = 12)
decomposed <- decompose(unemployment_ts)

# Plot the decomposed components
plot(decomposed)

# Add year and month for seasonal analysis
data$Year <- year(data$DATE)
data$Month <- month(data$DATE, label = TRUE)

# Boxplot to check for seasonal variations
ggplot(data, aes(x = Month, y = CAUR)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Seasonal Variation in Unemployment Rates",
       x = "Month",
       y = "CAUR (%)") +
  theme_minimal()

##Trend Analysis (Regression or Smoothing)

ggplot(data, aes(x = DATE, y = CAUR)) +
  geom_line(color = "gray") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Unemployment Rate Trend with LOESS Smoothing",
       x = "Year",
       y = "CAUR (%)") +
  theme_minimal()
