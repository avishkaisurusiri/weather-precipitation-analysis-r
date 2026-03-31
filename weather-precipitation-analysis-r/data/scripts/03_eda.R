library(tidyverse)
library(readr)
df3 <- read_csv("data/raw/clean_data.csv")

#summary statistics

summary(df3)


missing_summary_numeric <- data.frame(
  Variable = names(df3)[sapply(df3, is.numeric)],
  Missing_Count = colSums(is.na(df3[sapply(df3, is.numeric)])),
  row.names = NULL
)

missing_summary_numeric <- missing_summary_numeric[order(missing_summary_numeric$Missing_Count), ]

print(missing_summary_numeric)

ncol(df3)

#Removing columns i F scale because celcius is available

df3 <- df3[, !(names(df3) %in% c("HOURLYDRYBULBTEMPF",
                                 "HOURLYDewPointTempF",
                                 "HOURLYWETBULBTEMPF" ))]

ncol(df3)


bp_HOURLYPrecip <- boxplot(df3$HOURLYPrecip,
              main = "Boxplot of Precipitation",
              ylab = "HOURLYPrecip")

outlier_values <- bp_HOURLYPrecip$out
outlier_rows <- which(df3$HOURLYPrecip %in% outlier_values)

boxplot(df3$HOURLYPrecip,
        main = "Boxplot of Precipitation",
        ylab = "HOURLYPrecip")

text(rep(1, length(outlier_rows)),
     df3$HOURLYPrecip[outlier_rows],
     labels = outlier_rows,
     pos = 4,
     cex = 0.7,
     col = "red")

nrow(df3)
df3 <- df3[-21800,]
nrow(df3)


hist(df3$HOURLYPrecip, main="Histogram of Precipitation", xlab="Precipitation")
hist(df$HOURLYPrecip,
     breaks = 50,
     main = "Histogram of Precipitation",
     xlab = "Precipitation",
     xlim = c(0, 0.5))

# keep only numeric columns
num_data <- df3[, sapply(df3, is.numeric)]
names(num_data)


# keep only numeric columns
num_data <- df3[, sapply(df3, is.numeric)]

# remove columns that are completely NA
num_data <- num_data[, colSums(!is.na(num_data)) > 0]

# correlation of all numeric variables with HOURLYPrecip
cor_with_precip <- cor(num_data, use = "pairwise.complete.obs")[, "HOURLYPrecip"]

# convert to data frame
cor_df <- data.frame(
  Variable = names(cor_with_precip),
  Correlation = cor_with_precip
)

# remove HOURLYPrecip itself
cor_df <- cor_df[cor_df$Variable != "HOURLYPrecip", ]

# remove NA correlations
cor_df <- cor_df[!is.na(cor_df$Correlation), ]

# sort by absolute correlation
cor_df <- cor_df[order(abs(cor_df$Correlation), decreasing = TRUE), ]

# first 10 highest correlated variables
top10 <- head(cor_df, 10, )
top10

top10_vars <- top10$Variable

# correlation matrix among top 10 variables
intercor_mat <- cor(num_data[, top10_vars], use = "pairwise.complete.obs")

round(intercor_mat, 3)

plot(df3$HOURLYRelativeHumidity, df3$HOURLYPrecip,
     main = "Scatter Plot of Precipitation vs Humidity",
     xlab = "HOURLYRelativeHumidity",
     ylab = "HOURLYPrecip")

plot(df3$HOURLYPressureChange, df3$HOURLYPrecip,
     main = "Scatter Plot of Precipitation vs HOURLYPressure",
     xlab = "HOURLYPressureChange",
     ylab = "HOURLYPrecip")

plot(df3$HOURLYRelativeHumidity, log1p(df3$HOURLYPrecip),
     main = "Scatter Plot of Log-Transformed Precipitation vs Humidity",
     xlab = "HOURLYRelativeHumidity",
     ylab = "log(1 + HOURLYPrecip)",
     pch = 16,
     cex = 0.5)
write.csv(df3, "C:/Users/AVISHKA/Desktop/Projects/weather-precipitation-analysis-r/data/raw/eda.csv", row.names = FALSE)
