library(tidyverse)
library(readr)
library(car)
df4 <- read_csv("data/raw/eda.csv")


# convert trace precipitation "T" to 0, then convert to numeric
df4$HOURLYPrecip[df4$HOURLYPrecip == "T"] <- "0"
df4$HOURLYPrecip <- as.numeric(df4$HOURLYPrecip)

# select hourly numeric variables for modeling
model_df <- df4[, c(
  "HOURLYPrecip",
  "HOURLYRelativeHumidity",
  "HOURLYDewPointTempC",
  "HOURLYWindSpeed",
  "HOURLYStationPressure",
  "HOURLYSeaLevelPressure",
  "HOURLYAltimeterSetting"
)]

# remove missing values
model_df <- na.omit(model_df)

# fit linear regression model
model1 <- lm(HOURLYPrecip ~ HOURLYRelativeHumidity +
               HOURLYDewPointTempC +
               HOURLYWindSpeed +
               HOURLYStationPressure +
               HOURLYSeaLevelPressure +
               HOURLYAltimeterSetting,
             data = model_df)

# view results
summary(model1)
vif(model1)

model2 <- lm(log1p(HOURLYPrecip) ~ HOURLYRelativeHumidity +
               HOURLYDewPointTempC +
               HOURLYWindSpeed +
               HOURLYStationPressure +
               HOURLYSeaLevelPressure +
               HOURLYAltimeterSetting,
             data = model_df)

summary(model2)

model3 <- lm(log1p(HOURLYPrecip) ~ HOURLYRelativeHumidity +
               HOURLYDewPointTempC +
               HOURLYWindSpeed,
             data = model_df)

summary(model3)


# model 2 is the better one 