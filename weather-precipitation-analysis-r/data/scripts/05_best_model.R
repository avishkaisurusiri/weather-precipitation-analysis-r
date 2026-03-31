library(tidyverse)
library(readr)
library(car)
df4 <- read_csv("data/raw/eda.csv")

model_improved <- lm(log1p(HOURLYPrecip) ~ HOURLYRelativeHumidity +
                       HOURLYDewPointTempC +
                       HOURLYWindSpeed,
                     data = model_df)

summary(model_improved)

vif(model_improved)

model_improved2 <- lm(log1p(HOURLYPrecip) ~ HOURLYRelativeHumidity +
                        HOURLYDewPointTempC +
                        HOURLYWindSpeed +
                        HOURLYStationPressure,
                      data = model_df)

summary(model_improved2)



vif(model_improved2)

model_poly <- lm(log1p(HOURLYPrecip) ~ HOURLYRelativeHumidity +
                   I(HOURLYRelativeHumidity^2) +
                   HOURLYDewPointTempC +
                   HOURLYWindSpeed,
                 data = model_df)

summary(model_poly)

vif(model_poly)

