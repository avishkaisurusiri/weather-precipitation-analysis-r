library(tidyverse)
glimpse(jfk_weather)
df <- jfk_weather
library(dplyr)
summarise(df)

#Finding the data type of the variables
col_info <- data.frame(
  DataType = sapply(df, function(x) class(x)[1])
)

print(col_info)

#counting the no.of variable relevant to each type availble in the dataset

count.variable <- data.frame(
  type = c("character", "numeric", "logical", "POSIXct"), 
  count = c(
    sum(sapply(df, is.character)),
    sum(sapply(df, is.numeric)),
    sum(sapply(df, is.logical)), 
    sum(sapply(df, is.POSIXct))
  )
)

print(count.variable)


col <- df$HOURLYPrecip
print(col)


df$HOURLYPrecip <- as.numeric(df$HOURLYPrecip)

write.csv(df, "C:/Users/AVISHKA/Desktop/Projects/weather-precipitation-analysis-r/data/raw/load_data.csv", row.names = FALSE)

#Selected the response variable to be the HOURLYPrecip