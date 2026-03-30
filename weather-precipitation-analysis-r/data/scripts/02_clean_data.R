library(tidyverse)
library(readr)
load_data <- read_csv("data/raw/load_data.csv")

df1 <- load_data

#counting missing values

colSums(is.na(df1))

missing_summary <- data.frame(
  Variable = names(df1), 
  Missing_Count = colSums(is.na(df)),
  Missing_percentage = ((colSums(is.na(df))/nrow(df1))*100),
  row.names = NULL
)

print(missing_summary)

high_missing <- missing_summary[missing_summary$Missing_percentage > 95,]

print(high_missing)

nrow(high_missing)

df2 <- df1[, !(names(df1) %in% high_missing$Variable)]

ncol(df2)

#Finding the data type of the variables
col_info <- data.frame(
  DataType = sapply(df2, function(x) class(x)[1])
)

print(col_info)

#counting the no.of variable relevant to each type availble in the dataset

count.variable <- data.frame(
  type = c("character", "numeric", "logical", "POSIXct"), 
  count = c(
    sum(sapply(df2, is.character)),
    sum(sapply(df2, is.numeric)),
    sum(sapply(df2, is.logical)), 
    sum(sapply(df2, is.POSIXct))
  )
)

print(count.variable)

#counting the identical rows in character columns

char_cols <- names(df2)[sapply(df2, is.character)]

duplicate_summary <- data.frame(
  Variable = char_cols,
  identical_row_count = sapply(df2[char_cols], function(x) sum(duplicated(x))),
  row.names = NULL
)

print(duplicate_summary)
nrow(df2)

#so no clear groups are observed 

#next moving to finding outliers of numerical data sets


missing_summary_numeric <- data.frame(
  Variable = names(df2)[sapply(df2, is.numeric)],
  Missing_Count = colSums(is.na(df2[sapply(df2, is.numeric)])),
  row.names = NULL
)

missing_summary_numeric <- missing_summary_numeric[order(missing_summary_numeric$Missing_Count), ]

print(missing_summary_numeric)

