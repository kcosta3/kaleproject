# load package to read xlsx file
library(readxl)
# read in xlsx file as dataframe
testdf <- read_xlsx("~/Library/CloudStorage/Dropbox/kaleproject/Resource_Quality_Tests.xlsx")
# load packages for number to date conversion
library(tibble)
library(janitor)
# convert all date columns to date formats if currently numeric
testdf['Second'] <- excel_numeric_to_date(as.numeric(as.character(testdf$Second)), date_system = "modern")
testdf['Third'] <- excel_numeric_to_date(as.numeric(as.character(testdf$Third)), date_system = "modern")
testdf['Fourth'] <- excel_numeric_to_date(as.numeric(as.character(testdf$Fourth)), date_system = "modern")
testdf['Fifth'] <- excel_numeric_to_date(as.numeric(as.character(testdf$Fifth)), date_system = "modern")
testdf['Sixth'] <- excel_numeric_to_date(as.numeric(as.character(testdf$Sixth)), date_system = "modern")
testdf['Pup'] <- excel_numeric_to_date(as.numeric(as.character(testdf$Pup)), date_system = "modern")
testdf['Moth'] <- excel_numeric_to_date(as.numeric(as.character(testdf$Moth)), date_system = "modern")
testdf['Dead'] <- excel_numeric_to_date(as.numeric(as.character(testdf$Dead)), date_system = "modern")
# calculate time spent in each instar
first_length = as.numeric(difftime(testdf$Second, testdf$First, units = "days"))
second_length = as.numeric(difftime(testdf$Third, testdf$Second, units = "days"))
third_length = as.numeric(difftime(testdf$Fourth, testdf$Third, units = "days"))
fourth_length = as.numeric(difftime(testdf$Fifth, testdf$Fourth, units = "days"))
fifth_length = as.numeric(difftime(testdf$Sixth, testdf$Fifth, units = "days"))
sixth_length = as.numeric(difftime(testdf$Pup, testdf$Sixth, units = "days"))
