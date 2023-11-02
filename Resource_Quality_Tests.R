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

# calculate time spent in each instar, add as new columns
testdf$First_Length = as.numeric(difftime(testdf$Second, testdf$First, units = "days"))
testdf$Second_Length = as.numeric(difftime(testdf$Third, testdf$Second, units = "days"))
testdf$Third_Length = as.numeric(difftime(testdf$Fourth, testdf$Third, units = "days"))
testdf$Fourth_Length = as.numeric(difftime(testdf$Fifth, testdf$Fourth, units = "days"))
testdf$Fifth_Length = as.numeric(difftime(testdf$Sixth, testdf$Fifth, units = "days"))
testdf$Sixth_Length = as.numeric(difftime(testdf$Pup, testdf$Sixth, units = "days"))

# calculate age to pupation, add as new column
testdf$Age_to_Pup = as.numeric(difftime(testdf$Pup, testdf$First, units = "days"))

# histogram of all ages to pupation
hist(testdf$Age_to_Pup)
breaknum <- length(unique(testdf$Age_to_Pup, na.rm = TRUE))
min(testdf$Age_to_Pup, na.rm = TRUE) # fastest overall pupa (13)
max(testdf$Age_to_Pup, na.rm = TRUE) # slowest overall pupa (22)

# subset ages to pupation for controls, create histogram
control_pup_length <- subset(testdf, Treatment=="Control" & (!is.na(testdf[,23])), select="Age_to_Pup")
hist(control_pup_length$Age_to_Pup)
mean(control_pup_length$Age_to_Pup)
median(control_pup_length$Age_to_Pup)
abline(v = mean(control_pup_length$Age_to_Pup), col='red', lwd = 3)

# subset ages to pupation for Treatment A that started at 1st instar, create histogram
A1st_pup_length <- subset(testdf, Treatment=="A" & Initial_Age=="1st" & (!is.na(testdf[,23])), select="Age_to_Pup")
### 0 pupae for this treatment

# subset ages to pupation for Treatment A that started at 3rd instar, create histogram
A3rd_pup_length <- subset(testdf, Treatment=="A" & Initial_Age=="3rd" & (!is.na(testdf[,23])), select="Age_to_Pup")
hist(A3rd_pup_length$Age_to_Pup)
mean(A3rd_pup_length$Age_to_Pup)
median(A3rd_pup_length$Age_to_Pup)
abline(v = mean(A3rd_pup_length$Age_to_Pup), col='red', lwd = 3)

# subset ages to pupation for Treatment B that started at 1st instar, create histogram
B1st_pup_length <- subset(testdf, Treatment=="B" & Initial_Age=="1st" & (!is.na(testdf[,23])), select="Age_to_Pup")
hist(B1st_pup_length$Age_to_Pup)
mean(B1st_pup_length$Age_to_Pup)
median(B1st_pup_length$Age_to_Pup)
abline(v = mean(B1st_pup_length$Age_to_Pup), col='red', lwd = 3)

# subset ages to pupation for Treatment B that started at 3rd instar, create histogram
B3rd_pup_length <- subset(testdf, Treatment=="B" & Initial_Age=="3rd" & (!is.na(testdf[,23])), select="Age_to_Pup")
hist(B3rd_pup_length$Age_to_Pup)
mean(B3rd_pup_length$Age_to_Pup)
median(B3rd_pup_length$Age_to_Pup)
abline(v = mean(B3rd_pup_length$Age_to_Pup), col='red', lwd = 3)

# subset ages to pupation for Treatment C that started at 1st instar, create histogram
C1st_pup_length <- subset(testdf, Treatment=="C" & Initial_Age=="1st" & (!is.na(testdf[,23])), select="Age_to_Pup")
### 0 pupae for this treatment

# subset ages to pupation for Treatment C that started at 3rd instar, create histogram
C3rd_pup_length <- subset(testdf, Treatment=="C" & Initial_Age=="3rd" & (!is.na(testdf[,23])), select="Age_to_Pup")
### 0 pupae for this treatment

# create grid of histograms
par(mfrow = c(2, 2))
hist(control_pup_length$Age_to_Pup, xlim = c(12, 22), main = "Control Diet", xlab = "Days to Pupation")
hist(A3rd_pup_length$Age_to_Pup, xlim = c(12, 22), main = "Treatment A at 3rd Instar", xlab = "Days to Pupation")
hist(B1st_pup_length$Age_to_Pup, xlim = c(12, 22), main = "Treatment B at 1st Instar", xlab = "Days to Pupation")
hist(B3rd_pup_length$Age_to_Pup, xlim = c(12, 22), main = "Treatment B at 3rd Instar", xlab = "Days to Pupation")

# hist_pupation

# survival analysis & *chi squared test* (C A B, dead or alive)
# ^ percent survival against time for each treatment
# ANOVA (2) for age to pupation
# analyze pupation FROM 3rd instar time to end (ANOVA)
# if ANOVA is significant, do pre-hoc tests (pre-planned comparisons) & Tukey test
# fix bins?
# save outputs
# formulate written paper, RMarkdown coding & output PDF, readme file, etc.