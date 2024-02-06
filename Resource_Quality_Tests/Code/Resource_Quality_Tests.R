##############################
# "Effects of macronutrient manipulation in artificial diet on fall armyworm
# (Spodoptera frugiperda) performance"
#
# Kale Costanza
# 12/5/2023
##############################


### DATA SETUP ###


# both .xlsx (original) and .csv data files are provided

# read in csv file as data frame
projectdata <- as.data.frame(read.csv("~/Library/CloudStorage/Dropbox/BIOL7800/final_project/Data/Resource_Quality_Tests.csv"))
# remove notes/extra columns
projectdata <- projectdata[,-16:-26] 

# convert dates to standard R date format
library(lubridate)
# First through Sixth instars, Pupa, Moth, Dead
projectdata['First'] <- mdy(projectdata$First)
projectdata['Second'] <- mdy(projectdata$Second)
projectdata['Third'] <- mdy(projectdata$Third)
projectdata['Fourth'] <- mdy(projectdata$Fourth)
projectdata['Fifth'] <- mdy(projectdata$Fifth)
projectdata['Sixth'] <- mdy(projectdata$Sixth)
projectdata['Pup'] <- mdy(projectdata$Pup)
projectdata['Moth'] <- mdy(projectdata$Moth)
projectdata['Dead'] <- mdy(projectdata$Dead)

# fix any mistakes in data
projectdata[101,15] <- "2023-09-26" # error in month for this entry

# calculate time spent in each instar; add as new columns
# length in first instar "First_Length" then same until sixth instar
projectdata$First_Length = as.integer(difftime(projectdata$Second, projectdata$First))
projectdata$Second_Length = as.integer(difftime(projectdata$Third, projectdata$Second))
projectdata$Third_Length = as.integer(difftime(projectdata$Fourth, projectdata$Third))
projectdata$Fourth_Length = as.integer(difftime(projectdata$Fifth, projectdata$Fourth))
projectdata$Fifth_Length = as.integer(difftime(projectdata$Sixth, projectdata$Fifth))
projectdata$Sixth_Length = as.integer(difftime(projectdata$Pup, projectdata$Sixth))

# create new column for treatment and age combined
projectdata$Treatment_Age <- paste(projectdata$Treatment, projectdata$Initial_Age, sep = "")
# modify "Control1st" value to be just "Control"
projectdata$Treatment_Age[projectdata$Treatment_Age == "Control1st"] = "Control"


### PUPATION AND ECLOSION SUCCESS RATES ###


# PUPAE

# sum how many total larvae made it to pupation vs. dying
total_pupae <- projectdata |>
  group_by(Treatment_Age) |>
  summarise(Total_Pupae = sum(!is.na(Pup)))

total_pupae # see totals
# none for A1st, C1st, or C3rd; 11 for A3rd; 2 for B1st; 3 for B3rd;
# 26 for Control

# plot results

total_pupae_plot = ggplot(data = total_pupae) + aes(x = Treatment_Age, y = Total_Pupae, fill = Treatment_Age) +
  geom_col() + geom_text(aes(label=Total_Pupae), position=position_dodge(width=0.9), vjust=-0.25)

total_pupae_plot + xlab("Treatment and Initial Age") +
  ylab("Total # of Pupae") + ggtitle("Total Pupae by Treatment Group") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  guides(fill = FALSE) + scale_fill_brewer(palette="Set1")

# MOTHS

# sum how many total individuals made it to moth stage (attempted to emerge)
total_moths <- projectdata |>
  group_by(Treatment_Age) |>
  summarise(Total_Moths = sum(!is.na(Moth)))

total_moths # see totals
# none for any treatment except 25 for Controls and 1 for A3rd
# however, the 1 in A3rd treatment was a FTE (failed to emerge)
# therefore I will not plot these results
# only Controls successfully made it to moth stage


### TIME TO PUPATION CALCULATIONS & HISTOGRAMS ###


# calculate age to pupation for all; add as new column
projectdata$Age_to_Pup = as.integer(difftime(projectdata$Pup, projectdata$First))

# set sensible breaks for histogram
breaknum <- length(unique(projectdata$Age_to_Pup, na.rm = TRUE))
# histogram of all ages to pupation
hist(projectdata$Age_to_Pup, breaks = breaknum, main = "All Treatments",
     xlab = "Days to Pupation")
# min & max ages to pupation
min(projectdata$Age_to_Pup, na.rm = TRUE) # fastest overall pupa (13)
max(projectdata$Age_to_Pup, na.rm = TRUE) # slowest overall pupa (22)

# subset ages to pupation for controls, create histogram
control_pup_length <- subset(projectdata, Treatment=="Control" & (!is.na(projectdata[,22])), select="Age_to_Pup")
hist(control_pup_length$Age_to_Pup) # histogram
mean(control_pup_length$Age_to_Pup) # mean
median(control_pup_length$Age_to_Pup) # median
abline(v = mean(control_pup_length$Age_to_Pup), col='red', lwd = 3) # mean line in histogram

# subset ages to pupation for Treatment A that started at 1st instar
A1st_pup_length <- subset(projectdata, Treatment=="A" & Initial_Age=="1st" & (!is.na(projectdata[,22])), select="Age_to_Pup")
### 0 pupae for this treatment, will be excluded from pupa analysis

# subset ages to pupation for Treatment A that started at 3rd instar, create histogram
A3rd_pup_length <- subset(projectdata, Treatment=="A" & Initial_Age=="3rd" & (!is.na(projectdata[,22])), select="Age_to_Pup")
hist(A3rd_pup_length$Age_to_Pup) # histogram
mean(A3rd_pup_length$Age_to_Pup) # mean
median(A3rd_pup_length$Age_to_Pup) # median
abline(v = mean(A3rd_pup_length$Age_to_Pup), col='red', lwd = 3) # mean line in histogram

# subset ages to pupation for Treatment B that started at 1st instar, create histogram
B1st_pup_length <- subset(projectdata, Treatment=="B" & Initial_Age=="1st" & (!is.na(projectdata[,22])), select="Age_to_Pup")
hist(B1st_pup_length$Age_to_Pup) # histogram
mean(B1st_pup_length$Age_to_Pup) # mean
median(B1st_pup_length$Age_to_Pup) # median
abline(v = mean(B1st_pup_length$Age_to_Pup), col='red', lwd = 3) # mean line in histogram

# subset ages to pupation for Treatment B that started at 3rd instar
B3rd_pup_length <- subset(projectdata, Treatment=="B" & Initial_Age=="3rd" & (!is.na(projectdata[,22])), select="Age_to_Pup")
hist(B3rd_pup_length$Age_to_Pup) # histogram
mean(B3rd_pup_length$Age_to_Pup) # mean
median(B3rd_pup_length$Age_to_Pup) # median
abline(v = mean(B3rd_pup_length$Age_to_Pup), col='red', lwd = 3) # mean line in histogram

# subset ages to pupation for Treatment C that started at 1st instar, create histogram
C1st_pup_length <- subset(projectdata, Treatment=="C" & Initial_Age=="1st" & (!is.na(projectdata[,22])), select="Age_to_Pup")
### 0 pupae for this treatment, will be excluded from pupa analysis

# subset ages to pupation for Treatment C that started at 3rd instar, create histogram
C3rd_pup_length <- subset(projectdata, Treatment=="C" & Initial_Age=="3rd" & (!is.na(projectdata[,22])), select="Age_to_Pup")
### 0 pupae for this treatment, will be excluded from pupa analysis

# create grid of histograms for relevant pupation data
par(mfrow = c(2, 2))
# control diet
hist(control_pup_length$Age_to_Pup, xlim = c(12, 22), main = "Control Diet", xlab = "Days to Pupation")
abline(v = mean(control_pup_length$Age_to_Pup), col='red', lwd = 1) 
# treatment A diet placed at 3rd instar (1st instar had no pupae)
hist(A3rd_pup_length$Age_to_Pup, xlim = c(12, 22), main = "Treatment A at 3rd Instar", xlab = "Days to Pupation")
abline(v = mean(A3rd_pup_length$Age_to_Pup), col='red', lwd = 1) 
# treatment B diet placed at 1st instar
hist(B1st_pup_length$Age_to_Pup, xlim = c(12, 22), main = "Treatment B at 1st Instar", xlab = "Days to Pupation")
abline(v = mean(B1st_pup_length$Age_to_Pup), col='red', lwd = 1) 
# treatment B diet placed at 3rd instar
hist(B3rd_pup_length$Age_to_Pup, xlim = c(12, 22), main = "Treatment B at 3rd Instar", xlab = "Days to Pupation")
abline(v = mean(B3rd_pup_length$Age_to_Pup), col='red', lwd = 1) 
# treatment C diet placed at 1st and 3rd instar both did not have pupae


### ANALYSIS FOR PUPAE: 3RD TO PUPATION ###


# calculate time spent from 3rd instar to pupation for all
    # 3rd instar to pupation time was analyzed because most successful pupae
    # were placed at 3rd instar on treatment diet
projectdata$Third_to_Pup = as.integer(difftime(projectdata$Pup, projectdata$Third))

# create subset of only larvae placed at 3rd instar
    # with controls
only_3rd_to_pup <- subset(projectdata, (Initial_Age=="3rd" | Treatment=="Control") & (!is.na(projectdata[,22])), select=c(Treatment, Individual, Third_to_Pup, Mass))

# linear model for time to pupation from 3rd instar against treatment
puplm = lm(only_3rd_to_pup$Third_to_Pup ~ only_3rd_to_pup$Treatment)
summary(puplm) # significant

# anova
pup_anova = anova(puplm)
pup_anova # significant
pup_aov = aov(puplm)

# Tukey test
Tukey_pup <- TukeyHSD(pup_aov, 'only_3rd_to_pup$Treatment', conf.level=0.95)
Tukey_pup
# results: high significant difference between Control and A
    # significant difference (but less so) between Control and B
    # no significant difference between B and A
    # therefore, manipulated diets are different from Control diet
    # but B treatment (equal P:C) is more similar to Control
    # A and B treatments are not statistically different (in terms of time
    # from 3rd instar to pupation for successful pupae in 2 treatments)

# Tukey test representation plotted
plot(Tukey_pup, las=1 , col="brown")

# t-test of control compared to A3rd treatment (significant)
t.test(projectdata$Third_to_Pup[projectdata$Treatment == "Control"],
       projectdata$Third_to_Pup[projectdata$Treatment == "A" & projectdata$Initial_Age == "3rd"])

# t-test of control compared to B3rd treatment (not sig. but almost)
t.test(projectdata$Third_to_Pup[projectdata$Treatment == "Control"],
       projectdata$Third_to_Pup[projectdata$Treatment == "B" & projectdata$Initial_Age == "3rd"])

# t-test of both A & B treatment groups at 3rd to pupation (not sig.)
t.test(only_3rd_to_pup$Third_to_Pup[only_3rd_to_pup$Treatment == "A"],
       only_3rd_to_pup$Third_to_Pup[only_3rd_to_pup$Treatment == "B"])


### ANALYSIS FOR PUPAE: MASS/SEX ###


# subset data to include all pupae and their mass/sex information
pupal_mass <- subset(projectdata, !is.na(projectdata$Mass), select=c(Treatment, Individual, Initial_Age, Mass, Sex, Treatment_Age))

# mean mass of pupae in ALL treatments that produced pupae
mean(pupal_mass$Mass[pupal_mass$Treatment_Age == "Control"])
## Control mean mass = 181.770 mg
mean(pupal_mass$Mass[pupal_mass$Treatment_Age == "A3rd"])
# 1 pupa in the A3rd treatment was unable to have its mass/sex measured (NA)
# another pupa had its mass measured but not its sex
## A3rd mean mass = 122.3 mg
mean(pupal_mass$Mass[pupal_mass$Treatment_Age == "B1st"])
## B1st mean mass = 119.5 mg
mean(pupal_mass$Mass[pupal_mass$Treatment_Age == "B3rd"])
## B3rd mean mass = 147 mg

# boxplot pupal masses
library(ggplot2)
mass_plot = ggplot(data = pupal_mass) + aes(x = Treatment_Age, y = Mass, fill = Treatment_Age)

mass_plot + geom_boxplot() + xlab("Treatment and Initial Age") +
    ylab("Pupal Mass (mg)") + ggtitle("Pupal Mass by Treatment Group") +
    guides(fill = FALSE) + scale_fill_brewer(palette="Set1") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# linear model for mass against treatment for all pupae
masslm = lm(pupal_mass$Mass ~ pupal_mass$Treatment_Age)
summary(masslm) # significant

# anova
mass_anova = anova(masslm)
mass_anova # significant
mass_aov = aov(masslm)

# Tukey test
Tukey_mass <- TukeyHSD(mass_aov, 'pupal_mass$Treatment_Age', conf.level=0.95)
Tukey_mass
# results: no significant differences between any comparisons except
    # Control to A3rd. This makes sense if B is most similar to Control diet
    # while A varies significantly in protein content.

# Tukey test representation plotted
plot(Tukey_mass, las=1 , col="brown")

# t-test of control compared to A3rd pupal mass (highly significant)
t.test(pupal_mass$Mass[pupal_mass$Treatment_Age == "Control"],
       pupal_mass$Mass[pupal_mass$Treatment_Age == "A3rd"])

# t-test of control compared to B1st pupal mass (not significant but almost)
t.test(pupal_mass$Mass[pupal_mass$Treatment_Age == "Control"],
       pupal_mass$Mass[pupal_mass$Treatment_Age == "B1st"])

# t-test of control compared to B3rd pupal mass (slightly significant)
t.test(pupal_mass$Mass[pupal_mass$Treatment_Age == "Control"],
       pupal_mass$Mass[pupal_mass$Treatment_Age == "B3rd"])

# t-test of both A3rd & B3rd treatment groups' pupal mass (not sig.)
t.test(pupal_mass$Mass[pupal_mass$Treatment_Age == "A3rd"],
       pupal_mass$Mass[pupal_mass$Treatment_Age == "B3rd"])

# t-test of both B1st & B3rd treatment groups' pupal mass (not sig.)
t.test(pupal_mass$Mass[pupal_mass$Treatment_Age == "B1st"],
       pupal_mass$Mass[pupal_mass$Treatment_Age == "B3rd"])

# t-test of both A3rd & B1st treatment groups' pupal mass (not sig.)
t.test(pupal_mass$Mass[pupal_mass$Treatment_Age == "A3rd"],
       pupal_mass$Mass[pupal_mass$Treatment_Age == "B1st"])

# visually check how sexes are distributed
library(dplyr)
pupal_mass %>% filter(Treatment_Age == "Control") %>% count(Sex) # 10 F, 16 M
pupal_mass %>% filter(Treatment_Age == "A3rd") %>% count(Sex) # 6 F, 3 M, 1 NA
pupal_mass %>% filter(Treatment_Age == "B1st") %>% count(Sex) # 2 F
pupal_mass %>% filter(Treatment_Age == "B3rd") %>% count(Sex) # 3 F

# subset to get rid of 1 sex NA in A3rd
pupal_mass_sex <- subset(pupal_mass, is.na(pupal_mass$Sex) == FALSE)
# linear model for mass against treatment & sex for all pupae
mass_sex_lm = lm(pupal_mass_sex$Mass ~ pupal_mass_sex$Treatment_Age * pupal_mass_sex$Sex - 1)
summary(mass_sex_lm) # possible significant interactions


### SURVIVAL ANALYSIS: MORTALITY FOR 1ST INSTAR TREATMENTS ###


# create mortality/survival column
    # if Pup date is NA, count as dead (1), otherwise pupated (0)
projectdata$Mortality <- ifelse(is.na(projectdata$Pup) == TRUE, 1, 0)

# create survival time column (First to Dead)
    # only calculate for those who died before pupation (excludes dead Pupae)
    # mortality/survival time is only being considered for dead larvae
projectdata$Surv_Time <- ifelse(is.na(projectdata$Pup) == TRUE, as.integer(difftime(projectdata$Dead, projectdata$First)), NA)

# create subset of only larvae placed at 1st instar (most mortality)
    # with controls
only_1st_to_surv <- subset(projectdata, Initial_Age=="1st", select=c(Treatment, Individual, Surv_Time, Mortality))

# survival analysis for individuals started at 1st instar
library(survival)

# Surv function (objects) & survfit function (curves)
    # ~ 1 here
s1 <- survfit(Surv(only_1st_to_surv$Surv_Time, only_1st_to_surv$Mortality) ~ 1, data = only_1st_to_surv)
str(s1)
summary(s1)

# creating plot for survival curves ~ by Treatment
library(ggplot2)
library(ggsurvfit)
ggsurvplot <- survfit2(Surv(only_1st_to_surv$Surv_Time, only_1st_to_surv$Mortality) ~ only_1st_to_surv$Treatment, data = only_1st_to_surv) %>% 
  ggsurvfit() + # plotting survfit
  add_risktable() + # adds risk table
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# adding modifications then plotting
ggsurvplot + labs( # labels
  x = "Days Monitored",
  y = "Survival Probability",
  title = "Survival Summary for Larvae\nPlaced at First Instar")

# survival regression model
survreg = survreg(Surv(only_1st_to_surv$Surv_Time, only_1st_to_surv$Mortality) ~ Treatment - 1, only_1st_to_surv)
survreg # show results

# plotting survival regression
library(survminer)
library(tidyr)

# overall survival plot (95/120 total died)

custom_theme <- function() {
  theme_survminer() %+replace%
    theme(
      plot.title=element_text(hjust=0.5, face = "bold")
    )
} # need custom theme to center title on plot

survplot = ggsurvplot(s1, data = only_1st_to_surv, risk.table = T,
          xlab = "Days Monitored", ylab = "Overall Survival Probability",
          title = "Survival Summary for All Combined\nLarvae Placed at First Instar",
          legend = "none", ggtheme=custom_theme())

survplot

# chi-square test for all treatments from 1st instar
chisq.test(only_1st_to_surv$Treatment, only_1st_to_surv$Mortality)
# result: p-value < 2.2e-16