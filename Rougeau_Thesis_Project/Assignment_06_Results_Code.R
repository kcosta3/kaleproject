##########

# OCS 7313 DAR Assignment 6
# Results
# based on my thesis research data
# 2024-04-23
# due 2024-05-03

##########


### DATA SETUP ###


# original is a .xlsx file
# using .csv file here

# read in csv file as data frame
prelimdata <- as.data.frame(read.csv("~/Library/CloudStorage/Dropbox/ocs_dar/Lectures/DAR_Lecture_Assignment_06/Prelim_31s_Data.csv"))
# remove notes and empty columns
prelimdata <- prelimdata[,-25:-30]
# remove 3 individuals who did not receive conspecifics
prelimdata <- prelimdata[-158:-160,]

### CANNIBALISM RATES ###

attach(prelimdata)

# exploratory visualization

library(dplyr)
library(ggplot2)

## make new dataset with sum totals of cannibalism based on each diet type and con status

can_totals <- prelimdata |>
  group_by(Diet, ConStatus) |> # group by Diet type
  summarise(Total_Cans = sum(X24H)) # summarize total cannibalized per diet type

can_totals$Totals <- 20 # set sample size to 20 for each treatment
can_totals$Totals[5] <- 17 # correct sample size C infecteds (3 excluded due to no conspecifics offered)

can_totals$Percentages <- round((can_totals$Total_Cans / can_totals$Totals), 2)

ggplot(can_totals, aes(fill = ConStatus, x = Diet, x2  = ConStatus, y = Percentages)) +
  geom_bar(stat='identity', position='dodge') +
  labs(x = "Diet Type", y = "Cannibalism Totals") +
  geom_text(aes(label = paste((Percentages*100), "%", sep="")), fontface = "bold", vjust = -.5,
            position = position_dodge(1), size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()

## con status only

nrow(prelimdata[ConStatus == "uninfected" & X24H == "1", ]) # 70
# 70 out of 80 individuals cannibalized in the uninfected group
# 87.5%
nrow(prelimdata[ConStatus == "infected" & X24H == "1", ]) # 59
# 59 out of 80 individuals cannibalized in the infected group
# 73.75%

aov_con_can <- aov(X24H ~ as.factor(ConStatus)) # ANOVA
summary(aov_con_can) # not sig.

cannibal.glm1 <- glm(X24H ~ as.factor(ConStatus), family = binomial)
summary(cannibal.glm1)

library(ggplot2)
ggplot(prelimdata, aes(y = X24H, x = ConStatus)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.5) +
  geom_smooth(method="glm", method.args=list(family="binomial"),
              formula = y ~ x)

# bar plot using the model (1)

ggplot(prelimdata, aes(x = ConStatus, y = predict(cannibal.glm1, newdata = prelimdata, type = "response"))) +
  geom_bar(stat = "identity", aes(color = ConStatus)) +
  labs(x = "Conspecific Status", y = "Predicted Cannibalism Status")

AIC_glm1 <- AIC(cannibal.glm1) # AIC value of model 1 (con status, cannibalism)

## diet type only

nrow(prelimdata[Diet == "Southland" & X24H == "1", ]) # 31
# 31 out of 40 individuals cannibalized in the Southland diet treatment
# 77.5%
nrow(prelimdata[Diet == "A" & X24H == "1", ]) # 37
# 37 out of 40 individuals cannibalized in the A diet treatment
# 92.5%
nrow(prelimdata[Diet == "B" & X24H == "1", ]) # 33
# 33 out of 40 individuals cannibalized in the B diet treatment
# 82.5%
nrow(prelimdata[Diet == "C" & X24H == "1", ]) # 28
# 28 out of 40 individuals cannibalized in the C diet treatment
# 70%

aov_diet_can <- aov(X24H ~ as.factor(Diet)) # ANOVA
summary(aov_diet_can) # not sig.

cannibal.glm2 <- glm(X24H ~ as.factor(Diet), family = binomial)
summary(cannibal.glm2)

ggplot(prelimdata, aes(y = X24H, x = Diet)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.5) +
  geom_smooth(method="glm", method.args=list(family="binomial"),
              formula = y ~ x)

# bar plot using the model (2)

ggplot(prelimdata, aes(x = Diet, y = predict(cannibal.glm2, newdata = prelimdata, type = "response"))) +
  geom_bar(stat = "identity", aes(color = Diet)) +
  labs(x = "Diet Type", y = "Predicted Cannibalism Status")

AIC_glm2 <- AIC(cannibal.glm2)

## both diet type and con status

aov_dietcon_can <- aov(X24H ~ as.factor(Diet) + as.factor(ConStatus)) # ANOVA
summary(aov_dietcon_can) # not sig.

cannibal.glm3 <- glm(X24H ~ as.factor(Diet) + as.factor(ConStatus), family = binomial)
summary(cannibal.glm3)

ggplot(prelimdata, aes(y = X24H, x = Diet, x2 = ConStatus, color = ConStatus)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.5) +
  geom_smooth(method="glm", method.args=list(family="binomial"),
              formula = y ~ x + x2)

AIC_glm3 <- AIC(cannibal.glm3)

# bar plot using the model (3 - both diet and con status)

##################

ggplot(prelimdata, aes(fill = ConStatus, x = Diet, x2  = ConStatus, y = predict(cannibal.glm3, newdata = prelimdata, type = "response"))) +
  geom_bar(stat='identity', position='dodge') +
  labs(x = "Diet Type", y = "Predicted Cannibalism Status") +
  ylim(c(0.00, 1.00))

##################

# interaction mod

cannibal.glm4 <- glm(X24H ~ as.factor(Diet)*as.factor(ConStatus), data = prelimdata, family = binomial)
summary(cannibal.glm4)

AIC_glm4 <- AIC(cannibal.glm4)

# add null model here!!

cannibal.glm.null <- glm(X24H ~ 1, data = prelimdata, family = binomial)
summary(cannibal.glm.null)

AIC_glm_null <- AIC(cannibal.glm.null)

# do weights

## AIC model selection

AIC_glm1 # 148.0265 (1) # con status only
AIC_glm2 # 150.1164 (4) # diet type only
AIC_glm3 # 148.6615 (2) # diet + con status
AIC_glm4 # 148.4063 () # diet * con status
AIC_glm_null # 149.2261 (3)

AIC_glm1 - AIC_glm1 # 0
AIC_glm1 - AIC_glm2 # -2.090
AIC_glm1 - AIC_glm3 # -0.635
AIC_glm1 - AIC_glm3 # -0.635
AIC_glm1 - AIC_glm_null # -1.199

library(AICcmodavg)
can_models <- list(cannibal.glm1, cannibal.glm2, cannibal.glm3, cannibal.glm4, cannibal.glm.null)

model.names1 <- c('can_glm1', 'can_glm2', 'can_glm3', 'can_glm4', 'can_glm_null')

aictab(cand.set = can_models, modnames = model.names1)

### VIRUS TRANSMISSION ###

nrow(prelimdata[ConStatus == "infected" &
                  Inf. == "1" &
                  Diet == "Southland", ]) # 13 Southland infected (1 not can.)

nrow(prelimdata[ConStatus == "infected" &
                  Inf. == "1" &
                  Diet == "A", ]) # 3 A diet infected

nrow(prelimdata[ConStatus == "infected" &
                  Inf. == "1" &
                  Diet == "B", ]) # 6 B diet infected

nrow(prelimdata[ConStatus == "infected" &
                  Inf. == "1" &
                  Diet == "C", ]) # 6 C diet infected (1 not can.)

# subset only infecteds who cannibalized

cannibalized <- subset(prelimdata, ConStatus == "infected" & X24H =="1", select = c("Diet", "ConStatus", "X24H", "Inf."))

# create percentages of those infected
cannibalized_virus <- cannibalized |>
  group_by(Diet) |> # group by Diet type
  summarise(Total_Inf. = sum(Inf.)) # summarize total infected per diet type

# calculate total infected cons. that cannibalized whether infected or not

sum(cannibalized$X24H[cannibalized$Diet == "A"]) # 17
sum(cannibalized$X24H[cannibalized$Diet == "B"]) # 14
sum(cannibalized$X24H[cannibalized$Diet == "C"]) # 12
sum(cannibalized$X24H[cannibalized$Diet == "Southland"]) # 16

cannibalized_virus$Totals <- c(17, 14, 12, 16)
cannibalized_virus$Percentages <- round((cannibalized_virus$Total_Inf. / cannibalized_virus$Totals), 2)

# bar plot of infection based on diet type

ggplot(cannibalized_virus, aes(x = Diet, y = Percentages)) +
  geom_bar(stat='identity', position='dodge', fill='salmon') +
  labs(x = "Diet Type", y = "Transmission Rate") +
  geom_text(aes(label = paste((Percentages*100), "%", sep="")), fontface = "bold", vjust = -.5, position = position_dodge(1), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

# effect of diet on infection status
# for only infecteds who cannibalized

aov_diet_inf <- aov(cannibalized$Inf. ~ as.factor(cannibalized$Diet)) # ANOVA
summary(aov_diet_inf) # sig.

virus.glm1 <- glm(cannibalized$Inf. ~ as.factor(cannibalized$Diet), family = binomial)
summary(virus.glm1)

ggplot(cannibalized, aes(y = Inf., x = Diet)) +
  geom_jitter(width = 0.05, height = 0.05, alpha = 0.5) +
  geom_smooth(method="glm", method.args=list(family="binomial"),
              formula = y ~ x)

# add null model here!!

virus.glm.null <- glm(cannibalized$Inf. ~ 1, family = binomial)
summary(virus.glm.null)

AIC_v1 <- AIC(virus.glm1)
AIC_vnull <- AIC(virus.glm.null)

AIC_v1 # diet model
AIC_vnull # null model

AIC_v1 - AIC_v1 # 0
AIC_v1 - AIC_vnull # -5.698

library(AICcmodavg)
virus_models <- list(virus.glm1, virus.glm.null)

model.names2 <- c('virus.glm1', 'virus.glm.null')

aictab(cand.set = virus_models, modnames = model.names1)

######

#####

# NOTES:

# PREDICTORS: Diet type, Conspecific status

# RESPONSES: Ultimate cannibalism status, Rate/time of cannibalism
# Net larval mass change after arena period, Pupal mass (fitness -> fecundity)
