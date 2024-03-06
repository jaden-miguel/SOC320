# Name: Jaden Ji Miguel
# Prof. Pendergast
# Date: 11/24/2023

# NSDUH 2015

library(haven)
library(table1)

getwd()
setwd("D:/")

load("D:/NSDUH-2015-DONE/NSDUH_2015.Rdata")

# Y = Substance Use Disorder
# keep these variables
# control variables:
# gender, age, location, health insurance, physical status
# mental health history, type of military service, exercise habits
# diet, time period of service

# X1: Socioeconomic status of veterans
# Income level, educational attainment, employment status

# X2: Level of Support veterans receive
# Freq. of support services used, drug treatment
# or adult mental health service utilization.

# Subset data to only US Veterans
nsduh_15 <- subset(PUF2015_021518, service == 1)


# Recode age: AGE2
nsduh_15$age <- NA
nsduh_15$age[nsduh_15$AGE2 == 6] <- "17 y/o"
nsduh_15$age[nsduh_15$AGE2 == 7] <- "18 y/o"
nsduh_15$age[nsduh_15$AGE2 == 8] <- "19 y/o"
nsduh_15$age[nsduh_15$AGE2 == 9] <- "20 y/o"
nsduh_15$age[nsduh_15$AGE2 == 10] <- "21 y/o"
nsduh_15$age[nsduh_15$AGE2 == 11] <- "22 or 23 y/o"
nsduh_15$age[nsduh_15$AGE2 == 12] <- "24 or 25 y/o"
nsduh_15$age[nsduh_15$AGE2 == 13] <- "Between 26 and 29 y/o"
nsduh_15$age[nsduh_15$AGE2 == 14] <- "Between 30 and 34 y/o"
nsduh_15$age[nsduh_15$AGE2 == 15] <- "Between 35 and 49 y/o"
nsduh_15$age[nsduh_15$AGE2 == 16] <- "Between 50 and 64 y/o"
nsduh_15$age[nsduh_15$AGE2 == 17] <- "65+"

nsduh_15$age <- ordered(nsduh_15$age, levels = c("17 y/o", "18 y/o", "19 y/o",
                                                                   "20 y/o", "21 y/o,", "22 or 23 y/o", "24 or 25 y/o",
                                                 "Between 26 and 29 y/o", "Between 30 and 34 y/o", "Between 35 and 49 y/o",
                                                 "Between 50 and 64 y/o", "65+"))




# Focus on one aspect of socioeconomic status
# Recode income RC- Total Family Income
nsduh_15$inc <- NA
nsduh_15$inc[nsduh_15$income == 1] <- "Less than $20,000"
nsduh_15$inc[nsduh_15$income == 2] <- "$20,000 - $49,999"
nsduh_15$inc[nsduh_15$income == 3] <- "$50,000 - $74,999"
nsduh_15$inc[nsduh_15$income == 4] <- "$75,000+"
nsduh_15$inc <- factor(nsduh_15$inc, levels = c("Less than $20,000", "$20,000 - $49,999",
                                                "$50,000 - $74,999", "$75,000+"))


# DSM-4
# Recode Illicit drug or alcohol dependence - past year, binary
nsduh_15$doad <- NA
nsduh_15$doad <- nsduh_15$dppyillalc


# RC- Illicit drug or alcohol dependence OR abuse - past year, binary
nsduh_15$drugab <- NA
nsduh_15$drugab <- nsduh_15$udpyilal




# Recode binary SUD checking drugab, doad
nsduh_15$sud <- NA
nsduh_15$sud <- ifelse(nsduh_15$doad == 1 | nsduh_15$drugab == 1, 1, 0)



# Recoded drug treatment 
# RC - Received treatment for drug or alcohol use in lifetime
nsduh_15$treatment <- NA
nsduh_15$treatment[nsduh_15$TXEVRRCVD2 == 1] <- "Yes"
nsduh_15$treatment[nsduh_15$TXEVRRCVD2 == 0] <- "No"
nsduh_15$treatment <- ordered(nsduh_15$treatment, levels = c("Yes", "No"))


# Recoded mental health treatment
# RC - Received any mental health treatment in the past year
nsduh_15$mentalheal <- NA
nsduh_15$mentalheal[nsduh_15$AMHTXRC3 == 1] <- "Yes"
nsduh_15$mentalheal[nsduh_15$AMHTXRC3 == 2] <- "No"
nsduh_15$mentalheal <- ordered(nsduh_15$mentalheal, levels = c("Yes", "No"))



# Checkpoint 4 revisions and comments from Phil:
# Binary measure for professional support received
nsduh_15$prof_support <- NA
nsduh_15$prof_support <- ifelse(nsduh_15$treatment == "Yes" | nsduh_15$mentalheal == "Yes", 1, 0)



# Categorical measure combining drug & alc treatment and mental health treatment
# Reference group for regression: group responding to "None".
nsduh_15$support_cat <- NA
nsduh_15$support_cat <- factor(with(nsduh_15, ifelse(treatment == "Yes" & mentalheal == "Yes", "Both",
                                                         ifelse(treatment == "Yes", "Drug & Alc Treatment",
                                                                ifelse(mentalheal == "Yes", "Mental Health", "None"))),
                                      levels = c("None", "Drug & Alc Treatment", "Mental Health", "Both")))



# Recode combat zone. Ever in combat zone on active duty - COMBATPY
nsduh_15$combat <- NA
nsduh_15$combat[nsduh_15$combatpy == 1] <- "Yes, in combat"
nsduh_15$combat[nsduh_15$combatpy == 2] <- "No, not in combat"
nsduh_15$combat <- ordered(nsduh_15$combat, levels = c("Yes, in combat", "No, not in combat"))


# For testing interactions between combat service and support,
# we need to create a binary variable for combat veterans
nsduh_15$combat_vet <- ifelse(nsduh_15$combat == "Yes, in combat", 1, 0)



# Service in forces
nsduh_15$forces <- NA
nsduh_15$forces[nsduh_15$service == 1] <- "Yes, served in US forces"
nsduh_15$forces[nsduh_15$service == 2] <- "No, did not serve"
nsduh_15$forces <- ordered(nsduh_15$forces, levels = c("Yes, served in US forces", "No, did not serve"))



#drop missing and pare down to smaller dataset of only needed variables
nsduh15df <- na.omit(subset(nsduh_15, , c(combat, forces, combat_vet, support_cat, prof_support,
                                   mentalheal, treatment, drugab, doad, inc, age, sud)))




# combine all data from 2015 - 2020
# combined_data <- rbind(nsduh17df, nsduh18df)




table1(~  inc +treatment +mentalheal +age | forces, overall=F, data=df, topclass="Rtable1-zebra",
       caption = "<B>Descriptive Statistics by Veteran Adults</B>")


baseline <- lm(sud ~ 1, data = df)
summary(baseline)


m1 <- lm(sud ~ prof_support, data = df)

summary(m1)







