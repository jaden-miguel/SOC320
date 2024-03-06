# Name: Jaden Ji Miguel
# Prof. Pendergast
# Date: 11/24/2023

# NSDUH 2018

library(haven)
library(table1)

#install.packages("ggplot2")
library(ggplot2)


getwd()
load("D:/NSDUH-2018-DONE/NSDUH_2018.RData")

nsduh_18 <- subset(PUF2018_100819, service == 1)


# Recode income:
nsduh_18$inc <- NA
nsduh_18$inc[nsduh_18$income == 1] <- "Less than $20,000"
nsduh_18$inc[nsduh_18$income == 2] <- "$20,000 - $49,999"
nsduh_18$inc[nsduh_18$income == 3] <- "$50,000 - $74,999"
nsduh_18$inc[nsduh_18$income == 4] <- "$75,000+"
nsduh_18$inc <- factor(nsduh_18$inc, levels = c("Less than $20,000", "$20,000 - $49,999",
                                                "$50,000 - $74,999", "$75,000+"))

# DSM IV (4)
# RC - Illicit Drug or Alcohol dependence - past year
# Binary, 1 = Yes, 0 = No.
nsduh_18$doad <- NA
nsduh_18$doad <- nsduh_18$dppyillalc



# Recode: RC-Illicit drug or alcohol dependence OR abuse - past year
# Binary, 1 = Yes, 0 = No.
nsduh_18$drugab <- NA
nsduh_18$drugab <- nsduh_18$udpyilal



# Recode for SUD:
nsduh_18$sud <- NA
nsduh_18$sud <- ifelse((nsduh_18$doad == 1) | (nsduh_18$drugab == 1), 1, 0)



# Recoded drug treatment: 
# RC-RCVD TRT ANY LOC FOR ILL DRUG OR ALC USE IN LIFETIME
nsduh_18$treatment <- NA
nsduh_18$treatment[nsduh_18$TXEVRRCVD2 == 1] <- "Yes"
nsduh_18$treatment[nsduh_18$TXEVRRCVD2 == 0] <- "No"
nsduh_18$treatment <- ordered(nsduh_18$treatment, levels = c("Yes", "No"))



# Recoded mental health treatment
# RC-RCVD ANY MENTAL HEALTH TRT IN PST YR
nsduh_18$mentalheal <- NA
nsduh_18$mentalheal[nsduh_18$AMHTXRC3 == 1] <- "Yes"
nsduh_18$mentalheal[nsduh_18$AMHTXRC3== 2] <- "No"
nsduh_18$mentalheal <- ordered(nsduh_18$mentalheal, levels = c("Yes", "No"))



# Checkpoint 4 revisions and comments from Phil:
# Binary measure for professional support received
nsduh_18$prof_support <- NA
nsduh_18$prof_support <- ifelse(nsduh_18$treatment == "Yes" | nsduh_18$mentalheal == "Yes", 1, 0)



# Categorical measure combining drug & alc treatment and mental health treatment
# Reference group for regression: group responding to "None".
nsduh_18$support_cat <- NA
nsduh_18$support_cat <- factor(with(nsduh_18, ifelse(treatment == "Yes" & mentalheal == "Yes", "Both",
                                                     ifelse(treatment == "Yes", "Drug & Alc Treatment",
                                                            ifelse(mentalheal == "Yes", "Mental Health", "None"))),
                                    levels = c("None", "Drug & Alc Treatment", "Mental Health", "Both")))






# Recode combat zone. Ever in combat zone on active duty - COMBATPY
nsduh_18$combat <- NA
nsduh_18$combat[nsduh_18$combatpy == 1] <- "Yes, in combat"
nsduh_18$combat[nsduh_18$combatpy == 2] <- "No, not in combat"
nsduh_18$combat <- ordered(nsduh_18$combat, levels = c("Yes, in combat", "No, not in combat"))



# For testing interactions between combat service and support,
# we need to create a binary variable for combat veterans
nsduh_18$combat_vet <- ifelse(nsduh_18$combat == "Yes, in combat", 1, 0)






# Service in forces - factor
nsduh_18$forces <- NA
nsduh_18$forces[nsduh_18$service == 1] <- "Yes, served in US forces"
nsduh_18$forces[nsduh_18$service == 2] <- "No, did not serve"
nsduh_18$forces <- ordered(nsduh_18$forces, levels = c("Yes, served in US forces", "No, did not serve"))




# Recode - Final edited Age
# Since 17 years old is the age of entry in the military, we account for only this cohort
nsduh_18$age <- NA
nsduh_18$age[nsduh_18$AGE2 == 6] <- "17 y/o"
nsduh_18$age[nsduh_18$AGE2 == 7] <- "18 y/o"
nsduh_18$age[nsduh_18$AGE2 == 8] <- "19 y/o"
nsduh_18$age[nsduh_18$AGE2 == 9] <- "20 y/o"
nsduh_18$age[nsduh_18$AGE2 == 10] <- "21 y/o"
nsduh_18$age[nsduh_18$AGE2 == 11] <- "22 or 23 y/o"
nsduh_18$age[nsduh_18$AGE2 == 12] <- "24 or 25 y/o"
nsduh_18$age[nsduh_18$AGE2 == 13] <- "Between 26 and 29 y/o"
nsduh_18$age[nsduh_18$AGE2 == 14] <- "Between 30 and 34 y/o"
nsduh_18$age[nsduh_18$AGE2 == 15] <- "Between 35 and 49 y/o"
nsduh_18$age[nsduh_18$AGE2 == 16] <- "Between 50 and 64 y/o"
nsduh_18$age[nsduh_18$AGE2 == 17] <- "65 years+"
nsduh_18$age <- ordered(nsduh_18$age, levels = c("17 y/o", "18 y/o", "19 y/o", "20 y/o",
                                                 "21 y/o", "22 or 23 y/o", "24 or 25 y/o",
                                                 "Between 26 and 29 y/o", "Between 30 and 34 y/o",
                                                 "Between 35 and 49 y/o,", "Between 50 and 64 y/o",
                                                 "65 years+"))





#drop missing and pare down to smaller dataset of only needed variables
nsduh18df <- na.omit(subset(nsduh_18, , c(combat, forces, combat_vet, support_cat, prof_support,
                                   mentalheal, treatment, drugab, doad, inc, age, sud)))



