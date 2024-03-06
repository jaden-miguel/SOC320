# Name: Jaden Ji Miguel
# Prof. Pendergast
# Date: 11/24/2023

# NSDUH 2019

library(haven)
library(table1)

install.packages("ggplot2")
library(ggplot2)

getwd()

load("D:/NSDUH-2019-DONE/NSDUH_2019.RData")


# Subset data to veterans only, Yes to ever been in US Armed Forces
nsduh_19 <- subset(PUF2019_100920, service == 1)


# Recode income:
# RC - Total Family Income Recode
nsduh_19$inc <- NA
nsduh_19$inc[nsduh_19$income == 1] <- "Less than $20,000"
nsduh_19$inc[nsduh_19$income == 2] <- "$20,000 - $49,999"
nsduh_19$inc[nsduh_19$income == 3] <- "$50,000 - $74,999"
nsduh_19$inc[nsduh_19$income == 4] <- "$75,000+"
nsduh_19$inc <- factor(nsduh_19$inc, levels = c("Less than $20,000", "$20,000 - $49,999",
                                                "$50,000 - $74,999", "$75,000+"))


# DSM IV (4)
# Recode: RC-Illicit drug or alcohol dependence - past year
# Binary, 1 = Yes, 0 = No.
nsduh_19$doad <- NA
nsduh_19$doad <- nsduh_19$dppyillalc



# Recode: RC-Illicit drug or alcohol dependence OR abuse - past year
# Binary, 1 = Yes, 0 = No.
nsduh_19$drugab <- NA
nsduh_19$drugab <- nsduh_19$udpyilal

# Recode for SUD:
nsduh_19$sud <- NA
nsduh_19$sud <- ifelse((nsduh_19$doad == 1) | (nsduh_19$drugab == 1), 1, 0)


# Recoded drug treatment: 
# RC-RCVD TRT ANY LOC FOR ILL DRUG OR ALC USE IN LIFETIME
nsduh_19$treatment <- NA
nsduh_19$treatment[nsduh_19$TXEVRRCVD2 == 1] <- "Yes"
nsduh_19$treatment[nsduh_19$TXEVRRCVD2 == 0] <- "No"
nsduh_19$treatment <- ordered(nsduh_19$treatment, levels = c("Yes", "No"))



# Recoded mental health treatment
# RC-RCVD ANY MENTAL HEALTH TRT IN PST YR
nsduh_19$mentalheal <- NA
nsduh_19$mentalheal[nsduh_19$AMHTXRC3 == 1] <- "Yes"
nsduh_19$mentalheal[nsduh_19$AMHTXRC3== 2] <- "No"
nsduh_19$mentalheal <- ordered(nsduh_19$mentalheal, levels = c("Yes", "No"))



# Checkpoint 4 revisions and comments from Phil:
# Binary measure for professional support received
# 1 = Recieved Treatment or MH services, 0 = Did not access services
nsduh_19$prof_support <- NA
nsduh_19$prof_support <- ifelse(nsduh_19$treatment == "Yes" | nsduh_19$mentalheal == "Yes", 1, 0)




# Categorical measure combining drug & alc treatment and mental health treatment
# Reference group for regression: group responding to "None".
nsduh_19$support_cat <- NA
nsduh_19$support_cat <- factor(with(nsduh_19, ifelse(treatment == "Yes" & mentalheal == "Yes", "Both",
                                                     ifelse(treatment == "Yes", "Drug & Alc Treatment",
                                                            ifelse(mentalheal == "Yes", "Mental Health", "None"))),
                                    levels = c("None", "Drug & Alc Treatment", "Mental Health", "Both")))





# Recode combat zone. Ever in combat zone on active duty - COMBATPY
nsduh_19$combat <- NA
nsduh_19$combat[nsduh_19$combatpy == 1] <- "Yes, in combat"
nsduh_19$combat[nsduh_19$combatpy == 2] <- "No, not in combat"
nsduh_19$combat <- ordered(nsduh_19$combat, levels = c("Yes, in combat", "No, not in combat"))


# For testing interactions between combat service and support,
# we need to create a binary variable for combat veterans
nsduh_19$combat_vet <- ifelse(nsduh_19$combat == "Yes, in combat", 1, 0)



# Service in forces - factor
nsduh_19$forces <- NA
nsduh_19$forces[nsduh_19$service == 1] <- "Yes, served in US forces"
nsduh_19$forces[nsduh_19$service == 2] <- "No, did not serve"
nsduh_19$forces <- ordered(nsduh_19$forces, levels = c("Yes, served in US forces", "No, did not serve"))









# Recode - Final edited Age
# Since 17 years old is the age of entry in the military, we account for only this cohort
nsduh_19$age <- NA
nsduh_19$age[nsduh_19$AGE2 == 6] <- "17 y/o"
nsduh_19$age[nsduh_19$AGE2 == 7] <- "18 y/o"
nsduh_19$age[nsduh_19$AGE2 == 8] <- "19 y/o"
nsduh_19$age[nsduh_19$AGE2 == 9] <- "20 y/o"
nsduh_19$age[nsduh_19$AGE2 == 10] <- "21 y/o"
nsduh_19$age[nsduh_19$AGE2 == 11] <- "22 or 23 y/o"
nsduh_19$age[nsduh_19$AGE2 == 12] <- "24 or 25 y/o"
nsduh_19$age[nsduh_19$AGE2 == 13] <- "Between 26 and 29 y/o"
nsduh_19$age[nsduh_19$AGE2 == 14] <- "Between 30 and 34 y/o"
nsduh_19$age[nsduh_19$AGE2 == 15] <- "Between 35 and 49 y/o"
nsduh_19$age[nsduh_19$AGE2 == 16] <- "Between 50 and 64 y/o"
nsduh_19$age[nsduh_19$AGE2 == 17] <- "65 years+"
nsduh_19$age <- ordered(nsduh_19$age, levels = c("17 y/o", "18 y/o", "19 y/o", "20 y/o",
                                                 "21 y/o", "22 or 23 y/o", "24 or 25 y/o",
                                                 "Between 26 and 29 y/o", "Between 30 and 34 y/o",
                                                 "Between 35 and 49 y/o,", "Between 50 and 64 y/o",
                                                 "65 years+"))








#drop missing and pare down to smaller dataset of only needed variables
nsduh19df <- na.omit(subset(nsduh_19, , c(combat, forces, combat_vet, support_cat, prof_support,
                                   mentalheal, treatment, drugab, doad, inc, age, sud)))





