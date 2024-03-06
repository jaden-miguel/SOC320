# Name: Jaden Ji Miguel
# Prof. Pendergast
# Date: 11/24/2023

# NSDUH 2020 updated code
library(haven)
library(table1)
library(ggplot2)

getwd()

load("D:/NSDUH-2020-DONE/NSDUH_2020.RData")


# Keep: combat, forces, combat_vet, support_cat, prof_support,
# mentalheal, treatment, drugab, doad, inc, age, sud)))

nsduh_20 <- subset(NSDUH_2020, service == 1)


# Recode income
# RC - TOTAL FAMILY INCOME RECODE
nsduh_20$inc <- NA
nsduh_20$inc[nsduh_20$income == 1] <- "Less than $20,000"
nsduh_20$inc[nsduh_20$income == 2] <- "$20,000 - $49,999"
nsduh_20$inc[nsduh_20$income == 3] <- "$50,000 - $74,999"
nsduh_20$inc[nsduh_20$income == 4] <- "$75,000+"

nsduh_20$inc <- factor(nsduh_20$inc, levels = c("Less than $20,000", "$20,000 - $49,999",
                                                "$50,000 - $74,999", "$75,000+"))


# Recode Age
nsduh_20$age <- NA
nsduh_20$age[nsduh_20$AGE2 == 6] <- "17 y/o"
nsduh_20$age[nsduh_20$AGE2 == 7] <- "18 y/o"
nsduh_20$age[nsduh_20$AGE2 == 8] <- "19 y/o"
nsduh_20$age[nsduh_20$AGE2 == 9] <- "20 y/o"
nsduh_20$age[nsduh_20$AGE2 == 10] <- "21 y/o"
nsduh_20$age[nsduh_20$AGE2 == 11] <- "22 or 23 y/o"
nsduh_20$age[nsduh_20$AGE2 == 12] <- "24 or 25 y/o"
nsduh_20$age[nsduh_20$AGE2 == 13] <- "Between 26 and 29 y/o"
nsduh_20$age[nsduh_20$AGE2 == 14] <- "Between 30 and 34 y/o"
nsduh_20$age[nsduh_20$AGE2 == 15] <- "Between 35 and 49 y/o"
nsduh_20$age[nsduh_20$AGE2 == 16] <- "Between 50 and 64 y/o"
nsduh_20$age[nsduh_20$AGE2 == 17] <- "65+"

nsduh_20$age <- ordered(nsduh_20$age, levels = c("17 y/o", "18 y/o", "19 y/o", "20 y/o", "21 y/o",
                                                 "22 or 23 y/o", "24 or 25 y/o", "Between 26 and 29 y/o",
                                                 "Between 30 and 34 y/o", "Between 35 and 49 y/o",
                                                 "Between 50 and 64 y/o", "65+"))




# Recode combat zone
# EVER BEEN IN COMBAT ZONE ON ACTIVE DUTY
nsduh_20$combat <- NA
nsduh_20$combat[nsduh_20$combatpy == 1] <- "Yes, in combat"
nsduh_20$combat[nsduh_20$combatpy == 2] <- "No, not in combat"

nsduh_20$combat <- ordered(nsduh_20$combat, levels = c("Yes, in combat", "No, not in combat"))


# For testing interactions between combat service and support,
# we need to create a binary variable for combat veterans
nsduh_20$combat_vet <- ifelse(nsduh_20$combat == "Yes, in combat", 1, 0)


# Recode forces for graphical purposes
nsduh_20$forces <- NA
nsduh_20$forces[nsduh_20$service == 1] <- "Yes, served in US forces"
nsduh_20$forces[nsduh_20$service == 2] <- "No, did not serve"
nsduh_20$forces <- ordered(nsduh_20$forces, levels = c("Yes, served in US forces", "No, did not serve"))


#####################
# Recode X2 ########
#####################

# Recode drug treatment
# RC - RCVD TREATMENT ANY LOCATION FOR ILLICIT DRUG OR ALCOHOL USE
# IN LIFETIME.
# BINARY: 1 = Yes, 0 = No.
nsduh_20$treatment <- NA
nsduh_20$treatment[nsduh_20$TXEVRRCVD2 == 1] <- "Yes"
nsduh_20$treatment[nsduh_20$TXEVRRCVD2 == 0] <- "No"
nsduh_20$treatment <- ordered(nsduh_20$treatment, levels = c("Yes", "No"))



# Recode mental health treatment
# RC - RCVD ANY MENTAL HEALTH TRT IN PAST YEAR
nsduh_20$mentalheal <- NA
nsduh_20$mentalheal[nsduh_20$AMHTXRC3 == 1] <- "Yes"
nsduh_20$mentalheal[nsduh_20$AMHTXRC3 == 2] <- "No"
nsduh_20$mentalheal <- ordered(nsduh_20$mentalheal, levels = c("Yes", "No"))




# Checkpoint 4 revisions and comments from Phil:
# Binary measure for professional support received
nsduh_20$prof_support <- NA
nsduh_20$prof_support <- ifelse(nsduh_20$treatment == "Yes" | nsduh_20$mentalheal == "Yes", 1, 0)



# Categorical measure combining drug & alc treatment and mental health treatment
nsduh_20$support_cat <- NA
nsduh_20$support_cat <- factor(with(nsduh_20, ifelse(treatment == "Yes" & mentalheal == "Yes", "Both",
                                                     ifelse(treatment == "Yes", "Drug & Alc Treatment",
                                                            ifelse(mentalheal == "Yes", "Mental Health", "None"))),
                                    levels = c("None", "Drug & Alc Treatment", "Mental Health", "Both")))



##################################################################




# DSM IV (4)
# RC - ILLICIT DRUG OR ALCOHOL DEPENDENCE - PAST YEAR
# 1 = Yes, 0 = No.
nsduh_20$doad <- NA
nsduh_20$doad <- nsduh_20$dppyillalc


# drug abuse
# RC - ILLICIT DRUG OR ALCOHOL DEPENDENCE OR ABUSE - PAST YEAR
# Binary, 1 = Yes, 0 = No.
nsduh_20$drugab <- NA
nsduh_20$drugab <- nsduh_20$udpyilal



# Recode for SUD:
nsduh_20$sud <- NA
nsduh_20$sud <- ifelse((nsduh_20$doad == 1) | (nsduh_20$drugab == 1), 1, 0)





#drop missing and pare down to smaller dataset of only needed variables
nsduh20df <- na.omit(subset(nsduh_20, , c(combat, forces, combat_vet, support_cat, prof_support,
                                          mentalheal, treatment, drugab, doad, inc, age, sud)))




model1 <- lm(sud ~ inc + support_cat, data = nsduh20df)

summary(model1)


