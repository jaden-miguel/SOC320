# Name: Jaden Ji Miguel
# Prof. Pendergast
# Date: 11/24/2023

# NSDUH 2017

library(haven)
library(table1)

#install.packages("ggplot2")
library(ggplot2)


getwd()
setwd("D:/")
load("D:/NSDUH-2017-DONE/NSDUH_2017.RData")

nsduh_17 <- subset(PUF2017_100918, service == 1)


# Recode income: X1 predictor variable
# Reference group: Less than 20,000
nsduh_17$inc <- NA
nsduh_17$inc[nsduh_17$income == 1] <- "Less than $20,000"
nsduh_17$inc[nsduh_17$income == 2] <- "$20,000 - $49,999"
nsduh_17$inc[nsduh_17$income == 3] <- "$50,000 - $74,999"
nsduh_17$inc[nsduh_17$income == 4] <- "$75,000+"
nsduh_17$inc <- factor(nsduh_17$inc, levels = c("Less than $20,000", "$20,000 - $49,999",
                                                "$50,000 - $74,999", "$75,000+"))



# Recode Age
nsduh_17$age <- NA
nsduh_17$age[nsduh_17$AGE2 == 6] <- "17 y/o"
nsduh_17$age[nsduh_17$AGE2 == 7] <- "18 y/o"
nsduh_17$age[nsduh_17$AGE2 == 8] <- "19 y/o"
nsduh_17$age[nsduh_17$AGE2 == 9] <- "20 y/o"
nsduh_17$age[nsduh_17$AGE2 == 10] <- "21 y/o"
nsduh_17$age[nsduh_17$AGE2 == 11] <- "22 or 23 y/o"
nsduh_17$age[nsduh_17$AGE2 == 12] <- "24 or 25 y/o"
nsduh_17$age[nsduh_17$AGE2 == 13] <- "Between 26 and 29 y/o"
nsduh_17$age[nsduh_17$AGE2 == 14] <- "Between 30 and 34 y/o"
nsduh_17$age[nsduh_17$AGE2 == 15] <- "Between 35 and 49 y/o"
nsduh_17$age[nsduh_17$AGE2 == 16] <- "Between 50 and 64 y/o"
nsduh_17$age[nsduh_17$AGE2 == 17] <- "65+"

nsduh_17$age <- ordered(nsduh_17$age, levels = c("17 y/o", "18 y/o", "19 y/o", "20 y/o", "21 y/o",
                                                 "22 or 23 y/o", "24 or 25 y/o", "Between 26 and 29 y/o",
                                                 "Between 30 and 34 y/o", "Between 35 and 49 y/o",
                                                 "Between 50 and 64 y/o", "65+"))



# Recode combat zone
# EVER IN COMBAT ZONE ON ACTIVE DUTY?
nsduh_17$combat <- NA
nsduh_17$combat[nsduh_17$combatpy == 1] <- "Yes, in combat"
nsduh_17$combat[nsduh_17$combatpy == 2] <- "No, not in combat"
nsduh_17$combat <- ordered(nsduh_17$combat, levels = c("Yes, in combat", "No, not in combat"))



# For testing interactions between combat service and support,
# we need to create a binary variable for combat veterans
nsduh_17$combat_vet <- ifelse(nsduh_17$combat == "Yes, in combat", 1, 0)




# Recode forces for graphical purposes
nsduh_17$forces <- NA
nsduh_17$forces[nsduh_17$service == 1] <- "Yes, served in US forces"
nsduh_17$forces[nsduh_17$service == 2] <- "No, did not serve"
nsduh_17$forces <- ordered(nsduh_17$forces, levels = c("Yes, served in US forces", "No, did not serve"))




#########################################################################
#########################################################################
#########################################################################
# Recode X2

# Recode drug treatment
# RC - RECEIVED TREATMENT FOR DRUG OR ALCOHOL USE IN LIFETIME
# BINARY: 1 = Yes, 0 = No.
nsduh_17$treatment <- NA
nsduh_17$treatment[nsduh_17$TXEVRRCVD2 == 1] <- "Yes"
nsduh_17$treatment[nsduh_17$TXEVRRCVD2 == 0] <- "No"
nsduh_17$treatment <- ordered(nsduh_17$treatment, levels = c("Yes", "No"))



# Recode mental health treatment
# RC - RCVD ANY MENTAL HEALTH TRT IN PAST YEAR
nsduh_17$mentalheal <- NA
nsduh_17$mentalheal[nsduh_17$AMHTXRC3 == 1] <- "Yes"
nsduh_17$mentalheal[nsduh_17$AMHTXRC3 == 2] <- "No"
nsduh_17$mentalheal <- ordered(nsduh_17$mentalheal, levels = c("Yes", "No"))



# Checkpoint 4 revisions and comments from Phil:
# Binary measure for professional support received
nsduh_17$prof_support <- NA
nsduh_17$prof_support <- ifelse(nsduh_17$treatment == "Yes" | nsduh_17$mentalheal == "Yes", 1, 0)



# Categorical measure combining drug & alc treatment and mental health treatment
nsduh_17$support_cat <- NA
nsduh_17$support_cat <- factor(with(nsduh_17, ifelse(treatment == "Yes" & mentalheal == "Yes", "Both",
                                                     ifelse(treatment == "Yes", "Drug & Alc Treatment",
                                                            ifelse(mentalheal == "Yes", "Mental Health", "None"))),
                                    levels = c("None", "Drug & Alc Treatment", "Mental Health", "Both")))




##################################################################




# DSM IV (4)
# RC - ILLICIT DRUG OR ALCOHOL DEPENDENCE - PAST YEAR
# 1 = Yes, 0 = No.
nsduh_17$doad <- NA
nsduh_17$doad <- nsduh_17$dppyillalc


# drug abuse
# RC - ILLICIT DRUG OR ALCOHOL DEPENDENCE OR ABUSE - PAST YEAR
# Binary, 1 = Yes, 0 = No.
nsduh_17$drugab <- NA
nsduh_17$drugab <- nsduh_17$udpyilal



# Recode for SUD:
nsduh_17$sud <- NA
nsduh_17$sud <- ifelse((nsduh_17$doad == 1) | (nsduh_17$drugab == 1), 1, 0)





#drop missing and pare down to smaller dataset of only needed variables
nsduh17df <- na.omit(subset(nsduh_17, , c(combat, forces, combat_vet, support_cat, prof_support,
                                   mentalheal, treatment, drugab, doad, inc, age, sud)))





# View(df)


# library(ggplot2)

# ggplot(data = df, aes(y = support_cat)) + geom_bar()

# ggplot(data = df, aes(y = inc)) + geom_bar()




model1 <- lm(sud ~ inc + support_cat + inc:support_cat, data = df)

summary(model1)



# combined_data <- rbind(nsduh17df, nsduh18df)





