# NSDUH 2016
# Jaden Ji Miguel


library(haven)
library(table1)
#install.packages("ggplot2")
library(ggplot2)

getwd()
load("D:/NSDUH-2016-DONE/NSDUH_2016.RData")

nsduh_16 <- subset(PUF2016_022818, service == 1)

# Recode income
nsduh_16$inc <- NA
nsduh_16$inc[nsduh_16$income == 1] <- "Less than $20,000"
nsduh_16$inc[nsduh_16$income == 2] <- "$20,000 - $49,999"
nsduh_16$inc[nsduh_16$income == 3] <- "$50,000 - $74,999"
nsduh_16$inc[nsduh_16$income == 4] <- "$75,000+"
nsduh_16$inc <- factor(nsduh_16$inc, levels = c("Less than $20,000", "$20,000 - $49,999",
                                                                "$50,000 - $74,999", "$75,000+"))


# Recode age
# FINAL EDITED AGE
nsduh_16$age <- NA
nsduh_16$age[nsduh_16$AGE2 == 6] <- "17 y/o"
nsduh_16$age[nsduh_16$AGE2 == 7] <- "18 y/o"
nsduh_16$age[nsduh_16$AGE2 == 8] <- "19 y/o"
nsduh_16$age[nsduh_16$AGE2 == 9] <- "20 y/o"
nsduh_16$age[nsduh_16$AGE2 == 10] <- "21 y/o"
nsduh_16$age[nsduh_16$AGE2 == 11] <- "22 or 23 y/o"
nsduh_16$age[nsduh_16$AGE2 == 12] <- "24 or 25 y/o"
nsduh_16$age[nsduh_16$AGE2 == 13] <- "Between 26 and 29 y/o"
nsduh_16$age[nsduh_16$AGE2 == 14] <- "Between 30 and 34 y/o"
nsduh_16$age[nsduh_16$AGE2 == 15] <- "Between 35 and 49 y/o"
nsduh_16$age[nsduh_16$AGE2 == 16] <- "Between 50 and 64 y/o"
nsduh_16$age[nsduh_16$AGE2 == 17] <- "65+"

nsduh_16$age <- ordered(nsduh_16$age, levels = c("17 y/o", "18 y/o", "19 y/o", "20 y/o", "21 y/o",
                                                 "22 or 23 y/o", "24 or 25 y/o", "Between 26 and 29 y/o",
                                                 "Between 30 and 34 y/o", "Between 35 and 49 y/o",
                                                 "Between 50 and 64 y/o", "65+"))




# Recode combat zone
# EVER BEEN IN COMBAT ZONE ON ACTIVE DUTY
nsduh_16$combat <- NA
nsduh_16$combat[nsduh_16$combatpy == 1] <- "Yes, in combat"
nsduh_16$combat[nsduh_16$combatpy == 2] <- "No, not in combat"

nsduh_16$combat <- ordered(nsduh_16$combat, levels = c("Yes, in combat", "No, not in combat"))




# For testing interactions between combat service and support,
# we need to create a binary variable for combat veteran 
nsduh_16$combat_vet <- ifelse(nsduh_16$combat == "Yes, in combat", 1, 0)



#####################
# Recode X2 ########
#####################

# Recode drug treatment
# RC - RCVD TREATMENT ANY LOCATION FOR ILLICIT DRUG OR ALCOHOL USE
# IN LIFETIME.
# BINARY: 1 = Yes, 0 = No.
nsduh_16$treatment <- NA
nsduh_16$treatment[nsduh_16$TXEVRRCVD2 == 1] <- "Yes"
nsduh_16$treatment[nsduh_16$TXEVRRCVD2 == 0] <- "No"
nsduh_16$treatment <- ordered(nsduh_16$treatment, levels = c("Yes", "No"))



# Recode mental health treatment
# RC - RCVD ANY MENTAL HEALTH TRT IN PAST YEAR
nsduh_16$mentalheal <- NA
nsduh_16$mentalheal[nsduh_16$AMHTXRC3 == 1] <- "Yes"
nsduh_16$mentalheal[nsduh_16$AMHTXRC3 == 2] <- "No"
nsduh_16$mentalheal <- ordered(nsduh_16$mentalheal, levels = c("Yes", "No"))




# Checkpoint 4 revisions and comments from Phil:
# Binary measure for professional support received
nsduh_16$prof_support <- NA
nsduh_16$prof_support <- ifelse(nsduh_16$treatment == "Yes" | nsduh_16$mentalheal == "Yes", 1, 0)



# Categorical measure combining drug & alc treatment and mental health treatment
nsduh_16$support_cat <- NA
nsduh_16$support_cat <- factor(with(nsduh_16, ifelse(treatment == "Yes" & mentalheal == "Yes", "Both",
                                                     ifelse(treatment == "Yes", "Drug & Alc Treatment",
                                                            ifelse(mentalheal == "Yes", "Mental Health", "None"))),
                                    levels = c("None", "Drug & Alc Treatment", "Mental Health", "Both")))



##################################################################




# DSM IV (4)
# RC - ILLICIT DRUG OR ALCOHOL DEPENDENCE - PAST YEAR
# 1 = Yes, 0 = No.
nsduh_16$doad <- NA
nsduh_16$doad <- nsduh_16$dppyillalc




# drug abuse
# RC - ILLICIT DRUG OR ALCOHOL DEPENDENCE OR ABUSE - PAST YEAR
# Binary, 1 = Yes, 0 = No.
nsduh_16$drugab <- NA
nsduh_16$drugab <- nsduh_16$udpyilal



# Recode for SUD:
nsduh_16$sud <- NA
nsduh_16$sud <- ifelse((nsduh_16$doad == 1) | (nsduh_16$drugab == 1), 1, 0)


# Create forces recode for graphical purposes
nsduh_16$forces <- NA
nsduh_16$forces[nsduh_16$service == 1] <- "Yes, served in US forces"
nsduh_16$forces[nsduh_16$service == 2] <- "No, did not serve"
nsduh_16$forces <- ordered(nsduh_16$forces, levels = c("Yes, served in US forces", "No, did not serve"))


#drop missing and pare down to smaller dataset of only needed variables
nsduh16df <- na.omit(subset(nsduh_16, , c(combat, forces, combat_vet, support_cat, prof_support,
                                          mentalheal, treatment, drugab, doad, inc, age, sud)))




# Run regression


model1 <- lm(sud ~ inc + support_cat, data = nsduh16df)

summary(model1)




