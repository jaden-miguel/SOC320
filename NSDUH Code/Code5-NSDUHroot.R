# Root file for merging data

install.packages("haven")
install.packages("ggplot2")
install.packages("table1")
install.packages("sjPlot")
library(ggplot2)
library(haven)
library(sjPlot)
library(table1)


#this turns off scientific notation in output displays
options(scipen=999)


final_data <- rbind(nsduh15df, nsduh16df, nsduh17df, nsduh18df, nsduh19df, nsduh20df)


# Prof note: instead of loading each year, could test code with my final data files.
# I saved the dataset and will turn it in with my code.
#getwd()
#load("C:\\Users\\miguelj2\\Downloads\\final_data.Rdata")
#setwd("D:/NSDUH Code")

# regression start

final_data$combat <- factor(final_data$combat, ordered = F)

final_data$support_cat <- relevel(final_data$support_cat, ref = "None")
final_data$combat <- relevel(final_data$combat, ref = "No, not in combat")
label(final_data$inc) <- "Total Family Income"
label(final_data$support_cat) <- "Support Category"
label(final_data$combat_vet) <- "Combat Veteran"
label(final_data$sud) <- "Substance Use Disorder (SUD): Veterans"
label(final_data$prof_support) <- "Professional Support Received"



# test using prof_support
m0 <- lm(sud ~ inc + prof_support, data = final_data)
tab_model(m0, show.se = T, show.ci = F, show.stat = T, digits = 3, p.style = "stars")


# Interpret LPM regression:
# Coefficients (Intercept): Baseline probability of having SUD, when other predictors are at reference levels.
# Reference group would be Less than $20,000 for our model.
# Reference for support would be None.
# Higher income brackets show a non-significant decrease in SUD probability.
# Receiving support, whether both types, drug & alc treatment, or mental health support, is significantly associated with an increased 
# likelihood of SUD. Being a combat veteran shows a non-significant slight increase in SUD probability. 
# The R-squared value is 0.069, indicating that about 6.9% of the variance in SUD
# is explained by the model. This is relatively low, suggesting other factors outside of the model
# may be important. Overall, the model explains a small portion of the variability among veterans.

m1 <- lm(sud ~ inc + support_cat + combat, data = final_data)
tab_model(m1, show.se = TRUE, show.ci = FALSE, show.stat = TRUE,
          digits = 3, p.style = "stars")


plot_model(m1, type = "pred", terms = c("inc", "support_cat"), dodge = 0.1)


# Moderation test
# RQ: Do veteran's socioeconomic status influence the occurrence of substance
# use disorders (SUD)? Does the level of support they receive play a role in
# modifying this relationship? Bonus: Does combat level moderate the relationship?
sup0 <- lm(sud ~ inc, data = final_data)
sup1 <- lm(sud ~ inc + support_cat, data = final_data)
sup2 <- lm(sud ~ inc + support_cat + (inc*support_cat), data = final_data)
sup3 <- lm(sud ~ inc + (inc*combat), data = final_data)

tab_model(sup0, sup1, sup2, sup3, title = "SUD: Veterans", show.se = T, show.ci = F, show.stat = T,
          digits = 3, dv.labels = c("Baseline", "Controls", "Moderation: Support", "Moderation: Combat"))


# Interpreting the Linear Probability Model (LPM) for Substance Use Disorder (SUD) among veterans

# Baseline Model Interpretation
# The positive significant intercept suggests a baseline prevalence of SUD.
# The significant positive intercept in the baseline model suggests that there is a baseline level of SUD prevalence 
# when all other variables are at their reference level.
# Income shows a negative association with SUD across all brackets, indicating
# higher income is related to lower SUD prevalence.

# Model with Controls Interpretation
# After introducing control variables:
# - The significant negative association between SUD and the $20,000-$49,999 income bracket disappears.
# - The $50,000-$74,999 income bracket retains a negative association.
# - The $75,000+ bracket shows no significant link to SUD.
# All support categories have a positive association with SUD, suggesting that those receiving
# any support have higher SUD prevalence. 'None' as a reference category is standard practice,
# ensuring it represents the baseline group without support services.

# Moderation by Support Interpretation
# Interaction terms mostly show no significant moderation by support categories,
# except for the $20,000-$49,999 and $75,000+ brackets when combined with 'Both' support,
# where a negative association with SUD is evident.

# Moderation by Combat Experience Interpretation
# Being a combat veteran does not significantly moderate the effect of income on SUD,
# as all interaction terms are non-significant.

# Overall Model Summary
# The LPM effectively captures the probability of SUD presence. It suggests that while higher income
# tends to be associated with lower SUD prevalence, receiving support correlates with higher SUD prevalence.
# However, moderation effects are largely non-significant, with few exceptions.
# The low adjusted R-squared values across models suggest additional factors may be influential in explaining SUD.

# Recommendations for Model Refinement
# - To improve model fit, I would consider exploring other predictors or interaction terms.
# - Assess variable coding and consider non-linear transformations if appropriate.
# - For more robust interpretations, further investigation into the nature of the support categories may be warranted.



# Looking at mean as opposed to the model
# proportion of people who have SUD
bar <- ggplot(final_data, aes(x = inc, y = sud))
bar + geom_bar(stat = "summary", fun = "mean", aes(fill = support_cat), position = "dodge")

# Based on inc & support_cat
bar2 <- ggplot(final_data, aes(x = support_cat, y = sud))
bar2 + geom_bar(stat = "summary", fun = "mean", aes(fill = inc), position = "dodge")

# Based on support_cat & combat
bar2 <- ggplot(final_data, aes(x = support_cat, y = sud))
bar2 + geom_bar(stat = "summary", fun = "mean", aes(fill = combat), position = "dodge")





summary(lm(sud ~ inc + support_cat, data = final_data))

getwd()
setwd("D:/NSDUH Code")
write_sav(final_data, "final_data.sav")
save(final_data, file = "final_data.Rdata")

