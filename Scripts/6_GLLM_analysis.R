##############################
##Generalize linear mixed model
##############################
rm(list=ls())
#load libraries
library(lme4); library(glmmTMB); library(dplyr); library(tidyr); library(DHARMa); library(ggplot2); library(MASS); library(readxl); library(patchwork)
library(sjPlot); library(ggeffects)

butterfly <- read_excel("~/Desktop/DataAnalysis/6_GLLM_data.xlsx")
head(butterfly) 

# Standardize temperature and precipitation
#site and month should be factor
butterfly <- butterfly %>% mutate(Sites = as.factor(Sites), Months = as.factor(Months), Temp_std = scale(Temperature), Prec_std = scale(Precipitation))

#histogram richness
ggplot(butterfly, aes(x = Richness)) + geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Richness", x = "Richness", y = "Count") + theme_minimal()
#positively skewed

#histogram abundance
ggplot(butterfly, aes(x = Abundance)) + geom_histogram(binwidth = 20, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Abundance", x = "Abundance", y = "Count") + theme_minimal()
#positive skewed

# Poisson GLMM for richness
fit.reduced <- glmer(Richness ~ Temp_std + Prec_std + (1 | Sites) + (1 | Months), 
                   family = poisson(link = "log"), 
                   data = butterfly)

model_no_sites <- glmer(Richness ~ Temp_std + Prec_std + (1 | Months),
                        family = poisson(link = "log"), data = butterfly)

model_no_months <- glmer(Richness ~ Temp_std + Prec_std + (1 | Sites),
                         family = poisson(link = "log"), data = butterfly)

anova(fit.reduced, model_no_sites)
anova(fit.reduced, model_no_months)

summary(fit.reduced)
summary(model_no_sites)
summary(model_no_months)

#Sites Variance = 0.1922, Std.Dev. = 0.4385
#Months Variance = 0.08082, Std.Dev. = 0.2843

AIC(fit.reduced); AIC(model_no_sites); AIC(model_no_months)
#267.789, 496.0648, 337.3893
#fit.reduced is best

#interaction term
fit.full <- glmer (Richness ~ Temp_std * Prec_std + (1 | Sites) + (1 | Months), 
                        family = poisson(link = "log"), 
                        data = butterfly)
fit.full1 <- glmer (Richness ~ Temp_std * Prec_std + (1 | Months), 
                   family = poisson(link = "log"), 
                   data = butterfly)
fit.full2 <- glmer (Richness ~ Temp_std * Prec_std + (1 | Sites), 
                   family = poisson(link = "log"), 
                   data = butterfly)
AIC(fit.full); AIC(fit.full1); AIC(fit.full2)
#257.0737, 484.3012, 266.4491

summary(fit.full); summary(fit.full1); summary(fit.full2)
#fit.full model is best

#diagnosis for model fit
sim_res <- simulateResiduals(fit.reduced)
plot(sim_res)

sim_res <- simulateResiduals(fit.full)
plot(sim_res)

shapiro.test(residuals(fit.full))
#W = 0.96838, p-value = 0.383
shapiro.test(residuals(fit.reduced))
#W = 0.97742, p-value = 0.6577 #Normal

#dispersion
overdispersion_stat <- sum(residuals(fit.reduced, type = "pearson")^2) / df.residual(fit.reduced)
overdispersion_stat
#0.8200303

overdispersion_stat <- sum(residuals(fit.full, type = "pearson")^2) / df.residual(fit.full)
overdispersion_stat # If > 1, consider using a Negative Binomial GLMM
#0.9108943 #fine

#shows red with fit.full for Dharma plot but 
#is best based on AIC and significant interaction

#look for negative binomial too
fit.nb <- glmmTMB(Richness ~ Temp_std * Prec_std + (1 | Sites) + (1 | Months), 
                  family = nbinom2, 
                  data = butterfly)

#diagnostics again
sim_res_nb <- simulateResiduals(fit.nb)
plot(sim_res_nb)
summary(fit.nb)
AIC(fit.nb)

overdispersion_stat <- sum(residuals(fit.nb, type = "pearson")^2) / df.residual(fit.nb)
overdispersion_stat
#0.8877066

shapiro.test(residuals(fit.nb))
#W = 0.9892, p-value = 0.9743

#stick with fit.full model

#########################################################################
#abundance
fit.reduced1 <- glmer (Abundance ~ Temp_std + Prec_std + (1 | Sites) + (1 | Months), 
                    family = poisson(link = "log"), 
                    data = butterfly)

model_no_sites1 <- glmer(Abundance ~ Temp_std + Prec_std + (1 | Months),
                         family = poisson(link = "log"), data = butterfly)

model_no_months1 <- glmer(Abundance ~ Temp_std + Prec_std + (1 | Sites),
                          family = poisson(link = "log"), data = butterfly)

fit.full1 <- glmer (Abundance ~ Temp_std * Prec_std + (1 | Sites) + (1 | Months), 
                            family = poisson(link = "log"), 
                            data = butterfly)

anova(fit.reduced1, model_no_sites1)
anova(fit.reduced1, model_no_months1)

AIC(fit.reduced1); AIC(model_no_sites1); AIC(model_no_months1); AIC(fit.full1)
#454.1674, 1354.874, 924.8799, 449.7381
#fit.full1 model is best

summary(fit.reduced1); summary(model_no_sites1); summary(model_no_months1); summary(fit.full1)
#fit.full1 model is best

sim_res1 <- simulateResiduals(fit.reduced1)
plot(sim_res1)

sim_res1 <- simulateResiduals(fit.full1)
plot(sim_res1)
#quantile deviations detected

shapiro.test(residuals(fit.reduced1))
#W = 0.95678, p-value = 0.1707 #Normal
shapiro.test(residuals(fit.full1))
#W = 0.95678, p-value = 0.1707 #Normal

#dispersion
overdispersion_stat1 <- sum(residuals(fit.reduced1, type = "pearson")^2) / df.residual(fit.reduced1)
overdispersion_stat1 # If > 1, consider using a Negative Binomial GLMM
#5.169726 #overdispersed

#dispersion
overdispersion_stat1 <- sum(residuals(fit.full1, type = "pearson")^2) / df.residual(fit.full1)
overdispersion_stat1 # If > 1, consider using a Negative Binomial GLMM
#5.347385 #overdispersed

#so fit negative binomial
fit.nb_reduced <- glmmTMB(Abundance ~ Temp_std + Prec_std + (1 | Sites) + (1 | Months),
                  family = nbinom2(link = "log"), data = butterfly)
fit.nb_reduced1 <- glmmTMB(Abundance ~ Temp_std + Prec_std + (1 | Months),
                  family = nbinom2(link = "log"), data = butterfly)
fit.nb_reduced2 <- glmmTMB(Abundance ~ Temp_std + Prec_std + (1 | Sites),
                  family = nbinom2(link = "log"), data = butterfly)
fit.nb_full <- glmmTMB(Abundance ~ Temp_std * Prec_std + (1 | Sites) + (1 | Months),
                   family = nbinom2(link = "log"), data = butterfly)
fit.nb_full1 <- glmmTMB(Abundance ~ Temp_std * Prec_std + (1 | Months),
                        family = nbinom2(link = "log"), data = butterfly)
fit.nb_full2 <- glmmTMB(Abundance ~ Temp_std * Prec_std + (1 | Sites),
                        family = nbinom2(link = "log"), data = butterfly)

AIC(fit.nb_reduced); AIC(fit.nb_reduced1); AIC(fit.nb_reduced2); AIC(fit.nb_full); AIC(fit.nb_full2); AIC(fit.nb_full2)
#372.9087, 395.8279, 378.805, 367.585, 368.9523, 368.9523
#fit.nb.full is best

summary(fit.nb_reduced); summary(fit.nb_reduced1); summary(fit.nb_reduced2); summary(fit.nb_full2); summary(fit.nb_full2); summary(fit.nb_full2)

#### diagnosis
sim_res2 <- simulateResiduals(fit.nb_reduced)
plot(sim_res2)
#No problem detected
sim_res3 <- simulateResiduals(fit.nb_full)
plot(sim_res3)
#no problem

#dispersion
overdispersion_stat2 <- sum(residuals(fit.nb_reduced, type = "pearson")^2) / df.residual(fit.nb_reduced)
overdispersion_stat2 
#0.803793 #fine

overdispersion_stat3 <- sum(residuals(fit.nb_full, type = "pearson")^2) / df.residual(fit.nb_full)
overdispersion_stat3  
#0.8935107 #better
#choose fit fit.nb_full

######################################################
#visualization
# Forest Plot for Richness
p1 <- plot_model(fit.full, 
                 show.values = TRUE, 
                 value.offset = .4,
                 title = "(a)",
                 axis.labels = c("Temp_std:Prec_std" = "Interaction (T*P)", 
                                 "Prec_std" = "Precipitation (Std)", 
                                 "Temp_std" = "Temperature (Std)")) +
  theme_bw(base_size = 12, base_family = "serif") 

#Forest Plot for Abundance
p2 <- plot_model(fit.nb_full, 
                 show.values = TRUE, 
                 value.offset = .4,
                 title = "(b)",
                 axis.labels = c("Temp_std:Prec_std" = "Interaction (T*P)", 
                                 "Prec_std" = "Precipitation (Std)", 
                                 "Temp_std" = "Temperature (Std)")) +
  theme_bw(base_size = 12, base_family = "serif")

#Combine
final <- p1 + p2
# Display the final plot
print(final)
