### Required packages
library(readxl) ## To load excel sheet
library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad

Df <- read_excel("~/fmd_hiit_couri.xlsx", sheet = "MG")
View(Df)


## Convert from character to factor data
Accurate$Condition <- as.factor(Accurate$Condition)
Accurate$Sex <- as.factor(Accurate$Sex)
Accurate$Visit <- as.factor(Accurate$Visit)

## Order Conditions
Accurate$Condition <- ordered(Accurate$Condition,
                              levels = c("1", "2", "3", "4", "5", "6"))

Accurate$Visit <- ordered(Accurate$Visit, levels = c("1", "2", "3", "4", "5", "6"))

Accurate$Time <- ordered(Accurate$Time, levels = c("Pre", "Post_10", "Post_60"))


Accurate %>% group_by(Condition) %>% 
  shapiro_test(Diameter_Baseline)

Accurate %>% group_by(Condition) %>%
  shapiro_test(Diameter_Peak)

Accurate %>% group_by(Condition) %>%
  shapiro_test(FMD_change)

Df %>% group_by(Condition) %>% 
  shapiro_test(FMD_Basal2)

Df %>% group_by(Condition) %>%
  shapiro_test(FMD_Absolute_Change2)

Accurate %>% group_by(Condition) %>%
  shapiro_test(FMD_Absolute_Change)

#####Lmmodels

lmModel = lmer(FMD_change ~ Condition + Sex + (1|Participant),
               data=Accurate, REML=FALSE)

summary(lmModel)


# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Accurate %>%
  pairwise_t_test(FMD_change ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")

pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Accurate %>% cohens_d(FMD_change ~ Condition,
                      paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")


####FMD_CHANGE2
lmModel = lmer(FMD_Change2 ~ Condition + Sex + (1|Participant),
               data=Df, REML=FALSE)

summary(lmModel)


# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df %>%
  pairwise_t_test(FMD_change ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")

pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(FMD_change ~ Condition,
                      paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")



####FMD_Absolute_Change
#####Lmmodels

lmModel = lmer(FMD_Absolute_Change ~ Condition + Sex + (1|Participant),
               data=Accurate, REML=FALSE)

summary(lmModel)


# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Accurate %>%
  pairwise_t_test(FMD_Absolute_Change ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")

pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Accurate %>% cohens_d(FMD_Absolute_Change ~ Condition,
                      paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")


####FMD_CHANGE2
lmModel = lmer(FMD_Absolute_Change2 ~ Condition + Sex + (1|Participant),
               data=Df, REML=FALSE)

summary(lmModel)


# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df %>%
  pairwise_t_test(FMD_Absolute_Change2 ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")

pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(FMD_Absolute_Change2 ~ Condition,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

###Basal
lmModel = lmer(Diameter_Baseline ~ Condition + Sex + (1|Participant),
               data=Accurate, REML=FALSE)

summary(lmModel)


# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Accurate %>%
  pairwise_t_test(Diameter_Baseline ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")

pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Accurate %>% cohens_d(Diameter_Baseline ~ Condition,
                      paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")


####Baseline 2
lmModel = lmer(FMD_Basal2 ~ Condition + Sex + (1|Participant),
               data=Df, REML=FALSE)

summary(lmModel)


# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df %>%
  pairwise_t_test(FMD_Basal2 ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")

pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(FMD_Basal2 ~ Condition,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")




####TAbles###
####FMD_Change 

ggboxplot(Accurate, x = "Time", y = "FMD_change",
          color = "Time", palette = get_palette("Set1", 4),
          ylab = "FMD_change") +  facet_grid(~Condition)


ggboxplot(Df, x = "Time", y = "FMD_Change2",
          color = "Time", palette = get_palette("Set1", 4),
          ylab = "FMD_Change2") +  facet_grid(~Condition)

###FMD_Absolute_Change

ggboxplot(Accurate, x = "Time", y = "FMD_Absolute_Change",
          color = "Time", palette = get_palette("Set1", 4),
          ylab = "FMD_Absolute_Change") +  facet_grid(~Condition)

ggboxplot(Df, x = "Time", y = "FMD_Absolute_Change2",
          color = "Time", palette = get_palette("Set1", 4),
          ylab = "FMD_Absolute_Change2") +  facet_grid(~Condition)

###FMD_BAsal

ggboxplot(Accurate, x = "Time", y = "Diameter_Baseline",
          color = "Time", palette = get_palette("Set1", 4),
          ylab = "Diameter_Baseline") +  facet_grid(~Condition)

ggboxplot(Df, x = "Time", y = "FMD_Basal2",
          color = "Time", palette = get_palette("Set1", 4),
          ylab = "FMD_Basal2") +  facet_grid(~Condition)
