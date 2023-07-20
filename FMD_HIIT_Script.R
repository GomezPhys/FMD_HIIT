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

Accurate <- read_excel("~/CAPH lab/HIIT FMD/Dataset.xlsx", sheet = "Accurate")

View(Accurate)
attach(Accurate)



Accurate_descriptives <- Accurate  %>% select(Participant, Sex, Age, Weight) %>% na.omit()



## Convert from character to factor data
Accurate$Condition <- as.factor(Accurate$Condition)
Accurate$Sex <- as.factor(Accurate$Sex)
Accurate$Visit <- as.factor(Accurate$Visit)

## Order Conditions
Accurate$Condition <- ordered(Accurate$Condition,
                              levels = c("1", "2", "3", "4", "5", "6"))

Accurate$Visit <- ordered(Accurate$Visit, levels = c("1", "2", "3", "4", "5", "6"))

Accurate$Time <- ordered(Accurate$Time, levels = c("Pre", "Post_10", "Post_60"))


## Data Normality test ##

Accurate %>% group_by(Condition) %>% 
  shapiro_test(Diameter_Baseline)

Accurate %>% group_by(Condition) %>%
  shapiro_test(Diameter_Peak)

Accurate %>% group_by(Condition) %>%
  shapiro_test(FMD_change)



###### Linear Mixed models ESS Antegrade
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

#Plots

ggboxplot(Accurate, x = "Time", y = "FMD_change",
          color = "Time", palette = get_palette("Set1", 4),
          ylab = "FMD_change)") +  facet_grid(~Condition)
#Save Plot
ggsave("FMD_by_sex2.png")


ggboxplot(Accurate, x = "Time", y = "FMD_Absolute_Change",
          color = "Time", palette = get_palette("Set1", 4),
          ylab = "FMD_Absolute_Change") +  facet_grid(~Condition)
#Save Plot
ggsave("FMD_by_sex_ABSC.png")

ggboxplot(Accurate, x = "Time", y = "Time_to_Peak",
          color = "Time", palette = get_palette("Set1", 4),
          ylab = "Time_to_Peak(S)") +  facet_grid(~Condition)
#Save Plot
ggsave("FMD_by_sex_Time_t_P.png")