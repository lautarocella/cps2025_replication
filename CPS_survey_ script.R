###### ------ Replication script for "Building Tolerance for Backsliding by Trash-Talking Democracy Theory and Evidence from Mexico" ------ ######

library(tidyverse)
library(estimatr)
library(readxl)
library(cobalt)
library(ggplot2)
library(dplyr)
library(car)
library(texreg)
library(vtable)
library(lm.beta)
library(jtools)
library(modelsummary)
library(Rmisc)
library(dplyr)
library(knitr)
library(kableExtra)
library(coefplot)
library(texreg)
library(dotwhisker)
library(mediation)

# I. Load survey data
load("Data/mexico.RData")


# II. Create subgroups
## MORENA
morena <- mexico %>% filter(morena==1)
## NONPARTISAN
nonpartisan <- mexico %>% filter(nonpartisan==1)
## OPPOSITION-PARTISAN (PAN,PRI,PRD)
otherpartisan <- mexico %>% filter(otherpartisan==1)
## NON-MORENA
nonmorena <- mexico %>% filter(nonpartisan==1 | otherpartisan==1)



# III. Run regression models
## Anti-Democratic Index
### Models without controls
#### POOLED 
anti_democratic_index_trashtalking <- lm_robust(anti_democratic_index ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA 
anti_democratic_index_trashtalking_morena <- lm_robust(anti_democratic_index ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA 
anti_democratic_index_trashtalking_nonmorena <- lm_robust(anti_democratic_index ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
anti_democratic_index_trashtalking_nonpartisan<- lm_robust(anti_democratic_index ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
anti_democratic_index_trashtalking_otherpartisan <- lm_robust(anti_democratic_index ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
### Models with controls
#### POOLED
anti_democratic_index_trashtalking_controls <- lm_lin(anti_democratic_index ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=mexico %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### MORENA
anti_democratic_index_trashtalking_controls_morena <- lm_lin(anti_democratic_index ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=morena %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-MORENA
anti_democratic_index_trashtalking_controls_nonmorena <- lm_lin(anti_democratic_index ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NONPARTISANS
anti_democratic_index_trashtalking_controls_nonpartisan <- lm_lin(anti_democratic_index ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### OPPOSITION-PARTISANS
anti_democratic_index_trashtalking_controls_otherpartisan <- lm_lin(anti_democratic_index ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=otherpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))


# Fire Judges
### Models without controls
#### POOLED
fire_judges_trashtalking <- lm_robust(fire_judges ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA IDENTIFIERS
fire_judges_trashtalking_morena <- lm_robust(fire_judges ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
fire_judges_trashtalking_nonpartisan <- lm_robust(fire_judges ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
fire_judges_trashtalking_otherpartisan <- lm_robust(fire_judges ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA IDENTIFIERS
fire_judges_trashtalking_nonmorena <- lm_robust(fire_judges ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))
### Models with controls
#### POOLED
fire_judges_trashtalking_controls <- lm_lin(fire_judges ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=mexico %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### MORENA IDENTIFIERS
fire_judges_trashtalking_controls_morena <- lm_lin(fire_judges ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=morena %>% filter(treatment == "Trash-talking"|treatment == "Control"))
####  NON-PARTISAN IDENTIFIERS
fire_judges_trashtalking_controls_nonpartisan <- lm_lin(fire_judges ~ I(treatment == "Trash-talking"), covariates = ~age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### OPPOSITION-PARTY IDENTIFIERS
fire_judges_trashtalking_controls_otherpartisan <- lm_lin(fire_judges ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=otherpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
####  NON-MORENA IDENTIFIERS
fire_judges_trashtalking_controls_nonmorena <- lm_lin(fire_judges ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena %>% filter(treatment == "Trash-talking"|treatment == "Control"))


## Disobey Justice Outcome
### Models without controls
#### POOLED
disobey_justice_trashtalking <- lm_robust(disobey_justice_2 ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA IDENTIFIERS 
disobey_justice_trashtalking_morena <- lm_robust(disobey_justice_2 ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
disobey_justice_trashtalking_nonpartisan <- lm_robust(disobey_justice_2 ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
disobey_justice_trashtalking_otherpartisan<- lm_robust(disobey_justice_2 ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA IDENTIFIERS
disobey_justice_trashtalking_nonmorena <- lm_robust(disobey_justice_2 ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))
### Models with controls
#### POOLED
disobey_justice_trashtalking_controls <- lm_lin(disobey_justice_2 ~ I(treatment == "Trash-talking"), covariates = ~age + male + socioeconomic_level+political_interest+pres_approval, data=mexico %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### MORENA IDENTIFIERS
disobey_justice_trashtalking_controls_morena <- lm_lin(disobey_justice_2 ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=morena %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-PARTISAN IDENTIFIERS
disobey_justice_trashtalking_controls_nonpartisan <- lm_lin(disobey_justice_2 ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### OPPOSITION PARTY IDENTIFIERS
disobey_justice_trashtalking_controls_otherpartisan <- lm_lin(disobey_justice_2 ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=otherpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-MORENA IDENTIFIERS
disobey_justice_trashtalking_controls_nonmorena <- lm_lin(disobey_justice_2 ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena %>% filter(treatment == "Trash-talking"|treatment == "Control"))


## Institutional Nihilism
### Models without controls
#### POOLED
overthrow_institutions_trashtalking <- lm_robust(overthrow_institutions ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA IDENTIFIERS
overthrow_institutions_trashtalking_morena <- lm_robust(overthrow_institutions ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
overthrow_institutions_trashtalking_nonpartisan <- lm_robust(overthrow_institutions ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
overthrow_institutions_trashtalking_otherpartisan <- lm_robust(overthrow_institutions ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA IDENTIFIERS
overthrow_institutions_trashtalking_nonmorena <- lm_robust(overthrow_institutions ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))
### Models with controls
#### POOLED
overthrow_institutions_trashtalking_controls <- lm_lin(overthrow_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=mexico %>% filter(treatment == "Trash-talking"|treatment == "Control"))
####  MORENA IDENTIFIERS
overthrow_institutions_trashtalking_controls_morena <- lm_lin(overthrow_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=morena %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-PARTISAN IDENTIFIERS
overthrow_institutions_trashtalking_controls_nonpartisan <- lm_lin(overthrow_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### OPPOSITION PARTY IDENTIFIERS
overthrow_institutions_trashtalking_controls_otherpartisan <- lm_lin(overthrow_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=otherpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-MORENA IDENTIFIERS
overthrow_institutions_trashtalking_controls_nonmorena <- lm_lin(overthrow_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena %>% filter(treatment == "Trash-talking"|treatment == "Control"))


## Less Power to Institutions
### Models without controls
#### POOLED
power_institutions_trashtalking <- lm_robust(power_institutions ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
####  MORENA IDENTIFIERS
power_institutions_trashtalking_morena <- lm_robust(power_institutions ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
power_institutions_trashtalking_nonpartisan <- lm_robust(power_institutions ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
power_institutions_trashtalking_otherpartisan <- lm_robust(power_institutions ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA IDENTIFIERS
power_institutions_trashtalking_nonmorena <- lm_robust(power_institutions ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))
### Models with controls
#### POOLED
power_institutions_trashtalking_controls <- lm_lin(power_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=mexico %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### MORENA IDENTIFIERS
power_institutions_trashtalking_controls_morena <- lm_lin(power_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=morena %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-PARTISAN IDENTIFIERS
power_institutions_trashtalking_controls_nonpartisan <- lm_lin(power_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### OPPOSITION PARTY IDENTIFIERS
power_institutions_trashtalking_controls_otherpartisan <- lm_lin(power_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=otherpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-MORENA IDENTIFIERS
power_institutions_trashtalking_controls_nonmorena <- lm_lin(power_institutions ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena %>% filter(treatment == "Trash-talking"|treatment == "Control"))


## Corruption
### Models without controls
#### POOLED
check_corruption_trashtalking <- lm_robust(check_corruption ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA IDENTIFIERS
check_corruption_trashtalking_morena <- lm_robust(check_corruption ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA IDENTIFIERS
check_corruption_trashtalking_nonmorena <- lm_robust(check_corruption ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
check_corruption_trashtalking_nonpartisan <- lm_robust(check_corruption ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
check_corruption_trashtalking_otherpartisan <- lm_robust(check_corruption ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
### Models with controls
#### POOLED
check_corruption_trashtalking_controls <- lm_lin(check_corruption ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=mexico %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### MORENA IDENTIFIERS
check_corruption_trashtalking_controls_morena <- lm_lin(check_corruption ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=morena %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-MORENA IDENTIFIERS
check_corruption_trashtalking_controls_nonmorena <- lm_lin(check_corruption ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NONPARTISAN IDENTIFIERS
check_corruption_trashtalking_controls_nonpartisan <- lm_lin(check_corruption ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### OPPOSITION-PARTY IDENTIFIERS
check_corruption_trashtalking_controls_otherpartisan <- lm_lin(check_corruption ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=otherpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))


## Feeling Angry
### Models without controls
#### POOLED
angry_trashtalking <- lm_robust(angry ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA IDENTIFIERS
angry_trashtalking_morena <- lm_robust(angry ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
angry_trashtalking_nonpartisan <- lm_robust(angry ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
####  OPPOSITION-PARTY IDENTIFIERS
angry_trashtalking_otherpartisan <- lm_robust(angry ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA IDENTIFIERS
angry_trashtalking_nonmorena <- lm_robust(angry ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))


## Strong Leader 
### Models without controls
#### POOLED
strong_leader_trashtalking <- lm_robust(strong_leader ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA IDENTIFIERS
strong_leader_trashtalking_morena <- lm_robust(strong_leader ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
strong_leader_trashtalking_nonpartisan <- lm_robust(strong_leader ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
strong_leader_trashtalking_otherpartisan <- lm_robust(strong_leader ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA IDENTIFIERS
strong_leader_trashtalking_nonmorena <- lm_robust(strong_leader ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))
### Models with controls
#### POOLED
strong_leader_trashtalking_controls <- lm_lin(strong_leader ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=mexico %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### MORENA IDENTIFIERS
strong_leader_trashtalking_controls_morena <- lm_lin(strong_leader ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=morena %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-PARTISAN IDENTIFIERS
strong_leader_trashtalking_controls_nonpartisan <- lm_lin(strong_leader ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### OPPOSITION PARTY IDENTIFIERS
strong_leader_trashtalking_controls_otherpartisan <- lm_lin(strong_leader ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=otherpartisan %>% filter(treatment == "Trash-talking"|treatment == "Control"))
#### NON-MORENA IDENTIFIERS
strong_leader_trashtalking_controls_nonmorena <- lm_lin(strong_leader ~ I(treatment == "Trash-talking"), covariates = ~ age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena %>% filter(treatment == "Trash-talking"|treatment == "Control"))


## Feeling Optimistic
### Models without controls
#### POOLED
optimistic_trashtalking <- lm_robust(optimistic ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA
optimistic_trashtalking_morena <- lm_robust(optimistic ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
optimistic_trashtalking_nonpartisan <- lm_robust(optimistic ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
optimistic_trashtalking_otherpartisan <- lm_robust(optimistic ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA
optimistic_trashtalking_nonmorena <- lm_robust(optimistic ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))


## Feeling Scared
### Models without controls
#### POOLED
scared_trashtalking <- lm_robust(scared ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA IDENTIFIERS
scared_trashtalking_morena <- lm_robust(scared ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
scared_trashtalking_nonpartisan <- lm_robust(scared ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
scared_trashtalking_otherpartisan<- lm_robust(scared ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
# NON-MORENA IDENTIFIERS
scared_trashtalking_nonmorena <- lm_robust(scared ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))


## Feeling Nervous
### Models without controls
#### POOLED
nervous_trashtalking <- lm_robust(nervous ~ I(treatment=="Trash-talking"), data=mexico %>% filter(treatment%in%c("Control", "Trash-talking")))
#### MORENA IDENTIFIERS
nervous_trashtalking_morena <- lm_robust(nervous ~ I(treatment=="Trash-talking"), data=morena %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-PARTISAN IDENTIFIERS
nervous_trashtalking_nonpartisan <- lm_robust(nervous ~ I(treatment=="Trash-talking"), data=nonpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### OPPOSITION-PARTY IDENTIFIERS
nervous_trashtalking_otherpartisan<- lm_robust(nervous ~ I(treatment=="Trash-talking"), data=otherpartisan %>% filter(treatment%in%c("Control", "Trash-talking")))
#### NON-MORENA IDENTIFIERS
nervous_trashtalking_nonmorena <- lm_robust(nervous ~ I(treatment=="Trash-talking"), data=nonmorena %>% filter(treatment%in%c("Control", "Trash-talking")))



# IV Run Mediation Analyses
## Set Seed
set.seed(13)
## Mediation: Corruption
### MORENA
model_m_index_morena <- lm(check_corruption ~ treatment_binary+age + male + socioeconomic_level+political_interest+pres_approval, data=morena)
model_o_index_morena <- lm(anti_democratic_index ~ treatment_binary+check_corruption+age + male + socioeconomic_level+political_interest+pres_approval, data=morena)
mediate_morena_corrupt <- mediate(model_m_index_morena, model_o_index_morena, boot = TRUE, sims = 1000, treat = "treatment_binary", mediator = "check_corruption")
# Opposition
otherpartisan$socioeconomic_level<-as.numeric(otherpartisan$socioeconomic_level)
model_m_index_otherpartisan <- lm(check_corruption ~ treatment_binary+age + male +political_interest+pres_approval+socioeconomic_level, data=otherpartisan)
model_o_index_otherpartisan <- lm(anti_democratic_index ~ treatment_binary+check_corruption+age + male  +political_interest+pres_approval+socioeconomic_level, data=otherpartisan)
mediate_opposition_corrupt <- mediate(model_m_index_otherpartisan, model_o_index_otherpartisan, boot = TRUE, sims = 1000, treat = "treatment_binary", mediator = "check_corruption")
### Non-Partisan
model_m_index_nonpartisan<- lm(check_corruption ~ treatment_binary+age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan)
model_o_index_nonpartisan<- lm(anti_democratic_index ~ treatment_binary+check_corruption+age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan)
mediate_nonpartisan_corrupt <- mediate(model_m_index_nonpartisan, model_o_index_nonpartisan, boot = TRUE, sims = 1000, treat = "treatment_binary", mediator = "check_corruption")
### Non-Morena
model_m_index_nonmorena <- lm(check_corruption ~ treatment_binary+age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena)
model_o_index_nonmorena<- lm(anti_democratic_index ~ treatment_binary+check_corruption+age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena)
mediate_nonmorena_corrupt <- mediate(model_m_index_nonmorena, model_o_index_nonmorena, boot = TRUE, sims = 1000, treat = "treatment_binary", mediator = "check_corruption")


## Mediation: Feeling Angry
### MORENA
model_m_index_morena <- lm(angry ~ treatment_binary+age + male + socioeconomic_level+political_interest+pres_approval, data=morena)
model_o_index_morena <- lm(anti_democratic_index ~ treatment_binary+angry+age + male + socioeconomic_level+political_interest+pres_approval, data=morena)
mediate_morena_angry <- mediate(model_m_index_morena, model_o_index_morena, boot = TRUE, sims = 1000, treat = "treatment_binary", mediator = "angry")
### Opposition
model_m_index_otherpartisan <- lm(angry ~ treatment_binary+age + male + socioeconomic_level+political_interest+pres_approval, data=otherpartisan)
model_o_index_otherpartisan <- lm(anti_democratic_index ~ treatment_binary+angry+age + male + socioeconomic_level+political_interest+pres_approval, data=otherpartisan)
mediate_opposition_angry <- mediate(model_m_index_otherpartisan, model_o_index_otherpartisan, boot = TRUE, sims = 1000, treat = "treatment_binary", mediator = "angry")
### Non-Partisan
model_m_index_nonpartisan<- lm(angry ~ treatment_binary+age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan)
model_o_index_nonpartisan<- lm(anti_democratic_index ~ treatment_binary+angry+age + male + socioeconomic_level+political_interest+pres_approval, data=nonpartisan)
mediate_nonpartisan_angry <- mediate(model_m_index_nonpartisan, model_o_index_nonpartisan, boot = TRUE, sims = 1000, treat = "treatment_binary", mediator = "angry")
# Non-Morena
model_m_index_nonmorena <- lm(angry ~ treatment_binary+age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena)
model_o_index_nonmorena<- lm(anti_democratic_index ~ treatment_binary+angry+age + male + socioeconomic_level+political_interest+pres_approval, data=nonmorena)
mediate_nonmorena_angry <- mediate(model_m_index_nonmorena, model_o_index_nonmorena, boot = TRUE, sims = 1000, treat = "treatment_binary", mediator = "angry")



# V. Create Paper Figures
## Figure 3
### Create a data frame
ATE_df <- data.frame(
  term = character(5),
  estimate = numeric(5),
  std.error = numeric(5),
  statistic = numeric(5),
  p.value = numeric(5)
)
### Populate the data frame
ATE_df$term[1] <- "Anti-Democratic Index"
ATE_df$estimate[1] <- anti_democratic_index_trashtalking$coefficients[2]
ATE_df$std.error[1] <- anti_democratic_index_trashtalking$std.error[2]
ATE_df$p.value[1] <- anti_democratic_index_trashtalking$p.value[2]

ATE_df$term[2] <- "Fire Judges"
ATE_df$estimate[2] <- fire_judges_trashtalking$coefficients[2]
ATE_df$std.error[2] <- fire_judges_trashtalking$std.error[2]
ATE_df$p.value[2] <- fire_judges_trashtalking$p.value[2]

ATE_df$term[3] <- "Disobey Justice"
ATE_df$estimate[3] <- disobey_justice_trashtalking$coefficients[2]
ATE_df$std.error[3] <- disobey_justice_trashtalking$std.error[2]
ATE_df$p.value[3] <- disobey_justice_trashtalking$p.value[2]

ATE_df$term[4] <- "Institutional Nihilism"
ATE_df$estimate[4] <- overthrow_institutions_trashtalking$coefficients[2]
ATE_df$std.error[4] <- overthrow_institutions_trashtalking$std.error[2]
ATE_df$p.value[4] <- overthrow_institutions_trashtalking$p.value[2]

ATE_df$term[5] <- "Less Power to Institutions"
ATE_df$estimate[5] <- power_institutions_trashtalking$coefficients[2]
ATE_df$std.error[5] <- power_institutions_trashtalking$std.error[2]
ATE_df$p.value[5] <- power_institutions_trashtalking$p.value[2]

### Create the dot-and-whisker plot for Figure 3
ate_plot<- dwplot(
  ATE_df,
  ci = 0.9,
  dodge_size = 0.3, whisker_args = list(size = 0.8),  dot_args = list(size = 1.5)
) +
  theme_bw(base_size = 8) +xlab("\n Coefficient estimates and 95% confidence intervals") + ylab("") +
  xlim(-0.10,0.30) + 
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  theme(legend.position="none") +ggtitle("Effect of Trash-Talking on Support for Democracy")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size=18),
        axis.text=element_text(size=14),
        axis.title = element_text(size=14))
ate_plot


## Figure 4
### Create a data frame
ATE_df_subgroup <- data.frame(
  subgroup = c(rep("pooled", 5), rep("morena", 5), rep("nonpartisan", 5), rep("otherpartisan", 5), rep("nonmorena", 5)),
  term = rep(c("Anti-Democratic Index", "Fire Judges", "Disobey Justice", "Institutional Nihilism", "Less Power to Institutions"), 5),
  estimate = numeric(25),
  std.error = numeric(25),
  p.value = numeric(25),  # Add a column for p-values
  model = character(25)  # Add a column for model names
)

### Fill the coefficients
ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "pooled"] <- c(
  anti_democratic_index_trashtalking$coefficients[2],
  fire_judges_trashtalking$coefficients[2],
  disobey_justice_trashtalking$coefficients[2],
  overthrow_institutions_trashtalking$coefficients[2],
  power_institutions_trashtalking$coefficients[2]
)
ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "morena"] <- c(
  anti_democratic_index_trashtalking_morena$coefficients[2],
  fire_judges_trashtalking_morena$coefficients[2],
  disobey_justice_trashtalking_morena$coefficients[2],
  overthrow_institutions_trashtalking_morena$coefficients[2],
  power_institutions_trashtalking_morena$coefficients[2]
)
ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "nonpartisan"] <- c(
  anti_democratic_index_trashtalking_nonpartisan$coefficients[2],
  fire_judges_trashtalking_nonpartisan$coefficients[2],
  disobey_justice_trashtalking_nonpartisan$coefficients[2],
  overthrow_institutions_trashtalking_nonpartisan$coefficients[2],
  power_institutions_trashtalking_nonpartisan$coefficients[2]
)
ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "otherpartisan"] <- c(
  anti_democratic_index_trashtalking_otherpartisan$coefficients[2],
  fire_judges_trashtalking_otherpartisan$coefficients[2],
  disobey_justice_trashtalking_otherpartisan$coefficients[2],
  overthrow_institutions_trashtalking_otherpartisan$coefficients[2],
  power_institutions_trashtalking_otherpartisan$coefficients[2]
)
ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "nonmorena"] <- c(
  anti_democratic_index_trashtalking_nonmorena$coefficients[2],
  fire_judges_trashtalking_nonmorena$coefficients[2],
  disobey_justice_trashtalking_nonmorena$coefficients[2],
  overthrow_institutions_trashtalking_nonmorena$coefficients[2],
  power_institutions_trashtalking_nonmorena$coefficients[2]
)

### Fill p-values
ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "pooled"] <- c(
  anti_democratic_index_trashtalking$p.value[2],
  fire_judges_trashtalking$p.value[2],
  disobey_justice_trashtalking$p.value[2],
  overthrow_institutions_trashtalking$p.value[2],
  power_institutions_trashtalking$p.value[2]
)
ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "morena"] <- c(
  anti_democratic_index_trashtalking_morena$p.value[2],
  fire_judges_trashtalking_morena$p.value[2],
  disobey_justice_trashtalking_morena$p.value[2],
  overthrow_institutions_trashtalking_morena$p.value[2],
  power_institutions_trashtalking_morena$p.value[2]
)
ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "nonpartisan"] <- c(
  anti_democratic_index_trashtalking_nonpartisan$p.value[2],
  fire_judges_trashtalking_nonpartisan$p.value[2],
  disobey_justice_trashtalking_nonpartisan$p.value[2],
  overthrow_institutions_trashtalking_nonpartisan$p.value[2],
  power_institutions_trashtalking_nonpartisan$p.value[2]
)
ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "otherpartisan"] <- c(
  anti_democratic_index_trashtalking_otherpartisan$p.value[2],
  fire_judges_trashtalking_otherpartisan$p.value[2],
  disobey_justice_trashtalking_otherpartisan$p.value[2],
  overthrow_institutions_trashtalking_otherpartisan$p.value[2],
  power_institutions_trashtalking_otherpartisan$p.value[2]
)
ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "nonmorena"] <- c(
  anti_democratic_index_trashtalking_nonmorena$p.value[2],
  fire_judges_trashtalking_nonmorena$p.value[2],
  disobey_justice_trashtalking_nonmorena$p.value[2],
  overthrow_institutions_trashtalking_nonmorena$p.value[2],
  power_institutions_trashtalking_nonmorena$p.value[2]
)

### Fill SE
ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "pooled"] <- c(
  anti_democratic_index_trashtalking$std.error[2],
  fire_judges_trashtalking$std.error[2],
  disobey_justice_trashtalking$std.error[2],
  overthrow_institutions_trashtalking$std.error[2],
  power_institutions_trashtalking$std.error[2]
)
ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "morena"] <- c(
  anti_democratic_index_trashtalking_morena$std.error[2],
  fire_judges_trashtalking_morena$std.error[2],
  disobey_justice_trashtalking_morena$std.error[2],
  overthrow_institutions_trashtalking_morena$std.error[2],
  power_institutions_trashtalking_morena$std.error[2]
)
ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "nonpartisan"] <- c(
  anti_democratic_index_trashtalking_nonpartisan$std.error[2],
  fire_judges_trashtalking_nonpartisan$std.error[2],
  disobey_justice_trashtalking_nonpartisan$std.error[2],
  overthrow_institutions_trashtalking_nonpartisan$std.error[2],
  power_institutions_trashtalking_nonpartisan$std.error[2]
)
ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "otherpartisan"] <- c(
  anti_democratic_index_trashtalking_otherpartisan$std.error[2],
  fire_judges_trashtalking_otherpartisan$std.error[2],
  disobey_justice_trashtalking_otherpartisan$std.error[2],
  overthrow_institutions_trashtalking_otherpartisan$std.error[2],
  power_institutions_trashtalking_otherpartisan$std.error[2]
)
ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "nonmorena"] <- c(
  anti_democratic_index_trashtalking_nonmorena$std.error[2],
  fire_judges_trashtalking_nonmorena$std.error[2],
  disobey_justice_trashtalking_nonmorena$std.error[2],
  overthrow_institutions_trashtalking_nonmorena$std.error[2],
  power_institutions_trashtalking_nonmorena$std.error[2]
)

### Label groups
ATE_df_subgroup <- ATE_df_subgroup %>%
  mutate(
    model = case_when(
      subgroup == "pooled" ~ "Pooled",
      subgroup == "morena" ~ "MORENA",
      subgroup == "nonpartisan" ~ "Non-partisan",
      subgroup == "otherpartisan" ~ "Opposition partisan",
      subgroup == "nonmorena" ~ "Non-MORENA"))

### Filter OUT non-MORENA
ATE_df_subgroup <- ATE_df_subgroup %>% filter(subgroup!="nonmorena")

### Create colors and shapes
cols <- c("Pooled" = "black",  "MORENA" = "red2", "Non-partisan" = "green2", "Opposition partisan" = "blue")
shapes <- c("Pooled" = 18,  "MORENA" = 17, "Non-partisan" = 16, "Opposition partisan" = 15)

### Create the dot-and-whisker plot for Figure 4
ate_democracy_plot_partisanship<- dwplot(ATE_df_subgroup,
                                         dodge_size = 0.5,
                                         dot_args = list(aes(colour = model, shape = model, size=0.1)),
                                         whisker_args = list(size = 0.8),
) +
  scale_color_manual(values = cols, name = "Partisan type") + 
  scale_shape_manual(values = shapes, 
                     name = "Partisan type",
                     guide = guide_legend(reverse = TRUE)) +
  theme_bw(base_size = 18.5) +xlab("\n Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  xlab("\n Coefficient estimates and 95% confidence intervals")+
  ggtitle("Effect of Trash-talking on Support for Democracy", subtitle="By Partisanship") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=2), reverse=TRUE), 
         size = "none") + 
  theme(legend.position = c(0.88, .88),
        legend.box.background = element_rect(colour = "black"),
        legend.title=element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text=element_text(size=20),
        legend.text = element_text(size = 12.5))
ate_democracy_plot_partisanship


## Figure 5
### Create data set with means for control and treatment, by party group 
mexico <- mexico %>% mutate (
  party=case_when(morena==1 ~ "MORENA",
                  nonpartisan==1 ~ "Non-partisan",
                  otherpartisan==1 ~ "Opposition partisan"))
df.arrow <- mexico %>% 
  group_by(treatment,party) %>% 
  dplyr::summarise(mean_value = mean(anti_democratic_index, na=T))
df.arrow <- spread(df.arrow, treatment, mean_value)
df.arrow$Treatment <- df.arrow$`Trash-talking` 
df.arrow <- df.arrow %>%filter(!is.na(party))
df.arrow$sig <- c(1,0,1)
df.arrow$sig <-as.factor(df.arrow$sig )
df.arrow <- df.arrow %>% select (-`Trash-talking`)
df.arrow$party <- factor(df.arrow$party, levels = c("Opposition partisan", "Non-partisan", "MORENA"))

### Create Figure 5
arrow_plot <-df.arrow%>%
  ggplot(aes(y=party,color=as.factor(party),
             group=as.factor(party)))+
  geom_point(aes(x=Treatment,shape=sig),size=3,
             show.legend=F,
             position=ggstance::position_dodgev(height=0.5))+
  geom_point(aes(x=Control),shape="\u007C",size=3,
             show.legend=F,
             position=ggstance::position_dodgev(height=0.5))+
    geom_errorbarh(aes(xmin=Control,xmax=Treatment),height=0, size=0.8,
                 position=ggstance::position_dodgev(height=0.5))+
  scale_shape_manual(values=c("\u25B7","\u25BA"))+
  scale_color_manual(
    values=c("blue2", "green2", "red3"),guide="none")+
  
    xlab("Group Mean")+
  ylab("")+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Anti-Democratic Index by Partisanship")
arrow_plot


## Figure 6
### Create a data frame for all subgroups manually
ATE_df_subgroup <- data.frame(
  subgroup = c(rep("pooled", 1), rep("morena", 1), rep("nonpartisan", 1), rep("otherpartisan", 1), rep("nonmorena", 1)),
  term = rep(c("Perception of Corruption"), 5),
  estimate = numeric(5),
  std.error = numeric(5),
  p.value = numeric(5),  # Add a column for p-values
  model = character(5)  # Add a column for model names
)

### Fill the coefficients
ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "pooled"] <- c(
  check_corruption_trashtalking$coefficients[2]
)
ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "morena"] <- c(
  check_corruption_trashtalking_morena$coefficients[2]
)

ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "nonpartisan"] <- c(
  check_corruption_trashtalking_nonpartisan$coefficients[2]
)

ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "otherpartisan"] <- c(
  check_corruption_trashtalking_otherpartisan$coefficients[2]
)

ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "nonmorena"] <- c(
  check_corruption_trashtalking_nonmorena$coefficients[2]
)

### Fill p-values
ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "pooled"] <- c(
  check_corruption_trashtalking$p.value[2]
)

ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "morena"] <- c(
  check_corruption_trashtalking_morena$p.value[2]
)

ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "nonpartisan"] <- c(
  check_corruption_trashtalking_nonpartisan$p.value[2]
)

ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "otherpartisan"] <- c(
  check_corruption_trashtalking_otherpartisan$p.value[2]
)

ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "nonmorena"] <- c(
  check_corruption_trashtalking_nonmorena$p.value[2]
)

### Fill SE
ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "pooled"] <- c(
  check_corruption_trashtalking$std.error[2]
)
ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "morena"] <- c(
  check_corruption_trashtalking_morena$std.error[2]
)

ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "nonpartisan"] <- c(
  check_corruption_trashtalking_nonpartisan$std.error[2]
)

ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "otherpartisan"] <- c(
  check_corruption_trashtalking_otherpartisan$std.error[2]
)

ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "nonmorena"] <- c(
  check_corruption_trashtalking_nonmorena$std.error[2]
)

### Label groups
ATE_df_subgroup <- ATE_df_subgroup %>%
  mutate(
    model = case_when(
      subgroup == "pooled" ~ "Pooled",
      subgroup == "morena" ~ "MORENA",
      subgroup == "nonpartisan" ~ "Non-partisan",
      subgroup == "otherpartisan" ~ "Opposition partisan",
      subgroup == "nonmorena" ~ "Non-MORENA"))


### Filter OUT non-MORENA
ATE_df_subgroup <- ATE_df_subgroup %>% filter(subgroup!="nonmorena")

### Colors and shapes
cols <- c("Pooled" = "black",  "MORENA" = "red2", "Non-partisan" = "green2", "Opposition partisan" = "blue")
shapes <- c("Pooled" = 18,  "MORENA" = 17, "Non-partisan" = 16, "Opposition partisan" = 15)

### Create the dot-and-whisker plot for Figure 6
ate_corruption_plot<- dwplot(ATE_df_subgroup,
                             dodge_size = 0.5,
                             dot_args = list(aes(colour = model, shape = model, size=0.1))
) +
  scale_color_manual(values = cols, name = "Partisan type") + 
  scale_shape_manual(values = shapes, 
                     name = "Partisan type",
                     guide = guide_legend(reverse = TRUE)) +
  theme_bw(base_size = 14) +xlab("\n Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Effect of Trash-talking Treatment on Perceptions of Corruption by Partisanship") +
  guides(colour = guide_legend(override.aes = list(size=2), reverse=TRUE), 
         size = "none") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.1, .75),
        legend.box.background = element_rect(colour = "black"),
        legend.title=element_blank(), plot.title = element_text(face = "bold", size=18),
        axis.text=element_text(size=14),
        axis.title = element_text(size=14),
        legend.text = element_text(size = 10))  # Adjust text size here
ate_corruption_plot


## Figure 7
### Create a data frame for all subgroups manually
ATE_df_subgroup <- data.frame(
  subgroup = c(rep("pooled", 1), rep("morena", 1), rep("nonpartisan", 1), rep("otherpartisan", 1), rep("nonmorena", 1)),
  term = rep(c("Feeling Angry"), 5),
  estimate = numeric(5),
  std.error = numeric(5),
  p.value = numeric(5),  # Add a column for p-values
  model = character(5)  # Add a column for model names
)

### Fill the coefficients
ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "pooled"] <- c(
  angry_trashtalking$coefficients[2]
)
ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "morena"] <- c(
  angry_trashtalking_morena$coefficients[2]
)

ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "nonpartisan"] <- c(
  angry_trashtalking_nonpartisan$coefficients[2]
)

ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "otherpartisan"] <- c(
  angry_trashtalking_otherpartisan$coefficients[2]
)

ATE_df_subgroup$estimate[ATE_df_subgroup$subgroup == "nonmorena"] <- c(
  angry_trashtalking_nonmorena$coefficients[2]
)

### Fill p-values
ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "pooled"] <- c(
  angry_trashtalking$p.value[2]
)

ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "morena"] <- c(
  angry_trashtalking_morena$p.value[2]
)

ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "nonpartisan"] <- c(
  angry_trashtalking_nonpartisan$p.value[2]
)

ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "otherpartisan"] <- c(
  angry_trashtalking_otherpartisan$p.value[2]
)

ATE_df_subgroup$p.value[ATE_df_subgroup$subgroup == "nonmorena"] <- c(
  angry_trashtalking_nonmorena$p.value[2]
)

### Fill SE
ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "pooled"] <- c(
  angry_trashtalking$std.error[2]
)
ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "morena"] <- c(
  angry_trashtalking_morena$std.error[2]
)

ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "nonpartisan"] <- c(
  angry_trashtalking_nonpartisan$std.error[2]
)

ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "otherpartisan"] <- c(
  angry_trashtalking_otherpartisan$std.error[2]
)

ATE_df_subgroup$std.error[ATE_df_subgroup$subgroup == "nonmorena"] <- c(
  angry_trashtalking_nonmorena$std.error[2]
)

### Label groups
ATE_df_subgroup <- ATE_df_subgroup %>%
  mutate(
    model = case_when(
      subgroup == "pooled" ~ "Pooled",
      subgroup == "morena" ~ "MORENA",
      subgroup == "nonpartisan" ~ "Non-partisan",
      subgroup == "otherpartisan" ~ "Opposition partisan",
      subgroup == "nonmorena" ~ "Non-MORENA"))


### Filter OUT non-MORENA
ATE_df_subgroup <- ATE_df_subgroup %>% filter(subgroup!="nonmorena")

### Colors and shapes
cols <- c("Pooled" = "black",  "MORENA" = "red2", "Non-partisan" = "green2", "Opposition partisan" = "blue")
shapes <- c("Pooled" = 18,  "MORENA" = 17, "Non-partisan" = 16, "Opposition partisan" = 15)

### Create the dot-and-whisker plot for Figure 7
ate_angry_plot<- dwplot(ATE_df_subgroup,
                        dodge_size = 0.5,
                        dot_args = list(aes(colour = model, shape = model, size=0.1))
) +
  scale_color_manual(values = cols, name = "Partisan type") + 
  scale_shape_manual(values = shapes, 
                     name = "Partisan type",
                     guide = guide_legend(reverse = TRUE)) +
  theme_bw(base_size = 14) +xlab("\n Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("Effect of Trash-talking Treatment on Feeling Angry by Partisanship") +
  guides(colour = guide_legend(override.aes = list(size=2), reverse=TRUE), 
         size = "none") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.2, .75),
        legend.box.background = element_rect(colour = "black"),
        legend.title=element_blank(), plot.title = element_text(face = "bold", size=18),
        axis.text=element_text(size=14),
        axis.title = element_text(size=14))
ate_angry_plot



# VI. Appendix Tables and Figures
## Table A.1 and Figure A.10
### Subset variables to assess balance
covs_trashtalking <- mexico %>% subset(select = c(male, age, as.numeric(socioeconomic_level), political_interest, pres_approval, region))

# Change structure of factors for presentation 
covs_trashtalking$region <- as.factor(covs_trashtalking$region)
covs_trashtalking$socioeconomic_level <- as.numeric(covs_trashtalking$socioeconomic_level)

levels(covs_trashtalking$region) <- c(
  "1", "2", "3", 
  "4", "5", "6")

# Create a named vector with the covariate labels
covariate_labels <- c(
  "male" = "Gender (Male)",
  "age" = "Age",
  "socioeconomic_level" = "Socioeconomic Level",
  "political_interest" = "Political Interest",
  "pres_approval" = "Presidential Approval",
  "region" = "Region"
)

names(covs_trashtalking) <- covariate_labels[names(covs_trashtalking)]
###  Create Tab A.1.
bal.tab(covs_trashtalking, treat = mexico$treatment,
        binary = "std", continuous = "std")
### Create Figure Figure A.10
love.plot(covs_trashtalking, treat = mexico$treatment, thresholds = c(m = .1), var.names = covariate_labels,
          binary = "std", continuous = "std")


## Table A.2
texreg(list(anti_democratic_index_trashtalking, fire_judges_trashtalking,
            disobey_justice_trashtalking, overthrow_institutions_trashtalking,
            power_institutions_trashtalking), 
       include.ci = FALSE,
       custom.model.names = c("Anti-democratic index", "Fire judges", "Disobey justice", "Institutional nihilism", "Less power to institutions"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.3
texreg(list(anti_democratic_index_trashtalking_controls, fire_judges_trashtalking_controls,
            disobey_justice_trashtalking_controls, overthrow_institutions_trashtalking_controls,
            power_institutions_trashtalking_controls), 
       include.ci = FALSE,
       custom.model.names = c("Anti-democratic index", "Fire judges", "Disobey justice", "Institutional nihilism", "Less power to institutions"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.4
texreg(list(anti_democratic_index_trashtalking, anti_democratic_index_trashtalking_morena,
            anti_democratic_index_trashtalking_nonpartisan, anti_democratic_index_trashtalking_otherpartisan,
            anti_democratic_index_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.5
texreg(list(anti_democratic_index_trashtalking_controls, anti_democratic_index_trashtalking_controls_morena,
            anti_democratic_index_trashtalking_controls_nonpartisan, anti_democratic_index_trashtalking_controls_otherpartisan,
            anti_democratic_index_trashtalking_controls_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.6
texreg(list(fire_judges_trashtalking, fire_judges_trashtalking_morena,
            fire_judges_trashtalking_nonpartisan, fire_judges_trashtalking_otherpartisan,
            fire_judges_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.7
texreg(list(fire_judges_trashtalking_controls, fire_judges_trashtalking_controls_morena,
            fire_judges_trashtalking_controls_nonpartisan, fire_judges_trashtalking_controls_otherpartisan,
            fire_judges_trashtalking_controls_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.8
texreg(list(disobey_justice_trashtalking, disobey_justice_trashtalking_morena,
            disobey_justice_trashtalking_nonpartisan, disobey_justice_trashtalking_otherpartisan,
            disobey_justice_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.9
texreg(list(disobey_justice_trashtalking_controls, disobey_justice_trashtalking_controls_morena,
            disobey_justice_trashtalking_controls_nonpartisan, disobey_justice_trashtalking_controls_otherpartisan,
            disobey_justice_trashtalking_controls_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.10
texreg(list(overthrow_institutions_trashtalking, overthrow_institutions_trashtalking_morena,
            overthrow_institutions_trashtalking_nonpartisan, overthrow_institutions_trashtalking_otherpartisan,
            overthrow_institutions_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.11
texreg(list(overthrow_institutions_trashtalking_controls, overthrow_institutions_trashtalking_controls_morena,
            overthrow_institutions_trashtalking_controls_nonpartisan, overthrow_institutions_trashtalking_controls_otherpartisan,
            overthrow_institutions_trashtalking_controls_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.12
texreg(list(power_institutions_trashtalking, power_institutions_trashtalking_morena,
            power_institutions_trashtalking_nonpartisan, power_institutions_trashtalking_otherpartisan,
            power_institutions_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.13
texreg(list(power_institutions_trashtalking_controls, power_institutions_trashtalking_controls_morena,
            power_institutions_trashtalking_controls_nonpartisan, power_institutions_trashtalking_controls_otherpartisan,
            power_institutions_trashtalking_controls_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')

## Table A.14
mexico %>%
  group_by(treatment) %>%
  dplyr::summarise(
    `Anti-Democratic Index` = mean(anti_democratic_index, na.rm = TRUE),
    `Fire Judges` = mean(fire_judges, na.rm = TRUE),
    `Disobey Justice` = mean(disobey_justice_2, na.rm = TRUE),
    `Institutional Nihilism` = mean(overthrow_institutions, na.rm = TRUE),
    `Less Power to Institutions` = mean(power_institutions, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -treatment, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = treatment, values_from = Mean) %>%
  mutate(
    `Percentage Increase` = ((`Trash-talking` - Control) / Control) * 100
  ) %>% 
  mutate(    Control = round(Control, 2),
             `Trash-talking` = round(`Trash-talking`, 2),
             `Percentage Increase` = round(`Percentage Increase`, 1))


## Table A.15
morena %>%
  group_by(treatment) %>%
  dplyr::summarise(
    `Anti-Democratic Index` = mean(anti_democratic_index, na.rm = TRUE),
    `Fire Judges` = mean(fire_judges, na.rm = TRUE),
    `Disobey Justice` = mean(disobey_justice_2, na.rm = TRUE),
    `Institutional Nihilism` = mean(overthrow_institutions, na.rm = TRUE),
    `Less Power to Institutions` = mean(power_institutions, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -treatment, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = treatment, values_from = Mean) %>%
  mutate(
    `Percentage Increase` = ((`Trash-talking` - Control) / Control) * 100
  ) %>% 
  mutate(    Control = round(Control, 2),
             `Trash-talking` = round(`Trash-talking`, 2),
             `Percentage Increase` = round(`Percentage Increase`, 1))


## Table A.16
nonpartisan %>%
  group_by(treatment) %>%
  dplyr::summarise(
    `Anti-Democratic Index` = mean(anti_democratic_index, na.rm = TRUE),
    `Fire Judges` = mean(fire_judges, na.rm = TRUE),
    `Disobey Justice` = mean(disobey_justice_2, na.rm = TRUE),
    `Institutional Nihilism` = mean(overthrow_institutions, na.rm = TRUE),
    `Less Power to Institutions` = mean(power_institutions, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -treatment, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = treatment, values_from = Mean) %>%
  mutate(
    `Percentage Increase` = ((`Trash-talking` - Control) / Control) * 100
  ) %>% 
  mutate(    Control = round(Control, 2),
             `Trash-talking` = round(`Trash-talking`, 2),
             `Percentage Increase` = round(`Percentage Increase`, 1))


## Table A.17
otherpartisan %>%
  group_by(treatment) %>%
  dplyr::summarise(
    `Anti-Democratic Index` = mean(anti_democratic_index, na.rm = TRUE),
    `Fire Judges` = mean(fire_judges, na.rm = TRUE),
    `Disobey Justice` = mean(disobey_justice_2, na.rm = TRUE),
    `Institutional Nihilism` = mean(overthrow_institutions, na.rm = TRUE),
    `Less Power to Institutions` = mean(power_institutions, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -treatment, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = treatment, values_from = Mean) %>%
  mutate(
    `Percentage Increase` = ((`Trash-talking` - Control) / Control) * 100
  ) %>% 
  mutate(    Control = round(Control, 2),
             `Trash-talking` = round(`Trash-talking`, 2),
             `Percentage Increase` = round(`Percentage Increase`, 1))


## Table A.18
nonmorena %>%
  group_by(treatment) %>%
  dplyr::summarise(
    `Anti-Democratic Index` = mean(anti_democratic_index, na.rm = TRUE),
    `Fire Judges` = mean(fire_judges, na.rm = TRUE),
    `Disobey Justice` = mean(disobey_justice_2, na.rm = TRUE),
    `Institutional Nihilism` = mean(overthrow_institutions, na.rm = TRUE),
    `Less Power to Institutions` = mean(power_institutions, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -treatment, names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = treatment, values_from = Mean) %>%
  mutate(
    `Percentage Increase` = ((`Trash-talking` - Control) / Control) * 100
  ) %>% 
  mutate(    Control = round(Control, 2),
             `Trash-talking` = round(`Trash-talking`, 2),
             `Percentage Increase` = round(`Percentage Increase`, 1))


## Table A.19
texreg(list(check_corruption_trashtalking, check_corruption_trashtalking_morena,
            check_corruption_trashtalking_nonpartisan, check_corruption_trashtalking_otherpartisan,
            check_corruption_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.20
texreg(list(check_corruption_trashtalking_controls, check_corruption_trashtalking_controls_morena,
            check_corruption_trashtalking_controls_nonpartisan, check_corruption_trashtalking_controls_otherpartisan,
            check_corruption_trashtalking_controls_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.21
texreg(list(angry_trashtalking, optimistic_trashtalking,
            scared_trashtalking, nervous_trashtalking), 
       include.ci = FALSE,
       custom.model.names = c("Angry", "Optimistic", "Scared", "Nervous"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.22
texreg(list(angry_trashtalking, angry_trashtalking_morena,
            angry_trashtalking_nonpartisan, angry_trashtalking_otherpartisan,
            angry_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.23
texreg(list(optimistic_trashtalking, optimistic_trashtalking_morena,
            optimistic_trashtalking_nonpartisan, optimistic_trashtalking_otherpartisan,
            optimistic_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.24
texreg(list(scared_trashtalking, scared_trashtalking_morena,
            scared_trashtalking_nonpartisan, scared_trashtalking_otherpartisan,
            scared_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.25
texreg(list(nervous_trashtalking, nervous_trashtalking_morena,
            nervous_trashtalking_nonpartisan, nervous_trashtalking_otherpartisan,
            nervous_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.26
### Extract mediation results into a structured dataframe
mediation_results <- data.frame(
  Effect = rep(c("ACME", "ADE", "Total Effect"), each = 4),
  Group = rep(c("MORENA", "Non-partisan", "Opposition partisan", "Non-MORENA"), times = 3),
  Estimate = c(
    summary(mediate_morena_corrupt)$d0, 
    summary(mediate_nonpartisan_corrupt)$d0, 
    summary(mediate_opposition_corrupt)$d0, 
    summary(mediate_nonmorena_corrupt)$d0,
    
    summary(mediate_morena_corrupt)$z0, 
    summary(mediate_nonpartisan_corrupt)$z0, 
    summary(mediate_opposition_corrupt)$z0, 
    summary(mediate_nonmorena_corrupt)$z0,
    
    summary(mediate_morena_corrupt)$tau.coef, 
    summary(mediate_nonpartisan_corrupt)$tau.coef, 
    summary(mediate_opposition_corrupt)$tau.coef, 
    summary(mediate_nonmorena_corrupt)$tau.coef
  ),
  CI_Lower = c(
    summary(mediate_morena_corrupt)$d0.ci[1], 
    summary(mediate_nonpartisan_corrupt)$d0.ci[1], 
    summary(mediate_opposition_corrupt)$d0.ci[1], 
    summary(mediate_nonmorena_corrupt)$d0.ci[1],
    
    summary(mediate_morena_corrupt)$z0.ci[1], 
    summary(mediate_nonpartisan_corrupt)$z0.ci[1], 
    summary(mediate_opposition_corrupt)$z0.ci[1], 
    summary(mediate_nonmorena_corrupt)$z0.ci[1],
    
    summary(mediate_morena_corrupt)$tau.ci[1], 
    summary(mediate_nonpartisan_corrupt)$tau.ci[1], 
    summary(mediate_opposition_corrupt)$tau.ci[1], 
    summary(mediate_nonmorena_corrupt)$tau.ci[1]
  ),
  CI_Upper = c(
    summary(mediate_morena_corrupt)$d0.ci[2], 
    summary(mediate_nonpartisan_corrupt)$d0.ci[2], 
    summary(mediate_opposition_corrupt)$d0.ci[2], 
    summary(mediate_nonmorena_corrupt)$d0.ci[2],
    
    summary(mediate_morena_corrupt)$z0.ci[2], 
    summary(mediate_nonpartisan_corrupt)$z0.ci[2], 
    summary(mediate_opposition_corrupt)$z0.ci[2], 
    summary(mediate_nonmorena_corrupt)$z0.ci[2],
    
    summary(mediate_morena_corrupt)$tau.ci[2], 
    summary(mediate_nonpartisan_corrupt)$tau.ci[2], 
    summary(mediate_opposition_corrupt)$tau.ci[2], 
    summary(mediate_nonmorena_corrupt)$tau.ci[2]
  ),
  P_Value = c(
    summary(mediate_morena_corrupt)$d0.p, 
    summary(mediate_nonpartisan_corrupt)$d0.p, 
    summary(mediate_opposition_corrupt)$d0.p, 
    summary(mediate_nonmorena_corrupt)$d0.p,
    
    summary(mediate_morena_corrupt)$z0.p, 
    summary(mediate_nonpartisan_corrupt)$z0.p, 
    summary(mediate_opposition_corrupt)$z0.p, 
    summary(mediate_nonmorena_corrupt)$z0.p,
    
    summary(mediate_morena_corrupt)$tau.p, 
    summary(mediate_nonpartisan_corrupt)$tau.p, 
    summary(mediate_opposition_corrupt)$tau.p, 
    summary(mediate_nonmorena_corrupt)$tau.p
  ),
  Num_Obs = rep(c(nrow(morena), nrow(nonpartisan), nrow(otherpartisan), nrow(nonmorena)), 3)
)

### Function to add significance stars
add_stars <- function(estimate, p_value) {
  stars <- ifelse(p_value < 0.001, "***", 
                  ifelse(p_value < 0.01, "**", 
                         ifelse(p_value < 0.05, "*", "")))
  paste0(round(estimate, 2), stars)
}

### Format estimates and confidence intervals for display
mediation_results <- mediation_results %>%
  mutate(
    Estimate = mapply(add_stars, Estimate, P_Value),
    CI_Lower = round(CI_Lower, 2),
    CI_Upper = round(CI_Upper, 2),
    Display = paste0(Estimate, " (", CI_Lower, ", ", CI_Upper, ")")
  ) %>%
  dplyr::select(Effect, Group, Display) %>%  # Fix: Use "Effect" instead of "Statistic"
  pivot_wider(names_from = Group, values_from = Display)

### Create the Num. obs row as a dataframe
num_obs_row <- data.frame(
  Effect = "Num. obs",  # Fix: Match "Effect" instead of "Statistic"
  MORENA = as.character(nrow(morena)),  
  `Non-partisan` = as.character(nrow(nonpartisan)),
  `Opposition partisan` = as.character(nrow(otherpartisan)),
  `Non-MORENA` = as.character(nrow(nonmorena))
)

### Bind the sample size row to the main dataframe
colnames(num_obs_row) <- colnames(mediation_results)
mediation_results <- bind_rows(mediation_results, num_obs_row)

# Print Table A.26
mediation_results


## Table A.27
### Extract mediation results into a structured dataframe
mediation_results_2 <- data.frame(
  Effect = rep(c("ACME", "ADE", "Total Effect"), each = 4),
  Group = rep(c("MORENA", "Non-partisan", "Opposition partisan", "Non-MORENA"), times = 3),
  Estimate = c(
    summary(mediate_morena_angry)$d0, 
    summary(mediate_nonpartisan_angry)$d0, 
    summary(mediate_opposition_angry)$d0, 
    summary(mediate_nonmorena_angry)$d0,
    
    summary(mediate_morena_angry)$z0, 
    summary(mediate_nonpartisan_angry)$z0, 
    summary(mediate_opposition_angry)$z0, 
    summary(mediate_nonmorena_angry)$z0,
    
    summary(mediate_morena_angry)$tau.coef, 
    summary(mediate_nonpartisan_angry)$tau.coef, 
    summary(mediate_opposition_angry)$tau.coef, 
    summary(mediate_nonmorena_angry)$tau.coef
  ),
  CI_Lower = c(
    summary(mediate_morena_angry)$d0.ci[1], 
    summary(mediate_nonpartisan_angry)$d0.ci[1], 
    summary(mediate_opposition_angry)$d0.ci[1], 
    summary(mediate_nonmorena_angry)$d0.ci[1],
    
    summary(mediate_morena_angry)$z0.ci[1], 
    summary(mediate_nonpartisan_angry)$z0.ci[1], 
    summary(mediate_opposition_angry)$z0.ci[1], 
    summary(mediate_nonmorena_angry)$z0.ci[1],
    
    summary(mediate_morena_angry)$tau.ci[1], 
    summary(mediate_nonpartisan_angry)$tau.ci[1], 
    summary(mediate_opposition_angry)$tau.ci[1], 
    summary(mediate_nonmorena_angry)$tau.ci[1]
  ),
  CI_Upper = c(
    summary(mediate_morena_angry)$d0.ci[2], 
    summary(mediate_nonpartisan_angry)$d0.ci[2], 
    summary(mediate_opposition_angry)$d0.ci[2], 
    summary(mediate_nonmorena_angry)$d0.ci[2],
    
    summary(mediate_morena_angry)$z0.ci[2], 
    summary(mediate_nonpartisan_angry)$z0.ci[2], 
    summary(mediate_opposition_angry)$z0.ci[2], 
    summary(mediate_nonmorena_angry)$z0.ci[2],
    
    summary(mediate_morena_angry)$tau.ci[2], 
    summary(mediate_nonpartisan_angry)$tau.ci[2], 
    summary(mediate_opposition_angry)$tau.ci[2], 
    summary(mediate_nonmorena_angry)$tau.ci[2]
  ),
  P_Value = c(
    summary(mediate_morena_angry)$d0.p, 
    summary(mediate_nonpartisan_angry)$d0.p, 
    summary(mediate_opposition_angry)$d0.p, 
    summary(mediate_nonmorena_angry)$d0.p,
    
    summary(mediate_morena_angry)$z0.p, 
    summary(mediate_nonpartisan_angry)$z0.p, 
    summary(mediate_opposition_angry)$z0.p, 
    summary(mediate_nonmorena_angry)$z0.p,
    
    summary(mediate_morena_angry)$tau.p, 
    summary(mediate_nonpartisan_angry)$tau.p, 
    summary(mediate_opposition_angry)$tau.p, 
    summary(mediate_nonmorena_angry)$tau.p
  ),
  Num_Obs = rep(c(nrow(morena), nrow(nonpartisan), nrow(otherpartisan), nrow(nonmorena)), 3)
)

### Format estimates and confidence intervals for display
mediation_results_2 <- mediation_results_2 %>%
  mutate(
    Estimate = mapply(add_stars, Estimate, P_Value),
    CI_Lower = round(CI_Lower, 2),
    CI_Upper = round(CI_Upper, 2),
    Display = paste0(Estimate, " (", CI_Lower, ", ", CI_Upper, ")")
  ) %>%
  dplyr::select(Effect, Group, Display) %>%  # Fix: Use "Effect" instead of "Statistic"
  pivot_wider(names_from = Group, values_from = Display)

###  Bind the sample size row to the main dataframe
colnames(num_obs_row) <- colnames(mediation_results_2)
mediation_results_2 <- bind_rows(mediation_results_2, num_obs_row)

### Print Table A.27
mediation_results_2


## Table A.28
texreg(list(strong_leader_trashtalking, strong_leader_trashtalking_morena,
            strong_leader_trashtalking_nonpartisan, strong_leader_trashtalking_otherpartisan,
            strong_leader_trashtalking_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')


## Table A.29
texreg(list(strong_leader_trashtalking_controls, strong_leader_trashtalking_controls_morena,
            strong_leader_trashtalking_controls_nonpartisan, strong_leader_trashtalking_controls_otherpartisan,
            strong_leader_trashtalking_controls_nonmorena), 
       include.ci = FALSE,
       custom.model.names = c("Pooled", "MORENA", "Non-partisan", "Opposition partisan", "NON-Morena"),
       custom.coef.names= c('Treatment'),
       reorder.gof = c(3,1,2,4),
       omit.coef = '(Intercept)|(age)|(male)|(socio)|(political)|(pres)|treatment == "Trash-talking"TRUE:')