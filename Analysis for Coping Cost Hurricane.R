library(readr)
library(bayesplot)
library(brms)
library(ggplot2)
library(plyr)
library(cowplot)

setwd("~/Coping Cost Anal")

dat <- read_csv("Hurricane Game - Prolific - Coping Cost - Final_April 11, 2022_05.23.csv")
dat_3rd <- read_csv("Hurricane Game - Prolific - Coping Cost - Final - Low util_April 11, 2022_14.34.csv")


dat <- dat[-c(1,2),]
dat <- dat[-which(dat$Finished=="False"),]
dat_3rd <- dat_3rd[-c(1,2),]
dat_3rd$Condition <- "High Uncertainty - Low Util"

dim(dat)
dim(dat_3rd)
dat <- rbind(dat,dat_3rd)

## Cleaning data 
subset_var <- c("Duration (in seconds)","FI_Time_First Click","FI_Time_Last Click"  
                ,"FI_Time_Page Submit","FI_Time_Click Count","B1_wind_1"           
                ,"B1_flood_1"            ,"Appraisal1_1"        ,"Decision"             
                ,"SI_Time_First Click"   ,"SI_Time_Last Click"    ,"SI_Time_Page Submit"  
                ,"SI_Time_Click Count"   ,"Appraisal2_evac_4"     ,"Appraisal2_stay_1"    
                ,"B2_wind_1"             ,"B2_flood_1"            ,"age"                  
                ,"gender"                ,"zipcode"               ,"prior_experience"     
                ,"Condition","QCity_zipcode","QState_zipcode")
dat <- dat[subset_var]

dat$age_n <- as.numeric(dat$age)
dat$age_log <- log(dat$age_n)
dat$duration <- as.numeric(dat$`Duration (in seconds)`)
dat$`FI_Time_Page Submit` <- as.numeric(dat$`FI_Time_Page Submit`)
dat$`SI_Time_Page Submit` <- as.numeric(dat$`SI_Time_Page Submit`)

dat$decision <- ifelse(dat$Decision == "Evacuate to a hotel up north paying at least $150 per night.",
                       "evac","stay")
old_cond <- c("High Uncertainty - Cat 4","Low Uncertainty - Cat 4","High Uncertainty - Cat 2",
          "High Uncertainty - Low Util")
new_cond <- c("High Unc, Cat 4, 1st Fl","Low Unc, Cat 4, 1st Fl","High Unc, Cat 2, 1st Fl",
              "High Unc, Cat 4, 3rd Fl")
dat$condition <- mapvalues(dat$Condition, from = old_cond, to = new_cond)

dat$B1_flood  <- as.numeric(dat$B1_flood_1)
dat$B1_wind   <- as.numeric(dat$B1_wind_1)
dat$B2_flood  <- as.numeric(dat$B2_flood_1)
dat$B2_wind   <- as.numeric(dat$B2_wind_1)

dat$diff_flood <- dat$B2_flood - dat$B1_flood
dat$diff_wind  <- dat$B2_wind  - dat$B1_wind

dat$Appraisal1 <- as.numeric(dat$Appraisal1_1)
dat$Appraisal2_evac <- as.numeric(dat$Appraisal2_evac_4)
dat$Appraisal2_stay <- as.numeric(dat$Appraisal2_stay_1)
dat$Appraisal2_evac <- ifelse(is.na(dat$Appraisal2_evac),"",dat$Appraisal2_evac)
dat$Appraisal2_stay <- ifelse(is.na(dat$Appraisal2_stay),"",dat$Appraisal2_stay)
dat$Appraisal2 <- as.numeric(paste0(dat$Appraisal2_evac,dat$Appraisal2_stay))

dat$decision_x_condition <- paste0(dat$decision,":",dat$condition)

exclude_FL <- which(dat$QState_zipcode != "FL")

dat_exclude <- dat[-exclude_FL,]
#dat_exclude <- dat
dat_exclude <- dat_exclude[-which(dat_exclude$gender=="Other"),]

## First Condition: High vs Low Uncertainty
dat_exc_uncertain <- dat_exclude[which(dat_exclude$Condition == "High Uncertainty - Cat 4" |
                                        dat_exclude$Condition == "Low Uncertainty - Cat 4"),]

dat_exc_uncer_stay <- dat_exc_uncertain[which(dat_exc_uncertain$decision == "stay"),]
dat_exc_uncer_evac <- dat_exc_uncertain[which(dat_exc_uncertain$decision == "evac"),]

## Second Condition: High vs Low Utility
dat_exc_utility <- dat_exclude[which(dat_exclude$Condition == "High Uncertainty - Cat 4" |
                                       dat_exclude$Condition == "High Uncertainty - Low Util"),]
dat_exc_util_stay <- dat_exc_utility[which(dat_exc_utility$decision == "stay"),]
dat_exc_util_evac <- dat_exc_utility[which(dat_exc_utility$decision == "evac"),]

## Third Condition: Cat 4 vs Cat 2
## Subtract the post beliefs from the mean/mid point (the stimuli)
dat_exclude$diff_mid_flood <- ifelse(dat_exclude$Condition == "High Uncertainty - Cat 2",
                                     dat_exclude$B2_flood - 8, 
                                     dat_exclude$B2_flood - 16)

dat_exclude$diff_mid_wind <- ifelse(dat_exclude$Condition == "High Uncertainty - Cat 2",
                                    dat_exclude$B2_wind - 100, 
                                    dat_exclude$B2_wind - 140)

dat_exclude_42 <- dat_exclude[which(dat_exclude$Condition == "High Uncertainty - Cat 4" |
                                      dat_exclude$Condition == "High Uncertainty - Cat 2"),]
dat_exc_42_stay <- dat_exclude_42[which(dat_exclude_42$decision == "stay"),]
dat_exc_42_evac <- dat_exclude_42[which(dat_exclude_42$decision == "evac"),]

## Each condition 
dat_exclude_high <- dat_exclude[which(dat_exclude$Condition == "High Uncertainty - Cat 4"),]
dat_exclude_low  <- dat_exclude[which(dat_exclude$Condition == "Low Uncertainty - Cat 4"),]
dat_exclude_cat2 <- dat_exclude[which(dat_exclude$Condition == "High Uncertainty - Cat 2"),]
dat_exclude_util <- dat_exclude[which(dat_exclude$Condition == "High Uncertainty - Low Util"),]

#View(dat[,c("Condition","decision","B1_flood","B2_flood","B1_wind","B2_wind","Appraisal1","Appraisal2","gender" ,"age",                 "QState_zipcode",    "duration",  "FI_Time_Page Submit", "SI_Time_Page Submit")])

################################################################################

##  Descriptive Stats  ##
### Overall 
dim(dat)
dim(dat_exclude)
table(dat_exclude$gender,dat_exclude$condition)
summary(dat_exclude$age_n[dat_exclude$condition=="High Unc, 1st Fl, Cat 4"])
summary(dat_exclude$age_n[dat_exclude$condition=="Low Unc, 1st Fl, Cat 4"])
summary(dat_exclude$age_n[dat_exclude$condition=="High Unc, 3rd Fl, Cat 4"])
summary(dat_exclude$age_n[dat_exclude$condition=="High Unc, 1st Fl, Cat 2"])
table(dat_exclude$decision, dat_exclude$condition)
##Apraisal


##  Data Analysis  ## 
seed_n <- 777
### Uncertainty: High vs Low ###
m_uncer_stay_flood <- brm(B2_flood ~ condition + B1_flood, 
                          data = dat_exc_uncer_stay,
                        family = gaussian(), iter = 6000, warmup = 3000,
                        file = "model/m_uncer_stay_flood",
                        seed = seed_n)

m_uncer_stay_wind <- brm(B2_wind ~ condition + B1_wind, data = dat_exc_uncer_stay,
                          family = gaussian(), iter = 6000, warmup = 3000,
                         file = "model/m_uncer_stay_wind",
                         seed = seed_n)

##### Evac case #####
m_uncer_evac_flood <- brm(B2_flood ~ condition , data = dat_exc_uncer_evac,
                          family = gaussian(), iter = 6000, warmup = 3000,
                          file = "model/m_uncer_evac_flood",
                          seed = seed_n)

m_uncer_evac_wind <- brm(B2_wind ~ condition, data = dat_exc_uncer_evac,
                          family = gaussian(), iter = 6000, warmup = 3000,
                         file = "model/m_uncer_evac_wind",
                         seed = seed_n)

### Output ###

h1 <- hypothesis(m_uncer_stay_flood, "conditionLowUncCat41stFl  > 0 ")
h2 <- hypothesis(m_uncer_stay_wind, "conditionLowUncCat41stFl  > 0 ")
h3 <- hypothesis(m_uncer_evac_flood, "conditionLowUncCat41stFl  > 0 ")
h4 <- hypothesis(m_uncer_evac_wind, "conditionLowUncCat41stFl  > 0 ")
dat_h_uncer <- data.frame(
  Bel = c("Stay:Flood","Stay:Wind","Evac;Flood","Evac:Wind"),
  Est = round(c(h1$hypothesis$Estimate,h2$hypothesis$Estimate,
          h3$hypothesis$Estimate,h4$hypothesis$Estimate),2),
  SE  = round(c(h1$hypothesis$Est.Error,h2$hypothesis$Est.Error,
          h3$hypothesis$Est.Error,h4$hypothesis$Est.Error),2),
  CI.Lower = round(c(h1$hypothesis$CI.Lower,h2$hypothesis$CI.Lower,
                h3$hypothesis$CI.Lower,h4$hypothesis$CI.Lower),2),
  CI.Upper = round(c(h1$hypothesis$CI.Upper,h2$hypothesis$CI.Upper,
              h3$hypothesis$CI.Upper,h4$hypothesis$CI.Upper),2),
  prob = round(c(h1$hypothesis$Post.Prob,h2$hypothesis$Post.Prob,
           h3$hypothesis$Post.Prob,h4$hypothesis$Post.Prob),2)
)
dat_h_uncer

g1 <- plot(conditional_effects(m_uncer_stay_flood), plot=FALSE,ask=FALSE)[1]$condition + 
  ylab("Flood Depth (B2)")
g2 <- plot(conditional_effects(m_uncer_stay_wind), plot=FALSE,ask=FALSE)[1]$condition + 
  ylab("Wind Speed (B2)")
#g3 <- plot(conditional_effects(m_uncer_evac_flood), plot=FALSE,ask=FALSE)[1]$condition 
#g4 <- plot(conditional_effects(m_uncer_evac_wind), plot=FALSE,ask=FALSE)[1]$condition  
plot_grid(g1,g2, labels="AUTO")


### Utility: High vs Low ###
m_util_stay_flood <- brm(B2_flood ~ condition + B1_flood, data = dat_exc_util_stay,
                         family = gaussian(), iter = 6000, warmup = 3000,
                         file = "model/m_util_stay_flood",
                         seed = seed_n)

m_util_stay_wind <- brm(B2_wind ~ condition + B1_wind, data = dat_exc_util_stay,
                         family = gaussian(), iter = 6000, warmup = 3000,
                        file = "model/m_util_stay_wind",
                        seed = seed_n)
hypothesis(m_util_stay_wind, "conditionHighUncCat43rdFl < 0")
##### Evac Case #####
m_util_evac_flood <- brm(B2_flood ~ condition, data = dat_exc_util_evac,
                          family = gaussian(), iter = 6000, warmup = 3000,
                         file = "model/m_util_evac_flood",
                         seed = seed_n)

m_util_evac_wind <- brm(B2_wind ~ condition, data = dat_exc_util_evac,
                         family = gaussian(), iter = 6000, warmup = 3000,
                        file = "model/m_util_evac_wind",
                        seed = seed_n)

h1 <- hypothesis(m_util_stay_flood, "conditionHighUncCat43rdFl < 0")
h2 <- hypothesis(m_util_stay_wind, "conditionHighUncCat43rdFl < 0")
h3 <- hypothesis(m_util_evac_flood, "conditionHighUncCat43rdFl < 0")
h4 <- hypothesis(m_util_evac_wind, "conditionHighUncCat43rdFl < 0")
dat_h_util <- data.frame(
  Bel = c("Stay:Flood","Stay:Wind","Evac;Flood","Evac:Wind"),
  Est = round(c(h1$hypothesis$Estimate,h2$hypothesis$Estimate,
                h3$hypothesis$Estimate,h4$hypothesis$Estimate),2),
  SE  = round(c(h1$hypothesis$Est.Error,h2$hypothesis$Est.Error,
                h3$hypothesis$Est.Error,h4$hypothesis$Est.Error),2),
  CI.Lower = round(c(h1$hypothesis$CI.Lower,h2$hypothesis$CI.Lower,
                     h3$hypothesis$CI.Lower,h4$hypothesis$CI.Lower),2),
  CI.Upper = round(c(h1$hypothesis$CI.Upper,h2$hypothesis$CI.Upper,
                     h3$hypothesis$CI.Upper,h4$hypothesis$CI.Upper),2),
  prob = round(c(h1$hypothesis$Post.Prob,h2$hypothesis$Post.Prob,
                 h3$hypothesis$Post.Prob,h4$hypothesis$Post.Prob),2)
)
dat_h_util

g1 <- plot(conditional_effects(m_util_stay_flood), plot=FALSE,ask=FALSE)[1]$condition +
  ylab("Flood Depth (B2)")
g2 <- plot(conditional_effects(m_util_stay_wind), plot=FALSE,ask=FALSE)[1]$condition + 
  ylab("Wind Speed (B2)")
#g3 <- plot(conditional_effects(m_util_evac_flood), plot=FALSE,ask=FALSE)[1]$condition 
#g4 <- plot(conditional_effects(m_util_evac_wind), plot=FALSE,ask=FALSE)[1]$condition  
plot_grid(g1,g2, labels="AUTO")


################################################################################

## Main Coping Effect ##

m1 <- brm(B2_flood ~ decision, data = dat_exclude_high,
            family = gaussian(), iter = 6000, warmup = 3000,
          file = "model/flood_exclude_high")
h1 <- hypothesis(m1, "decisionstay < 0")

m1w <- brm(B2_wind ~ decision, data = dat_exclude_high,
          family = gaussian(), iter = 6000, warmup = 3000,
          file = "model/wind_exclude_high")
h1w <- hypothesis(m1w, "decisionstay < 0")

m2 <- brm(B2_flood~ decision, data = dat_exclude_low,
           family = gaussian(), iter = 6000, warmup = 3000,
          file = "model/flood_exclude_low")
h2 <- hypothesis(m2, "decisionstay < 0")


m2w <- brm(B2_wind ~ decision, data = dat_exclude_low,
           family = gaussian(), iter = 6000, warmup = 3000,
           file = "model/wind_exclude_low")
h2w <- hypothesis(m2w, "decisionstay < 0")

m3 <- brm(B2_flood~ decision, data = dat_exclude_cat2,
          family = gaussian(), iter = 6000, warmup = 3000,
          file = "model/flood_exclude_cat2")
h3 <- hypothesis(m3, "decisionstay < 0")


m3w <- brm(B2_wind ~ decision, data = dat_exclude_cat2,
           family = gaussian(), iter = 6000, warmup = 3000,
           file = "model/wind_exclude_cat2")
h3w <- hypothesis(m3w, "decisionstay < 0")


m4 <- brm(B2_flood ~ decision, data = dat_exclude_util,
          family = gaussian(), iter = 6000, warmup = 3000,
          file = "model/flood_exclude_util")
h4 <- hypothesis(m4, "decisionstay < 0")

m4w <- brm(B2_wind ~ decision, data = dat_exclude_util,
           family = gaussian(), iter = 6000, warmup = 3000,
           file = "model/wind_exclude_util")
h4w <- hypothesis(m4w, "decisionstay < 0")


dat_flood_main <- data.frame(
  Bel = c("High Uncertain","Low Uncertain","Cat 2","Util"),
  Est = round(c(h1$hypothesis$Estimate,h2$hypothesis$Estimate,
                h3$hypothesis$Estimate,h4$hypothesis$Estimate),2),
  SE  = round(c(h1$hypothesis$Est.Error,h2$hypothesis$Est.Error,
                h3$hypothesis$Est.Error,h4$hypothesis$Est.Error),2),
  CI.Lower = round(c(h1$hypothesis$CI.Lower,h2$hypothesis$CI.Lower,
                     h3$hypothesis$CI.Lower,h4$hypothesis$CI.Lower),2),
  CI.Upper = round(c(h1$hypothesis$CI.Upper,h2$hypothesis$CI.Upper,
                     h3$hypothesis$CI.Upper,h4$hypothesis$CI.Upper),2),
  prob = round(c(h1$hypothesis$Post.Prob,h2$hypothesis$Post.Prob,
                 h3$hypothesis$Post.Prob,h4$hypothesis$Post.Prob),2)
)
dat_flood_main


dat_wind_main <- data.frame(
  Bel = c("High Uncertain","Low Uncertain","Cat 2","Util"),
  Est = round(c(h1w$hypothesis$Estimate,h2w$hypothesis$Estimate,
                h3w$hypothesis$Estimate,h4w$hypothesis$Estimate),2),
  SE  = round(c(h1w$hypothesis$Est.Error,h2w$hypothesis$Est.Error,
                h3w$hypothesis$Est.Error,h4w$hypothesis$Est.Error),2),
  CI.Lower = round(c(h1w$hypothesis$CI.Lower,h2w$hypothesis$CI.Lower,
                     h3w$hypothesis$CI.Lower,h4w$hypothesis$CI.Lower),2),
  CI.Upper = round(c(h1w$hypothesis$CI.Upper,h2w$hypothesis$CI.Upper,
                     h3w$hypothesis$CI.Upper,h4w$hypothesis$CI.Upper),2),
  prob = round(c(h1w$hypothesis$Post.Prob,h2w$hypothesis$Post.Prob,
                 h3w$hypothesis$Post.Prob,h4w$hypothesis$Post.Prob),2)
)
dat_wind_main

g1 <- plot(conditional_effects(m1), plot=FALSE,ask=FALSE)[1]$decision + 
  geom_hline(yintercept = 16, linetype = "dashed") + ylim(11.5,19) + 
  ggtitle("High Uncertainty, Cat 4, 1st Fl.") + ylab("Flood Depth (B2)") + 
  scale_x_discrete(labels=c("evac" = "Evacuate", "stay" = "Stay"))
g3 <- plot(conditional_effects(m2), plot=FALSE,ask=FALSE)[1]$decision + 
  geom_hline(yintercept = 16, linetype = "dashed") + ylim(11.5,19) + 
  ggtitle("Low Uncertainty, Cat 4, 1st Fl.") + ylab("Flood Depth (B2)") + 
  scale_x_discrete(labels=c("evac" = "Evacuate", "stay" = "Stay"))
g7 <- plot(conditional_effects(m4), plot=FALSE,ask=FALSE)[1]$decision + 
  geom_hline(yintercept = 16, linetype = "dashed") + ylim(11.5,19) + 
  ggtitle("High Uncertainty, Cat 2, 1st Fl.") + ylab("Flood Depth (B2)") + 
  scale_x_discrete(labels=c("evac" = "Evacuate", "stay" = "Stay"))
g5 <- plot(conditional_effects(m3), plot=FALSE,ask=FALSE)[1]$decision+ 
  geom_hline(yintercept = 8, linetype = "dashed") + ylim(6,9.5) +
  ggtitle("High Uncertainty, Cat 4, 3rd Fl.") + ylab("Flood Depth (B2)") + 
  scale_x_discrete(labels=c("evac" = "Evacuate", "stay" = "Stay"))
plot_grid(g1,g3,g7,g5)

g2 <- plot(conditional_effects(m1w), plot=FALSE,ask=FALSE)[1]$decision + 
  geom_hline(yintercept = 140, linetype = "dashed") + ylim(123.5,146) + 
  ggtitle("High Uncertainty, Cat 4, 1st Fl.") + ylab("Wind Speed (B2)") + 
  scale_x_discrete(labels=c("evac" = "Evacuate", "stay" = "Stay"))
g4 <- plot(conditional_effects(m2w), plot=FALSE,ask=FALSE)[1]$decision + 
  geom_hline(yintercept = 140, linetype = "dashed")  + ylim(123.5,146) + 
  ggtitle("Low Uncertainty, Cat 4, 1st Fl.") + ylab("Wind Speed (B2)") + 
  scale_x_discrete(labels=c("evac" = "Evacuate", "stay" = "Stay"))
g8 <- plot(conditional_effects(m4w), plot=FALSE,ask=FALSE)[1]$decision + 
  geom_hline(yintercept = 140, linetype = "dashed")  + ylim(123.5,146) + 
  ggtitle("High Uncertainty, Cat 2, 1st Fl.") + ylab("Wind Speed (B2)") + 
  scale_x_discrete(labels=c("evac" = "Evacuate", "stay" = "Stay"))
g6 <- plot(conditional_effects(m3w), plot=FALSE,ask=FALSE)[1]$decision+ 
  geom_hline(yintercept = 100, linetype = "dashed") + ylim(95,110) +
  ggtitle("High Uncertainty, Cat 4, 3rd Fl.") + ylab("Wind Speed (B2)") + 
  scale_x_discrete(labels=c("evac" = "Evacuate", "stay" = "Stay"))
plot_grid(g2,g4,g8,g6)




################################################################################


