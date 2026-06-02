
library(piecewiseSEM)
library(lme4)
library(lmerTest)
library(car)
library(MASS)
library(MuMIn)
library(parallel)
library(raster)
library(lavaan)
library(survey)
library(ggplot2)
library(gridExtra)
library(tidyverse)

library(performance)
library(ncf)

div.table <- read.csv("D:/heathycare data/贝叶斯_income.csv", sep = ",") # from manage.clean.Rd
print(div.table )
div.table <- div.table %>%
  filter(
    all > 0,
    pharmacy > 0,
    hospital > 0,
    clinic > 0,
    GDP > 0,
    
    PM25 > 0,
    
    Cardiovascular.diseases> 0,
    
    
    BMI>0,
    Lpa>0,
    Lp>0
  )

div.table$all.log.sc <- scale(log(div.table$all)) 
div.table$pharmacy.log.sc <- scale(log(div.table$pharmacy))
div.table$hospital.log.sc <- scale(log(div.table$hospital))
div.table$clinic.log.sc <- scale(log(div.table$clinic))
div.table$GDP.log.sc <- scale(log(div.table$GDP)) 
div.table$schooling.log.sc <- scale(log(div.table$schooling))
div.table$PM25.log.sc <- scale(log(div.table$PM25))
div.table$Urban.log.sc <- scale(log(div.table$Urban)) 
div.table$Smoking.log.sc <- scale(log(div.table$Smoking))
div.table$Meat.log.sc <- scale(log(div.table$Meat))
div.table$lowvegetables.log.sc <- scale(log(div.table$lowvegetables))
div.table$NDVI.log.sc <- scale(log(div.table$NDVI))
div.table$BMI.log.sc <- scale(log(div.table$BMI))
div.table$Lpa.log.sc <- scale(log(div.table$Lpa))
div.table$LP.log.sc <- scale(log(div.table$Lp))
div.table$HHD.log.sc <- scale(log(div.table$Hypertensive.heart.disease))
div.table$IHD.log.sc <- scale(log(div.table$Ischemic.heart.disease))
div.table$Str.log.sc <- scale(log(div.table$Stroke))

div.table$LDL.log.sc <- scale(log(div.table$LDL))
div.table$Hfpg.log.sc <- scale(log(div.table$Hfpg))
div.table$Hbp.log.sc <- scale(log(div.table$Hbp))
div.table$Kd.log.sc <- scale(log(div.table$Kd))
div.table$cvd.log.sc <-scale(log (div.table$Cardiovascular.diseases))

div.table$all2=div.table$all.log.sc^2
div.table <- as.data.frame(div.table)

div.table$Country <- as.factor(div.table$Country)

div.table$income_cat <- factor(
  div.table$income,
  levels = c(1, 2, 3, 4),
  labels = c("Low", "Lower-Mid", "Upper-Mid", "High")
)

LI_data <- div.table %>% filter(income_cat == "Low")  
LMI_data <- div.table %>% filter(income_cat =="Lower-Mid") 
UMI_data <- div.table %>% filter(income_cat=="Upper-Mid")
HI_data <- div.table %>% filter(income_cat == "High") 

div.table <-div.table %>%
  mutate(
    all_class = cut(
      all,
      breaks = quantile(all, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Low", "Mid", "High")
    )
  )
LSHA_data <- div.table %>% filter( all_class == "Low")  

MSHA_data <- div.table %>% filter( all_class=="Mid")
HSHA_data <- div.table %>% filter( all_class == "High") 

numeric_vars <- c("all.log.sc","all2" ,"pharmacy.log.sc", "hospital.log.sc", "clinic.log.sc", 
                  "GDP.log.sc", "schooling.log.sc", "PM25.log.sc", "Urban.log.sc",
                  "Smoking.log.sc", "Meat.log.sc", "lowvegetables.log.sc", "NDVI.log.sc",
                  "BMI.log.sc", "Lpa.log.sc", "LP.log.sc", "LDL.log.sc", "Hfpg.log.sc",
                  "Hbp.log.sc", "Kd.log.sc", "cvd.log.sc","HHD.log.sc","IHD.log.sc","Str.log.sc")



for(var in numeric_vars) {
  if(var %in% names(div.table)) {
    LI_data [[var]] <- as.numeric(LI_data [[var]])
  }
}
for(var in numeric_vars) {
  if(var %in% names(div.table)) {
    LMI_data [[var]] <- as.numeric(LMI_data [[var]])
  }
}
for(var in numeric_vars) {
  if(var %in% names(div.table)) {
    UMI_data [[var]] <- as.numeric(UMI_data [[var]])
  }
}
for(var in numeric_vars) {
  if(var %in% names(div.table)) {
    HI_data [[var]] <- as.numeric(HI_data [[var]])
  }
}
for(var in numeric_vars) {
  if(var %in% names(div.table)) {
    LSHA_data [[var]] <- as.numeric(LSHA_data [[var]])
  }
}
for(var in numeric_vars) {
  if(var %in% names(div.table)) {
    MSHA_data [[var]] <- as.numeric(MSHA_data [[var]])
  }
}
for(var in numeric_vars) {
  if(var %in% names(div.table)) {
    HSHA_data [[var]] <- as.numeric(HSHA_data [[var]])
  }
}


psem_covara <- psem(
  

  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  LI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  
  
  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  LI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  
  
  
  lmer_cvd = lmer(cvd.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  # 协方差路径
  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% all.log.sc
)

summary(psem_covara)
fisherC(psem_covara)
coefs(psem_covara)



rsquared(psem_covara)

psem_covarb <- psem(

  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  LMI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  

  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  LMI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  
  
  
  lmer_cvd = lmer(cvd.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data = LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% all.log.sc
  
)


summary(psem_covarb)
fisherC(psem_covarb)
coefs(psem_covarb)


rsquared(psem_covarb)

psem_covarc <- psem(
  

  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =   UMI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  
  
  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data = UMI_data, na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  lmer_cvd = lmer(cvd.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc
  
)


summary(psem_covarc)
fisherC(psem_covarc)
coefs(psem_covarc)


rsquared(psem_covarc)

psem_covard <- psem(
  
  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  HI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  HI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc + LP.log.sc + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  
  lmer_cvd = lmer(cvd.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc ++
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data = HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% BMI.log.sc,
  PM25.log.sc %~~%  LDL.log.sc ,
  PM25.log.sc %~~% all.log.sc ,
  cvd.log.sc %~~% BMI.log.sc 
)

summary(psem_covard)
fisherC(psem_covard)


rsquared(psem_covard)


psem_covara_str <- psem(
  
  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  LI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  
  
  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  LI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_cvd = lmer(Str.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% all.log.sc
)


summary(psem_covara_str)
fisherC(psem_covara_str)





psem_covarb_str <- psem(
  

  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  LMI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + (1|Country),
                 data =  LMI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  
  
  
  lmer_cvd = lmer(Str.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data = LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),

  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% all.log.sc,
  Str.log.sc  %~~%BMI.log.sc
)


summary(psem_covarb_str)
fisherC(psem_covarb_str)


psem_covarc_str <- psem(
  

  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =   UMI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  
  
  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data = UMI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  
  
  
  lmer_cvd = lmer(Str.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc
  
)


summary(psem_covarc_str)
fisherC(psem_covarc_str)


psem_covard_str <- psem(
  
 
  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  HI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  
  

  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  HI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc + LP.log.sc + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_cvd = lmer(Str.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc ++
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data = HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% BMI.log.sc,
  PM25.log.sc %~~%  LDL.log.sc ,
  PM25.log.sc %~~% all.log.sc ,
  Str.log.sc %~~% BMI.log.sc 
)

summary(psem_covard_str)
fisherC(psem_covard_str)



psem_covara_hhd <- psem(
  

  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  LI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  LI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
 
  
  lmer_cvd = lmer(HHD.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),

  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% all.log.sc
)

summary(psem_covara_hhd)
fisherC(psem_covara_hhd)





psem_covarb_hhd <- psem(
  
  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  LMI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  

  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  LMI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),

  
  
  
  lmer_cvd = lmer(HHD.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data = LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),

  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% all.log.sc
  
)

summary(psem_covarb_hhd)
fisherC(psem_covarb_hhd)


psem_covarc_hhd <- psem(
  
  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =   UMI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  

  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data = UMI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  lmer_cvd = lmer(HHD.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc
  
)

summary(psem_covarc_hhd)
fisherC(psem_covarc_hhd)


psem_covard_hhd <- psem(
  
  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  HI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  HI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc + LP.log.sc + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  lmer_cvd = lmer(HHD.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc ++
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data = HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% BMI.log.sc,
  PM25.log.sc %~~%  LDL.log.sc ,
  PM25.log.sc %~~% all.log.sc ,
  HHD.log.sc %~~% BMI.log.sc 
)

summary(psem_covard_hhd)
fisherC(psem_covard_hhd)


psem_covara_ihd <- psem(

  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  LI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  LI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
 
  
  lmer_cvd = lmer(IHD.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data =  LI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  

  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% all.log.sc
)

summary(psem_covara_ihd)
fisherC(psem_covara_ihd)





psem_covarb_ihd <- psem(
  
  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  LMI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  LMI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =  LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_cvd = lmer(IHD.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data = LMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% all.log.sc
  
)

summary(psem_covarb_ihd)
fisherC(psem_covarb_ihd)


psem_covarc_ihd <- psem(

  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =   UMI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc  + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data = UMI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc +
                    BMI.log.sc + LP.log.sc + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
 
  
  lmer_cvd = lmer(IHD.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc +
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data =   UMI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
 
  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  IHD.log.sc  %~~%BMI.log.sc
)

summary(psem_covarc_ihd)
fisherC(psem_covarc_ihd)


psem_covard_ihd <- psem(
  
  lmer_all = lmer(all.log.sc ~ GDP.log.sc  + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_PM25 = lmer(PM25.log.sc ~ GDP.log.sc  + (1|Country),
                   data =  HI_data , na.action = na.omit,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
 
  lmer_BMI = lmer(BMI.log.sc ~GDP.log.sc + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_Lp = lmer(LP.log.sc ~ GDP.log.sc + PM25.log.sc + (1|Country),
                 data =  HI_data , na.action = na.omit,
                 control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  lmer_LDL = lmer(LDL.log.sc ~ all.log.sc  + GDP.log.sc + LP.log.sc + (1|Country),
                  data =  HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  
  
  lmer_cvd = lmer(IHD.log.sc ~ all.log.sc + LDL.log.sc + LP.log.sc ++
                    PM25.log.sc+ GDP.log.sc  + (1|Country),
                  data = HI_data , na.action = na.omit,
                  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))),
  
  all.log.sc %~~% LP.log.sc,
  all.log.sc %~~% BMI.log.sc,
  LP.log.sc  %~~%BMI.log.sc,
  PM25.log.sc %~~% BMI.log.sc,
  PM25.log.sc %~~%  LDL.log.sc ,
  PM25.log.sc %~~% all.log.sc ,
  IHD.log.sc %~~% BMI.log.sc 
)

summary(psem_covard_ihd)
fisherC(psem_covard_ihd)