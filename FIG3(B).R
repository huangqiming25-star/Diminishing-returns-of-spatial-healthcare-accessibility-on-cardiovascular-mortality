library(tidyverse)
library(brms); options(mc.cores = parallel::detectCores()) 
library(performance)
library(ncf)
library(ggplot2)

div.table <- read.csv("D:/heathycare data/data and code/TEST/FIG3/data3.csv", sep = ",") 
print(div.table )
div.table <- div.table %>%
  filter(
    all >0,
    pharmacy > 0,
    hospital >0,
    clinic > 0,
    GDP > 0,
    
    PM25 > 0,
    
    Cardiovascular.diseases > 0
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
div.table$Lp.log.sc <- scale(log(div.table$Lp))


all.q1.logn_inter <- brm(formula = Cardiovascular.diseases ~ 
                           all.log.sc+I(all.log.sc^2)+GDP.log.sc+PM25.log.sc+(1|Country), 
                         family=lognormal(),
                         data = div.table, 
                         
                         warmup = 4000, 
                         iter = 30000,
                         chains = 4, 
                         thin = 10, 
                         refresh = 0) 
summary(all.q1.logn_inter)
r2_bayes(all.q1.logn_inter)


hospital.q1.logn_inter <- brm(formula = Cardiovascular.diseases ~ 
                                hospital.log.sc+I(hospital.log.sc^2)+
                                GDP.log.sc+PM25.log.sc 
                              
                              +(1|Country), 
                              family=lognormal(),
                              data = div.table, 
                              
                              warmup = 4000, 
                              iter = 30000,
                              chains = 4, 
                              thin = 10, 
                              refresh = 0) 
summary(hospital.q1.logn_inter)
r2_bayes(hospital.q1.logn_inter)


clinic.q1.logn_inter <- brm(formula = Cardiovascular.diseases ~ 
                              clinic.log.sc+I(clinic.log.sc^2)+
                              GDP.log.sc+PM25.log.sc +(1|Country), 
                            family=lognormal(),
                            data = div.table, 
                            
                            warmup = 4000, 
                            iter = 30000,
                            chains = 4, 
                            thin = 10, 
                            refresh = 0
) 
summary(clinic.q1.logn_inter)
r2_bayes(clinic.q1.logn_inter)


pharmacy.q1.logn_inter <- brm(formula = Cardiovascular.diseases ~ 
                                pharmacy.log.sc+I(pharmacy.log.sc^2)+
                                GDP.log.sc+PM25.log.sc +(1|Country), 
                              family=lognormal(),
                              data = div.table, 
                              
                              warmup = 4000, 
                              iter = 30000,
                              chains = 4, 
                              thin = 10, 
                              refresh = 0
) 
summary(pharmacy.q1.logn_inter)

all.q1.lognIhd <- brm(formula =Ischemic.heart.disease ~ all.log.sc +GDP.log.sc+I( all.log.sc^2)+PM25.log.sc + (1|Country), 
                      family=lognormal(), data = div.table, warmup = 4000, iter = 30000,chains = 4,  thin = 10, refresh = 0) 
summary(all.q1.lognIhd )


all.q1.lognStr<- brm(formula = Stroke~ all.log.sc +GDP.log.sc+I( all.log.sc^2)+PM25.log.sc + (1|Country), 
                     family=lognormal(), data = div.table, warmup = 4000, iter = 30000,chains = 4,  thin = 10, refresh = 0) 
summary(all.q1.lognStr)

all.q1.lognHHP<- brm(formula = Hypertensive.heart.disease~ all.log.sc +GDP.log.sc+I( all.log.sc^2)+PM25.log.sc + (1|Country), 
                     family=lognormal(), data = div.table, warmup = 4000, iter = 30000,chains = 4,  thin = 10, refresh = 0) 
summary(all.q1.lognHHP)



post_base_all <- posterior_epred(all.q1.logn_inter, newdata = div.table, re_formula = NULL)
post_base_hospital <- posterior_epred(hospital.q1.logn_inter, newdata = div.table, re_formula = NULL)
post_base_clinic <- posterior_epred(clinic.q1.logn_inter, newdata = div.table, re_formula = NULL)
post_base_pharmacy<- posterior_epred(pharmacy.q1.logn_inter, newdata = div.table, re_formula = NULL)

post_base_Ihd <- posterior_epred(all.q1.lognIhd, newdata = div.table, re_formula = NULL)
post_base_Str <- posterior_epred(all.q1.lognStr, newdata = div.table, re_formula = NULL)
post_base_HHP <- posterior_epred(all.q1.lognHHP, newdata = div.table, re_formula = NULL)


q_all       <- quantile(div.table$all.log.sc, 0.7, na.rm = TRUE)
q_hospital  <- quantile(div.table$hospital.log.sc, 0.7, na.rm = TRUE)
q_clinic    <- quantile(div.table$clinic.log.sc, 0.7, na.rm = TRUE)
q_pharmacy  <- quantile(div.table$pharmacy.log.sc, 0.7, na.rm = TRUE)

cf_data <- div.table %>%
  mutate(
    all.log.sc = ifelse(all.log.sc <= q_all, q_all, all.log.sc),
    hospital.log.sc = ifelse(hospital.log.sc <= q_hospital, q_hospital, hospital.log.sc),
    clinic.log.sc = ifelse(clinic.log.sc <= q_clinic, q_clinic, clinic.log.sc),
    pharmacy.log.sc = ifelse(pharmacy.log.sc <= q_pharmacy, q_pharmacy, pharmacy.log.sc)
  )
post_cf_all <- posterior_epred(all.q1.logn_inter, newdata = cf_data, re_formula = NULL)
post_cf_hospital <- posterior_epred(hospital.q1.logn_inter, newdata = cf_data, re_formula = NULL)
post_cf_clinic <- posterior_epred(clinic.q1.logn_inter, newdata = cf_data, re_formula = NULL)
post_cf_pharmacy <- posterior_epred(pharmacy.q1.logn_inter, newdata = cf_data, re_formula = NULL)

post_cf_Ihd <- posterior_epred(all.q1.lognIhd, newdata = cf_data, re_formula = NULL)
post_cf_Str <- posterior_epred(all.q1.lognStr, newdata = cf_data, re_formula = NULL)
post_cf_HHP <- posterior_epred(all.q1.lognHHP, newdata = cf_data, re_formula = NULL)



avoidable_all <- (post_base_all -post_cf_all) * (div.table$Population / 100000)
avoidable_hospital <- (post_base_hospital -post_cf_hospital) * (div.table$Population / 100000)
avoidable_clinic <- (post_base_clinic -post_cf_clinic ) * (div.table$Population / 100000)
avoidable_pharmacy <- (post_base_pharmacy -post_cf_pharmacy) * (div.table$Population / 100000)
avoidable_Ihd <- (post_base_Ihd -post_cf_Ihd) * (div.table$Population / 100000)
avoidable_Str <- (post_base_Str -post_cf_Str) * (div.table$Population / 100000)
avoidable_HHP <- (post_base_HHP -post_cf_HHP) * (div.table$Population / 100000)


country_vec <- div.table$Country
countries <- unique(country_vec)
country_summary <- div.table %>%
  group_by(Country) %>%
  summarise(
    Continent = first(Continent),  
    MA_mean = mean(all, na.rm = TRUE),
    GDP_mean = mean(GDP, na.rm = TRUE),
    hospital_mean = mean(all, na.rm = TRUE),
    clinic_mean= mean(clinic, na.rm = TRUE),
    pharmacy_mean = mean(pharmacy, na.rm = TRUE)
  )

res_list_all <- list()

for (c in countries) {
  idx <- which(country_vec == c)
 
  avoid_draws <- rowSums(avoidable_all[, idx, drop = FALSE])
  
  country_info <- country_summary %>% filter(Country == c)
  
  res_list_all[[c]] <- data.frame(
    Country = c,
    median = median(avoid_draws),
    lower = quantile(avoid_draws, 0.025),
    upper = quantile(avoid_draws, 0.975),
    Continent = country_info$Continent,
    MA = country_info$MA_mean,
    GDP = country_info$GDP_mean
  )
}

res_list_hospital <- list()
for (c in countries) {
  idx <- which(country_vec == c)

  avoid_draws <- rowSums(avoidable_hospital[, idx, drop = FALSE])
  
  country_info <- country_summary %>% filter(Country == c)
  
  res_list_hospital[[c]] <- data.frame(
    Country = c,
    median = median(avoid_draws),
    lower = quantile(avoid_draws, 0.025),
    upper = quantile(avoid_draws, 0.975),
    Continent = country_info$Continent,
    hospital = country_info$hospital_mean,
    GDP = country_info$GDP_mean
  )
}

res_list_clinic <- list()
for (c in countries) {
  idx <- which(country_vec == c)
  
  avoid_draws <- rowSums(avoidable_clinic[, idx, drop = FALSE])
  
  country_info <- country_summary %>% filter(Country == c)
  
  res_list_clinic[[c]] <- data.frame(
    Country = c,
    median = median(avoid_draws),
    lower = quantile(avoid_draws, 0.025),
    upper = quantile(avoid_draws, 0.975),
    Continent = country_info$Continent,
    clinic = country_info$clinic_mean,
    GDP = country_info$GDP_mean
  )
}
res_list_pharmacy <- list()
for (c in countries) {
  idx <- which(country_vec == c)

  avoid_draws <- rowSums(avoidable_pharmacy[, idx, drop = FALSE])

  country_info <- country_summary %>% filter(Country == c)
  
  res_list_pharmacy[[c]] <- data.frame(
    Country = c,
    median = median(avoid_draws),
    lower = quantile(avoid_draws, 0.025),
    upper = quantile(avoid_draws, 0.975),
    Continent = country_info$Continent,
    pharmacy = country_info$pharmacy_mean,
    GDP = country_info$GDP_mean
  )
}

res_list_Ihd <- list()
for (c in countries) {
  idx <- which(country_vec == c)

  avoid_draws <- rowSums(avoidable_Ihd[, idx, drop = FALSE])
  
  country_info <- country_summary %>% filter(Country == c)
  
  res_list_Ihd[[c]] <- data.frame(
    Country = c,
    median = median(avoid_draws),
    lower = quantile(avoid_draws, 0.025),
    upper = quantile(avoid_draws, 0.975),
    Continent = country_info$Continent,
    MA = country_info$MA_mean,
    GDP = country_info$GDP_mean
  )
}
res_list_Str <- list()
for (c in countries) {
  idx <- which(country_vec == c)

  avoid_draws <- rowSums(avoidable_Str[, idx, drop = FALSE])
  

  country_info <- country_summary %>% filter(Country == c)
  
  res_list_Str[[c]] <- data.frame(
    Country = c,
    median = median(avoid_draws),
    lower = quantile(avoid_draws, 0.025),
    upper = quantile(avoid_draws, 0.975),
    Continent = country_info$Continent,
    MA = country_info$MA_mean,
    GDP = country_info$GDP_mean
  )
}

res_list_HHP <- list()
for (c in countries) {
  idx <- which(country_vec == c)

  avoid_draws <- rowSums(avoidable_HHP[, idx, drop = FALSE])
  
  country_info <- country_summary %>% filter(Country == c)
  
  res_list_HHP[[c]] <- data.frame(
    Country = c,
    median = median(avoid_draws),
    lower = quantile(avoid_draws, 0.025),
    upper = quantile(avoid_draws, 0.975),
    Continent = country_info$Continent,
    MA = country_info$MA_mean,
    GDP = country_info$GDP_mean
  )
}




avoidable_by_country_hospital <- bind_rows(res_list_hospital)

avoidable_by_country_all <- bind_rows(res_list_all)


avoidable_by_country_clinic <- bind_rows(res_list_clinic)

avoidable_by_country_pharmacy<- bind_rows(res_list_pharmacy)
avoidable_by_country_Ihd<- bind_rows(res_list_Ihd)

avoidable_by_country_Str<- bind_rows(res_list_Str)
avoidable_by_country_HHP<- bind_rows(res_list_HHP)



model_dataframes <- list(
  avoidable_by_country_all,
  avoidable_by_country_hospital,
  avoidable_by_country_clinic,
  avoidable_by_country_pharmacy,
  avoidable_by_country_Ihd,
  avoidable_by_country_Str,
  avoidable_by_country_HHP
)


model_names <- c("all", "hospital", "clinic", "pharmacy", "Ihd", "Str", "HHP")


combined_results <- data.frame(Country = avoidable_by_country_all$Country)


for (i in 1:length(model_names)) {
  model_name <- model_names[i]
  df <- model_dataframes[[i]]
  

  temp_df <- df[, c("Country","Continent", "median")]
  

  colnames(temp_df)[2] <- model_name
  

  combined_results <- merge(combined_results, temp_df, by = "Country", all = TRUE)
}


head(combined_results)

head(combined_results, 20)

write.csv(combined_results, "D:/heathycare data/picture4/all_country.csv", row.names = FALSE)
cat("combined_models_median_by_country.csv\n")

write.csv(avoidable_by_country_all , "D:/heathycare data/picture4/all_ad.csv", row.names = FALSE)

write.csv(avoidable_by_country_hospital , "D:/heathycare data/picture4/hosptal_ad.csv", row.names = FALSE)

write.csv(avoidable_by_country_clinic , "D:/heathycare data/picture4/clinic_ad.csv", row.names = FALSE)

write.csv(avoidable_by_country_pharmacy, "D:/heathycare data/picture4/pharmacy_ad.csv", row.names = FALSE)

write.csv(avoidable_by_country_Ihd , "D:/heathycare data/picture4/Ihd_ad.csv", row.names = FALSE)

write.csv(avoidable_by_country_Str , "D:/heathycare data/picture4/Str_ad.csv", row.names = FALSE)

write.csv(avoidable_by_country_HHP , "D:/heathycare data/picture4/HHP_ad.csv", row.names = FALSE)

