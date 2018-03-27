tbl = read.csv("D:/121/Ageeva/MathMod/eddypro.csv", skip = 1, na = c("", "NA","-9999", "-9999.0"),  comment = c("["))
library("readr")
library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
tbl=tbl[-1]
tbl
tbl = tbl[tbl$DOY > 62 & tbl$DOY < 156,]
tbl
glimpse(tbl)
tbl = select(tbl, -(roll))
tbl = tbl%>% mutate_if(is.character, factor)
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_")
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_")
glimpse(tbl)
sapply(tbl,is.numeric)
tbl_numeric = tbl[,sapply(tbl,is.numeric)]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric)]
cor_td = cor(tbl_numeric)
cor_td
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))
formula
m1 = lm(formula,  data = tbl)
m1
anova(m1)
summary(m1)
formula1 = as.formula(paste("co2_flux ~ Tau + rand_err_Tau + H + rand_err_H + LE + qc_LE + 
    rand_err_LE + co2_flux + h2o_flux + qc_h2o_flux + rand_err_h2o_flux + 
                            H_strg + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + 
                            h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                            air_molar_volume + es + RH + VPD + u_rot + wind_speed + u. + 
                            TKE + X.z.d..L + T. + x_offset + x_90. + un_Tau + un_H + 
                            H_scf + un_LE + LE_scf + un_co2_flux + co2_scf + un_h2o_flux + 
                            h2o_scf + w_spikes + v_var + w_var + w.co2_cov + co2 + co2.1 + flowrate"))
m2 = lm(formula1, data = tbl)
m2
anova(m2)
summary(m2)
formula2 = as.formula(paste("co2_flux ~ Tau + H + LE +
    rand_err_LE + co2_flux + h2o_flux + rand_err_h2o_flux + co2_mole_fraction +
    wind_speed + T. + x_offset + un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + co2.1"))
m3 = lm(formula2, data = tbl)
m3
anova(m3)
summary(m3)

m4 = lm(co2_flux ~ (Tau + H + LE +
          rand_err_LE + co2_flux + h2o_flux + rand_err_h2o_flux + 
          co2_mole_fraction + wind_speed + T. + x_offset + un_Tau + 
          un_H + un_LE + un_co2_flux + un_h2o_flux + co2.1)^2, data = tbl)
m4
anova(m4)
summary(m4)
m5 = lm(co2_flux ~ Tau + H + LE + rand_err_LE + h2o_flux + rand_err_h2o_flux + co2_mole_fraction +                
          wind_speed + T. + x_offset + un_Tau + un_H + un_LE + un_co2_flux + un_h2o_flux + co2.1 +                                
          Tau:H + Tau:LE + Tau:rand_err_LE + co2_flux:Tau + Tau:h2o_flux + Tau:rand_err_h2o_flux +
          Tau:co2_mole_fraction + Tau:wind_speed + Tau:T. + Tau:x_offset + Tau:un_Tau + Tau:un_H +
          Tau:un_LE + Tau:un_co2_flux + Tau:un_h2o_flux + Tau:co2.1 + H:LE + H:rand_err_LE + co2_flux:H +                          
          H:h2o_flux + H:rand_err_h2o_flux + H:co2_mole_fraction + H:wind_speed + H:T. +                          
          H:x_offset + H:un_Tau + H:un_H + H:un_LE + H:un_co2_flux + H:un_h2o_flux + H:co2.1 +                              
          LE:rand_err_LE + co2_flux:LE + LE:h2o_flux + LE:rand_err_h2o_flux + LE:co2_mole_fraction +                 
          LE:wind_speed + LE:T. + LE:x_offset + LE:un_Tau + LE:un_H + LE:un_LE + LE:un_co2_flux + LE:un_h2o_flux +                        
          LE:co2.1 + co2_flux:rand_err_LE + rand_err_LE:rand_err_h2o_flux + rand_err_LE:co2_mole_fraction +        
          rand_err_LE:wind_speed + rand_err_LE:T. + rand_err_LE:x_offset + rand_err_LE:un_Tau +                  
          rand_err_LE:un_LE + rand_err_LE:un_co2_flux + rand_err_LE:un_h2o_flux +              
          rand_err_LE:co2.1+co2_flux:h2o_flux +co2_flux:rand_err_h2o_flux+co2_flux:co2_mole_fraction+            
          co2_flux:wind_speed+co2_flux:T. +co2_flux:x_offset +co2_flux:un_Tau +co2_flux:un_H +                        
          co2_flux:un_co2_flux +co2_flux:un_h2o_flux +co2_flux:co2.1 +h2o_flux:rand_err_h2o_flux+            
          h2o_flux:co2_mole_fraction +h2o_flux:T. + h2o_flux:x_offset +h2o_flux:un_Tau + h2o_flux:un_H +                        
          h2o_flux:un_h2o_flux +rand_err_h2o_flux:co2_mole_fraction +         
          rand_err_h2o_flux:x_offset +rand_err_h2o_flux:un_H +               
          rand_err_h2o_flux:un_co2_flux + co2_mole_fraction:wind_speed + co2_mole_fraction:T. +co2_mole_fraction:x_offset +           
          co2_mole_fraction:un_LE +co2_mole_fraction:un_co2_flux +        
          wind_speed:un_H +wind_speed:un_co2_flux  + T.:un_co2_flux +x_offset:un_LE + un_LE:un_co2_flux, data = tbl)
m5
anova(m5)        
summary(m5)
m6 = lm(co2_flux ~ H + LE + rand_err_LE + h2o_flux + rand_err_h2o_flux + T. + x_offset + un_Tau + un_H + un_co2_flux +                                 
           Tau:un_H +
          Tau:un_LE + Tau:un_co2_flux + Tau:un_h2o_flux + Tau:co2.1 + H:LE + H:rand_err_LE + co2_flux:H +                          
          H:h2o_flux + H:rand_err_h2o_flux + H:co2_mole_fraction + H:wind_speed + H:T. +                          
          H:x_offset + H:un_Tau + H:un_H + H:un_LE + H:un_co2_flux +  co2_flux:LE + LE:h2o_flux +                 
          LE:wind_speed + LE:un_co2_flux + co2_flux:rand_err_LE + rand_err_LE:T. + rand_err_LE:x_offset + rand_err_LE:un_Tau +                  
          rand_err_LE:un_co2_flux + co2_flux:h2o_flux +co2_flux:rand_err_h2o_flux+          
          co2_flux:wind_speed+co2_flux:T. +h2o_flux:T. + rand_err_h2o_flux:co2_mole_fraction + rand_err_h2o_flux:un_H +               
          rand_err_h2o_flux:un_co2_flux + co2_mole_fraction:T. +co2_mole_fraction:x_offset +           
          co2_mole_fraction:un_co2_flux + wind_speed:un_H +wind_speed:un_co2_flux  + T.:un_co2_flux + un_LE:un_co2_flux, data = tbl)
m6
anova(m6)
summary(m6)
m7 = lm(co2_flux ~ H + T. + x_offset + un_H + un_co2_flux + Tau:un_H +
          Tau:un_LE + Tau:un_co2_flux + Tau:un_h2o_flux + co2_flux:H +                          
           H:wind_speed + H:T. +                          
          H:x_offset + H:un_Tau + H:un_H + H:un_co2_flux +                 
          LE:wind_speed + LE:un_co2_flux + co2_flux:wind_speed+co2_flux:T. +  rand_err_h2o_flux:un_co2_flux + 
          co2_mole_fraction:T. +co2_mole_fraction:x_offset +           
          co2_mole_fraction:un_co2_flux + wind_speed:un_H +wind_speed:un_co2_flux  + T.:un_co2_flux + un_LE:un_co2_flux, data = tbl)
m7
anova(m7)
summary(m7)
