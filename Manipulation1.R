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


