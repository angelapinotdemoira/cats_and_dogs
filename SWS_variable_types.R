################################################################################
## Description: Fixing discrepancies in variable types
## Date: August 2020
## Author: Angela Pinot de Moira
################################################################################

library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
#library(remotes)
#install_github("lifecycle-project/ds-helper")
library(dsHelper)



#Obtain a list of variables in dataframe:

discrepancy <- dh.classDescrepancy("D2")

#Create a new dataframe with only variables with a discrepancy
discrepancy2 <- discrepancy %>%
  filter(discrepancy == 'yes') %>%
  filter(inma == 'factor') %>%
  filter(variable!= 'hhincome_') %>%
  print()

#Create a list of variables to be converted to factor variables
factor_vars <- list(discrepancy2$variable)

#integer/numeric to factor
for (j in c(factor_vars)) {
  to_eval = paste0("ds.asFactor(input.var.name = 'D2$",j,"', newobj.name ='",j,"')")
  eval(parse(text=to_eval))
}

#subset dataframe D2 to remove these mismatching variable:
D3_variables =c("child_id", "hhincome_", "age_years", "mother_id", "preg_no", "child_no",  "agebirth_m_y",  
                 "ga_lmp", "ga_bj", "birth_weight", "breastfed_excl", "breastfed_any", "eusilc_income", 
                "cats_quant_preg", "dogs_quant_preg", 
                "sens_cat7", "sens_cat4", "sens_dog4", "sens_dog7", "f_infant_cats", 
                "f_infant_dogs", "cats", "dogs", "early_life_cats", "early_life_dogs", "infant_dogs_cats", "infant_dogs_cats2", "preg_dogs_cats2", 
                "preg_dogs_cats", "dogs_quant_preg2", "cats_quant_preg2", "pets_quant_preg", 
                "pets_quant_preg2", "isaac", "medall", "agebirth_m_y_c", "agebirth_m_y2", "breastfedcat", "csection", "ga_c", "ga", "ga2", 
                "birth_weight_c", "birth_weight2", "log_ga", "sibling_pos2", "famsize_child", "famsize_childcat", "dogs_quant_inf", 
                "cats_quant_inf", "dogs_quant_inf2", "cats_quant_inf2", "pets_quant_inf", "pets_quant_inf2")

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.subset(x='D2', subset = 'D3', cols =D3_variables, datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D3',   'edu_m_', 'cohort_id', 'cob_m',  'ethn1_m', 'ethn2_m', 'ethn3_m', 'asthma_m',  'preg_smk',  'preg_cig',  
'mode_delivery', 'asthma_bf', 'sex', 'plurality', 'sibling_pos',   
'breastfed_ever', 'eusilc_income_quintiles', 'cats_preg', 'dogs_preg', 
'allergy_inh_m', 'allergy_any_m', 'asthma_ever_CHICOS', 'pets_preg', 'dogs_quant_preg_cat', 'cats_quant_preg_cat', 'pets_quant_preg_cat', 
'income', 'dogs_quant_inf_cat', 'cats_quant_inf_cat', 'pets_quant_inf_cat'), newobj = 'D3', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

######
#Check other discrepancies:

discrepancy <- dh.classDescrepancy("D3")

#Create a new dataframe with only variables with a discrepancy
discrepancy2 <- discrepancy %>%
  filter(discrepancy == 'yes') %>%
  print()


ds.asFactor("D3$hhincome_", "hhincome_") 
ds.asInteger("D3$pets_quant_preg", "pets_quant_preg") 

D3_variables =c("child_id", "edu_m_", "age_years", "mother_id", "preg_no", "child_no", "cohort_id", "cob_m", "ethn1_m", 
                "ethn2_m", "ethn3_m", "agebirth_m_y", "asthma_m", "preg_smk", "preg_cig", "mode_delivery", "asthma_bf", 
                "sex", "plurality", "ga_lmp", "ga_bj", "birth_weight", "sibling_pos", "breastfed_excl", "breastfed_any", "breastfed_ever", "eusilc_income", 
                "eusilc_income_quintiles", "cats_preg", "cats_quant_preg", "dogs_preg", "dogs_quant_preg", "allergy_inh_m", 
                "allergy_any_m", "asthma_ever_CHICOS", "pets_preg", "sens_cat7", "sens_cat4", "sens_dog4", "sens_dog7", "f_infant_cats", 
                "f_infant_dogs", "cats", "dogs", "early_life_cats", "early_life_dogs", "infant_dogs_cats", "infant_dogs_cats2", "preg_dogs_cats2", 
                "preg_dogs_cats", "dogs_quant_preg_cat", "cats_quant_preg_cat", "dogs_quant_preg2", "cats_quant_preg2", 
                "pets_quant_preg_cat", "pets_quant_preg2", "isaac", "medall", "agebirth_m_y_c", "agebirth_m_y2", "breastfedcat", "csection", "ga_c", "ga", "ga2", 
                "birth_weight_c", "birth_weight2", "log_ga", "sibling_pos2", "famsize_child", "famsize_childcat", "income", "dogs_quant_inf", 
                "dogs_quant_inf_cat", "cats_quant_inf", "cats_quant_inf_cat", "dogs_quant_inf2", "cats_quant_inf2", "pets_quant_inf", "pets_quant_inf_cat", "pets_quant_inf2")

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.subset(x='D3', subset = 'D3', cols =D3_variables, datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D3', 'pets_quant_preg', 'hhincome_'), newobj = 'D3', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


###########################################################################################################
#Now clean up workspace:
df <- list("allergy_any_m", "allergy_inh_m", "asthma_bf", "asthma_ever_CHICOS", "asthma_m", "breastfed_ever", "cats_preg", 
           "cats_quant_inf_cat", "cats_quant_preg_cat", "cob_m", "cohort_id", "dogs_preg", "dogs_quant_inf_cat", "dogs_quant_preg_cat", "edu_m_", 
           "ethn1_m", "ethn2_m", "ethn3_m", "eusilc_income_quintiles", "hhincome_", "income", "mode_delivery", "pets_preg", "pets_quant_inf_cat", 
           "pets_quant_preg", "pets_quant_preg_cat", "plurality", "preg_cig", "preg_smk", "sex", "sibling_pos")



lapply(df, function(x){
  ds.rm(x.name = x)
})
