################################################################################
## Description: Creating an extra "missing" category in categorical covariates
## Date: August 2020
## Author: Angela Pinot de Moira
################################################################################
library(dplyr)
library(tidyr)
library(dsHelper)


#Covariates:

#list factor variables:

# Set studynames and numstudies
temp <- ds.summary('D3$child_id', datasources = opals)
study_names <- names(temp)
rm(temp)

variables <- ds.colnames("D3")
variables <- variables$sws


types_table2 = data.frame(cbind(study_names))


for (j in 1:length(variables)){
  print(paste0(j," start"))
  type_vect = ds.class(paste0('D3$',variables[j]))
  types_table2 = cbind(types_table2,unlist(type_vect))
  print(paste0(j," end"))
}

types_table2 = types_table2[,-1]
colnames(types_table2) = variables
types_table2 = cbind(types_table2,study_names)
#types_summary = apply(X = types_table3, MARGIN = 2, FUN = function(x){names(sort(table(x),decreasing=TRUE)[1])})

###
types_table2 <- types_table2 %>%
pivot_longer(
  cols = child_id:hhincome_,
  names_to = "variable",
  values_to = "type",
  values_drop_na = TRUE
)


#Create a list of variables with a discrepancy
factors <- types_table2 %>%
  filter(study_names=="dnbc")  %>%
  filter(type == 'factor') %>%
  print()

factors <- list(factors$variable)

#recode missing data to a separate category:

#first change to numeric variable:

for (j in c(factors)) {
  to_eval = paste0("ds.asNumeric(x.name = 'D3$",j,"', newobj ='",j,"')")
  eval(parse(text=to_eval))
}

for (j in c(factors)) {
  to_eval = paste0("ds.replaceNA(x = '",j,"', forNA=list(999,999,999,999,999,999,999), newobj ='",j,"_noNA')")
  eval(parse(text=to_eval))
}

for (j in c(factors)) {
  to_eval = paste0("ds.asFactor(input.var.name = '",j,"_noNA', newobj.name ='",j,"_noNA')")
  eval(parse(text=to_eval))
}


D3_variables =c("child_id", "edu_m_", "age_years", "mother_id", "preg_no", "child_no", "cohort_id", "cob_m", "ethn1_m", 
                "ethn2_m", "ethn3_m", "agebirth_m_y", "asthma_m", "preg_smk", "preg_cig", "mode_delivery", "asthma_bf", 
                "sex", "plurality", "ga_lmp", "ga_bj", "birth_weight", "sibling_pos", "breastfed_excl", "breastfed_any", "breastfed_ever", "eusilc_income", 
                "eusilc_income_quintiles", "cats_preg", "cats_quant_preg", "dogs_preg", "dogs_quant_preg", "allergy_inh_m", 
                "allergy_any_m", "asthma_ever_CHICOS", "pets_preg", "sens_cat7", "sens_cat4", "sens_dog4", "sens_dog7", "f_infant_cats", 
                "f_infant_dogs", "cats", "dogs", "early_life_cats", "early_life_dogs", "infant_dogs_cats", "infant_dogs_cats2", "preg_dogs_cats2", 
                "preg_dogs_cats", "dogs_quant_preg_cat", "cats_quant_preg_cat", "dogs_quant_preg2", "cats_quant_preg2", 
                "pets_quant_preg_cat", "pets_quant_preg2", "isaac", "medall", "agebirth_m_y_c", "agebirth_m_y2", "breastfedcat", "csection", "ga_c", "ga", "ga2", 
                "birth_weight_c", "birth_weight2", "log_ga", "sibling_pos2", "famsize_child", "famsize_childcat", "income", "dogs_quant_inf", 
                "dogs_quant_inf_cat", "cats_quant_inf", "cats_quant_inf_cat", "dogs_quant_inf2", "cats_quant_inf2", "pets_quant_inf", 
                "pets_quant_inf_cat", "pets_quant_inf2", 'pets_quant_preg', 'hhincome_')

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.subset(x='D3', subset = 'D3', cols =D3_variables, datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


#cbind new variables onto dataframe
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D3', 'edu_m__noNA', 'cohort_id_noNA', 'cob_m_noNA', 'ethn1_m_noNA', 'ethn2_m_noNA', 'ethn3_m_noNA', 'asthma_m_noNA', 'preg_smk_noNA', 'preg_cig_noNA', 'mode_delivery_noNA', 'asthma_bf_noNA', 'sex_noNA', 
'plurality_noNA', 'sibling_pos_noNA', 'breastfed_ever_noNA', 'eusilc_income_quintiles_noNA', 'cats_preg_noNA', 'dogs_preg_noNA', 'allergy_inh_m_noNA', 'allergy_any_m_noNA', 
'asthma_ever_CHICOS_noNA', 'pets_preg_noNA', 'sens_cat7_noNA', 'sens_cat4_noNA', 'sens_dog4_noNA', 'sens_dog7_noNA', 'f_infant_cats_noNA', 'f_infant_dogs_noNA', 
'cats_noNA', 'dogs_noNA', 'early_life_cats_noNA', 'early_life_dogs_noNA', 'infant_dogs_cats_noNA', 'infant_dogs_cats2_noNA', 'preg_dogs_cats2_noNA', 'preg_dogs_cats_noNA', 
'dogs_quant_preg_cat_noNA', 'cats_quant_preg_cat_noNA', 'pets_quant_preg_cat_noNA', 'isaac_noNA', 'medall_noNA', 'breastfedcat_noNA', 'csection_noNA', 'sibling_pos2_noNA', 
'famsize_childcat_noNA', 'income_noNA', 'dogs_quant_inf_cat_noNA', 'cats_quant_inf_cat_noNA', 'pets_quant_inf_cat_noNA', 'hhincome__noNA'), newobj = 'D3', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#tidy-up workspace:

df = list('edu_m__noNA', 'cohort_id_noNA', 'cob_m_noNA', 'ethn1_m_noNA', 'ethn2_m_noNA', 'ethn3_m_noNA', 'asthma_m_noNA', 'preg_smk_noNA', 'preg_cig_noNA', 'mode_delivery_noNA', 'asthma_bf_noNA', 'sex_noNA', 
          'plurality_noNA', 'sibling_pos_noNA', 'breastfed_ever_noNA', 'eusilc_income_quintiles_noNA', 'cats_preg_noNA', 'dogs_preg_noNA', 'allergy_inh_m_noNA', 'allergy_any_m_noNA', 
          'asthma_ever_CHICOS_noNA', 'pets_preg_noNA', 'sens_cat7_noNA', 'sens_cat4_noNA', 'sens_dog4_noNA', 'sens_dog7_noNA', 'f_infant_cats_noNA', 'f_infant_dogs_noNA', 
          'cats_noNA', 'dogs_noNA', 'early_life_cats_noNA', 'early_life_dogs_noNA', 'infant_dogs_cats_noNA', 'infant_dogs_cats2_noNA', 'preg_dogs_cats2_noNA', 'preg_dogs_cats_noNA', 
          'dogs_quant_preg_cat_noNA', 'cats_quant_preg_cat_noNA', 'pets_quant_preg_cat_noNA', 'isaac_noNA', 'medall_noNA', 'breastfedcat_noNA', 'csection_noNA', 'sibling_pos2_noNA', 
          'famsize_childcat_noNA', 'income_noNA', 'dogs_quant_inf_cat_noNA', 'cats_quant_inf_cat_noNA', 'pets_quant_inf_cat_noNA', 'hhincome__noNA')



lapply(df, function(x){
  ds.rm(x.name = x)
})

df = list('edu_m_', 'cohort_id', 'cob_m', 'ethn1_m', 'ethn2_m', 'ethn3_m', 'asthma_m', 'preg_smk', 'preg_cig', 'mode_delivery', 'asthma_bf', 'sex', 
          'plurality', 'sibling_pos', 'breastfed_ever', 'eusilc_income_quintiles', 'cats_preg', 'dogs_preg', 'allergy_inh_m', 'allergy_any_m', 
          'asthma_ever_CHICOS', 'pets_preg', 'sens_cat7', 'sens_cat4', 'sens_dog4', 'sens_dog7', 'f_infant_cats', 'f_infant_dogs', 
          'cats', 'dogs', 'early_life_cats', 'early_life_dogs', 'infant_dogs_cats', 'infant_dogs_cats2', 'preg_dogs_cats2', 'preg_dogs_cats', 
          'dogs_quant_preg_cat', 'cats_quant_preg_cat', 'pets_quant_preg_cat', 'isaac', 'medall', 'breastfedcat', 'csection', 'sibling_pos2', 
          'famsize_childcat', 'income', 'dogs_quant_inf_cat', 'cats_quant_inf_cat', 'pets_quant_inf_cat', 'hhincome_')



lapply(df, function(x){
  ds.rm(x.name = x)
})


df = list ("cats_quant_inf_cat_noNA", "cats_quant_preg_cat_noNA", 
           "dogs_quant_inf_cat_noNA", "dogs_quant_preg_cat_noNA", "edu_m_noNA", 
           "eusilc_income_quintiles", "eusilc_income_quintiles_noNA", "famsize_childcat_noNA", 
           "infant_dogs_cats2_noNA", "infant_dogs_cats_noNA", "pets_quant_inf_cat_noNA",  
           "pets_quant_preg_cat_noNA")

lapply(df, function(x){
  ds.rm(x.name = x)
})

ds.rm("cats_quant_inf_cat_noNA")