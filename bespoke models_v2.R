################################################################################
## Description: Models for LifeCycle cats and dogs study
## Date: 25th May 2020
## Author: Angela Pinot de Moira
################################################################################

library(opal)
library(dsBaseClient)
library(metafor)

#Open workspace:
setwd("/home/angela/angela/Cat dog analysis/cats_and_dogs")
datashield.logout(opals)
load("login.Rda")
#opals<-datashield.login(logindata, restore='june12')
#opals<-datashield.login(logindata, restore='aug7_sws')
opals<-datashield.login(logindata, restore='temp2')


# Source in the Tom's extra functions for analysis
source("toms_variable_functions.R")



# Set studynames and numstudies
temp <- ds.summary('D3$child_id', datasources = opals)
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# remove twins and other multiples
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.subset(x = 'D3', subset = 'E1', logicalOperator = 'plurality==', threshold = 1, datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}
#check that child_no is now "1" for all cohorts (n.b.: there is an issue with moba's child_no)
ds.table1D("E1$child_no", datasources = opals[c(1,2,3,4,5,7)])

# all participants
all_participants <- ds.length('E1$child_id', datasources = opals)
all_participants_split <- ds.length('E1$child_id',type = 'split', datasources = opals)


# List of variables:
variables <- ds.colnames("E1")
variables <- variables$sws

#Establish variables missing/numbers missing and variable types:
pre_missings_table = data.frame(cbind(all_participants_split))

for (j in 1:length(variables)){
  print(paste0(j," start"))
  missing_vect = ds.numNA(paste0('E1$',variables[j]))
  pre_missings_table = cbind(pre_missings_table,unlist(missing_vect))
  print(paste0(j," end"))
}

colnames(pre_missings_table) <- c('Total in Study', variables)
pre_missings_table = t(pre_missings_table)
pre_missings_table = as.data.frame(pre_missings_table)
View(pre_missings_table)

###################################################################################
###################################################################################
# ___  ___          _      _   __  
# |  \/  |         | |    | | /  | 
# | .  . | ___   __| | ___| | `| | 
# | |\/| |/ _ \ / _` |/ _ \ |  | | 
# | |  | | (_) | (_| |  __/ | _| |_
# \_|  |_/\___/ \__,_|\___|_| \___/

# Exposure: cats and dogs (yes/no) in pregnancy
# Outcome: current asthma at around 7 years
# Covariates: age, sex, mother's education, hh income, smoking, mother's asthma, mother's allergy, 
# breastfeeding, mother's cob/mother's ethnicity, sibling position, mother's age, mode of delivery (c-section), 
# gestational age, birth weight, family size (children) 

# Now read in the table which lists which variables are missing for each study
# the _vars.csv file stores information about which variables a study is expected to have and
# which variables are completely missing for all participants
setwd("/home/angela/angela/Cat dog analysis/cats_and_dogs")
filter_csv = read.csv(file = 'cat_dog_vars.csv',  header=TRUE, row.names = 1 )

#This file lists all possible variables that could be incorporated into models
#not all variables will be used for all models 



#Tom's function has the option to disregard some variables (for e.g. that won't be used in a model)

none_cc_vars = c("edu_m_", "age_years",
                 "mother_id", "preg_no", "child_no",
                 "cohort_id", "cob_m", "ethn1_m",
                 "ethn2_m", "ethn3_m", 
                 "asthma_m", "preg_smk", "preg_cig",
                 "mode_delivery", "asthma_bf", "sex",
                 "plurality", "ga_lmp", "ga_bj", 
                 "sibling_pos", "breastfed_excl",
                 "breastfed_any", "breastfed_ever", "eusilc_income",
                 "eusilc_income_quintiles", "cats_quant_preg",
                 "dogs_quant_preg", "allergy_inh_m",
                 "allergy_any_m", "asthma_ever_CHICOS", "pets_preg",
                 "sens_cat7", "sens_cat4", "sens_dog4",
                 "sens_dog7", "f_infant_cats", "f_infant_dogs",
                 "cats", "dogs", "early_life_cats",
                 "early_life_dogs", "infant_dogs_cats", "infant_dogs_cats2",
                 "preg_dogs_cats2", "preg_dogs_cats", "dogs_quant_preg_cat",
                 "cats_quant_preg_cat", "dogs_quant_preg2", "cats_quant_preg2",
                 "pets_quant_preg_cat", "pets_quant_preg2", 
                 "isaac", 
                 "breastfedcat", "csection", "sibling_pos2",
                 "famsize_child", "famsize_childcat", "income",
                 "dogs_quant_inf", "dogs_quant_inf_cat", "cats_quant_inf",
                 "cats_quant_inf_cat", "dogs_quant_inf2", "cats_quant_inf2",
                 "pets_quant_inf", "pets_quant_inf_cat", "pets_quant_inf2",
                 "pets_quant_preg", "hhincome_", 
                 "cohort_id_noNA", "ethn1_m_noNA",
                 "preg_cig_noNA", "mode_delivery_noNA",
                 "asthma_bf_noNA", "plurality_noNA",
                 "sibling_pos_noNA", "eusilc_income_quintiles_noNA",
                 "cats_preg_noNA", "dogs_preg_noNA", 
                 "allergy_any_m_noNA", "asthma_ever_CHICOS_noNA", "pets_preg_noNA",
                 "sens_cat7_noNA", "sens_cat4_noNA", "sens_dog4_noNA",
                 "sens_dog7_noNA", "f_infant_cats_noNA", "f_infant_dogs_noNA",
                 "cats_noNA", "dogs_noNA", "early_life_cats_noNA",
                 "early_life_dogs_noNA", "infant_dogs_cats_noNA", "infant_dogs_cats2_noNA",
                 "preg_dogs_cats2_noNA", "preg_dogs_cats_noNA", "dogs_quant_preg_cat_noNA",
                 "cats_quant_preg_cat_noNA", "pets_quant_preg_cat_noNA", "isaac_noNA",
                 "medall_noNA", 
                 "dogs_quant_inf_cat_noNA", "cats_quant_inf_cat_noNA", "pets_quant_inf_cat_noNA",
                 "hhincome__noNA")



#Now restrict data frame and select complete cases:
for (i in c(1:num_studies)) {
  my_name = names(opals[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  ds.subset(x = 'E1', subset = 'E2', cols = list_variables, datasources = opals[i])
  ds.subset(x = 'E2', subset = 'E3', completeCases = TRUE, datasources = opals[i])
}

length_complete = ds.length("E3$child_id", type = "split", datasources = opals)

#Use dataframe fill to add back excluded variables?

#Now build model:
data_table = "E3"

#outcome = c('isaac')
outcome = c('medall')


#exposure = 'f_infant_dogs'
exposure = "cats_preg"


interaction = "asthma_m"

covariates =  c('dogs_preg', "asthma_m_noNA", "edu_m__noNA", "income_noNA", "preg_smk_noNA", "sex_noNA", "cob_m_noNA", 
                  "allergy_inh_m_noNA", "sibling_pos2_noNA", "agebirth_m_y_c_noNA", "csection_noNA", "log_ga", "birth_weight_c",
                  "famsize_childcat_noNA", "breastfedcat_noNA", "ethn3_m_noNA", "ethn2_m_noNA")
#covariates =  c("edu_m_", "hhincome_", "asthma_m", "eusilc_income_quintiles", "preg_smk", "sex", "cob_m", 
#                  "allergy_any_m", "sibling_pos2.1", "agebirth_m_y_c", "csection", "ga_c", "birth_weight_c",
#                  "famsize_childcat", "famsizecat", "breastfed_ever") # different breastfeeding variable

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
#my_vars_check = c(my_exposure, my_outcome)
#temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

#Model0 (unadjusted)
for (i in c(1,2,3,5,6,7)) {
  my_name = names(opals[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table,'$',interaction,'*', data_table,'$', exposure)))))")
  eval(parse(text=to_eval))
  to_eval = paste0("model1_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


# Model1 (adjusted)
for (i in c(1,2,3,5,6)) {
  my_name = names(opals[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                                                                  paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+')))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model1_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
  }

#meta analyse here
#for (i in c("f_infant_dogs1")) {
for (i in c("f_infant_cats1")) {
#  for (i in c("f_infant_cats1", "f_infant_dogs1")) {
# for (i in c("infant_dogs_cats1", "infant_dogs_cats2", "infant_dogs_cats3")) {
    coefficient = paste0(data_table, '$', i)
    to_eval1 = paste0("yi_",i," = c(model1_1$coefficients[coefficient,'Estimate'], model1_2$coefficients[coefficient,'Estimate'], 
                                  model1_3$coefficients[coefficient,'Estimate'], 
                                  model1_5$coefficients[coefficient,'Estimate'],
                                  model1_6$coefficients[coefficient,'Estimate'])")
    eval(parse(text=to_eval1))
    to_eval2 = paste0("sei_",i," <- c(model1_1$coefficients[coefficient,'Std. Error'], 
                                    model1_2$coefficients[coefficient,'Std. Error'], 
                                    model1_3$coefficients[coefficient,'Std. Error'], 
                                    model1_5$coefficients[coefficient,'Std. Error'],
                                    model1_6$coefficients[coefficient,'Std. Error'])")
    eval(parse(text=to_eval2))
    #Random effects model:
    to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
    eval(parse(text=to_eval3))
 #   forest[i] <- eval(parse(text=(paste0("forest(res_",i,", xlab='Adjusted* OR', transf=exp, refline=1, slab=c('DNBC', 'INMA', 'NINFEA', 'Raine', 'MoBa'))"))))
#this last step doesn't work..
         } 
#Interaction
for (coefficient in c("E3$asthma_m1:E3$f_infant_dogs1")) {
# coefficient = "E3$f_infant_dogs1:E3$f_infant_cats1"
  yi_ = c(model1_1$coefficients[coefficient,'Estimate'], model1_2$coefficients[coefficient,'Estimate'], 
                                  model1_3$coefficients[coefficient,'Estimate'], 
                                  model1_5$coefficients[coefficient,'Estimate'],
                                  model1_6$coefficients[coefficient,'Estimate'])
   sei_ <- c(model1_1$coefficients[coefficient,'Std. Error'], 
                                    model1_2$coefficients[coefficient,'Std. Error'], 
                                    model1_3$coefficients[coefficient,'Std. Error'], 
                                    model1_5$coefficients[coefficient,'Std. Error'],
                                    model1_6$coefficients[coefficient,'Std. Error'])
  #Random effects model:
  res_ <- rma(yi_, sei=sei_)
 
  #   forest[i] <- eval(parse(text=(paste0("forest(res_",i,", xlab='Adjusted* OR', transf=exp, refline=1, slab=c('DNBC', 'INMA', 'NINFEA', 'Raine', 'MoBa'))"))))
  #this last step doesn't work..
} 

  #Forest plots:
forest(res_, xlab="Crude OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))

forest(res_f_infant_cats1, xlab="Crude OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))
forest(res_f_infant_dogs1, xlab="Crude OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))

    forest(res_f_infant_cats1, xlab="Adjusted* OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))
    forest(res_f_infant_dogs1, xlab="Adjusted* OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))
  

      forest(res, digits=3, mlab=bquote(paste('Overall (I'^2*' = ', .(round(res$I2)),'%, p = ',
                                              .(sprintf("%.3f", round(res$QEp,3))),')')),
             xlab=bquote(paste('Test of H'[0]*': true relative risk = 1, p = ',
                               .(sprintf("%.3f", round(res$pval,3))))), atransf = exp, cex=1, cex.lab=0.75, cex.axis=1)
      usr <- par("usr")
      
      text(usr[2], usr[4], "Relative Risk [95% CI]", adj = c(1, 4),cex=1)
      text(usr[1], usr[4], paste0(gsub(paste0(ref_table,"\\$"),"", deparse(fmla)),collapse="\n"), adj = c( 0, 1 ),cex=1)
      text(usr[1], usr[3], variable, adj = c( 0, 0),cex=1)
############################################################################

#############################################################################
  # ___  ___          _      _   _____ 
  # |  \/  |         | |    | | / __  \
  # | .  . | ___   __| | ___| | `' / /'
  # | |\/| |/ _ \ / _` |/ _ \ |   / /  
  # | |  | | (_) | (_| |  __/ | ./ /___
  # \_|  |_/\___/ \__,_|\___|_| \_____/
  # Model 2: cats and dogs (yes/no) in pregnancy
  
#Change the variables included in complete case data frames:

#  none_cc_vars = c("cats", "cats_preg", "cats_quant_inf", 
#                 "cats_quant_inf2", "cats_quant_inf_cat", "cats_quant_preg",    
#                 "cohort_id", "dogs", "dogs_preg", "dogs_quant_inf", "dogs_quant_inf2", "dogs_quant_inf_cat", 
#                 "dogs_quant_preg", 'f_infant_cats', 'f_infant_dogs',  
#                 "infant_dogs_cats", "sens_cat4", "sens_cat7", "sens_dog7")
      
      
      none_cc_vars = c( "cats", 'f_infant_cats', 'f_infant_dogs',  "cats_quant_inf", "infant_dogs_cats", "isaac",
      "cats_quant_inf2", "cats_quant_inf_cat", "cats_quant_preg", "cats_quant_preg2",  "cats_quant_preg_cat",  
      "cohort_id", "dogs", "dogs_quant_inf", "dogs_quant_inf2", "dogs_quant_inf_cat", 
      "dogs_quant_preg","dogs_quant_preg2", "dogs_quant_preg_cat", "early_life_cats", "early_life_dogs",  
      "sens_cat4", "sens_cat7", "sens_dog4", "sens_dog7", "ethn1_m", "ethn2_m", "hhincome_", "eusilc_income_quintiles",
      "pets_quant_preg", "pets_quant_preg2",  "pets_quant_preg_cat")
  
for (i in c(1:num_studies)) {
    my_name = names(opals[i])
    list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
    ds.subset(x = 'E1', subset = 'E2', cols = list_variables, datasources = opals[i])
    ds.subset(x = 'E2', subset = 'E3', completeCases = TRUE, datasources = opals[i])
  }
  
  length_complete = ds.length("E3$child_id", type = "split", datasources = opals)

#Now build model:  
data_table = "E3"
  
#outcome = c('isaac')
outcome = c('medall')
exposure = c('cats_preg')
#exposure = c('dogs_preg')

covariates =  c("edu_m_", "hhincome_", "asthma_m", "eusilc_income_quintiles", "preg_smk", "sex", "cob_m", 
                "allergy_any_m", "sibling_pos2", "agebirth_m_y_c", "csection", "log_ga", "birth_weight_c",
                "famsize_childcat", "famsizecat", "breastfedcat")


#Unadjusted model
for (i in c(1,2,3,5,6)) {
  my_name = names(opals[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table,'$',interaction,'*', data_table,'$', exposure)))))")
  eval(parse(text=to_eval))
  to_eval = paste0("model2_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


# Adjusted 
for (i in c(1,2,3,5,6)) {
  my_name = names(opals[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                                                                  paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+')))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model2_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


#meta analyse here
for (i in c("early_life_cats1", "early_life_cats2", "early_life_cats3")) {
  # for (i in c("XXXXX", "XXXX", "XXXXX")) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model2_1$coefficients[coefficient,'Estimate'], model2_2$coefficients[coefficient,'Estimate'], 
                                  model2_3$coefficients[coefficient,'Estimate'], 
                                  model2_5$coefficients[coefficient,'Estimate'],
                                  model2_6$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model2_1$coefficients[coefficient,'Std. Error'], 
                                    model2_2$coefficients[coefficient,'Std. Error'], 
                                    model2_3$coefficients[coefficient,'Std. Error'], 
                                    model2_5$coefficients[coefficient,'Std. Error'],
                                    model2_6$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
  #   forest[i] <- eval(parse(text=(paste0("forest(res_",i,", xlab='Adjusted* OR', transf=exp, refline=1, slab=c('DNBC', 'INMA', 'NINFEA', 'Raine', 'MoBa'))"))))
  #this last step doesn't work..
} 

#Forest plots:
forest(res_early_life_cats1, xlab="Adjusted OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))
forest(res_early_life_cats2, xlab="Adjusted OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))
forest(res_early_life_cats3, xlab="Adjusted OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))


#########################################

###########################################################################################################
###########################################################################################################
# ___  ___          _      _   _____ 
# |  \/  |         | |    | | |____ |
# | .  . | ___   __| | ___| |     / /
# | |\/| |/ _ \ / _` |/ _ \ |     \ \
# | |  | | (_) | (_| |  __/ | .___/ /
# \_|  |_/\___/ \__,_|\___|_| \____/ 

# Model 3: Quantity of cats or dogs ("cats_quant_inf", 
#"cats_quant_inf2", "cats_quant_inf_cat","dogs_quant_inf" and "dogs_quant_inf2", "dogs_quant_inf_cat")

#Change the variables included in complete case data frames:

none_cc_vars = c("cats", "cats_preg",  "cats_quant_preg",    
                 "cohort_id", "dogs", "dogs_preg",  
                 "dogs_quant_preg", "early_life_cats", "early_life_dogs", 
                 "infant_dogs_cats", "sens_cat4", "sens_cat7", "sens_dog7")

for (i in c(1:num_studies)) {
  my_name = names(opals[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  ds.subset(x = 'E1', subset = 'E2', cols = list_variables, datasources = opals[i])
  ds.subset(x = 'E2', subset = 'E3', completeCases = TRUE, datasources = opals[i])
}

length_complete = ds.length("E3$child_id", type = "split", datasources = opals)

#Now build model:  
data_table = "E3"

outcome = c('isaac')
exposure = c("cats_quant_inf")
#exposure = c("cats_quant_inf_cat")

covariates =  c("edu_m_", "hhincome_", "asthma_m", "eusilc_income_quintiles", "preg_smk", "sex", "cob_m", 
                "allergy_any_m", "sibling_pos2", "agebirth_m_y_c", "csection", "log_ga", "birth_weight_c",
                "famsize_childcat", "famsizecat", "breastfedcat", 'f_infant_dogs' )

#Not all cohorts have these variables, so restrict to cohorts with data
#only run on opals with the exposure and outcome
my_vars_check = c(exposure, outcome)
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)
names_temp_opals = names(temp_opals)

#Model0 (unadjusted)
for (i in c(names_temp_opals)) {
  my_name = names(opals[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model1_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = opals['",i,"'])")
  eval(parse(text=to_eval))
}


# Model1 (adjusted)
for (i in c(1,2,3,5,6)) {
  my_name = names(opals[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                   paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+')))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model1_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#meta analyse here
for (i in c("cats_quant_inf")) {
#for (i in c("cats_quant_inf_cat1", "cats_quant_inf_cat2")) {
  # for (i in c("dogs_quant_inf_cat1", "dogs_quant_inf_cat2")) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model1_inma$coefficients[coefficient,'Estimate'], 
                    model1_ninfea$coefficients[coefficient,'Estimate'], 
                    model1_raine$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model1_inma$coefficients[coefficient,'Std. Error'], 
                    model1_ninfea$coefficients[coefficient,'Std. Error'], 
                     model1_raine$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 


#Forest plots:
forest(res_cats_quant_inf, xlab="Adjusted* OR", transf=exp, refline=1, slab=c("INMA", "NINFEA", "Raine"))

forest(res_cats_quant_inf_cat1, xlab="Adjusted* OR", transf=exp, refline=1, slab=c("INMA", "NINFEA", "Raine"))
forest(res_cats_quant_inf_cat2, xlab="Adjusted* OR", transf=exp, refline=1, slab=c("INMA", "NINFEA", "Raine"))

######################################################################################################

######################################################################################################
# ___  ___          _      _   __  
# |  \/  |         | |    | | 
# | .  . | ___   __| | ___| | 
# | |\/| |/ _ \ / _` |/ _ \ | 
# | |  | | (_) | (_| |  __/ |
# \_|  |_/\___/ \__,_|\___|_| 4

# Exposure: cats and dogs (yes/no) in infancy
# Outcome: cat sensitisation at around 7 years
# Covariates: age, sex, mother's education, hh income, smoking, mother's asthma, mother's allergy, 
# breastfeeding, mother's cob, sibling position, mother's age, mode of delivery (c-section), 
# gestational age, birth weight, family size, 

# Now read in the table which lists which variables are missing for each study
# the _vars.csv file stores information about which variables a study is expected to have and
# which variables are completely missing for all participants
setwd("/home/angela/angela/Cat dog analysis/cats_and_dogs")
filter_csv = read.csv(file = 'cat_dog_vars.csv',  header=TRUE, row.names = 1 )

#This file lists all possible variables that could be incorporated into models
# not all variables will be used for all models 

#Tom's function has the option to disregard some variables (for e.g. that won't be used in a model)

none_cc_vars = c("cats", "cats_preg", "cats_quant_inf", "infant_dogs_cats", 
                 "cats_quant_inf2", "cats_quant_inf_cat", "cats_quant_preg",    
                 "cohort_id", "dogs", "dogs_preg", "dogs_quant_inf", "dogs_quant_inf2", "dogs_quant_inf_cat", 
                 "dogs_quant_preg", "early_life_cats", "early_life_dogs",  
                 "sens_cat4", "isaac", "sens_dog7", "ethn1_m", "ethn2_m", "hhincome_", "eusilc_income_quintiles")

#Not all cohorts have these variables, so restrict to cohorts with data
#only run on opals with the exposure and outcome
my_vars_check = 'sens_cat7'
temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)
names_temp_opals = names(temp_opals)


#Now restrict data frame and select complete cases:
for (i in c(names_temp_opals)) {
  my_name = names(opals[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  ds.subset(x = 'E1', subset = 'E2', cols = list_variables, datasources = opals[i])
  ds.subset(x = 'E2', subset = 'E3', completeCases = TRUE, datasources = opals[i])
}

length_complete = ds.length("E3$child_id", type = "split", datasources = opals[c(names_temp_opals)])

#Use dataframe fill to add back excluded variables?

#Now build model:
data_table = "E3"

outcome = c('sens_cat7')

#exposure = 'f_infant_dogs'
exposure = "f_infant_cats"


interaction = "asthma_m"

covariates =  c('f_infant_cats', "asthma_m", "edu_m_", "income", "preg_smk", "sex", "cob_m", 
                "allergy_any_m", "sibling_pos2", "agebirth_m_y_c", "csection", "log_ga", "birth_weight_c",
                "famsize_childcat", "famsizecat", "breastfedcat", "ethn3_m")
#covariates =  c("edu_m_", "hhincome_", "asthma_m", "eusilc_income_quintiles", "preg_smk", "sex", "cob_m", 
#                  "allergy_any_m", "sibling_pos2.1", "agebirth_m_y_c", "csection", "ga_c", "birth_weight_c",
#                  "famsize_childcat", "famsizecat", "breastfed_ever") # different breastfeeding variable

#my_vars_all = c(my_exposure, my_outcome, my_covariate, my_exit_col, "newStartDate", "burtonWeights")
#only run on opals with the exposure and outcome
#my_vars_check = c(my_exposure, my_outcome)
#temp_opals = opal_creator(variables_to_filter = my_vars_check, filter_df = filter_csv, opals_to_filter = opals)

#Model0 (unadjusted)
for (i in c(2,4,5)) {
  my_name = names(opals[i])
  #exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table,'$',interaction,'*', data_table,'$', exposure)))))")
  eval(parse(text=to_eval))
  to_eval = paste0("model1_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


# Model1 (adjusted)
for (i in c(2,4,5)) {
  my_name = names(opals[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                   paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+')))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model1_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#meta analyse here
#for (i in c("f_infant_dogs1")) {
for (i in c("f_infant_cats1")) {
  #  for (i in c("f_infant_cats1", "f_infant_dogs1")) {
  # for (i in c("infant_dogs_cats1", "infant_dogs_cats2", "infant_dogs_cats3")) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model1_2$coefficients[coefficient,'Estimate'], 
                    model1_4$coefficients[coefficient,'Estimate'], 
                    model1_5$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model1_2$coefficients[coefficient,'Estimate'], 
                    model1_4$coefficients[coefficient,'Estimate'], 
                    model1_5$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
  #   forest[i] <- eval(parse(text=(paste0("forest(res_",i,", xlab='Adjusted* OR', transf=exp, refline=1, slab=c('DNBC', 'INMA', 'NINFEA', 'Raine', 'MoBa'))"))))
  #this last step doesn't work..
} 
#Interaction
for (coefficient in c("E3$asthma_m1:E3$f_infant_dogs1")) {
  # coefficient = "E3$f_infant_dogs1:E3$f_infant_cats1"
  yi_ = c(model1_1$coefficients[coefficient,'Estimate'], model1_2$coefficients[coefficient,'Estimate'], 
          model1_3$coefficients[coefficient,'Estimate'], 
          model1_5$coefficients[coefficient,'Estimate'],
          model1_6$coefficients[coefficient,'Estimate'])
  sei_ <- c(model1_1$coefficients[coefficient,'Std. Error'], 
            model1_2$coefficients[coefficient,'Std. Error'], 
            model1_3$coefficients[coefficient,'Std. Error'], 
            model1_5$coefficients[coefficient,'Std. Error'],
            model1_6$coefficients[coefficient,'Std. Error'])
  #Random effects model:
  res_ <- rma(yi_, sei=sei_)
  
  #   forest[i] <- eval(parse(text=(paste0("forest(res_",i,", xlab='Adjusted* OR', transf=exp, refline=1, slab=c('DNBC', 'INMA', 'NINFEA', 'Raine', 'MoBa'))"))))
  #this last step doesn't work..
} 

#Forest plots:
forest(res_, xlab="Crude OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))

forest(res_f_infant_cats1, xlab="Crude OR", transf=exp, refline=1, slab=c("INMA", "Repro_pl", "Raine"))
forest(res_f_infant_dogs1, xlab="Crude OR", transf=exp, refline=1, slab=c("INMA", "Repro_pl", "Raine"))

forest(res_f_infant_cats1, xlab="Adjusted* OR", transf=exp, refline=1, slab=c("INMA", "Repro_pl", "Raine"))
forest(res_f_infant_dogs1, xlab="Adjusted* OR", transf=exp, refline=1, slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa"))

