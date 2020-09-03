library(opal)
library(dsBaseClient)


#Open workspace:
setwd("/home/angela/angela/Cat dog analysis/cats_and_dogs")
datashield.logout(opals)
load("login.Rda")
#opals<-datashield.login(logindata, restore='aug7_sws')
opals<-datashield.login(logindata, restore='temp')


####################################################################################
#Initially include all participants, then restrict to participants in models once these are finalised:

#Source Tim's variables and other libraries:

library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
#library(remotes)
#install_github("lifecycle-project/ds-helper")
library(dsHelper)

#setwd("/home/angela/angela/WP1 paper")
#source("Tims_getStats.R")


########################################################################################

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

# Now read in the table which lists which variables are missing for each study
# the _vars.csv file stores information about which variables a study is expected to have and
# which variables are completely missing for all participants
setwd("/home/angela/angela/Cat dog analysis/cats_and_dogs")
filter_csv = read.csv(file = 'cat_dog_vars.csv',  header=TRUE, row.names = 1 )

#This file lists all possible variables that could be incorporated into models
# not all variables will be used for all models 

#########################
#Model 1 population#####

#Tom's function has the option to disregard some variables (for e.g. that won't be used in a model)

none_cc_vars = c("edu_m_", "age_years","mother_id", "preg_no", "child_no", "cohort_id", "cob_m", "ethn1_m",
                 "ethn2_m", "ethn3_m", "asthma_m", "preg_smk", "preg_cig", "mode_delivery", "asthma_bf", "sex",
                 "plurality", "ga_lmp", "ga_bj", "sibling_pos", "breastfed_excl", "breastfed_any", "breastfed_ever", "eusilc_income",
                 "eusilc_income_quintiles", "cats_quant_preg", "dogs_quant_preg", "allergy_inh_m", "allergy_any_m", "asthma_ever_CHICOS", "pets_preg",
                 "sens_cat7", "sens_cat4", "sens_dog4", "sens_dog7", "f_infant_cats", "f_infant_dogs", "cats", "dogs", "early_life_cats", 
                 "early_life_dogs", "infant_dogs_cats", "infant_dogs_cats2", "preg_dogs_cats2", "preg_dogs_cats", "dogs_quant_preg_cat",
                 "cats_quant_preg_cat", "dogs_quant_preg2", "cats_quant_preg2", "pets_quant_preg_cat", "pets_quant_preg2", 
                 "isaac", "breastfedcat", "csection", "sibling_pos2", "famsize_child", "famsize_childcat", "income",
                 "dogs_quant_inf", "dogs_quant_inf_cat", "cats_quant_inf", "cats_quant_inf_cat", "dogs_quant_inf2", "cats_quant_inf2",
                 "pets_quant_inf", "pets_quant_inf_cat", "pets_quant_inf2", "pets_quant_preg", "hhincome_", "cohort_id_noNA", "ethn1_m_noNA",
                 "preg_cig_noNA", "mode_delivery_noNA", "asthma_bf_noNA", "plurality_noNA", "sibling_pos_noNA", "eusilc_income_quintiles_noNA",
                 "cats_preg_noNA", "dogs_preg_noNA", "allergy_any_m_noNA", "asthma_ever_CHICOS_noNA", "pets_preg_noNA",
                 "sens_cat7_noNA", "sens_cat4_noNA", "sens_dog4_noNA", "sens_dog7_noNA", "f_infant_cats_noNA", "f_infant_dogs_noNA",
                 "cats_noNA", "dogs_noNA", "early_life_cats_noNA", "early_life_dogs_noNA", "infant_dogs_cats_noNA", "infant_dogs_cats2_noNA",
                 "preg_dogs_cats2_noNA", "preg_dogs_cats_noNA", "dogs_quant_preg_cat_noNA", "cats_quant_preg_cat_noNA", "pets_quant_preg_cat_noNA", "isaac_noNA",
                 "medall_noNA", "dogs_quant_inf_cat_noNA", "cats_quant_inf_cat_noNA", "pets_quant_inf_cat_noNA",
                 "hhincome__noNA")



#Now restrict data frame and select complete cases:
for (i in c(1:num_studies)) {
  my_name = names(opals[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  ds.subset(x = 'E1', subset = 'E2', cols = list_variables, datasources = opals[i])
  ds.subset(x = 'E2', subset = 'E3', completeCases = TRUE, datasources = opals[i])
}

length_complete = ds.length("E3$child_id", type = "split", datasources = opals)


#Merge sensitisation, infancy and asthma_ever data back in:

#First create a data frame with required variables:
for (i in c(1:num_studies)) {
  ds.subset(x = 'E1', subset = 'other', cols = c("child_id", "sens_cat7", "sens_cat4", "sens_dog4", "sens_dog7", "asthma_ever_CHICOS_noNA", "asthma_ever_CHICOS", 
                                                "f_infant_cats", "f_infant_dogs"), datasources = opals[i]) }

ds.merge(
  x.name = 'E3', 
  y.name = 'other', 
  by.x.names = 'child_id',
  by.y.names = 'child_id', 
  newobj = 'table1',
  datasources = opals
)



#################################################################################
###TABLES########################################################################

#Table 1 

table_1 <- dh.getStats(
  df = "E1",
  vars = c("cats_preg", "dogs_preg", "cats_quant_preg_cat", "dogs_quant_preg_cat", "pets_quant_preg_cat", 
           "f_infant_cats", "f_infant_dogs", "asthma_ever_CHICOS", "isaac", "medall", "sens_cat7", "sens_cat4", "sens_dog4", "sens_dog7")
)

View(table_1[["categorical"]])

#Table S1 (child covariates)

table_s1 <- dh.getStats(
  df = "E1",
  vars = c("sex", "birth_weight", "breastfedcat", "csection", "ga", "sibling_pos2"))

View(table_s1)
View(table_s1[["categorical"]])
View(table_s1[["continuous"]])

#Table S2 (mother covariates)

table_s2 <- dh.getStats(
  df = "E1",
  vars = c("edu_m_", "cob_m", "ethn2_m", "ethn3_m", "agebirth_m_y", "asthma_m", "preg_smk", 
           "allergy_inh_m", "allergy_any_m"))

#Table S3 (father covariates)

table_s3 <- dh.getStats(
  df = "E1",
  vars = c("asthma_bf"))

#Table S4 (environmental covariates)
table_s4 <- dh.getStats(
  df = "E1",
  vars = c("sibling_pos2", "famsize_child", "income")) # including "famsize_childcat" creates an error

View(table_s4)


#Other tables of interest:
exposure <- dh.getStats(
  df = "E1",
  vars = c("cats_preg", "dogs_preg", "pets_preg",  "cats_quant_preg", "dogs_quant_preg", 
           "f_infant_cats", "f_infant_dogs", "cats", "dogs", 
           "early_life_cats", "early_life_dogs", "infant_dogs_cats", "infant_dogs_cats2", "preg_dogs_cats2", 
           "preg_dogs_cats", "dogs_quant_preg_cat", "cats_quant_preg_cat", "pets_quant_preg_cat"))

