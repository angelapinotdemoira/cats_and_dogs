################################################################################
## Description: Models for LifeCycle cats and dogs study
## Date: 25th May 2020
## Author: Angela Pinot de Moira
################################################################################

library(DSI)
library(DSOpal)
library(dsBaseClient)
library(metafor)
library(tidyr)
library(purrr)
library(dsHelper)
library(dplyr)
#Open workspace:
setwd("/home/angela/angela/Cat dog analysis/cats_and_dogs")
DSI::datashield.logout(connections)
load("login.Rda")
connections <- DSI::datashield.login(logins = logindata, restore='14oct')
datashield.connections_default('connections')


# Source in the Tom's extra functions for analysis
source("toms_variable_functions.R")



# Set studynames and numstudies
temp <- ds.summary('D3$child_id', datasources = connections)
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

###############################################################################
########################### SET UP DATA  ######################################
###############################################################################
# remove twins and other multiples
ds.dataFrameSubset(
  df.name = "D3",
  V1.name = "D3$plurality",
  V2.name = "1",
  Boolean.operator = "==",
  newobj = "E1",
  datasources = connections[c(1,2,3,4,5,6,7)],
  notify.of.progress = FALSE
)

ds.dataFrameSubset(
  df.name = "D3",
  V1.name = "D3$plurality_noNA",
  V2.name = "999",
  Boolean.operator = "==",
  newobj = "E1",
  datasources = connections[8],
  notify.of.progress = FALSE
)

#check that child_no is now "1" for all cohorts (n.b.: there is an issue with moba's child_no)
ds.table("E1$child_no", datasources = connections[c(1,2,3,4,5,6,7)])

# all participants
all_participants <- ds.length('E1$child_id', datasources = connections)
all_participants_split <- ds.length('E1$child_id',type = 'split', datasources = connections)


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

# Exposure: cats and dogs (yes/no) in infancy
# Outcome: current asthma at around 7 years
# Covariates: sex, mother's education, hh income, smoking, mother's asthma, mother's allergy, 
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
model1 = read.csv(file = 'model1NA_vars.csv', header=TRUE)
#model1 = read.csv(file = 'model1bNA_vars.csv', header=TRUE) #interaction with wheeze
#model1 = read.csv(file = 'model1_vars.csv', header=TRUE)

model1 = read.csv(file = 'model1dNA_vars.csv', header=TRUE)


none_cc_vars <- model1 %>%
  filter(Include=="0")  %>%
  print()
none_cc_vars = unlist(list(none_cc_vars$Variable))


#Now restrict data frame and select complete cases:
for (i in c(1:length(connections))) {
  my_name = names(connections[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  list_variables <- purrr::map_chr(list_variables, ~ paste0("E1$", .))
  ds.dataFrame(x = list_variables, newobj = "E2", datasources = connections[i])
  ds.completeCases(x1 = "E2", newobj = "E3", datasources = connections[i])
  }


length_complete = ds.length("E3$child_id", type = "split", datasources = connections)

#Use dataframe fill to add back excluded variables?

############################################################3
#MODEL

#Crude model

#Build model:
data_table = "E3"

#outcome = c('isaac')
outcome = c('medall')

exposure = 'f_infant_dogs'
exposure = 'f_infant_cats'



interaction = "asthma_m"
#interaction = "wheeze"

#Model0 (unadjusted)
#for (i in c(1:length(connections)))
for (i in c(1,2,3,5,6,7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table,'$',interaction,'*', data_table,'$', exposure)))))")
  eval(parse(text=to_eval))
  to_eval = paste0("model0_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}


############################

#meta analyse here
for (i in c("f_infant_dogs1")) {
#for (i in c("f_infant_cats1")) {
                    coefficient = paste0(data_table, '$', i)
                    to_eval1 = paste0("yi_",i," = c(model0_1$coefficients[coefficient,'Estimate'], 
                    model0_2$coefficients[coefficient,'Estimate'], 
                    model0_3$coefficients[coefficient,'Estimate'], 
                    model0_5$coefficients[coefficient,'Estimate'],
                    model0_6$coefficients[coefficient,'Estimate'],
                    model0_7$coefficients[coefficient,'Estimate'],
                    model0_8$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model0_1$coefficients[coefficient,'Std. Error'], 
                                    model0_2$coefficients[coefficient,'Std. Error'], 
                                    model0_3$coefficients[coefficient,'Std. Error'], 
                                    model0_5$coefficients[coefficient,'Std. Error'],
                                    model0_6$coefficients[coefficient,'Std. Error'],
                                    model0_7$coefficients[coefficient,'Std. Error'],
                                    model0_8$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 


###Forest plots---------------------------
#DOGS
#Create a dataframe from 2x2 table which displays Asthma N(%) by cat:
# Set studynames and numstudies
temp <- ds.summary('E3$child_id', datasources = connections[c(1,2,3,5,6,7,8)])
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

output <- ds.table2D('E3$medall', 'E3$f_infant_dogs',  datasources =connections[c(1,2,3,5,6,7,8)])
counts <- data.frame(matrix(unlist(output$counts), nrow = num_studies, ncol = 9, byrow=T))
perc <- data.frame(matrix(unlist(output$colPercent), nrow = num_studies, ncol = 9, byrow=T))
counts1 = counts[,c(2)] 
perc1 = perc[,c(2)]
counts2 = counts[,c(5)] 
perc2 = perc[,c(5)]
dogs_model1 <- data.frame(cbind(counts1,perc1,counts2,perc2))
rownames(dogs_model1) <- study_names
colnames(dogs_model1) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
rm(output,counts, perc, counts1,perc1,counts2,perc2)
dogs_model1

#Forest plots with Asthma N(%):
forest(res_f_infant_dogs1, xlim=c(-8,6), at=log(c(0.25, 1, 3)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(dogs_model1$PnAcount, " (", dogs_model1$PnAperc, ")"), paste0(dogs_model1$PpAcount, " (", 
                                                                                       dogs_model1$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 0.9,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.8, font=2)
text(c(-5.5, -3.5), 8.3, c("No dog", "Dog"))
text(c(-4.5),     9, c("Asthma N (%)"))
text(3.75, 9, "Weight and Odds ratio [95% CI]")
text(-7.5, 9, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.7)
text(-8,-1.5, pos=4, cex=1, bquote(paste("RE Model for all studies (Q = ",
                                         .(formatC(res_f_infant_dogs1$QE, digits=2, format="f")), ", df = ", .(res_f_infant_dogs1$k - res_f_infant_dogs1$p),
                                         ", p = ", .(formatC(res_f_infant_dogs1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_f_infant_dogs1$I2, digits = 2, format="f")), "%)")))


###------------------------------------------------------

#Cats
#Create a dataframe from 2x2 table which displays Asthma N(%) by cat:
output <- ds.table2D('E3$medall', 'E3$f_infant_cats',  datasources =connections[c(1,2,3,5,6,7,8)])
counts <- data.frame(matrix(unlist(output$counts), nrow = num_studies, ncol = 9, byrow=T))
perc <- data.frame(matrix(unlist(output$colPercent), nrow = num_studies, ncol = 9, byrow=T))
counts1 = counts[,c(2)] 
perc1 = perc[,c(2)]
counts2 = counts[,c(5)] 
perc2 = perc[,c(5)]
cats_model1 <- data.frame(cbind(counts1,perc1,counts2,perc2))
rownames(cats_model1) <- study_names
colnames(cats_model1) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
rm(output,counts, perc, counts1,perc1,counts2,perc2)
cats_model1

#Forest plots with Asthma N(%):
forest(res_f_infant_cats1, xlim=c(-8,6), at=log(c(0.25, 1, 3)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(cats_model1$PnAcount, " (", cats_model1$PnAperc, ")"), paste0(cats_model1$PpAcount, " (", 
                                                                                      cats_model1$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 0.9,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.8, font=2)
text(c(-5.5, -3.5), 8.3, c("No cat", "Cat"))
text(c(-4.5),     9, c("Asthma N (%)"))
text(3.75, 9, "Weight and Odds ratio [95% CI]")
text(-7.5, 9, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.7)
text(-8,-1.5, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_f_infant_cats1$QE, digits=2, format="f")), ", df = ", .(res_f_infant_cats1$k - res_f_infant_cats1$p),
                                       ", p = ", .(formatC(res_f_infant_cats1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                       .(formatC(res_f_infant_cats1$I2, digits = 2, format="f")), "%)")))
#\n 

rm(res_f_infant_cats1)
rm(res_f_infant_dogs1)
##############################################################################
#Adjusted model:

#Build model:
data_table = "E3"

#outcome = c('isaac')
outcome = c('medall')

exposure = 'f_infant_dogs'
#exposure = 'f_infant_cats'


#interaction = "asthma_m"

covariates =  c('f_infant_cats', "asthma_m_noNA", "edu_m_", "income_noNA", "preg_smk", "sex", "cob_m", 
                "sibling_pos2", "agebirth_m_y_c", "csection_noNA", "log_ga", "birth_weight_c",
                "famsize_childcat", "breastfed_ever", "ethn3_m", "ethn2_m")

#"allergy_any_m_noNA", "allergy_any_m", 

# Model1 (adjusted)
for (i in c(1,2,3,5,6,7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                                                                  paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+')))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model1_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
  }

############################

#meta analyse here
#for (i in c("f_infant_dogs1")) {
for (i in c("f_infant_cats1")) {
    coefficient = paste0(data_table, '$', i)
    to_eval1 = paste0("yi_",i," = c(model1_1$coefficients[coefficient,'Estimate'], model1_2$coefficients[coefficient,'Estimate'], 
                                  model1_3$coefficients[coefficient,'Estimate'], 
                                  model1_5$coefficients[coefficient,'Estimate'],
                                  model1_6$coefficients[coefficient,'Estimate'],
                                  model1_7$coefficients[coefficient,'Estimate'],
                                  model1_8$coefficients[coefficient,'Estimate'])")
    eval(parse(text=to_eval1))
    to_eval2 = paste0("sei_",i," <- c(model1_1$coefficients[coefficient,'Std. Error'], 
                                    model1_2$coefficients[coefficient,'Std. Error'], 
                                    model1_3$coefficients[coefficient,'Std. Error'], 
                                    model1_5$coefficients[coefficient,'Std. Error'],
                                    model1_6$coefficients[coefficient,'Std. Error'],
                                    model1_7$coefficients[coefficient,'Std. Error'],
                                    model1_8$coefficients[coefficient,'Std. Error'])")
    eval(parse(text=to_eval2))
    #Random effects model:
    to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
    eval(parse(text=to_eval3))
         } 



###Forest plots---------------------------
#DOGS

#Forest plots with Asthma N(%):
forest(res_f_infant_dogs1, xlim=c(-8,6), at=log(c(0.25, 1, 3)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(dogs_model1$PnAcount, " (", dogs_model1$PnAperc, ")"), paste0(dogs_model1$PpAcount, " (", 
                                                                                       dogs_model1$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 0.9,
       xlab="Adjusted* Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.8, font=2)
text(c(-5.5, -3.5), 8.3, c("No dog", "Dog"))
text(c(-4.5),     9, c("Asthma N (%)"))
text(3.75, 9, "Weight and Odds Ratio [95% CI]")
text(-7.5, 9, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.7)
text(-8,-1.5, pos=4, cex=1, bquote(paste("RE Model for all studies (Q = ",
                                         .(formatC(res_f_infant_dogs1$QE, digits=2, format="f")), ", df = ", .(res_f_infant_dogs1$k - res_f_infant_dogs1$p),
                                         ", p = ", .(formatC(res_f_infant_dogs1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_f_infant_dogs1$I2, digits = 2, format="f")), "%)")))


###------------------------------------------------------

#Cats

#Forest plots with Asthma N(%):
forest(res_f_infant_cats1, xlim=c(-8,6), at=log(c(0.25, 1, 3)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(cats_model1$PnAcount, " (", cats_model1$PnAperc, ")"), paste0(cats_model1$PpAcount, " (", 
                                                                                       cats_model1$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 0.9,
       xlab="Adjusted* Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.8, font=2)
text(c(-5.5, -3.5), 8.3, c("No cat", "Cat"))
text(c(-4.5),     9, c("Asthma N (%)"))
text(3.75, 9, "Weight and Odds ratio [95% CI]")
text(-7.5, 9, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.7)
text(-8,-1.5, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_f_infant_cats1$QE, digits=2, format="f")), ", df = ", .(res_f_infant_cats1$k - res_f_infant_cats1$p),
                                         ", p = ", .(formatC(res_f_infant_cats1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_f_infant_cats1$I2, digits = 2, format="f")), "%)")))






#Interaction------------------------------------

for (coefficient in c("E3$asthma_m1:E3$f_infant_dogs1")) {
#for (coefficient in c("E3$asthma_m1:E3$f_infant_cats1")) {
# coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model0_1$coefficients[coefficient,'Estimate'], 
                    model0_2$coefficients[coefficient,'Estimate'], 
                    model0_3$coefficients[coefficient,'Estimate'], 
                    model0_5$coefficients[coefficient,'Estimate'],
                    model0_6$coefficients[coefficient,'Estimate'],
                    model0_7$coefficients[coefficient,'Estimate'],
                    model0_8$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model0_1$coefficients[coefficient,'Std. Error'], 
                                    model0_2$coefficients[coefficient,'Std. Error'], 
                                    model0_3$coefficients[coefficient,'Std. Error'], 
                                    model0_5$coefficients[coefficient,'Std. Error'],
                                    model0_6$coefficients[coefficient,'Std. Error'],
                                    model0_7$coefficients[coefficient,'Std. Error'],
                                    model0_8$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 

forest(res_8, xlim=c(-8,6), at=log(c(0.01, 1, 10)), atransf=exp, showweights=TRUE,
       header = TRUE, cex = 1,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")


#for (coefficient in c("E3$wheeze1:E3$f_infant_dogs1")) {
for (coefficient in c("E3$wheeze1:E3$f_infant_cats1")) {
  # coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model0_1$coefficients[coefficient,'Estimate'], 
                    model0_2$coefficients[coefficient,'Estimate'], 
                    model0_3$coefficients[coefficient,'Estimate'], 
                    model0_5$coefficients[coefficient,'Estimate'],
                    model0_6$coefficients[coefficient,'Estimate'],
                    model0_7$coefficients[coefficient,'Estimate'],
                    model0_8$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model0_1$coefficients[coefficient,'Std. Error'], 
                    model0_2$coefficients[coefficient,'Std. Error'], 
                    model0_3$coefficients[coefficient,'Std. Error'], 
                    model0_5$coefficients[coefficient,'Std. Error'],
                    model0_6$coefficients[coefficient,'Std. Error'],
                    model0_7$coefficients[coefficient,'Std. Error'],
                    model0_8$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 

forest(res_8, xlim=c(-8,6), at=log(c(0.01, 1, 10)), atransf=exp, showweights=TRUE,
       header = TRUE, cex = 1,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")

forest(res_f_infant_dogs1, xlim=c(-8,6), at=log(c(0.01, 1, 10)), atransf=exp, showweights=TRUE,
       header = TRUE, cex = 1,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")






#for (coefficient in c("E3$asthma_m1:E3$f_infant_dogs1")) {
for (coefficient in c("E3$asthma_m1:E3$f_infant_cats1")) {
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

setwd("/home/angela/angela/Cat dog analysis/cats_and_dogs")
filter_csv = read.csv(file = 'cat_dog_vars.csv',  header=TRUE, row.names = 1 )

#This file lists all possible variables that could be incorporated into models
#not all variables will be used for all models 



#Tom's function has the option to disregard some variables (for e.g. that won't be used in a model)
#model2 = read.csv(file = 'model2NA_vars.csv', header=TRUE)
model2 = read.csv(file = 'model2bNA_vars.csv', header=TRUE)


none_cc_vars <- model2 %>%
  filter(Include=="0")  %>%
  print()
none_cc_vars = unlist(list(none_cc_vars$Variable))


#Now restrict data frame and select complete cases:
for (i in c(1:length(connections))) {
  my_name = names(connections[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  list_variables <- purrr::map_chr(list_variables, ~ paste0("E1$", .))
  ds.dataFrame(x = list_variables, newobj = "E2", datasources = connections[i])
  ds.completeCases(x1 = "E2", newobj = "E3", datasources = connections[i])
}


length_complete = ds.length("E3$child_id", type = "split", datasources = connections)

#Use dataframe fill to add back excluded variables?

############################################################3
#MODEL

#Crude model

#Build model:
data_table = "E3"

#outcome = c('isaac')
outcome = c('medall')

exposure = 'dogs_preg'
exposure = 'cats_preg'


interaction = "asthma_m"

#Model0 (unadjusted)
#for (i in c(1:length(connections)))
for (i in c(1,2,3,5,6,7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table,'$',interaction,'*', data_table,'$', exposure)))))")
  eval(parse(text=to_eval))
  to_eval = paste0("model0_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}


############################

#meta analyse here
#for (i in c("dogs_preg1")) {
for (i in c("cats_preg1")) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model0_1$coefficients[coefficient,'Estimate'], 
                    model0_2$coefficients[coefficient,'Estimate'], 
                    model0_3$coefficients[coefficient,'Estimate'], 
                    model0_5$coefficients[coefficient,'Estimate'],
                    model0_6$coefficients[coefficient,'Estimate'],
                    model0_7$coefficients[coefficient,'Estimate'],
                    model0_8$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model0_1$coefficients[coefficient,'Std. Error'], 
                    model0_2$coefficients[coefficient,'Std. Error'], 
                    model0_3$coefficients[coefficient,'Std. Error'], 
                    model0_5$coefficients[coefficient,'Std. Error'],
                    model0_6$coefficients[coefficient,'Std. Error'],
                    model0_7$coefficients[coefficient,'Std. Error'],
                    model0_8$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 


###Forest plots---------------------------
#DOGS
#Create a dataframe from 2x2 table which displays Asthma N(%) by cat:
# Set studynames and numstudies
temp <- ds.summary('E3$child_id', datasources = connections[c(1,2,3,5,6,7,8)])
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

output <- ds.table2D('E3$medall', 'E3$dogs_preg',  datasources =connections[c(1,2,3,5,6,7,8)])
counts <- data.frame(matrix(unlist(output$counts), nrow = num_studies, ncol = 9, byrow=T))
perc <- data.frame(matrix(unlist(output$colPercent), nrow = num_studies, ncol = 9, byrow=T))
counts1 = counts[,c(2)] 
perc1 = perc[,c(2)]
counts2 = counts[,c(5)] 
perc2 = perc[,c(5)]
dogs_model2 <- data.frame(cbind(counts1,perc1,counts2,perc2))
rownames(dogs_model2) <- study_names
colnames(dogs_model2) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
rm(output,counts, perc, counts1,perc1,counts2,perc2)
dogs_model2

#Forest plots with Asthma N(%):
forest(res_dogs_preg1, xlim=c(-8,6), at=log(c(0.25, 1, 3)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(dogs_model2$PnAcount, " (", dogs_model2$PnAperc, ")"), paste0(dogs_model2$PpAcount, " (", 
                                                                                       dogs_model2$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 0.9,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.8, font=2)
text(c(-5.5, -3.5), 8.3, c("No dog", "Dog"))
text(c(-4.5),     9, c("Asthma N (%)"))
text(3.75, 9, "Weight and Odds ratio [95% CI]")
text(-7.5, 9, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.7)
text(-8,-1.5, pos=4, cex=1, bquote(paste("RE Model for all studies (Q = ",
                                         .(formatC(res_dogs_preg1$QE, digits=2, format="f")), ", df = ", .(res_dogs_preg1$k - res_dogs_preg1$p),
                                         ", p = ", .(formatC(res_dogs_preg1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_dogs_preg1$I2, digits = 2, format="f")), "%)")))


###------------------------------------------------------

#Cats
#Create a dataframe from 2x2 table which displays Asthma N(%) by cat:
output <- ds.table2D('E3$medall', 'E3$cats_preg',  datasources =connections[c(1,2,3,5,6,7,8)])
counts <- data.frame(matrix(unlist(output$counts), nrow = num_studies, ncol = 9, byrow=T))
perc <- data.frame(matrix(unlist(output$colPercent), nrow = num_studies, ncol = 9, byrow=T))
counts1 = counts[,c(2)] 
perc1 = perc[,c(2)]
counts2 = counts[,c(5)] 
perc2 = perc[,c(5)]
cats_model2 <- data.frame(cbind(counts1,perc1,counts2,perc2))
rownames(cats_model2) <- study_names
colnames(cats_model2) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
rm(output,counts, perc, counts1,perc1,counts2,perc2)
cats_model2

#Forest plots with Asthma N(%):
forest(res_cats_preg1, xlim=c(-8,6), at=log(c(0.25, 1, 3.5)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(cats_model2$PnAcount, " (", cats_model2$PnAperc, ")"), paste0(cats_model2$PpAcount, " (", 
                                                                                       cats_model2$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 0.9,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.8, font=2)
text(c(-5.5, -3.5), 8.3, c("No cat", "Cat"))
text(c(-4.5),     9, c("Asthma N (%)"))
text(3.75, 9, "Weight and Odds Ratio [95% CI]")
text(-7.5, 9, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.7)
text(-8,-1.5, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_cats_preg1$QE, digits=2, format="f")), ", df = ", .(res_cats_preg1$k - res_cats_preg1$p),
                                         ", p = ", .(formatC(res_cats_preg1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_cats_preg1$I2, digits = 2, format="f")), "%)")))
#\n 

rm(res_dogs_preg1)
rm(res_cats_preg1)
##############################################################################
#Adjusted model:

#Build model:
data_table = "E3"

#outcome = c('isaac')
outcome = c('medall')

exposure = 'dogs_preg'


#interaction = "asthma_m"

covariates =  c("cats_preg", "asthma_m_noNA", "edu_m_", "income_noNA", "preg_smk", "sex", "cob_m", 
                "sibling_pos2", "agebirth_m_y_c", 
                "ethn3_m", "ethn2_m")

#"allergy_any_m_noNA", "allergy_any_m", 

# Model2 (adjusted)
for (i in c(1,2,3,5,6,7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                   paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+')))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model2_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}

############################

#meta analyse here
#for (i in c("dogs_preg1")) {
for (i in c("cats_preg1")) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model2_1$coefficients[coefficient,'Estimate'], model2_2$coefficients[coefficient,'Estimate'], 
                    model2_3$coefficients[coefficient,'Estimate'], 
                    model2_5$coefficients[coefficient,'Estimate'],
                    model2_6$coefficients[coefficient,'Estimate'],
                    model2_7$coefficients[coefficient,'Estimate'],
                    model2_8$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model2_1$coefficients[coefficient,'Std. Error'], 
                    model2_2$coefficients[coefficient,'Std. Error'], 
                    model2_3$coefficients[coefficient,'Std. Error'], 
                    model2_5$coefficients[coefficient,'Std. Error'],
                    model2_6$coefficients[coefficient,'Std. Error'],
                    model2_7$coefficients[coefficient,'Std. Error'],
                    model2_8$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 



###Forest plots---------------------------
#DOGS

#Forest plots with Asthma N(%):
forest(res_dogs_preg1, xlim=c(-8,6), at=log(c(0.25, 1, 3)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(dogs_model2$PnAcount, " (", dogs_model2$PnAperc, ")"), paste0(dogs_model2$PpAcount, " (", 
                                                                                       dogs_model2$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 0.9,
       xlab="Adjusted* Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.8, font=2)
text(c(-5.5, -3.5), 8.3, c("No dog", "Dog"))
text(c(-4.5),     9, c("Asthma N (%)"))
text(3.75, 9, "Weight and Odds Ratio [95% CI]")
text(-7.5, 9, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.7)
text(-8,-1.5, pos=4, cex=1, bquote(paste("RE Model for all studies (Q = ",
                                         .(formatC(res_dogs_preg1$QE, digits=2, format="f")), ", df = ", .(res_dogs_preg1$k - res_dogs_preg1$p),
                                         ", p = ", .(formatC(res_dogs_preg1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_dogs_preg1$I2, digits = 2, format="f")), "%)")))


###------------------------------------------------------

#Cats

#Forest plots with Asthma N(%):
forest(res_cats_preg1, xlim=c(-8,6), at=log(c(0.25, 1, 3.5)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(cats_model2$PnAcount, " (", cats_model2$PnAperc, ")"), paste0(cats_model2$PpAcount, " (", 
                                                                                       cats_model2$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 0.9,
       xlab="Adjusted* Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.8, font=2)
text(c(-5.5, -3.5), 8.3, c("No cat", "Cat"))
text(c(-4.5),     9, c("Asthma N (%)"))
text(3.75, 9, "Weight and Odds ratio [95% CI]")
text(-7.5, 9, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.7)
text(-8,-1.5, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_cats_preg1$QE, digits=2, format="f")), ", df = ", .(res_cats_preg1$k - res_cats_preg1$p),
                                         ", p = ", .(formatC(res_cats_preg1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_cats_preg1$I2, digits = 2, format="f")), "%)")))






#Interaction------------------------------------

#for (coefficient in c("E3$asthma_m1:E3$f_infant_dogs1")) {
for (coefficient in c("E3$asthma_m1:E3$f_infant_cats1")) {
  # coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model0_1$coefficients[coefficient,'Estimate'], 
                    model0_2$coefficients[coefficient,'Estimate'], 
                    model0_3$coefficients[coefficient,'Estimate'], 
                    model0_5$coefficients[coefficient,'Estimate'],
                    model0_6$coefficients[coefficient,'Estimate'],
                    model0_7$coefficients[coefficient,'Estimate'],
                    model0_8$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model0_1$coefficients[coefficient,'Std. Error'], 
                    model0_2$coefficients[coefficient,'Std. Error'], 
                    model0_3$coefficients[coefficient,'Std. Error'], 
                    model0_5$coefficients[coefficient,'Std. Error'],
                    model0_6$coefficients[coefficient,'Std. Error'],
                    model0_7$coefficients[coefficient,'Std. Error'],
                    model0_8$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 

forest(res_8, xlim=c(-8,6), at=log(c(0.01, 1, 10)), atransf=exp, showweights=TRUE,
       header = TRUE, cex = 1,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")



#for (coefficient in c("E3$asthma_m1:E3$f_infant_dogs1")) {
for (coefficient in c("E3$asthma_m1:E3$f_infant_cats1")) {
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
# |  \/  |         | |    | | | |
# | .  . | ___   __| | ___| | | |
# | |\/| |/ _ \ / _` |/ _ \ | | |  __
# | |  | | (_) | (_| |  __/ | | |_| |_
# \_|  |_/\___/ \__,_|\___|_| |_______|

# Exposure: cats and dogs (yes/no) in infancy
# Outcome: cat/dog sensitisation at around 7 years
# Covariates: age, sex, mother's education, hh income, smoking, mother's asthma, mother's allergy?, 
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
model4 = read.csv(file = 'model4NA_vars.csv', header=TRUE)



none_cc_vars <- model4 %>%
  filter(Include=="0")  %>%
  print()
none_cc_vars = unlist(list(none_cc_vars$Variable))


#Now restrict data frame and select complete cases:
for (i in c(1:length(connections))) {
  my_name = names(connections[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  list_variables <- purrr::map_chr(list_variables, ~ paste0("E1$", .))
  ds.dataFrame(x = list_variables, newobj = "E2", datasources = connections[i])
  ds.completeCases(x1 = "E2", newobj = "E3", datasources = connections[i])
}


length_complete = ds.length("E3$child_id", type = "split", datasources = connections)

#Use dataframe fill to add back excluded variables?

############################################################3
#MODEL

#Crude model

#Build model:
data_table = "E3"

#outcome = c('isaac')
outcome = c('sens_cat7')

exposure = 'f_infant_dogs'
#exposure = 'f_infant_cats'


#interaction = "asthma_m"

#Model0 (unadjusted)
#for (i in c(1:length(connections)))
for (i in c(2,5,7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table,'$',interaction,'*', data_table,'$', exposure)))))")
  eval(parse(text=to_eval))
  to_eval = paste0("model0_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}


############################

#meta analyse here
for (i in c("f_infant_dogs1")) {
#for (i in c("f_infant_cats1")) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model0_2$coefficients[coefficient,'Estimate'], 
                   # model0_4$coefficients[coefficient,'Estimate'], 
                    model0_5$coefficients[coefficient,'Estimate'], 
                    model0_7$coefficients[coefficient,'Estimate'],
                    model0_8$coefficients[coefficient,'Estimate']
                    )")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model0_2$coefficients[coefficient,'Std. Error'], 
                   # model0_4$coefficients[coefficient,'Std. Error'], 
                    model0_5$coefficients[coefficient,'Std. Error'], 
                    model0_7$coefficients[coefficient,'Std. Error'],
                    model0_8$coefficients[coefficient,'Std. Error']
                    )")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 


###Forest plots---------------------------
#DOGS
# Set studynames and numstudies
temp <- ds.summary('E3$child_id', datasources = connections[c(2,5,7,8)])
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

#Create a dataframe from 2x2 table which displays Asthma N(%) by cat:
output <- ds.table2D('E3$sens_cat7', 'E3$f_infant_dogs',  datasources =connections[c(2,5,7,8)])
counts <- data.frame(matrix(unlist(output$counts), nrow = 4, ncol = 9, byrow=T))
perc <- data.frame(matrix(unlist(output$colPercent), nrow = 4, ncol = 9, byrow=T))
counts1 = counts[,c(2)] 
perc1 = perc[,c(2)]
counts2 = counts[,c(5)] 
perc2 = perc[,c(5)]
dogs_model1 <- data.frame(cbind(counts1,perc1,counts2,perc2))
rownames(dogs_model1) <- study_names
colnames(dogs_model1) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
rm(output,counts, perc, counts1,perc1,counts2,perc2)
dogs_model1


#Forest plots with Cat sensitisation N(%):
forest(res_f_infant_dogs1, xlim=c(-8,8), at=log(c(0.20, 1, 10.5)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(dogs_model1$PnAcount, " (", dogs_model1$PnAperc, ")"), paste0(dogs_model1$PpAcount, " (", 
                                                                                       dogs_model1$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 1,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("INMA", "Raine", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.9, font=2)
text(c(-5.5, -3.5), 5.3, c("No dog", "Dog"))
text(c(-4.5),     6, c("Cat sensitised N (%)"))
text(5.0, 6, "Weight and Odds ratio [95% CI]")
text(-7.2, 6, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.8)
text(-8,-0.4, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_f_infant_dogs1$QE, digits=2, format="f")),  ", df = ", .(res_f_infant_dogs1$k - res_f_infant_dogs1$p),
                                         ", p = ", .(formatC(res_f_infant_dogs1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_f_infant_dogs1$I2, digits = 2, format="f")), "%)")))

###------------------------------------------------------

#Cats
#Create a dataframe from 2x2 table which displays Sensitisation N(%) by cat:
output <- ds.table2D('E3$sens_cat7', 'E3$f_infant_cats',  datasources =connections[c(2,5,7,8)])
counts <- data.frame(matrix(unlist(output$counts), nrow = 4, ncol = 9, byrow=T))
perc <- data.frame(matrix(unlist(output$colPercent), nrow = 4, ncol = 9, byrow=T))
counts1 = counts[,c(2)] 
perc1 = perc[,c(2)]
counts2 = counts[,c(5)] 
perc2 = perc[,c(5)]
cats_model1 <- data.frame(cbind(counts1,perc1,counts2,perc2))
rownames(cats_model1) <- study_names
colnames(cats_model1) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
rm(output,counts, perc, counts1,perc1,counts2,perc2)
cats_model1
#Change NAs to <5:
for (i in c("cats_model1$PnAcount", "cats_model1$PpAcount")) {
  eval(parse(text=(paste0("",i,"[is.na(",i,")]<- '<=13'"))))
}
for (i in c("cats_model1$PnAperc", "cats_model1$PpAperc")) {
  eval(parse(text=(paste0("",i,"[is.na(",i,")]<- '.'"))))
}



#Forest plots with Sensitisation N(%):
forest(res_f_infant_cats1, xlim=c(-8,8), at=log(c(0.10, 1, 4.5)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(cats_model1$PnAcount, " (", cats_model1$PnAperc, ")"), paste0(cats_model1$PpAcount, " (", 
                                                                                       cats_model1$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 1,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("INMA", "Raine", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.9, font=2)
text(c(-5.5, -3.5), 5.3, c("No cat", "Cat"))
text(c(-4.5),     6, c("Cat sensitised N (%)"))
text(5, 6, "Weight and Odds ratio [95% CI]")
text(-7.3, 6, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.8)
text(-8,-0.275, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_f_infant_cats1$QE, digits=2, format="f")),  ", df = ", .(res_f_infant_cats1$k - res_f_infant_cats1$p),
                                         ", p = ", .(formatC(res_f_infant_cats1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_f_infant_cats1$I2, digits = 2, format="f")), "%)")))
#\n 

rm(res_f_infant_cats1)
rm(res_f_infant_dogs1)
rm(model1_2, model1_5, model1_7, model1_8)
rm(model0_2, model0_7, model0_8)
##############################################################################
#Adjusted model:

#Build model:
data_table = "E3"

outcome = c('sens_cat7')

exposure = 'f_infant_dogs'
#exposure = 'f_infant_cats'


#interaction = "asthma_m"

covariates =  c('f_infant_dogs', "asthma_m_noNA", "edu_m_", "income_noNA", "sex", "cob_m", 
                "sibling_pos2", "agebirth_m_y_c", "csection_noNA", "log_ga", "birth_weight_c",
                "famsize_childcat", "breastfed_ever", "ethn3_m", "ethn2_m", "preg_smk")
# "preg_smk", 

#"allergy_any_m_noNA", "allergy_any_m", 

# Model1 (adjusted)
for (i in c(2,5,7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                   paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+')))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model1_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}

############################

#meta analyse here
for (i in c("f_infant_dogs1")) {
#for (i in c("f_infant_cats1")) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model1_2$coefficients[coefficient,'Estimate'],
                    model1_5$coefficients[coefficient,'Estimate'],
                    model1_7$coefficients[coefficient,'Estimate'],
                    model1_8$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model1_2$coefficients[coefficient,'Std. Error'], 
                    model1_5$coefficients[coefficient,'Std. Error'],
                    model1_7$coefficients[coefficient,'Std. Error'],
                    model1_8$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 



###Forest plots---------------------------
#DOGS

#Forest plots with Cat sensitisation N(%):
forest(res_f_infant_dogs1, xlim=c(-8,8), at=log(c(0.20, 1, 13.5)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(dogs_model1$PnAcount, " (", dogs_model1$PnAperc, ")"), paste0(dogs_model1$PpAcount, " (", 
                                                                                       dogs_model1$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 1,
       xlab="Adjusted Odds Ratio", refline=log(1), slab=c("INMA", "Raine", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.9, font=2)
text(c(-5.5, -3.5), 5.3, c("No dog", "Dog"))
text(c(-4.5),     6, c("Cat sensitised N (%)"))
text(5.0, 6, "Weight and Odds ratio [95% CI]")
text(-7.2, 6, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.8)
text(-8,-0.4, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_f_infant_dogs1$QE, digits=2, format="f")),  ", df = ", .(res_f_infant_dogs1$k - res_f_infant_dogs1$p),
                                         ", p = ", .(formatC(res_f_infant_dogs1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_f_infant_dogs1$I2, digits = 2, format="f")), "%)")))

###------------------------------------------------------

#Cats


#Forest plots with Sensitisation N(%):
forest(res_f_infant_cats1, xlim=c(-10,6), at=log(c(0.01, 0.25, 1, 4.5)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(cats_model1$PnAcount, " (", cats_model1$PnAperc, ")"), paste0(cats_model1$PpAcount, " (", 
                                                                                       cats_model1$PpAperc, ")")),
       ilab.xpos = c(-7, -5), header = FALSE, cex = 1,
       xlab="Adjusted Odds Ratio", refline=log(1), slab=c("INMA", "Raine", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=1, font=2)
text(c(-7, -5.0), 5.3, c("No cat", "Cat"))
text(c(-6.),     6, c("Cat sensitised N (%)"))
text(3, 6, "Weight and Odds ratio [95% CI]")
text(-9.2, 6, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.8)
text(-10,-0.275, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_f_infant_cats1$QE, digits=2, format="f")),  ", df = ", .(res_f_infant_cats1$k - res_f_infant_cats1$p),
                                           ", p = ", .(formatC(res_f_infant_cats1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                           .(formatC(res_f_infant_cats1$I2, digits = 2, format="f")), "%)")))




#Interaction------------------------------------

#for (coefficient in c("E3$asthma_m1:E3$f_infant_dogs1")) {
for (coefficient in c("E3$asthma_m1:E3$f_infant_cats1")) {
  # coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model0_1$coefficients[coefficient,'Estimate'], 
                    model0_2$coefficients[coefficient,'Estimate'], 
                    model0_3$coefficients[coefficient,'Estimate'], 
                    model0_5$coefficients[coefficient,'Estimate'],
                    model0_6$coefficients[coefficient,'Estimate'],
                    model0_7$coefficients[coefficient,'Estimate'],
                    model0_8$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model0_1$coefficients[coefficient,'Std. Error'], 
                    model0_2$coefficients[coefficient,'Std. Error'], 
                    model0_3$coefficients[coefficient,'Std. Error'], 
                    model0_5$coefficients[coefficient,'Std. Error'],
                    model0_6$coefficients[coefficient,'Std. Error'],
                    model0_7$coefficients[coefficient,'Std. Error'],
                    model0_8$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 

forest(res_8, xlim=c(-8,6), at=log(c(0.01, 1, 10)), atransf=exp, showweights=TRUE,
       header = TRUE, cex = 1,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("DNBC", "INMA", "NINFEA", "Raine", "MoBa", "SWS", "Gen R"), mlab = "")



#for (coefficient in c("E3$asthma_m1:E3$f_infant_dogs1")) {
for (coefficient in c("E3$asthma_m1:E3$f_infant_cats1")) {
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



######################################################################################################
# ___  ___          _      _   _____  
# |  \/  |         | |    | | | ____|
# | .  . | ___   __| | ___| | | |
# | |\/| |/ _ \ / _` |/ _ \ | |_|----  
# | |  | | (_) | (_| |  __/ | _____||
# \_|  |_/\___/ \__,_|\___|_| ______|

# Exposure: cats and dogs (yes/no) in pregnancy
# Outcome: cat sensitisation at around 7 years
# Covariates: age, sex, mother's education, hh income, smoking, mother's asthma, mother's allergy?, 
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
model5 = read.csv(file = 'model5NA_vars.csv', header=TRUE)


none_cc_vars <- model5 %>%
  filter(Include=="0")  %>%
  print()
none_cc_vars = unlist(list(none_cc_vars$Variable))


#Now restrict data frame and select complete cases:
for (i in c(1:length(connections))) {
  my_name = names(connections[i])
  list_variables = variable_creator(single_opal = my_name, filter_df = filter_csv, leave_out = none_cc_vars)
  list_variables <- purrr::map_chr(list_variables, ~ paste0("E1$", .))
  ds.dataFrame(x = list_variables, newobj = "E2", datasources = connections[i])
  ds.completeCases(x1 = "E2", newobj = "E3", datasources = connections[i])
}


length_complete = ds.length("E3$child_id", type = "split", datasources = connections)

#Use dataframe fill to add back excluded variables?

############################################################3
#MODEL

#Crude model

#Build model:
data_table = "E3"


outcome = c('sens_cat7')


exposure = 'dogs_preg'
exposure = 'cats_preg'


#interaction = "asthma_m"

#Model0 (unadjusted)
#for (i in c(1:length(connections)))
for (i in c(2,5,7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table,'$',interaction,'*', data_table,'$', exposure)))))")
  eval(parse(text=to_eval))
  to_eval = paste0("model0_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}


############################

#meta analyse here
for (i in c("dogs_preg1")) {
#for (i in c("cats_preg1")) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model0_2$coefficients[coefficient,'Estimate'], 
                    # model0_4$coefficients[coefficient,'Estimate'], 
                    model0_5$coefficients[coefficient,'Estimate'], 
                    model0_7$coefficients[coefficient,'Estimate'],
                    model0_8$coefficients[coefficient,'Estimate']
  )")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model0_2$coefficients[coefficient,'Std. Error'], 
                    # model0_4$coefficients[coefficient,'Std. Error'], 
                    model0_5$coefficients[coefficient,'Std. Error'], 
                    model0_7$coefficients[coefficient,'Std. Error'],
                    model0_8$coefficients[coefficient,'Std. Error']
  )")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 


###Forest plots---------------------------
#DOGS
# Set studynames and numstudies
temp <- ds.summary('E3$child_id', datasources = connections[c(2,5,7,8)])
study_names <- names(temp)
num_studies <- length(temp)
rm(temp)

#Create a dataframe from 2x2 table which displays Asthma N(%) by cat:
output <- ds.table2D('E3$sens_cat7', 'E3$dogs_preg',  datasources =connections[c(2,5,7,8)])
counts <- data.frame(matrix(unlist(output$counts), nrow = 4, ncol = 9, byrow=T))
perc <- data.frame(matrix(unlist(output$colPercent), nrow = 4, ncol = 9, byrow=T))
counts1 = counts[,c(2)] 
perc1 = perc[,c(2)]
counts2 = counts[,c(5)] 
perc2 = perc[,c(5)]
dogs_model1 <- data.frame(cbind(counts1,perc1,counts2,perc2))
rownames(dogs_model1) <- study_names
colnames(dogs_model1) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
rm(output,counts, perc, counts1,perc1,counts2,perc2)
dogs_model1


#Forest plots with Cat sensitisation N(%):
forest(res_dogs_preg1, xlim=c(-8,8), at=log(c(0.20, 1, 10.5)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(dogs_model1$PnAcount, " (", dogs_model1$PnAperc, ")"), paste0(dogs_model1$PpAcount, " (", 
                                                                                       dogs_model1$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 1,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("INMA", "Raine", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.9, font=2)
text(c(-5.5, -3.5), 5.3, c("No dog", "Dog"))
text(c(-4.5),     6, c("Cat sensitised N (%)"))
text(5.0, 6, "Weight and Odds ratio [95% CI]")
text(-7.2, 6, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.8)
text(-8,-0.4, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_dogs_preg1$QE, digits=2, format="f")),  ", df = ", .(res_dogs_preg1$k - res_dogs_preg1$p),
                                         ", p = ", .(formatC(res_dogs_preg1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_dogs_preg1$I2, digits = 2, format="f")), "%)")))

###------------------------------------------------------

#Cats
#Create a dataframe from 2x2 table which displays Sensitisation N(%) by cat:
output <- ds.table2D('E3$sens_cat7', 'E3$cats_preg',  datasources =connections[c(2,5,7,8)])
counts <- data.frame(matrix(unlist(output$counts), nrow = 4, ncol = 9, byrow=T))
perc <- data.frame(matrix(unlist(output$colPercent), nrow = 4, ncol = 9, byrow=T))
counts1 = counts[,c(2)] 
perc1 = perc[,c(2)]
counts2 = counts[,c(5)] 
perc2 = perc[,c(5)]
cats_model1 <- data.frame(cbind(counts1,perc1,counts2,perc2))
rownames(cats_model1) <- study_names
colnames(cats_model1) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
rm(output,counts, perc, counts1,perc1,counts2,perc2)
cats_model1



#Forest plots with Sensitisation N(%):
res<- forest(res_cats_preg1, xlim=c(-16,10), at=log(c(0.10, 1, 4.5)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(cats_model1$PnAcount, " (", cats_model1$PnAperc, ")"), paste0(cats_model1$PpAcount, " (", 
                                                                                       cats_model1$PpAperc, ")")),
       ilab.xpos = c(-11.5, -8), header = FALSE, cex = 1,
       xlab="Crude Odds Ratio", refline=log(1), slab=c("INMA", "Raine", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.9, font=2)
text(c(-11.5, -8), 5.3, c("No cat", "Cat"))
text(c(-9.5),     6, c("Cat sensitised N (%)"))
text(5, 6, "Weight and Odds ratio [95% CI]")
text(-15.3, 6, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.8)
text(-16.5,-0.275, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_cats_preg1$QE, digits=2, format="f")),  ", df = ", .(res_cats_preg1$k - res_cats_preg1$p),
                                           ", p = ", .(formatC(res_cats_preg1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                           .(formatC(res_cats_preg1$I2, digits = 2, format="f")), "%)")))
#\n 

rm(res_f_infant_cats1)
rm(res_f_infant_dogs1)
rm(model1_2, model1_5, model1_7, model1_8)
rm(model0_2, model0_7, model0_8)
##############################################################################
#Adjusted model:

#Build model:
data_table = "E3"

outcome = c('sens_cat7')

exposure = 'dogs_preg'


#interaction = "asthma_m"

covariates =  c("cats_preg", "asthma_m_noNA", "edu_m_", "income_noNA", "preg_smk", "sex", "cob_m", 
                "sibling_pos2", "agebirth_m_y_c", 
                "ethn3_m", "ethn2_m")

# Model1 (adjusted)
for (i in c(2,5,7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                   paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+')))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model1_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}

############################

#meta analyse here
for (i in c("dogs_preg1")) {
#for (i in c("cats_preg1")) {
  coefficient = paste0(data_table, '$', i)
  to_eval1 = paste0("yi_",i," = c(model1_2$coefficients[coefficient,'Estimate'],
                    model1_5$coefficients[coefficient,'Estimate'],
                    model1_7$coefficients[coefficient,'Estimate'],
                    model1_8$coefficients[coefficient,'Estimate'])")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model1_2$coefficients[coefficient,'Std. Error'], 
                    model1_5$coefficients[coefficient,'Std. Error'],
                    model1_7$coefficients[coefficient,'Std. Error'],
                    model1_8$coefficients[coefficient,'Std. Error'])")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
} 



###Forest plots---------------------------
#DOGS

#Forest plots with Cat sensitisation N(%):
forest(res_dogs_preg1, xlim=c(-8,8), at=log(c(0.20, 1, 10.5)), atransf=exp, showweights=TRUE,
       ilab=cbind(paste0(dogs_model1$PnAcount, " (", dogs_model1$PnAperc, ")"), paste0(dogs_model1$PpAcount, " (", 
                                                                                       dogs_model1$PpAperc, ")")),
       ilab.xpos = c(-5.5, -3.5), header = FALSE, cex = 1,
       xlab="Adjusted Odds Ratio", refline=log(1), slab=c("INMA", "Raine", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.9, font=2)
text(c(-5.5, -3.5), 5.3, c("No dog", "Dog"))
text(c(-4.5),     6, c("Cat sensitised N (%)"))
text(5.0, 6, "Weight and Odds ratio [95% CI]")
text(-7.2, 6, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.8)
text(-8,-0.4, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_dogs_preg1$QE, digits=2, format="f")),  ", df = ", .(res_dogs_preg1$k - res_dogs_preg1$p),
                                         ", p = ", .(formatC(res_dogs_preg1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                         .(formatC(res_dogs_preg1$I2, digits = 2, format="f")), "%)")))

###------------------------------------------------------

#Cats


#Forest plots with Sensitisation N(%):
res<- forest(res_cats_preg1, xlim=c(-16,10), at=log(c(0.10, 1, 4.5)), atransf=exp, showweights=TRUE,
             ilab=cbind(paste0(cats_model1$PnAcount, " (", cats_model1$PnAperc, ")"), paste0(cats_model1$PpAcount, " (", 
                                                                                             cats_model1$PpAperc, ")")),
             ilab.xpos = c(-11.5, -8), header = FALSE, cex = 1,
             xlab="Adjusted Odds Ratio", refline=log(1), slab=c("INMA", "Raine", "SWS", "Gen R"), mlab = "")

#Add labels:
op <- par(cex=0.9, font=2)
text(c(-11.5, -8), 5.3, c("No cat", "Cat"))
text(c(-9.5),     6, c("Cat sensitised N (%)"))
text(5, 6, "Weight and Odds ratio [95% CI]")
text(-17.3, 6, "Cohort")
par(op)
#Add text with Q-value, dfs, p-value and I^2 statistic
op <- par(cex=0.8)
text(-18.5,-0.275, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_cats_preg1$QE, digits=2, format="f")),  ", df = ", .(res_cats_preg1$k - res_cats_preg1$p),
                                              ", p = ", .(formatC(res_cats_preg1$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                              .(formatC(res_cats_preg1$I2, digits = 2, format="f")), "%)")))




 
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

forest(res_sens_cat71, xlab="Crude OR", header =TRUE, transf=exp, refline=1, slab=c("INMA", "Raine", "SWS", "Gen R"))


##########################################################################################################
###########################################################################################################
#Sensitisation and Asthma
# See "tables" for "tables" data frame

#MODEL

#Crude model

#Build model:

data_table ="E1"

outcome = 'medall'


exposure = 'sens_dog7'
exposure = 'sens_cat7'

#interaction = "asthma_m"

#Model0 (unadjusted)
#for (i in c(1:length(connections)))
for (i in c(2,5,7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure)))))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table,'$',interaction,'*', data_table,'$', exposure)))))")
  eval(parse(text=to_eval))
  to_eval = paste0("model0_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}

#Adjusted:

data_table ="E1"

outcome = 'medall'


exposure = 'sens_dog7'
#exposure = 'sens_cat7'

#interaction = "asthma_m"

covariates =  c("asthma_m_noNA", "sex", 
                "csection_noNA", "log_ga", "birth_weight_c",
                "famsize_childcat", "breastfed_ever", "ethn3_m", "ethn2_m", "preg_smk")

# Model1 (adjusted)
for (i in c(7,8)) {
  my_name = names(connections[i])
  exceptions = missing_variable_creator(single_opal = my_name, filter_df = filter_csv)
  #no interaction:
  to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',exposure), 
                   paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+')))")
  #interaction:
  #to_eval = paste0("fmla",i," <- as.formula(paste(data_table, '$', outcome,' ~ ', paste0(c(paste0(data_table, '$',covariates[! covariates %in% exceptions])), collapse= '+'),'+', data_table,'$',interaction,'*', data_table,'$', exposure))")
  eval(parse(text=to_eval))
  to_eval = paste0("model0_",i," = ds.glm(formula = fmla",i,", data = data_table, family = 'binomial', datasources = connections[",i,"])")
  eval(parse(text=to_eval))
}


############################

#meta analyse here

 # for (i in c("sens_dog71")) {
  for (i in c("sens_cat71")) {
    coefficient = paste0(data_table, '$', i)
    to_eval1 = paste0("yi_",i," = c(model0_2$coefficients[coefficient,'Estimate'], 
                      # model0_4$coefficients[coefficient,'Estimate'], 
                     model0_5$coefficients[coefficient,'Estimate'], 
                      model0_7$coefficients[coefficient,'Estimate'],
                      model0_8$coefficients[coefficient,'Estimate']
    )")
  eval(parse(text=to_eval1))
  to_eval2 = paste0("sei_",i," <- c(model0_2$coefficients[coefficient,'Std. Error'], 
                    # model0_4$coefficients[coefficient,'Std. Error'], 
                    model0_5$coefficients[coefficient,'Std. Error'], 
                    model0_7$coefficients[coefficient,'Std. Error'],
                    model0_8$coefficients[coefficient,'Std. Error']
  )")
  eval(parse(text=to_eval2))
  #Random effects model:
  to_eval3 = paste0("res_",i," <- rma(yi_",i,", sei=sei_",i,")")
  eval(parse(text=to_eval3))
  } 
  
  
  ###Forest plots---------------------------
  #DOGS
  # Set studynames and numstudies
  temp <- ds.summary('E1$child_id', datasources = connections[c(7,8)])
  study_names <- names(temp)
  num_studies <- length(temp)
  rm(temp)
  
  #Create a dataframe from 2x2 table which displays Asthma N(%) by cat:
  output <- ds.table2D('E1$medall', 'E1$sens_dog7',  datasources =connections[c(7,8)])
  counts <- data.frame(matrix(unlist(output$counts), nrow = 4, ncol = 9, byrow=T))
  perc <- data.frame(matrix(unlist(output$colPercent), nrow = 4, ncol = 9, byrow=T))
  counts1 = counts[,c(2)] 
  perc1 = perc[,c(2)]
  counts2 = counts[,c(5)] 
  perc2 = perc[,c(5)]
  dogs_model1 <- data.frame(cbind(counts1,perc1,counts2,perc2))
  rownames(dogs_model1) <- study_names
  colnames(dogs_model1) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
  rm(output,counts, perc, counts1,perc1,counts2,perc2)
  dogs_model1
  
  
  #Forest plots with Cat sensitisation N(%):
  forest(res_sens_dog71, xlim=c(-8,8), at=log(c(0.20, 1, 10.5)), atransf=exp, showweights=TRUE,
         ilab=cbind(paste0(dogs_model1$PnAcount, " (", dogs_model1$PnAperc, ")"), paste0(dogs_model1$PpAcount, " (", 
                                                                                         dogs_model1$PpAperc, ")")),
         ilab.xpos = c(-5.2, -2.8), header = FALSE, cex = 1,
         xlab="Crude Odds Ratio", refline=log(1), slab=c("SWS", "Gen R"), mlab = "")

  #Add labels:
  op <- par(cex=0.9, font=2)
  text(c(-5.2, -2.8), 3.1, c("Dog sens-", "Dog sens +"))
  text(c(-4.2),     3.5, c("Asthma N (%)"))
  text(4.5, 3.5, "Weight and Odds ratio [95% CI]")
  text(-7.2, 3.5, "Cohort")
  par(op)
  #Add text with Q-value, dfs, p-value and I^2 statistic
  op <- par(cex=0.8)
  text(-8,-0.4, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_sens_dog71$QE, digits=2, format="f")),  ", df = ", .(res_sens_dog71$k - res_sens_dog71$p),
                                           ", p = ", .(formatC(res_sens_dog71$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                           .(formatC(res_sens_dog71$I2, digits = 2, format="f")), "%)")))
  
  ###------------------------------------------------------
  
  #Cats
  #Create a dataframe from 2x2 table which displays Sensitisation N(%) by cat:
  output <- ds.table2D('E1$medall', 'E1$sens_cat7',  datasources =connections[c(2,5,7,8)])
  counts <- data.frame(matrix(unlist(output$counts), nrow = 4, ncol = 9, byrow=T))
  perc <- data.frame(matrix(unlist(output$colPercent), nrow = 4, ncol = 9, byrow=T))
  counts1 = counts[,c(2)] 
  perc1 = perc[,c(2)]
  counts2 = counts[,c(5)] 
  perc2 = perc[,c(5)]
  cats_model1 <- data.frame(cbind(counts1,perc1,counts2,perc2))
  rownames(cats_model1) <- study_names
  colnames(cats_model1) <- c("PnAcount", "PnAperc", "PpAcount", "PpAperc")
  rm(output,counts, perc, counts1,perc1,counts2,perc2)
  cats_model1
  #Change NAs to <5:
  for (i in c("cats_model1$PnAcount", "cats_model1$PpAcount")) {
    eval(parse(text=(paste0("",i,"[is.na(",i,")]<- '<=13'"))))
  }
  for (i in c("cats_model1$PnAperc", "cats_model1$PpAperc")) {
    eval(parse(text=(paste0("",i,"[is.na(",i,")]<- '.'"))))
  }
  
  
  
  #Forest plots with Sensitisation N(%):
  res<- forest(res_sens_cat71, xlim=c(-16,10), at=log(c(0.05, 1, 11)), atransf=exp, showweights=TRUE,
               ilab=cbind(paste0(cats_model1$PnAcount, " (", cats_model1$PnAperc, ")"), paste0(cats_model1$PpAcount, " (", 
                                                                                               cats_model1$PpAperc, ")")),
               ilab.xpos = c(-11.5, -8), header = FALSE, cex = 1,
               xlab="Crude Odds Ratio", refline=log(1), slab=c("INMA", "Raine", "SWS", "Gen R"), mlab = "")
  
  #Add labels:
  op <- par(cex=0.9, font=2)
  text(c(-11.5, -8), 5.3, c("Cat sens-", "Cat sens+"))
  text(c(-9.5),     6, c("Asthma N (%)"))
  text(5, 6, "Weight and Odds ratio [95% CI]")
  text(-15.0, 6, "Cohort")
  par(op)
  #Add text with Q-value, dfs, p-value and I^2 statistic
  op <- par(cex=0.8)
  text(-15.5,-0.275, pos=4, cex=1, bquote(paste("RE Model for all studies (Q =", .(formatC(res_sens_cat71$QE, digits=2, format="f")),  ", df = ", .(res_sens_cat71$k - res_sens_cat71$p),
                                                ", p = ", .(formatC(res_sens_cat71$QEp, digits = 2, format = "f")), "; ", I^2, " = ", 
                                                .(formatC(res_sens_cat71$I2, digits = 2, format="f")), "%)")))
  #\n 
  
  rm(res_f_infant_cats1)
  rm(res_f_infant_dogs1)
  rm(model1_2, model1_5, model1_7, model1_8)
  rm(model0_2, model0_7, model0_8)
  




