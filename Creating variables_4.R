#Allergen sensitivity://///////////////////////////////////////////

#CAT-------------------------------------------------

#####INMA---------------------------------------------
#Create a measure of allergic sensitisation

#Cat sensitisation at 2-4 years
#2-3 SWS (SPT)
#3-5 INMA (IgE)

#IgE (INMA)
for (i in c(3:5)) {
  to_eval = paste0("ds.Boole(V1 ='D$inh_all_sens_IgE_CAT_.",i,"', V2='0.35', Boolean.operator='>',
                   numeric.output=T, na.assign='NA', newobj='ige_cat_",i,"', datasources = opals)")
  eval(parse(text=to_eval))
}

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.dataFrame(x=c('ige_cat_3', 'ige_cat_4', 'ige_cat_5'), newobj = 'ige_cat', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

ds.rowColCalc(x='ige_cat', operation='rowMeans', newobj='sens_cat4', datasources=opals)


#SPT (SWS)
ds.subset(x='D', subset='spt_cat', cols=c('inh_all_sens_SPT_CAT_.2','inh_all_sens_SPT_CAT_.3'), datasources = opals['sws'])

#Calculate the average across the dataframe:
ds.rowColCalc(x='spt_cat', operation='rowMeans', newobj='sens_cat4', datasources=opals['sws'])

####BOTH IgE AND SPT:

ds.asFactor('sens_cat4', 'sens_cat4', forced.factor.levels=0:1, datasources = opals)

################################################

#Cat sensitisation at 7 years 
#IgE (RAINE AND INMA)
for (i in c(6:8)) {
  to_eval = paste0("ds.Boole(V1 ='D$inh_all_sens_IgE_CAT_.",i,"', V2='0.35', Boolean.operator='>',
                   numeric.output=T, na.assign='NA', newobj='ige_cat_",i,"', datasources = opals)")
  eval(parse(text=to_eval))
}

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.dataFrame(x=c('ige_cat_6', 'ige_cat_7', 'ige_cat_8'), newobj = 'ige_cat', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

ds.rowColCalc(x='ige_cat', operation='rowMeans', newobj='sens_cat7', datasources=opals)

#SPT (SWS)
ds.subset(x='D', subset='spt_cat', cols=c('inh_all_sens_SPT_CAT_.6','inh_all_sens_SPT_CAT_.7',
                                          'inh_all_sens_SPT_CAT_.8','inh_all_sens_SPT_CAT_.9'), datasources = opals['sws'])

#Calculate the average across the dataframe:
ds.rowColCalc(x='spt_cat', operation='rowMeans', newobj='sens_cat7', datasources=opals['sws'])


#convert this to a binary variable: (NOT REQUIRED FOR SWS)
#ds.Boole(V1 = 'spt_cat', V2 = "0", Boolean.operator = ">",
#         numeric.output = TRUE, na.assign = "NA", newobj = "spt_cat",
#         datasources = opals)

#############Repro_pl---------------------------------
#Note: cat-specific spt data temporarily stored under rye-Ige

ds.asFactor('D$inh_all_sens_IgE_RYE_.7', 'sens_cat7', forced.factor.levels=0:1, datasources = opals['repro_pl'])

####BOTH IgE AND SPT:

ds.asFactor('sens_cat7', 'sens_cat7', forced.factor.levels=0:1, datasources = opals)

############Combining---------------------------
#Combine data:
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D', 'sens_cat7', 'sens_cat4'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


ds.table1D("D1$sens_cat7", datasources = opals[c('repro_pl', 'inma', 'raine', 'sws')])



###################### DOG-------------------------
#SPT (SWS)
#2-3 years
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.subset(x='D', subset='spt_dog', cols=c('inh_all_sens_SPT_DOG_.2','inh_all_sens_SPT_DOG_.3'), datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#Calculate the average across the dataframe:
ds.rowColCalc(x='spt_dog', operation='rowMeans', newobj='sens_dog4', datasources=opals)
ds.asFactor('sens_dog4', 'sens_dog4', forced.factor.levels=0:1, datasources = opals)

#SPT @ approx. 7 years (SWS & repro_pl)

#Repro_pl 
#Note: dog-specific spt data temporarily stored under MOULD-IgE

ds.asFactor('D1$inh_all_sens_IgE_MOULD_.7', 'sens_dog7', forced.factor.levels=0:1, datasources = opals)

#SWS
ds.subset(x='D', subset='spt_dog', cols=c('inh_all_sens_SPT_DOG_.6','inh_all_sens_SPT_DOG_.7',
                                          'inh_all_sens_SPT_DOG_.8','inh_all_sens_SPT_DOG_.9'), datasources = opals['sws'])

#Calculate the average across the dataframe:
ds.rowColCalc(x='spt_dog', operation='rowMeans', newobj='sens_dog7', datasources=opals['sws'])
ds.asFactor('sens_dog7', 'sens_dog7', forced.factor.levels=0:1, datasources = opals['sws'])

############Combining---------------------------
#Combine data:

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'sens_dog4', 'sens_dog7'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

ds.table1D("D1$sens_dog7", datasources = opals['sws'])

###################################################################################
##############################Cat in infancy variable (0/1)-------------------------------------  

#Change the cat variables to integers:
for (i in c(0:2)) {
  to_eval = paste0("ds.asInteger(x='D$cats_.",i,"', newobj = 'icats",i,"', datasources = opals)")
  eval(parse(text=to_eval))
}
#check it worked:
ds.class(x='icats1')

#Create a new dataframe:
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.dataFrame(x=c('icats0', 'icats1', 'icats2'), newobj = 'cats', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#Calculate the average across the dataframe:
ds.rowColCalc(x='cats', operation='rowMeans', newobj='infant_cats', datasources=opals)
#convert this to a binary variable:
ds.Boole(V1 = 'infant_cats', V2 = "0", Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "infant_cats",
         datasources = opals)
ds.asFactor('infant_cats', 'f_infant_cats', datasources = opals)

#glue both back on to main dataframe (numeric infant_cats useful later):
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'infant_cats', 'f_infant_cats'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#check it worked:
ds.table1D('D1$f_infant_cats', datasources=opals)

#####################Dogs in infancy variable 0/1-------------------------------
#Change the dog variables to integers:
for (i in c(0:2)) {
  to_eval = paste0("ds.asInteger(x='D$dogs_.",i,"', newobj = 'idogs",i,"', datasources = opals)")
  eval(parse(text=to_eval))
}
#check it worked:
ds.class(x='idogs0')

#Create a new dataframe:
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.dataFrame(x=c('idogs0', 'idogs1', 'idogs2'), newobj = 'dogs', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#Calculate the average across the dataframe:
ds.rowColCalc(x='dogs', operation='rowMeans', newobj='infant_dogs', datasources=opals)
#convert this to a binary variable:
ds.Boole(V1 = 'infant_dogs', V2 = "0", Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "infant_dogs",
         datasources = opals)
ds.asFactor('infant_dogs', 'f_infant_dogs', datasources = opals)

#glue both back on to main dataframe (numeric infant_dogs useful later):
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'infant_dogs', 'f_infant_dogs'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#check it worked:
ds.table1D('D1$f_infant_dogs', datasources=opals)

######################## Cats pregnancy OR infancy----------------------------------
#first need to change pet exposures to integers: 
ds.asInteger('D1$cats_preg', 'i_cats_preg', datasources = opals)
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'i_cats_preg'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#Now create the summary measure:
ds.subset(x='D1', subset = 'summary_c', cols = c("i_cats_preg",  "infant_cats"), datasources = opals)
ds.rowColCalc(x='summary_c', operation='rowMeans', newobj='cats', datasources=opals)
#Recode so that it's a binary variable:
ds.Boole(V1 = 'cats', V2 = "0", Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "cats",
         datasources = opals)
ds.asFactor('cats', 'cats', datasources = opals)
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'cats'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

ds.table1D('D1$cats', datasources=opals)
ds.table2D("D1$cats_preg",  "D1$f_infant_cats",datasources=opals)
ds.table2D("D1$cats_preg",  "D1$cats",datasources=opals)
ds.table2D("D1$cats",  "D1$f_infant_cats",datasources=opals)


######################## dogs pregnancy OR infancy----------------------------------
#first need to change pet exposures to integers: 
ds.asInteger('D1$dogs_preg', 'i_dogs_preg', datasources = opals)
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'i_dogs_preg'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#Now create the summary measure:
ds.subset(x='D1', subset = 'summary_c', cols = c("i_dogs_preg",  "infant_dogs"), datasources = opals)
ds.rowColCalc(x='summary_c', operation='rowMeans', newobj='dogs', datasources=opals)
#Recode so that it's a binary variable:
ds.Boole(V1 = 'dogs', V2 = "0", Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "dogs",
         datasources = opals)
ds.asFactor('dogs', 'dogs', datasources = opals)
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'dogs'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

ds.table1D('D1$dogs', datasources=opals)
ds.table2D("D1$dogs_preg",  "D1$f_infant_dogs",datasources=opals)
ds.table2D("D1$dogs_preg",  "D1$dogs",datasources=opals)
ds.table2D("D1$dogs",  "D1$f_infant_dogs",datasources=opals)


#############################Cats in pregnancy AND infancy-------------------------------------
ds.recodeValues(var.name = "D1$i_cats_preg", values2replace.vector = c(0,1),
                new.values.vector = c(0,2), force.output.format = "no",
                newobj = "i_cats_preg2", datasources = opals, notify.of.progress = FALSE)
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'i_cats_preg2'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}
ds.table1D('D1$i_cats_preg2', datasources=opals)


ds.vectorCalc(x = c("D1$i_cats_preg2", "D1$infant_cats" ), calc = "+", newobj = "early_life_cats",
              datasources = opals)
ds.asFactor('early_life_cats', 'early_life_cats', datasources = opals)
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'early_life_cats'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

ds.table1D('D1$early_life_cats', datasources=opals)
ds.table2D("D1$cats_preg", "D1$f_infant_cats" ,datasources=opals)


#############################Dogs in pregnancy AND infancy------------------------------------
ds.recodeValues(var.name = "D1$i_dogs_preg", values2replace.vector = c(0,1),
                new.values.vector = c(0,2), force.output.format = "no",
                newobj = "i_dogs_preg2", datasources = opals, notify.of.progress = FALSE)
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'i_dogs_preg2'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}
ds.table1D('D1$i_dogs_preg2', datasources=opals)


ds.vectorCalc(x = c("D1$i_dogs_preg2", "D1$infant_dogs" ), calc = "+", newobj = "early_life_dogs",
              datasources = opals)
ds.asFactor('early_life_dogs', 'early_life_dogs', datasources = opals)
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'early_life_dogs'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

ds.table1D('D1$early_life_dogs', datasources=opals)
ds.table2D("D1$dogs_preg", "D1$f_infant_dogs" ,datasources=opals)


#####################################Dogs OR cats infancy   ################
ds.table1D("D1$infant_dogs", datasources = opals)
ds.table1D("D1$infant_cats", datasources = opals)

#cats vs. dogs:
ds.recodeValues(var.name = "D1$infant_cats", values2replace.vector = c(0,1),
                new.values.vector = c(0,2), force.output.format = "no",
                newobj = "infant_cats2", datasources = opals, notify.of.progress = FALSE)
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'infant_cats2'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

ds.table1D('D1$infant_cats2', datasources=opals)

ds.vectorCalc(x = c("D1$infant_cats2", "D1$infant_dogs" ), calc = "+", newobj = "infant_dogs_cats",
              datasources = opals)
ds.asFactor('infant_dogs_cats', 'infant_dogs_cats', datasources = opals)

#cats or dogs:
ds.recodeLevels(x = "infant_dogs_cats", newCategories=c('0','1','1','1'),
                newobj = "infant_dogs_cats2", datasources = opals)

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'infant_dogs_cats2', 'infant_dogs_cats'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


ds.table1D('D1$infant_dogs_cats', datasources=opals)
ds.table2D("D1$infant_dogs_cats", "D1$f_infant_cats" ,datasources=opals)

###########################Dogs or cats pregnancy########################
#cats vs. dogs:
ds.vectorCalc(x = c("D1$i_cats_preg2", "D1$i_dogs_preg" ), calc = "+", newobj = "preg_dogs_cats",
              datasources = opals)
ds.asFactor('preg_dogs_cats', 'preg_dogs_cats', datasources = opals)

#cats or dogs:
ds.recodeLevels(x = "preg_dogs_cats", newCategories=c('0','1','1','1'),
                newobj = "preg_dogs_cats2", datasources = opals)

ds.asFactor('preg_dogs_cats2', 'preg_dogs_cats2', datasources = opals)

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'preg_dogs_cats2', 'preg_dogs_cats'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


####################################Number of pets###############################
####Average number of cats in infancy#####

#Subset data to variables needed:
ds.subset(x='D1', subset='avcats', cols=c('cats_quant_.0', 'cats_quant_.1', 'cats_quant_.2'), datasources = opals)

#Calculate the average across the dataframe:
ds.rowColCalc(x='avcats', operation='rowMeans', newobj='cats_quant_inf', datasources=opals)

#number of cats squared
ds.assign(toAssign="cats_quant_inf^2", newobj='cats_quant_inf2', datasources=opals)


#Create three new binary variable:
ds.Boole(V1 = 'cats_quant_inf', V2 = 0, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "cats1",
         datasources = opals[c(2,3,5)])
ds.Boole(V1 = 'cats_quant_inf', V2 = 1, Boolean.operator = ">", #or 0.99 for transient cats
         numeric.output = TRUE, na.assign = "NA", newobj = "cats2",
         datasources = opals[c(2,3,5)])
#ds.Boole(V1 = 'cats_quant_inf', V2 = 1.5, Boolean.operator = ">",
#         numeric.output = TRUE, na.assign = "NA", newobj = "cats3",
#         datasources = opals[c(2,3,5)])

ds.vectorCalc(x = c("cats1", "cats2"), calc = "+", newobj = "cats_quant_inf_cat",
              datasources = opals[c(2,3,5)])
ds.asFactor("cats_quant_inf_cat","cats_quant_inf_cat", datasources = opals[c(2,3,5)]) 



####Average number of dogs in infancy#####

#Subset data to variables needed:
ds.subset(x='D1', subset='avdogs', cols=c('dogs_quant_.0', 'dogs_quant_.1', 'dogs_quant_.2'), datasources = opals)

#Calculate the average across the dataframe:
ds.rowColCalc(x='avdogs', operation='rowMeans', newobj='dogs_quant_inf', datasources=opals)

#number of dogs squared
ds.assign(toAssign="dogs_quant_inf^2", newobj='dogs_quant_inf2', datasources=opals)


#Create three new binary variable:
ds.Boole(V1 = 'dogs_quant_inf', V2 = 0, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "dogs1",
         datasources = opals[c(2,3,5)])
ds.Boole(V1 = 'dogs_quant_inf', V2 = 1, Boolean.operator = ">", #or 0.99 for transient dogs
         numeric.output = TRUE, na.assign = "NA", newobj = "dogs2",
         datasources = opals[c(2,3,5)])
#ds.Boole(V1 = 'dogs_quant_inf', V2 = 1.5, Boolean.operator = ">",
#         numeric.output = TRUE, na.assign = "NA", newobj = "dogs3",
#         datasources = opals[c(2,3,5)])

ds.vectorCalc(x = c("dogs1", "dogs2"), calc = "+", newobj = "dogs_quant_inf_cat",
              datasources = opals[c(2,3,5)])
ds.asFactor("dogs_quant_inf_cat","dogs_quant_inf_cat", datasources = opals[c(2,3,5)]) 

#total number of cats and dogs combined (add this to before reshaping):
#
#
#Subset data to variables needed:
ds.subset(x='D1', subset='avpets', cols=c('pets_quant.0', 'pets_quant.1', 'pets_quant.2'), datasources = opals)

#Calculate the average across the dataframe:
ds.rowColCalc(x='avpets', operation='rowMeans', newobj='pets_quant_inf', datasources=opals)

#number of pets squared
ds.assign(toAssign="pets_quant_inf^2", newobj='pets_quant_inf2', datasources=opals)


#Create three new binary variable:
ds.Boole(V1 = 'pets_quant_inf', V2 = 0, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "pets1",
         datasources = opals[c(2,3,5)])
ds.Boole(V1 = 'pets_quant_inf', V2 = 1, Boolean.operator = ">", #or 0.99 for transient pets
         numeric.output = TRUE, na.assign = "NA", newobj = "pets2",
         datasources = opals[c(2,3,5)])
#ds.Boole(V1 = 'pets_quant_inf', V2 = 1.5, Boolean.operator = ">",
#         numeric.output = TRUE, na.assign = "NA", newobj = "pets3",
#         datasources = opals[c(2,3,5)])

ds.vectorCalc(x = c("pets1", "pets2"), calc = "+", newobj = "pets_quant_inf_cat",
              datasources = opals[c(2,3,5)])
ds.asFactor("pets_quant_inf_cat","pets_quant_inf_cat", datasources = opals[c(2,3,5)]) 

#####Stick new variables onto dataframe D1:


for (i in c(2,3,5)) {
  to_eval = paste0("ds.cbind(x=c('D1', 'dogs_quant_inf', 'dogs_quant_inf_cat', 'cats_quant_inf', 'cats_quant_inf_cat', 
                   'dogs_quant_inf2', 'cats_quant_inf2', 'pets_quant_inf', 'pets_quant_inf_cat', 'pets_quant_inf2'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


################################################################################
#Number of cats in pregnancy - quadratic variable and categorical variable

#number of cats squared
ds.assign(toAssign="(D1$cats_quant_preg)^2", newobj='cats_quant_preg2', datasources=opals)


#Create three new binary variable:
ds.Boole(V1 = 'D1$cats_quant_preg', V2 = 0, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "cats1",
         datasources = opals[c(2,3,5,7)])
ds.Boole(V1 = 'D1$cats_quant_preg', V2 = 1, Boolean.operator = ">", #or 0.99 for transient cats
         numeric.output = TRUE, na.assign = "NA", newobj = "cats2",
         datasources = opals[c(2,3,5,7)])

ds.vectorCalc(x = c("cats1", "cats2"), calc = "+", newobj = "cats_quant_preg_cat",
              datasources = opals[c(2,3,5,7)])
ds.asFactor("cats_quant_preg_cat","cats_quant_preg_cat", datasources = opals[c(2,3,5,7)]) 

###############################################
#Number of dogs in pregnancy - quadratic variable and categorical variable

#number of dogs squared
ds.assign(toAssign="(D1$dogs_quant_preg)^2", newobj='dogs_quant_preg2', datasources=opals)


#Create three new binary variable:
ds.Boole(V1 = 'D1$dogs_quant_preg', V2 = 0, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "dogs1",
         datasources = opals[c(2,3,5,7)])
ds.Boole(V1 = 'D1$dogs_quant_preg', V2 = 1, Boolean.operator = ">", #or 0.99 for transient cats
         numeric.output = TRUE, na.assign = "NA", newobj = "dogs2",
         datasources = opals[c(2,3,5,7)])

ds.vectorCalc(x = c("dogs1", "dogs2"), calc = "+", newobj = "dogs_quant_preg_cat",
              datasources = opals[c(2,3,5,7)])
ds.asFactor("dogs_quant_preg_cat","dogs_quant_preg_cat", datasources = opals[c(2,3,5,7)]) 


##########################################
#Cats and dogs in pregnancy

ds.vectorCalc(x = c("D1$cats_quant_preg", "D1$dogs_quant_preg"), calc = "+", newobj = "pets_quant_preg",
              datasources = opals[c(2,3,5,7)])

#number of pets squared
ds.assign(toAssign="pets_quant_preg^2", newobj='pets_quant_preg2', datasources=opals[c(2,3,5,7)])


#Create three new binary variable:
ds.Boole(V1 = 'pets_quant_preg', V2 = 0, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "pets1",
         datasources = opals[c(2,3,5,7)])
ds.Boole(V1 = 'pets_quant_preg', V2 = 1, Boolean.operator = ">", #or 0.99 for transient pets
         numeric.output = TRUE, na.assign = "NA", newobj = "pets2",
         datasources = opals[c(2,3,5,7)])
#ds.Boole(V1 = 'pets_quant_inf', V2 = 1.5, Boolean.operator = ">",
#         numeric.output = TRUE, na.assign = "NA", newobj = "pets3",
#         datasources = opals[c(2,3,5,7)])

ds.vectorCalc(x = c("pets1", "pets2"), calc = "+", newobj = "pets_quant_preg_cat",
              datasources = opals[c(2,3,5,7)])
ds.asFactor("pets_quant_preg_cat","pets_quant_preg_cat", datasources = opals[c(2,3,5,7)]) 

#####Stick new variables onto dataframe D1:


for (i in c(2,3,5,7)) {
  to_eval = paste0("ds.cbind(x=c('D1', 'dogs_quant_preg_cat', 'cats_quant_preg_cat', 
                   'dogs_quant_preg2', 'cats_quant_preg2', 'pets_quant_preg', 'pets_quant_preg_cat', 'pets_quant_preg2'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}



  ########################################################################################
#ASTHMA OUTCOMES --------------------------------------------------
#------------------------------------------------------------------

#Current asthma at 7 years
#ISAAC
#INMA - take average of years 6-8
#NINFEA - 7 years
#Raine - 8 years
#SWS - no ISAAC data, only MeDALL

ds.table2D('D$asthma_current_ISAAC_.6', 'D$asthma_current_ISAAC_.7', datasources = opals['inma'])

for (i in c(6:8)) {
  to_eval = paste0("ds.asInteger(x='D$asthma_current_ISAAC_.",i,"', newobj = 'iisaac",i,"', datasources = opals)")
  eval(parse(text=to_eval)) 
}


#check it worked:
ds.table1D(x='iisaac7', datasources = opals['inma'])

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.dataFrame(x=c('iisaac6', 'iisaac7', 'iisaac8'), newobj = 'isaac', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}
ds.rowColCalc(x='isaac', operation='rowMeans', newobj='isaac', datasources=opals)
ds.table1D('isaac', datasources = opals['ninfea'])
ds.Boole(V1 = 'isaac', V2 = "0", Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "isaac",
         datasources = opals)
ds.asFactor('isaac', 'isaac', datasources = opals)
#Create the object in the remaining cohorts without repeated measures:
ds.assign(toAssign='D1$asthma_current_ISAAC', newobj='isaac', datasources = opals[c('dnbc', 'repro_pl')])
#ds.assign(toAssign='D1$asthma_current_MeDALL_.7', newobj='isaac', datasources = opals[c('moba')])


#####
#MeDALL
#INMA - take average of years 6-8
#NINFEA - 7 years
#Raine - 8 years
#SWS - no ISAAC data, only MeDALL

ds.table2D('D$asthma_current_MeDALL_.6', 'D$asthma_current_MeDALL_.7', datasources = opals['inma']) # could also incorporate 9 year data

for (i in c(6:8)) {
  to_eval = paste0("ds.asInteger(x='D$asthma_current_MeDALL_.",i,"', newobj = 'medall",i,"', datasources = opals)")
  eval(parse(text=to_eval)) 
}


#check it worked:
ds.table1D(x='medall7', datasources = opals['inma'])

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.dataFrame(x=c('medall6', 'medall7', 'medall8'), newobj = 'medall', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}
ds.rowColCalc(x='medall', operation='rowMeans', newobj='medall', datasources=opals)
ds.table1D('medall', datasources = opals['ninfea'])
ds.Boole(V1 = 'medall', V2 = "0", Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "medall",
         datasources = opals)
ds.asFactor('medall', 'medall', datasources = opals)
#Create the object in the remaining cohorts without repeated measures:
ds.assign(toAssign='D1$asthma_current_MeDALL', newobj='medall', datasources = opals[c('dnbc', 'repro_pl')])
ds.assign(toAssign='D1$asthma_current_MeDALL_.7', newobj='medall', datasources = opals[c('moba')])


for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'isaac', 'medall'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

ds.table1D('D1$isaac', datasources=opals)


ds.table1D('D1$medall', datasources=opals)



#####################################################################################################
#COVARIATES


#Mother's age
ds.histogram("D1$agebirth_m_y", datasources = opals)
mean_cen = ds.mean(x='D1$agebirth_m_y', type='c', datasources = opals)
mean = matrix(unlist(mean_cen$Global.Mean), nrow = 1, ncol=1, byrow=TRUE)
my_str = paste0('D1$agebirth_m_y-', mean)
ds.assign(toAssign=my_str, newobj='agebirth_m_y_c',datasources = opals)

#mother's age squared
ds.assign(toAssign="D1$agebirth_m_y^2", newobj='agebirth_m_y2', datasources=opals)


for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'agebirth_m_y_c', 'agebirth_m_y2'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


#Breastfed_any - categorise the variable
#Create a new variable with three categories: never, <6 months, >=6 months:
ds.Boole(V1 ='D1$breastfed_any', V2='6', Boolean.operator='>=',
         numeric.output=T, na.assign='NA', newobj='breastfed6m', datasources = opals)
ds.asNumeric("D1$breastfed_ever","bfever_n", datasources = opals)
#Add these up to create the new categorical variable
ds.vectorCalc(x = c("bfever_n", "breastfed6m" ), calc = "+", newobj = "breastfedcat",
              datasources = opals)
ds.asFactor("breastfedcat", "breastfedcat", baseline.level = 2) #convert to a factor variable

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'breastfedcat'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}
ds.table1D('D1$breastfedcat', datasources=opals)


#mode of delivery - csection
ds.asFactor("D1$mode_delivery", "mode_delivery")
ds.recodeLevels(x="mode_delivery", newCategories = c('0','0','1','1','1'), newobj = 'csection')
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'csection'), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

#birth weight

#mean centre birth weight:
mean_cen = ds.mean(x='D1$birth_weight', type='c', datasources = opals)
mean = matrix(unlist(mean_cen$Global.Mean), nrow = 1, ncol=1, byrow=TRUE)
my_str = paste0('D1$birth_weight-', mean)
ds.assign(toAssign=my_str, newobj='birth_weight_c',datasources = opals)
ds.histogram('birth_weight_c')

#birth weight squared

ds.assign(toAssign="D1$birth_weight^2", newobj='birth_weight2', datasources=opals)

#GA_BJ
#mean-centre GA:
mean_cen = ds.mean(x='D1$ga_bj', type='c', datasources = opals[c(1,2,3,4,5,7)])
mean = matrix(unlist(mean_cen$Global.Mean), nrow = 1, ncol=1, byrow=TRUE)
my_str = paste0('D1$ga_bj-', mean)
ds.assign(toAssign=my_str, newobj='ga_c',datasources = opals)
#For MoBa use ga_lmp (no ga_bj):
my_str = paste0('D1$ga_lmp-', mean)
ds.assign(toAssign=my_str, newobj='ga_c',datasources = opals['moba'])


ds.histogram('ga_c', datasources = opals)

#GA (all)
ds.assign(toAssign="D1$ga_lmp", newobj='ga',datasources = opals['moba'])
ds.assign(toAssign="D1$ga_bj", newobj='ga',datasources = opals[c(1,2,3,4,5,7)])

#GA2
ds.assign(toAssign="ga^2", newobj='ga2', datasources=opals)

#log GA
ds.log(x = "ga", newobj = "log_ga", datasources = opals)


for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'ga_c', 'ga', 'ga2', 'birth_weight_c', 'birth_weight2', 'log_ga' ), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}



###

#Sibling position

ds.asFactor("D1$sibling_pos", "sibling_pos")
#INMA and NINFEA have small cells, so need to recode using ds.recodeValues initially, otherwise recode fails:
ds.recodeValues(var.name = "sibling_pos", values2replace.vector = c(1,2,3,4,5),
                new.values.vector = c(1,2,3,3,3), force.output.format = "no",
                newobj = "sibling_pos", datasources = opals, notify.of.progress = FALSE)
#recode levels to get rid of empty categories:
ds.recodeLevels(x = "sibling_pos", newCategories = c(1,2,3,3,3), newobj = "sibling_pos2", datasources = opals)

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'sibling_pos2' ), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


###

#Family size

#Raine and repro_pl only have data on number of children (raine only at time 0)
#Repro_pl have very few children with data, so may need to be dropped from their model
#MoBa do not have data on family size

#create a subset in cohorts:
ds.subset(x='D1', subset = 'summary_c', cols = c("famsize_child.0",  "famsize_child.1", "famsize_child.2"), datasources = opals)
#INMA have not included the index child in their famsize_child variable
for (i in c(0:2)) {
  to_eval = paste0("ds.assign(toAssign='D1$famsize_child.",i,"+1', newobj='famsize_child.",i,"', datasources = opals['inma'])")
  eval(parse(text=to_eval))
}
ds.dataFrame(x=c("famsize_child.0",  "famsize_child.1", "famsize_child.2"), newobj = 'summary_c', datasources = opals['inma'])

ds.rowColCalc(x='summary_c', operation='rowMeans', newobj='famsize_child', datasources=opals)
#recode famsize:
output <- ds.quantileMean('famsize_child', datasources = opals[c(1,2,3,4,5,7)]) #moba missing variable
quant <- matrix(unlist(output), nrow = 1, ncol = 8, byrow=T)
q25 = round(quant[1,3])
q75 = round(quant[1,5]) 

#Create three new binary variable:
ds.Boole(V1 = 'famsize_child', V2 = q25, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "fam1",
         datasources = opals)
ds.Boole(V1 = 'famsize_child', V2 = q75, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "fam2",
         datasources = opals)
ds.vectorCalc(x = c("fam1", "fam2" ), calc = "+", newobj = "famsize_childcat",
              datasources = opals)
ds.asFactor("famsize_childcat","famsize_childcat")

#Log-transform family size:
#ds.log(x = 'famsize_child', newobj = 'logfamsize_child', datasources = opals)

for (i in c(1:length(opals))) {
  to_eval = paste0("ds.cbind(x=c('D1', 'famsize_child','famsize_childcat' ), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


#DNBC, INMA, NINFEA & SWS (adults too)
#take the average family size over the first 2 years of life:
#Now create the summary measure:
#INMA have not included the index child in their famsize_child variable
for (i in c(0:2)) {
  to_eval = paste0("ds.assign(toAssign='D1$famsize.",i,"+1', newobj='famsize.",i,"', datasources = opals['inma'])")
  eval(parse(text=to_eval))
}
ds.dataFrame(x=c("famsize.0",  "famsize.1", "famsize.2"), newobj = 'summary_f', datasources = opals['inma'])
ds.subset(x='D1', subset = 'summary_f', cols = c("famsize.0",  "famsize.1", "famsize.2"), datasources = opals[c('dnbc','ninfea','sws')])
ds.rowColCalc(x='summary_f', operation='rowMeans', newobj='famsize', datasources=opals[c(1,2,3,7)])
#recode famsize:
output <- ds.quantileMean('famsize', datasources = opals[c(1,2,3,7)])
quant <- matrix(unlist(output), nrow = 1, ncol = 8, byrow=T)
q25 = round(quant[1,3])
q75 = round(quant[1,5]) 

#Create three new binary variable:
ds.Boole(V1 = 'famsize', V2 = q25, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "fam1",
         datasources = opals[c(1,2,3,7)])
ds.Boole(V1 = 'famsize', V2 = q75, Boolean.operator = ">",
         numeric.output = TRUE, na.assign = "NA", newobj = "fam2",
         datasources = opals[c(1,2,3,7)])
ds.vectorCalc(x = c("fam1", "fam2" ), calc = "+", newobj = "famsizecat",
              datasources = opals[c(1,2,3,7)])
ds.asFactor("famsizecat","famsizecat", datasources = opals[c(1,2,3,7)]) 

#Log-transform family size:
#ds.log(x = 'famsize', newobj = 'logfamsize', datasources = opals)

for (i in c(1,2,3,7)) {
  to_eval = paste0("ds.cbind(x=c('D1', 'famsize','famsizecat' ), newobj = 'D1', datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}


##############################################################
##############################################################
#Hybrid variables:

#HH income
for (i in c(1,2,3,7)) {
  output <- ds.quantileMean('D1$eusilc_income', datasources = opals[i])
  quant <- matrix(unlist(output), nrow = 1, ncol = 8, byrow=T)
  q25 = quant[1,3]
  q50 = quant[1,4]
  q75 = quant[1,5]
  ds.Boole(V1 = 'D1$eusilc_income', V2 = q25, Boolean.operator = '>',
           numeric.output = TRUE, na.assign = 'NA', newobj = 'inc1',
           datasources = opals[i])
  ds.Boole(V1 = 'D1$eusilc_income', V2 = q50, Boolean.operator = '>',
           numeric.output = TRUE, na.assign = 'NA', newobj = 'inc2',
           datasources = opals[i])
  ds.Boole(V1 = 'D1$eusilc_income', V2 = q75, Boolean.operator = '>',
           numeric.output = TRUE, na.assign = 'NA', newobj = 'inc3',
           datasources = opals[i])
  ds.vectorCalc(x = c('inc1', 'inc2', 'inc3' ), calc = '+', newobj = 'income',
                datasources = opals[i])
  ds.asFactor("income","income", datasources = opals[i])
}
ds.recodeLevels(x="income", newCategories = c('1','2','3','4'), newobj = 'income', datasources = opals[c(1,2,3,7)])

#Use hh_income variable for Raine (swap round categories):
ds.recodeLevels(x="D1$hhincome_", newCategories = c('4','3','2','1'), newobj = 'income', datasources = opals['raine'])
ds.asInteger('income', 'income', datasources = opals['raine'])
ds.asFactor('income', 'income', datasources = opals['raine'])

#_Use EUSILC variable (quintiles for MoBa for now - check with Johanna why only 4 categories)
ds.assign(toAssign="D1$eusilc_income_quintiles", newobj='income', datasources=opals['moba'])
ds.recodeLevels(x="income", newCategories = c('1','2','3','4','4'), newobj = 'income', datasources = opals['moba'])


for (i in c(1,2,3,5,6,7)) {
  ds.cbind(x=c('D1', 'income'), newobj = 'D1', datasources = opals[i])
}



##############################################################
##############################################################

ds.dataFrameFill(df.name="D1", newobj="D1", datasources = opals)

#Create a new dataframe with a more restricted set of variables:
#first list the variables to keep:
D2_variables =c("child_id", "edu_m_", "hhincome_", "age_years", "mother_id", "preg_no", "child_no", "cohort_id", "cob_m", "ethn1_m", 
                "ethn2_m", "ethn3_m", "agebirth_m_y", "asthma_m", "preg_smk", "preg_cig", "mode_delivery", "asthma_bf", 
                "sex", "plurality", "ga_lmp", "ga_bj", "birth_weight", "sibling_pos", "breastfed_excl", "breastfed_any", "breastfed_ever", "eusilc_income", 
                "eusilc_income_quintiles", "cats_preg", "cats_quant_preg", "dogs_preg", "dogs_quant_preg", "allergy_inh_m", 
                "allergy_any_m", "asthma_ever_CHICOS", "pets_preg", "sens_cat7", "sens_cat4", "sens_dog4", "sens_dog7", "f_infant_cats", 
                "f_infant_dogs", "cats", "dogs", "early_life_cats", "early_life_dogs", "infant_dogs_cats", "infant_dogs_cats2", "preg_dogs_cats2", 
                "preg_dogs_cats", "dogs_quant_preg_cat", "cats_quant_preg_cat", "dogs_quant_preg2", "cats_quant_preg2", "pets_quant_preg", 
                "pets_quant_preg_cat", "pets_quant_preg2", "isaac", "medall", "agebirth_m_y_c", "agebirth_m_y2", "breastfedcat", "csection", "ga_c", "ga", "ga2", 
                "birth_weight_c", "birth_weight2", "log_ga", "sibling_pos2", "famsize_child", "famsize_childcat", "income", "dogs_quant_inf", 
                "dogs_quant_inf_cat", "cats_quant_inf", "cats_quant_inf_cat", "dogs_quant_inf2", "cats_quant_inf2", "pets_quant_inf", "pets_quant_inf_cat", "pets_quant_inf2")
                     
for (i in c(1:length(opals))) {
  to_eval = paste0("ds.subset(x='D1', subset = 'D2', cols =D2_variables, datasources = opals[",i,"])")
  eval(parse(text=to_eval))
}

  
#Now clean up workspace:
df <- list( "agebirth_m_y_c", "avcats", "avdogs", "bf", "bfever_n", "birth_weight_c", 
       "breastfed6m", "breastfedcat", "cats", "cats_quant_inf", "complete", 
       "dnbc", "dogs", "dogs_quant_inf", "early_life_cats", "early_life_dogs",
       "f_infant_cats", "f_infant_dogs", "fam1", "fam2", "famsize", "famsize_child", 
       "famsize_childcat", "fdogs_preg", "ga_bj_c", "ga_c", "hhsize", "hhsize_wide", 
       "i_asthma_ever", "i_cats_preg", "i_cats_preg2", "i_dogs_preg", "i_dogs_preg2", "i_isaac",
       "i_scat", "i_sdog", "icats0", "icats1", "icats2", "idogs0", 
       "idogs1", "idogs2", "ige_cat", "ige_cat_3", "ige_cat_4", "ige_cat_5",
       "ige_cat_6", "ige_cat_7", "ige_cat_8", "iisaac6", "iisaac7", "iisaac8",
       "infant_cats", "infant_cats2", "infant_dogs", "infant_dogs_cats", "inma", "isaac", 
       "ninfea", "outcome_wide", "pets", "pets_wide", "raine", "sens_cat4", 
       "sens_cat7", "sens_dog7", "ses", "sibling_pos2", "birth_weight2", "cats_quant_inf2", "dogs_quant_inf2", "ga2", "ga", 
       "sum_exposure", "sum_outcome", "summary_c", "summary_e", "summary_o", "table1", "agebirth_m_y2",
       "cats_quant_inf_cat", "cats1", "cats2", "csection", "dogs_quant_inf_cat", "dogs1", "dogs2", "csection",
       "summary_f", "famsize_child.0", "famsize_child.1", "famsize_child.2", "famsize.0",
       "famsize.1", "famsize.2", "famsizecat", "pets_quant", "pets_quant_inf", "pets_quant_inf2",
       "income", "avpets", "cats_quant_preg2", "cats_quant_preg_cat", "dogs_quant_preg2", "dogs_quant_preg_cat", 
       "inc1", "inc2", "inc3", "infant_dogs_cats2", "log_ga", 
       "medall", "medall6", "medall7", "medall8", "mode_delivery", 
       "pets1", "pets2", "pets_quant_preg", "pets_quant_preg2", "pets_quant_preg_cat", "preg_dogs_cats",
       "preg_dogs_cats2", "sens_dog4", "sibling_pos", "spt_cat", "spt_dog")



lapply(df, function(x){
    ds.rm(x.name = x)
})

#save workspace:
#datashield.workspace_save(opals, 'workspace3')
#datashield.workspace_save(opals, 'aug7_sws')
