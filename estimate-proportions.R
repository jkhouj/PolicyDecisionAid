
#2022 v. of this script originally created by Mark Gibson and adapted by Jasmine Khouja to estimate 
#proportions to be entered into a policy decision making aid. 
#2023 v. of this script is an adapted version of 2022 v. by Jenn Ferrar
#2024 v. of this script is an adapted version of 2023 v. by Jasmine Khouja

#The aid provides guidance for policy makers considering banning flavours in disposable e-cigarettes in order to prevent youth uptake.

#This script uses data from Action on Smoking and Health (ASH).
#ASH conduct a yearly youth (11-18 years) and adult (18+ years) survey. 
#The year and sample used are indicated in the variable names.
#For example, ASH_Y24 refers to youth data collected in 2024, ASH_A24 refers to ASH data collected from adults in 2024.

#This script uses data from the Smoking Toolkit Study (STS).
#STS collect data from 16+ years. 
#For the purposes of this estimation, we split the data by 16-20 and 21+ years.
#The year and sample used are indicated in the variable names.
#For example, STS_Y23 refers to youth data collected in 2023, 
#STS_A23 refers to adult data collected in 2023. 

#The function created allows for stratification by "grade" which refers to social grade

#The sections denoted in this script by capital and lower case letters A-D refer to the sections which can be found on the decision making aid output.


##################################
# INSTALL PACKAGES AND READ DATA #
##################################

# Install.packages("read.spss")
library(foreign)
library(dplyr)

# Set directory
setwd("Z:/2. Current project folders (unrestricted)/2023_PDA_JK MG KDL/3. Data and Scripts/Raw data analysis")

# Read in Data 

# =================================================================   2023   =================================================================
STS_23_JN<-read.spss("STS23Wave200.sav", use.value.labels = TRUE, to.data.frame = TRUE)
STS_23_JL<-read.spss("STS23Wave201.sav", use.value.labels = TRUE, to.data.frame = TRUE)
STS_23_AU<-read.spss("STS23Wave202.sav", use.value.labels = TRUE, to.data.frame = TRUE)
STS_23_SP<-read.spss("STS23Wave203.sav", use.value.labels = TRUE, to.data.frame = TRUE)
STS_23_OC<-read.spss("STS23Wave204.sav", use.value.labels = TRUE, to.data.frame = TRUE)
STS_23<-bind_rows(STS_23_JN, STS_23_JL, STS_23_AU, STS_23_SP, STS_23_OC)

ASH_Y24<-read.spss("ASH24Y.sav", use.value.labels = TRUE, to.data.frame = TRUE)
ASH_A24<-read.spss("ASH24A.sav", use.value.labels = TRUE, to.data.frame = TRUE)

#########################
# SPLIT STS DATA BY AGE #
#########################
 # Note, we have split age up to 20 years old due to the limited number of 16-18 year olds in STS.

STS_A23<-subset(STS_23, actage>20)
print(nrow(STS_A23))
STS_Y23<-subset(STS_23, actage<21)
print(nrow(STS_Y23))

###########################################################
# CREATE VARIABLES TO USE IN SOCIAL GRADE STRATIFICATION#
###########################################################
# 
# ASH_Y24_c2de<-subset(ASH_Y23, profile_socialgrade_cie_yks_2=="Yes")
# ASH_A24_c2de<-subset(ASH_A23, profile_socialgrade_cie=="C2" | profile_socialgrade_cie=="D" | profile_socialgrade_cie=="E")
# STS_Y23_c2de<-subset(STS_Y23, sgz=="C2" | sgz=="D" | sgz=="E")
# STS_A23_c2de<-subset(STS_A23, sgz=="C2" | sgz=="D" | sgz=="E")

######################
# CREATE FUNCTION #
######################

# Create a function to calculate all proportions which can be run on the general
# UK population and then the low-socioeconomic population

#analyse <- function(grade){
  
  # Write the output of this function to a file called ASH-STS23.txt
  sink(paste("ASH-STS 2024", ".txt", sep=""))
  print(paste("### UK ",  "- ASH2024/STS2023 ###", sep=""))
  print("-------------------------------------------------------------")
  # 
  # # Put each dataframe into a variable to be used in function 
  # # (either the general dataframe or the low-socioeconomic data-frame)
   ASHY24<-get(paste("ASH_Y24", grade, sep=""))
   ASHA24<-get(paste("ASH_A24", grade, sep=""))
   STSY23<-get(paste("STS_Y23", grade, sep=""))
   STSA23<-get(paste("STS_A23", grade, sep=""))
  
  #Or without analysis of grade, can use:
  
  #ASHY24<-ASH_Y24
  #ASHA24<-ASH_A24
  #STSY23<-STS_Y23
  #STSA23<-STS_A23

  ###########################################################
  # A. TO WHAT EXTENT DO FLAVOURS DRAW IN NON_SMOKING YOUTH #
  ###########################################################
  print("## A. TO WHAT EXTENT DO FLAVOURS DRAW IN NON_SMOKING YOUTH ##")
  print("-------------------------------------------------------------")
  
  ## Calculate the proportions to be inputted to calculate A:b ##
  ## the number of non-smoking youth who are are drawn into vaping by flavours ##
  
  #----------------------#
  # b. Non-smoking youth #
  #----------------------#
  print("# A.b. Non-smoking youth #")
  print("--------------------------")
  
  ## Calculate the proportions to be inputted to calculate A.b: ##
  ## the proportion of the youth population who are non-smoking ##
  
  ## Get number and weight of the subset of non-smoking youth ##
  
  #ASH
  # Include those who have never smoked
  ASHY24_neversmoked<-subset(ASHY24, B1=="I have never smoked cigarettes, not even a puff or two")
  # Get number of rows
  print("ASH Non-smoking youth")
  ASHY24_NS<-print(nrow(ASHY24_neversmoked))
  # Average the weighting assigned to these individuals
  print("Weight")
  ASHY24_NSW<-print(mean(ASHY24_neversmoked$weight))
  
  # STS
  # Include those who have never smoked
  STSY23_neversmoked<-subset(STSY23, q632a1=="I have never been a smoker (i.e. smoked for a year or more)")
  # Get number of rows
  print("STS Non-smoking youth")
  STSY23_NS<-print(nrow(STSY23_neversmoked))
  # Average the weighting assigned to these individuals
  print("Weight")
  STSY23_NSW<-print(mean(STSY23_neversmoked$weight_gb))
  
  ## Get number and weight of the subset of non-smoking youth ##
  
  # ASH
  # Exclude those who have never smoked or didn't want to say
  ASHY24_eversmoked<-subset(ASHY24, B1!="I have never smoked cigarettes, not even a puff or two")
  ASHY24_eversmoked<-subset(ASHY24_eversmoked, B1!="Don't want to say")
  # Get number of rows
  print("ASH smoking youth")
  ASHY24_ES<-print(nrow(ASHY24_eversmoked))
  # Average the weighting assigned to these individuals
  print("Weight")
  ASHY24_ESW<-print(mean(ASHY24_eversmoked$weight))
  
  # STS
  # Exclude those who have never smoked or didn't want to say
  STSY23_eversmoked<-subset(STSY23, q632a1!="I have never been a smoker (i.e. smoked for a year or more)")
  STSY23_eversmoked<-subset(STSY23_eversmoked, q632a1!="Don't Know")
  # Get number of rows
  print("STS smoking youth")
  STSY23_ES<-print(nrow(STSY23_eversmoked))
  # Average the weighting assigned to these individuals
  print("Weight")
  STSY23_ESW<-print(mean(STSY23_eversmoked$weight_gb))
  
  ## Calculate the percentage of non-smoking youth who vape using the weighted number ##
  ## of smokers and non-smokers in ASH and STS ##
  
  ## Get weighted total of non-smoking youth across ASH and STS ##
  
  NSW<-(ASHY24_NS*ASHY24_NSW)+(STSY23_NS*STSY23_NSW)
  # Get weighted total of smokers across ASH and STS
  ESW<-(ASHY24_ES*ASHY24_ESW)+(STSY23_ES*STSY23_ESW) 
  # Calculate proportion who are never smokers
  print("A.b. %")
  print(NSW/(NSW+ESW)*100)
  # Get N of population
  print("A.b. N of sample used")
  print(ASHY24_NS+STSY23_NS+ASHY24_ES+STSY23_ES)
  print("-----------------------------------------")
  
  #-------------------------------------#
  # c. Non-smoking youth who have vaped #
  #-------------------------------------#
  print("# A.c. Non-smoking youth who have vaped #")
  print("-----------------------------------------")
  
  ## Calculate the proportions to be inputted to calculate A.c: ##
  ## the proportion of non-smoking youth who have vaped ##
  
  ## Get number and weight of non-smoking youth who have vaped ##
  
  # ASH
  # Note: Only those who have heard of e-cigarettes, are asked if they had ever vaped
  # rather than entire sample, so NAs for this question must be included as negative responses
  # Exclude those non-smoking youth who have never vaped
  ASHY24_neversmoked_evervaped<-subset(ASHY24_neversmoked, C4b!="I have never used a vapes (e-cigarettes)")
  ASHY24_neversmoked_evervaped<-subset(ASHY24_neversmoked_evervaped, C4b!="Don't want to say")
  ASHY24_neversmoked_evervaped<-subset(ASHY24_neversmoked_evervaped, !is.na(ASHY24_neversmoked_evervaped$C4b))
 
   # Get number of rows
  
  print("ASH Non-smoking youth who have vaped")
  ASHY24_NSEV<-print(nrow(ASHY24_neversmoked_evervaped))
  # Average the weighting assigned to these individuals
  print("Weight")
  ASHY24_NSEVW<-print(mean(ASHY24_neversmoked_evervaped$weight))
  
  # STS
  # Include those never smokers who ever have vaped
  STSY23_neversmoked_evervaped<-subset(STSY23_neversmoked, imw184_12 == "Electronic cigarette")
  # Get number of rows
  print("STS Non-smoking youth who have vaped")
  STSY23_NSEV<-print(nrow(STSY23_neversmoked_evervaped))
  # Average weighting assigned to those individuals
  print("Weight")
  STSY23_NSEVW<-print(mean(STSY23_neversmoked_evervaped$weight_gb))
  
  # Get number and weight of youth who have never smoked or vaped 
 
  # ASH
  # Include those never smokers who have never vaped
  ASHY24_neversmoked_nevervaped<-subset(ASHY24_neversmoked, C4b=="I have never used a vapes (e-cigarettes)"| is.na(C4b) )
  # Get number of rows
  print("ASH Non-smoking youth who have never vaped")
  ASHY24_NSNV<-print(nrow(ASHY24_neversmoked_nevervaped))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHY24_NSNVW<-print(mean(ASHY24_neversmoked_nevervaped$weight))
  
  # STS 
  # Include those never smokers who have never vaped
  STSY23_neversmoked_nevervaped<-subset(STSY23_neversmoked, imw184_12=="no Electronic cigarette")
  # Get number of rows
  print("STS Non-smoking youth who have never vaped")
  STSY23_NSNV<-print(nrow(STSY23_neversmoked_nevervaped))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSY23_NSNVW<-print(mean(STSY23_neversmoked_nevervaped$weight_gb))
  
  ## Calculate the percentage of non-smoking youth who have vaped  ##
  ## using the weighted number of smokers and non-smokers in ASH and STS ##
  
  ## Get weighted total of non-smokers who have vaped across ASH and STS ##
  EVW<-(ASHY24_NSEV*ASHY24_NSEVW)+(STSY23_NSEV* STSY23_NSEVW)
  # Get weighted total of non-smokers who never vaped across ASH and STS 
  NVW<-(ASHY24_NSNV*ASHY24_NSNVW)+(STSY23_NSNV*STSY23_NSNVW)
  # Calculate proportion who are non-smokers but have vaped
  print("A.c. %")
  print(EVW/(EVW+NVW)*100)
  # Get N of population
  print("A.c. N of sample used")
  print(ASHY24_NSEV+STSY23_NSEV+ASHY24_NSNV+STSY23_NSNV)
  print("-------------------------------------------------------")
  
  
  #---------------------------------------------------#
  # d. Non-smoking youth who vape because of flavours #
  #---------------------------------------------------#
  print("# A.d. Non-smoking youth who vape because of flavours #")
  print("-------------------------------------------------------")
  
  ## Calculate the proportions to be inputted to calculate A.d: ##
  ## the proportion of non-smoking youth who vape because they like the flavours ##
  
  ## Get number and weight of non-smoking youth who vape because they like the flavours ##
  
  # ASH
  # Include those who vape and who vape because they like the flavours
  ASHY24_flavours<-subset(ASHY24_neversmoked_evervaped, C7b=="I like the flavours")
  # Get number of rows
  print("ASH Non-smoking youth who vape because they like the flavours")
  ASHY24_F<-print(nrow(ASHY24_flavours))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHY24_FW<-print(mean(ASHY24_flavours$weight))
  
  # STS
  # Include non-smokers who vape because they like the flavours
  STSY23_flavours<-subset(STSY23_neversmoked_evervaped, yvp12=="I like the flavours")
  # Get number of rows 
  print("STS Non-smoking youth who vape because they like the flavours")
  STSY23_F<-print(nrow(STSY23_flavours))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSY23_FW<-print(mean(STSY23_flavours$weight_gb))
  
  ## Get number and weight of non-smoking youth who have vaped but for reasons other than liking the flavours ##
  
  # ASH
  # Exclude non-smokers who vape because they like the flavours (or don't know)
  ASHY24_Xflavours<-subset(ASHY24_neversmoked_evervaped, C7b!="Don't know")
  ASHY24_Xflavours<-subset(ASHY24_Xflavours, C7b!="I like the flavours")
  # Get number of rows
  print("ASH Non-smoking youth who vape for other reasons")
  ASHY24_XF<-print(nrow(ASHY24_Xflavours))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHY24_XFW<-print(mean(ASHY24_Xflavours$weight))
  
  # STS 
  # Include non-smokers who have vaped for reasons other than liking the flavours
  STSY23_Xflavours<-subset(STSY23_neversmoked_evervaped, yvp12=="no I like the flavours")
  # Get number of rows
  print("STS Non-smoking youth who vape for other reasons")
  STSY23_XF<-print(nrow(STSY23_Xflavours))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSY23_XFW<-print(mean(STSY23_Xflavours$weight_gb))
  # If none change NA to 0
  if(is.na(STSY23_XFW)){STSY23_XFW<-0}
  print(STSY23_XFW)
  
  # Calculate the percentage of non-smoking youth who vape because they like the flavours  
  # using the weighted number of them versus those who vape for other reasons across ASH and STS 
  
  # Get weighted total of those who vape because they like the flavours
  FW<-(ASHY24_F*ASHY24_FW)+(STSY23_F*STSY23_FW)
  # Get weighted total of those who vape for other reasons
  XFW<-(ASHY24_XF*ASHY24_XFW)+(STSY23_XF*STSY23_XFW)
  print("A.d. %")
  print(FW/(FW+XFW)*100)
  # Get N of population
  print("A.d. N of sample used")
  print(ASHY24_F+STSY23_F+ASHY24_XF+STSY23_XF)
  print("----------------------------------------------------")
  
  ##################################################
  # B. HOW MANY YOUTH WHO VAPE SUBSEQUENTLY SMOKE? #
  ##################################################
  print("## B. HOW MANY YOUTH WHO VAPE SUBSEQUENTLY SMOKE? ##")
  print("----------------------------------------------------")
  
  #-------------------------------------------#
  # b. Youth who vape, and smoke after vaping #
  #-------------------------------------------#
  print("# B.b.  Youth who vape, and smoke after vaping #")
  print("------------------------------------------------")
  
  ## Calculate the proportions to be inputted to calculate B.b: ##
  ## the proportion of youth who vape, and who vape first and then later smoke ##
  
  ## Get number and weight of non-smoking youth who vape and smoke after vaping ##
  
  # ASH
  # Include those who vape and who tried vaping before smoking
  ASHY24_vapethensmoke<-subset(ASHY24, C5_s1=="I tried vaping before I first tried smoking a real cigarette" | C5_s2 == "I tried vaping before I first tried smoking a tobacco cigarette")
  # Get number of rows
  print("ASH Vapers who tried vaping before smoking")
  ASHY24_TS<-print(nrow(ASHY24_vapethensmoke))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHY24_TSW<-print(mean(ASHY24_vapethensmoke$weight))
  
  # STS
  # Include those who vape and who tried vaping before smoking
  STSY23_vapethensmoke<-subset(STSY23, yvp2=="I tried using an e-cigarette before I first tried smoking a regular cigarette")
  # Get number of rows
  print("STS Vapers who tried vaping before smoking")
  STSY23_TS<-print(nrow(STSY23_vapethensmoke))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSY23_TSW<-print(mean(STSY23_vapethensmoke$weight_gb))
  
  ## Get number and weight of non-smoking youth who vape but do not later smoke ##
  
  # ASH
  # Include those who have vaped, but did not later smoke
  ASHY24_vapethendontsmoke<-subset(ASHY24, C5_s1=="I have never smoked a real cigarette but have tried vaping" | C5_s2=="I have never smoked a tobacco cigarette but have tried vaping")
  # Get number of rows
  print("ASH Vapers who have never smoked")
  ASHY24_TD<-print(nrow(ASHY24_vapethendontsmoke))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHY24_TDW<-print(mean(ASHY24_vapethendontsmoke$weight))
  
  # STS
  # Include those who have vaped, but did not later smoke
  STSY23_vapethendontsmoke<-subset(STSY23, yvp2=="I have never smoked a regular cigarette but have tried an e-cigarette")
  # Get number of rows
  print("STS Vapers who have never smoked")
  STSY23_TD<-print(nrow(STSY23_vapethendontsmoke))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSY23_TDW<-print(mean(STSY23_vapethendontsmoke$weight_gb))
  
  # Calculate percentage of those who vape and started smoking after vaping 
  # using the weighted number of them versus those who did not start smoking across ASH and STS
  # Get weighted total of youth who started smoking after vaping
  TSW<-(ASHY24_TS*ASHY24_TSW)+(STSY23_TS*STSY23_TSW)
  # Get weighted total of youth who did not start smoking after vaping
  TDW<-( ASHY24_TD*ASHY24_TDW)+(STSY23_TD*STSY23_TDW)
  print("B.b. %")
  print( TSW/( TSW+TDW)*100)
  # Get N of population
  print("B.b. N of sample used")
  print(ASHY24_TS+STSY23_TS+ASHY24_TD+STSY23_TD)
  print("-----------------------------------------------------------------------------------------------------------")
  
  
  #############################################################
  # C. TO WHAT EXTENT DO FLAVOURS DRAW IN ADULT SMOKERS WHO   #
  #   WOULD QUIT SMOKING USING VAPES AS A RESOURCE?#
  ###############################################################
  print("## C. TO WHAT EXTENT DO FLAVOURS DRAW IN ADULT SMOKERS WHO WOULD QUIT SMOKING USING VAPES AS A RESOURCE? ##")
  print("-----------------------------------------------------------------------------------------------------------")
  
  #------------------------------------------------------#
  # b. Adult smokers who quit last year via e-cigarettes #
  #------------------------------------------------------#
  print("# C.b. Adult smokers who quit last year via e-cigarettes #")
  print("----------------------------------------------------------")
  
  ## Calculate the proportions to be inputted to calculate C.b: ##
  ## the proportion of adult smokers who quit via e-cigarettes  ##
  # ASH
  
  #Subset adult smokers and recent quitters
  ASH_currentsmokers_recentquitters <-subset(ASHA24, s2_edited2016!="I have never smoked" )
  ASHA24_SRQ<-subset(ASH_currentsmokers_recentquitters, Q3_New2024!=" More than 1, up to 2 years ago" & Q3_New2024!="More than 2, up to 5 years ago"& Q3_New2024!="More than 5 years ago" & Q3_New2024!="Don't know/ can't recall" )
  # Get number of rows
  print("ASH Adult Smokers + Recent Quitters")
  print(nrow(ASHA24_SRQ))
  
  # STS
  # Subset adult smokers and recent quitters
  STSA23_currentsmokers_recentquitters<-subset(STSA23, q632a1!="Don't Know")
  STSA23_SRQ<-subset(STSA23_currentsmokers_recentquitters, q632a1!="I have never been a smoker (i.e. smoked for a year or more)")
  STSA23_SRQ<-subset(STSA23_SRQ, q632a1!="I stopped smoking completely more than a year ago")
  # Get number of rows
  print("STS Adult Smokers + Recent Quitters")
  print(nrow(STSA23_SRQ))
  
  # Get number and weight of adult ex-smokers who recently quit via e-cigarettes

  # Include those who have recently quit and used e-cigarettes as a quit resource 
  # ASH
  ASHA24_recentquitter_vaper<-subset(ASHA24_SRQ, 
                                     (Q3_New2024=="More than 6 months, up to a year ago" 
                                     | Q3_New2024=="Within the last 6 months")
                                     & Q4_New2024=="Yes, I did")
  print("ASH Adult Recent quitters, who used e-cigarettes as a quit resource")
  ASHA24_RQV<-print(nrow(ASHA24_recentquitter_vaper))
  print("Weight")
  ASHA24_RQVW<-print(mean(ASHA24_recentquitter_vaper$weight))
  
  # STS
  STSA23_recentquitter_vaper<-subset(STSA23_SRQ, 
                              q632a1=="I have stopped smoking completely in the last year" 
                              & X.q632b1k=="Electronic cigarette")
  print("STS Adult Recent quitters, who used e-cigarettes as a quit resource")
  STSA23_RQV<-print(nrow(STSA23_recentquitter_vaper))
  print("Weight")
  STSA23_RQVW<-print(mean(STSA23_recentquitter_vaper$weight_gb))
  
  # Get number and weight of everyone else
  # ASH
  ASHA24_recentquitter_nonvaper<- ASHA24_SRQ[!(rownames(ASHA24_SRQ) %in% rownames(ASHA24_recentquitter_vaper)),]
  print("ASH Adult Recent quitters, who did not use e-cigarettes as a quit resource")
  ASHA24_RQNV<-print(nrow(ASHA24_recentquitter_nonvaper))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHA24_RQNVW<-print(mean(ASHA24_recentquitter_nonvaper$weight))
  
  # STS
  STSA23_recentquitter_nonvaper<- STSA23_SRQ[!(rownames(STSA23_SRQ) %in% rownames(STSA23_recentquitter_vaper)),]
  print("STS Adult Recent quitters, who did not use e-cigarettes as a quit resource")
  STSA23_RQNV<-print(nrow(STSA23_recentquitter_nonvaper))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSA23_RQNVW<-print(mean(STSA23_recentquitter_nonvaper$weight_gb))
  
  
  # Calculate percentage of adult smokers who quit in the last year via e-cigarettes,
  # using weighted numbers of them, and recent quitters who did not use e-cigs, across ASH and STS
  RQVW<-(ASHA24_RQV*ASHA24_RQVW)+(STSA23_RQV*STSA23_RQVW)
  # Get weighted total of recent quitters who used e-cigarettes as a resource
  RQNVW<-(ASHA24_RQNV*ASHA24_RQNVW)+(STSA23_RQNV*STSA23_RQNVW)
  # Get weighted total of recent quitters who did not use e-cigarettes as a resource
  print("C.b %")
  print(RQVW/(RQVW+RQNVW)*100)
  # Get N of population
  print("c.b. N of sample used")
  print(ASHA24_RQV+ASHA24_RQNV+STSA23_RQV+STSA23_RQNV)
  print("---------------------------------------------------------------------------------------------------------------")
  
  #-----------------------------------------------------------------------------------------------------------#
  # c. Adult smokers, who vape, and who would not quit smoking/smoke more if flavoured vapes were unavailable #
  #-----------------------------------------------------------------------------------------------------------#
  print("# C.c. Adult smokers, who vape, and who would not quit smoking/smoke more if flavoured vapes were unavailable #")
  print("---------------------------------------------------------------------------------------------------------------")
  
  ## Calculate the proportions to be inputted to calculate C.c: ##
  ## the proportion of adult smokers who vape and would not quit smoking / smoke more without flavoured vapes ##
  

  print("------------------------------------------------------------------------------------------------------")
  
  # Get number of adult smokers
  
  # ASH 
  # Include current smokers
  ASHA24_S<-subset(ASHA24, s2_edited2016=="I smoke but I don't smoke every day" | s2_edited2016=="I smoke every day")
  # Get number of rows
  print("ASH Adult Smokers")
  print(nrow(ASHA24_S))
  
  # STS
  # Exclude those who quit less than a year ago (non-smokers already excluded in previous section) 
  STSA23_S<-subset(STSA23_SRQ, q632a1!="I have stopped smoking completely in the last year")
  # Get number of rows
  print("STS Adult Smokers")
  print(nrow(STSA23_S))
  
  # Get number and weight of adult smokers, who vape, and who would smoke more 
  # or not quit smoking if flavoured vapes were no longer available
  
  # ASH
  # Include those smokers who vape who said they would smoke more or return to smoking if flavoured vapes were unavailable
  ASHA24_smokemore<-subset(ASHA24_S, Q11_New202406=="Yes" | Q11_New202405=="Yes")
  print("ASH Adult Smokers, who vape, and who would smoke more or return to smoking if flavours became unavailable")
  ASHA24_SM<-print(nrow(ASHA24_smokemore))
  print("Weight")
  ASHA24_SMW<-print(mean(ASHA24_smokemore$weight))
  
  # STS
  # Include those smokers who vape who and said they would smoke more, or return to smoking if flavoured vapes were unavailable
  STSA23_smokemore<-subset(STSA23_S, yvp33=="I would smoke more tobacco" | yvp34=="I would go back to smoking tobacco")
  print("STS Adult Smokers, who vape, and who would smoke more or return to smoking if flavours became unavailable")
  STSA23_SM<-print(nrow(STSA23_smokemore))
  print("Weight")
  STSA23_SMW<-print(mean(STSA23_smokemore$weight_gb))
  
  #Get number and weight of everyone else
  
  # ASH
  ASHA24_notsmokemore<- subset(ASHA24_S, Q11_New202406=="No" & Q11_New202405=="No")
  print("ASH Everyone Else")
  ASHA24_NSM<-print(nrow(ASHA24_notsmokemore))
  print("Weight")
  ASHA24_NSMW<-print(mean(ASHA24_notsmokemore$weight))
  
  # STS
  STSA23_notsmokemore<-subset(STSA23_S, yvp33=="no I would smoke more tobacco" & yvp34=="no I would go back to smoking tobacco")
  print("STS Everyone Else")
  STSA23_NSM<-print(nrow(STSA23_notsmokemore))
  print("Weight")
  STSA23_NSMW<-print(mean(STSA23_notsmokemore$weight_gb))
  
  
  # Calculate percentage of adult smokers who vape and who would smoke more 
  # if flavoured vapes were no longer available 
  # using weighted numbers of them, and everyone else, across ASH and STS
  # Get weighted total of those who would smoke more or return to smoking without flavours
  SMW<-(ASHA24_SM*ASHA24_SMW)+(STSA23_SM*STSA23_SMW)
  # Get weighted total of those who would NOT smoke more or NOT return to smoking without flavours
  NSMW<-(ASHA24_NSMW*ASHA24_NSMW)+(STSA23_NSM*STSA23_NSMW)
  # Calculate proportion
  print("C.c %")
  print(SMW/(SMW+NSMW)*100)
  # Get N of population
  print("C.c. N of sample used")
  print(ASHA24_SM+STSA23_SM+ASHA24_NSM+STSA23_NSM)
  print("---------------------------------------------------------------------------------------------------------")
  
  ######################################################
  # D. HOW MANY ADULT EX-SMOKERS WHO VAPE WOULD REALPSE#
  #    TO SMOKING IF FLAVOURED VAPES WERE UNAVAILABLE? #
  ######################################################
  print("## D. HOW MANY ADULT EX-SMOKERS WHO VAPE WOULD RELAPSE TO SMOKING IF FLAVOURED VAPES WERE UNAVAILABLE? ##")
  print("---------------------------------------------------------------------------------------------------------")
  
  #----------------------------------#
  # b. Adult ex-smokers who who vape #
  #----------------------------------#
  print("# D.b. Adult ex-smokers who vape #")
  print("----------------------------------")
  
  ## Calculate the proportions to be inputted to calculate D.b: ##
  ## the proportion of adult ex-smokers who vape ##
  
  # Get number of ex-smokers
  
  # ASH
  ASHA24_XS<-subset(ASHA24, s2_edited2016=="I used to smoke but I have given up now")
  # Get number of rows
  print("ASH Adult Ex-Smokers")
  print(nrow(ASHA24_XS))
  
  # STS
  STSA23_XS<-subset(STSA23, q632a1=="I have stopped smoking completely in the last year" | q632a1=="I stopped smoking completely more than a year ago")
  # Get number of rows
  print("STS Adult Ex-Smokers")
  print(nrow(STSA23_XS))
  
  # Get number and weight of ex-smokers who currently vape
  
  # ASH
  # Include ex-smokers who are current vapers
  ASHA24_exsmoker_currentvaper<-subset(ASHA24_XS, q3=="I have tried vapes (e-cigarettes) and still use them")
  # Get number of rows
  print("ASH Adult Ex-smokers - Current vapers")
  ASHA24_XSCV<-print(nrow(ASHA24_exsmoker_currentvaper))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHA24_XSCVW<-print(mean(ASHA24_exsmoker_currentvaper$weight))
  
  # STS 
  # Include ex-smokers who are current vapers
  STSA23_exsmoker_currentvaper<-subset(STSA23_XS, qimw866=="Electronic cigarette")
  # Get number of rows
  print("STS Adult Ex-smokers - Current vapers")
  STSA23_XSCV<-print(nrow(STSA23_exsmoker_currentvaper))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSA23_XSCVW<-print(mean(STSA23_exsmoker_currentvaper$weight_gb))
  
  # Get weight of ex-smokers who do not vape
  
  # ASH
  # Exclude current vapers from ex-smoker subset
  NonVapers<-c(" I have never heard of vapes (e-cigarettes) and have never tried them", "I have heard of vapes (e-cigarettes) but have never tried them", " I have tried vapes (e-cigarettes) but do not use them (anymore)")
  ASHA24_exsmoker_notcurrentvaper<-subset(ASHA24_XS, q3 %in% NonVapers)
  # Get number of rows
  print("ASH Adult Ex-smokers who are not current vapers")
  ASHA24_XSNCV<-print(nrow(ASHA24_exsmoker_notcurrentvaper))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHA24_XSNCVW<-print(mean(ASHA24_exsmoker_notcurrentvaper$weight))
  
  # STS 
  # Include ex-smokers who are not current vapers
  STSA23_exsmoker_noncurrentvaper<-subset(STSA23_XS, qimw866=="no Electronic cigarette")
  # Get number of rows
  print("STS Adult Ex-smokers who are not current vapers")
  STSA23_XSNCV<-print(nrow(STSA23_exsmoker_noncurrentvaper))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSA23_XSNCVW<-print(mean(STSA23_exsmoker_noncurrentvaper$weight_gb))
  
  # Calculate percentage of ex-smokers who currently vape using weighted numbers of them, 
  # and those who don't currently vape in ASH and STS
  # Get weighted total of ex-smokers who vape in ASH and STS 
  CVW<-(ASHA24_XSCV*ASHA24_XSCVW)+(STSA23_XSCV*STSA23_XSCVW) 
  # Get weighted total of Ex-Smokers who are not current vapers in ASH and STS 
  NCVW<-(ASHA24_XSNCV*ASHA24_XSNCVW)+(STSA23_XSNCV*STSA23_XSNCVW)
  print("D.b. %")
  print(CVW/(CVW+NCVW)*100)
  # Get N of population
  print("D.b. N of sample used")
  print(ASHA24_XSCV+STSA23_XSCV+ASHA24_XSNCV+STSA23_XSNCV)
  print("----------------------------------------------------------------------------------")
  
  
  #-------------------------------------------------------------------------------------#
  # c. Adult Ex-smokers who vape and would relapse if flavoured vapes were unavailable  #
  #-------------------------------------------------------------------------------------#
  print("# D.c. Adult Ex-smokers who vape and would relapse if flavoured vapes were unavailable #")
  print("----------------------------------------------------------------------------------------")
  
  ## Calculate the proportions to be inputted to calculate D.c: ##
  ## the proportion of ex-smokers who vape and would ##
  ## relapse to smoking without flavoured vapes ##
  
  print("------------------------------------------------------------------------------------------------------")
  
  # Get number and weight of ex-smokers, who vape and would relapse without flavoured vapes
  
  # ASH 
  # Include ex-smokers 
  ASHA24_XS<-subset(ASHA24, s2_edited2016=="I used to smoke but I have given up now")
  # Get number of rows
  print("ASH Adult Ex-Smokers")
  print(nrow(ASHA24_XS))
  
  # Get number and weight of ex-smokers who would relapse to smoking without flavoured vapes
  
  # ASH
  # Include ex-smokers who would relapse without flavours
  ASHA24_relapse<-subset(ASHA24_XS, Q11_New202406=="Yes" | Q11_New202405=="Yes"| Q11_New202404=="Yes")
  # Get number of rows
  print("ASH Adult Ex-Smokers who would relapse without flavours")
  ASHA24_R<-print(nrow(ASHA24_relapse))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHA24_RW<-print(mean(ASHA24_relapse$weight))
  
  # STS
  # Include ex-smokers who would relapse without flavours
  STSA23_relapse<-subset(STSA23_XS, yvp33=="I would smoke more tobacco" | yvp34=="I would go back to smoking tobacco")
  # Get number of rows
  print("STS Adult Ex-Smokers who would relapse without flavours")
  STSA23_R<-print(nrow(STSA23_relapse))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSA23_RW<-print(mean(STSA23_relapse$weight_gb))
  
  # Get number and weight of ex-smokers who would NOT relapse to smoking without flavoured vapes
  
  # ASH
  # Include ex-smokers who would not relapse without flavours
  ASHA24_norelapse<<-subset(ASHA24_XS, Q11_New202406=="No" & Q11_New202405=="No"& Q11_New202404=="No")
  # Get number of rows
  print("ASH Adult Ex-Smokers who would NOT relapse without flavours")
  ASHA24_NR<-print(nrow(ASHA24_norelapse))
  # Average the weighting assigned to those individuals
  print("Weight")
  ASHA24_NRW<-print(mean(ASHA24_norelapse$weight))
  
  # STS
  # Include ex-smokers who would not relapse without flavours
  STSA23_norelapse<-subset(STSA23_XS, yvp33=="no I would smoke more tobacco" & yvp34=="no I would go back to smoking tobacco")
  # Get number of rows
  print("STS Adult Ex-Smokers who would NOT relapse without flavours")
  STSA23_NR<-print(nrow(STSA23_norelapse))
  # Average the weighting assigned to those individuals
  print("Weight")
  STSA23_NRW<-print(mean(STSA23_norelapse$weight_gb))
  
  # Calculate percentage of ex-smokers who currently vape, and would relapse without flavoured vapes, 
  # using weighted numbers of them, and everyone else in ASH and STS
  # Get weighted total of ex-smokers who would relapse without flavours
  RW<-(ASHA24_R*ASHA24_RW)+(STSA23_R*STSA23_RW) 
  # Get weighted total of ex-smokers who would not relapse without flavours
  NRW<-(ASHA24_NR*ASHA24_NRW)+(STSA23_NR*STSA23_NRW)
  # Calculate proportion
  print("D.c. %")
  print(RW/(RW+NRW)*100)
  # Get N of population
  print("D.c. N of sample used")
  print(ASHA24_R+STSA23_R+ASHA24_NR+STSA23_NR)
  
  #Stop writing to output file
  sink()
}

# #Run for general Population (
grade<-""
analyse(grade)

# #Run for low social grade
# grade<-"_c2de"
# analyse(grade)

