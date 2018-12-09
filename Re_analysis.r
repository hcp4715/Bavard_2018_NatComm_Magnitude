####################################################################################################
###        This script is reproduce model-free analysis of Bavard et al., 2018. nat. comm.       ###
###           original data and matlab code:https://github.com/sophiebavard/Magnitude            ###
###            by Chuan-Peng Hu, PhD., twitter: @hcp4715, email: hcp4715@gmail.com               ###
####################################################################################################

####################################################################################################
###  Input data: Data_expe1.mat; Data_expe2.mat                                                  ###
###  Variables: subjectlist --  subject id no.                                                   ###
###             cond_s      --  conditions diff between sessions, from 1 ~ 8                     ###
###                             condition 1 (5): 75% vs. 25%  prob. of gain 1 Euro               ###
###                             condition 2 (6): 75% vs. 25%  prob. of gain 0.1 Euro             ###
###                             condition 3 (7): 25% vs. 75%  prob. of loss 0.1 Euro             ###
###                             condition 4 (8): 25% vs. 75%  prob. of loss 1 Euro               ###
###             cond_whole  --  conditions same between sessions, from 1 ~ 4 (as cond_s)         ###
###             choice      --  participants' choice during learning (1 - wrong; 2- correct)     ###
###             out_abs     --  outcome (of reward) under absolute model                         ###
###             out_rel     --  outcome under relative model                                     ###
###             out_c_abs   --  counterfactual outcome under absolute model (only for exp.2)     ###
###             out_c_rel   --  counterfactual outcome under relative model (only for exp.2)     ###
###                                                                                              ###
###             stim_test   --  stimuli presented during tranfer test                            ###
###                             including two stimulus each trias: left and right;               ###
###                             8 stimuli in total:                                              ###
###                                stim 1 (A): 25% of  1 Euro    0.25   fav                      ###
###                                stim 2 (B): 25% of  1 Euro    0.25   no-fav
###                                stim 3 (C): 75% of  0.1 Euro  0.075  fav
###                                stim 4 (D): 25% of  0.1 Euro  0.025  no-fav
###                                stim 5 (E): 25% of -0.1 Euro -0.025  fav
###                                stim 6 (F): 75% of -0.1 Euro -0.075  no-fav
###                                stim 7 (G): 25% of -1 Euro   -0.25   fav
###                                stim 8 (H): 75% of -0.1 Euro -0.75   no-fav
###
###             choice_test --  choise during tranfer test 
###                             incuding two options: 1- left-side; 2-right-side                 ###
####################################################################################################

# ---------------------------------------------------------------------------------------------------------------
# ------ preparation --------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------

### get the directory of the current script
curDir <- dirname(rstudioapi::getSourceEditorContext()$path) #Get the directory ofcurrent script
setwd(curDir)

### clear the working memory
rm(list = ls())

### load the library
library(R.matlab)
library(hBayesDM)
library(tidyverse)
library(mosaic)
library(ez)

### load the data
df1 <- R.matlab::readMat('Data_expe1.mat') # data from experiment 1
df2 <- R.matlab::readMat('Data_expe2.mat') # data from experiment 2

# ---------------------------------------------------------------------------------------------------------------
# ------ extract variables from loaded data (exp1) --------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------

### subject list
sublist    <- data.frame(matrix(unlist(df1[['subjects']]), nrow=1),stringsAsFactors=FALSE)

### conditions (4) in two sessions (1~8) for 20 subj, results a 160 * 20 dataframe
cond_s     <- data.frame(matrix(as.numeric(unlist(df1[['con']])), nrow=160),stringsAsFactors=FALSE)

### conditions (4) across two session (1~4) for 20 subj, 160 * 20
cond_whole <- data.frame(matrix(unlist(df1[['con2']]), nrow=160),stringsAsFactors=FALSE)

### choice
choice     <- data.frame(matrix(unlist(df1[['cho']]), nrow=160),stringsAsFactors=FALSE)

# obtained outcome in absolute value, for each trials
out_abs     <- data.frame(matrix(unlist(df1[['out']]), nrow=160),stringsAsFactors=FALSE)

# obtained outcome in relative value, for each trials
out_rel    <- data.frame(matrix(unlist(df1[['out2']]), nrow=160),stringsAsFactors=FALSE)

# symbols in transfer test, 112 * 2 * 20 = 112 * 40 
stim_test      <- data.frame(matrix(unlist(df1[['ss']]), nrow=112),stringsAsFactors=FALSE)

# 20 cells, each is 112 * 1 (actions in transfer test ) into a 112 * 20 datafame
choice_test      <- data.frame(matrix(unlist(df1[['aa']]), nrow=112),stringsAsFactors=FALSE)

### save each participant's data as well as combined data
saveDir <- paste0('./exp1_subj/')                                      # directory for saving indivdiual data
rm(tmp1, tmp2,df.exp1,df.exp1_tst)                                                 # clear variable if exist in memory
for (ii in 1:length(sublist)){
      expName <- paste(saveDir,sublist[ii],'_exp', '.csv', sep = '')   # file name for learning data
      tstName <- paste(saveDir,sublist[ii],'_test', '.csv', sep = '')  # file name for testing data
      tmp1 <- data.frame(matrix(ncol = 1, nrow = 160))                 # create a temporary variable
      colnames(tmp1) <- 'subjID'                                       
      tmp1$subjID <- as.numeric(sublist[ii])                           # subject id
      tmp1$cond1  <- cond_s[,ii]                                       # condtions (diff across session)
      tmp1$cond2  <- cond_whole[,ii]                                   # condtions (same across session)
      tmp1$choice <- choice[,ii]                                       # participant's choice
      tmp1$absOut <- out_abs[,ii]                                      # absolute outcome
      tmp1$rltOut <- out_rel[,ii]                                      # relative outcome
      if (!exists('df.exp1')){
         df.exp1 <- tmp1                                               # combine individual data
      } else {
         df.exp1 <- rbind(df.exp1,tmp1)
      }
      write.csv(file = expName,tmp1,row.names = F)                     # save individuals data
      
      tmp2 <- data.frame(matrix(ncol = 1, nrow = 112))
      colnames(tmp2) <- 'subjID'
      tmp2$subjID <- as.numeric(sublist[ii])                           # participant's id no.
      tmp2$stim1 <- stim_test[,ii*2-1]                                 # stimuli presented at left side
      tmp2$stim2 <- stim_test[,ii*2]                                   # stimuli presented at left side
      tmp2$action <- choice_test[,ii]                                  # participant's choice during test
      write.csv(file = tstName,tmp2,row.names = F)
      if (!exists('df.exp1_tst')){
         df.exp1_tst <- tmp2
      } else {
         df.exp1_tst <- rbind(df.exp1_tst,tmp2)
      }
}
rm(sublist, cond_s,cond_whole,choice,out_abs,out_rel,stim_test,choice_test,tmp1,tmp2)
# ---------------------------------------------------------------------------------------------------------------
# ------ learning data for model free analysis (exp1) -----------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------

###  add condition; using the derivedFactor function to get factors based on logical, then transfer to character.
df.exp1 <- df.exp1 %>%
   dplyr::mutate(
      Mag = mosaic::derivedFactor(
         'Hi' = (cond2 == 1 | cond2 == 3),                              # high magnitude (1 Euro)
         'Lo' = (cond2 == 2 | cond2 == 4),                              # low magnitude  (0.1 Euro)
         method ="first",.default = NA),
      Val = mosaic::derivedFactor(
         'gain' = (cond2 == 1 | cond2 == 2),                            # gain
         'loss' = (cond2 == 3 | cond2 == 4),                            # loss
         method ="first",.default = NA)) %>%
   dplyr::mutate_if(is.factor, as.character)                            # to character

df.exp1$ACC <- df.exp1$choice - 1                                       # calcualte the accuracy

### re-arrange the columns
df.exp1 <- df.exp1[,c("subjID", "Mag","Val", "cond1","cond2","choice","ACC","absOut","rltOut")]

### save the file
write.csv(file = 'exp1_data.csv',df.exp1,row.names = F)

##################################
###       ANOVA                ###
##################################
exp1.anova <- ez::ezANOVA(df.exp1, ACC, wid = subjID, within = c('Mag','Val'),within_full = c('Mag','Val'))
print(exp1.anova)                                # reproduced the results of the table 1, page 3

# accuracy for each participant of each condition, can be sued for plotting
Exp_Acc_indv <- df.exp1 %>%
   dplyr::group_by(subjID,cond2) %>%            # group data by subjID and cond2
   dplyr::summarise(ACC=mean(choice - 1)) %>%
   dplyr::ungroup()

### the code below can be used for calculating the group meaning of each condition
#Exp_Acc_sum <- Exp_Acc_indv %>%
#   dplyr::group_by(cond2) %>%
#   dplyr::summarise(ACC_m=mean(ACC),
#                    ACC_sd=sd(ACC),
#                    ACC_se=sd(ACC)/sqrt(length(ACC))) %>%
#   dplyr::ungroup()

# ---------------------------------------------------------------------------------------------------------------
# ------ testing data for model free analysis  (exp1) -----------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------

### correct choice rate:
# stim 1 (A): 75% of  1 Euro    0.75   fav
# stim 2 (B): 25% of  1 Euro    0.25   no-fav
# stim 3 (C): 75% of  0.1 Euro  0.075  fav
# stim 4 (D): 25% of  0.1 Euro  0.025  no-fav
# stim 5 (E): 25% of -0.1 Euro -0.025  fav
# stim 6 (F): 75% of -0.1 Euro -0.075  no-fav
# stim 7 (G): 25% of -1 Euro   -0.25   fav
# stim 8 (H): 75% of -0.1 Euro -0.75   no-fav
# if left stim # < right stim #, then left side is the correct answer
# if right stim # < left stim #, then right side is the correct answer
for (ii in 1:nrow(df.exp1_tst)){
   if ((df.exp1_tst$stim1[ii] < df.exp1_tst$stim2[ii]) & df.exp1_tst$action[ii] ==1) {
      df.exp1_tst$ACC[ii] <- 1
   } else if ((df.exp1_tst$stim1[ii] < df.exp1_tst$stim2[ii]) & df.exp1_tst$action[ii] == 2) {
      df.exp1_tst$ACC[ii] <- 0
   } else if ((df.exp1_tst$stim1[ii] > df.exp1_tst$stim2[ii]) & df.exp1_tst$action[ii] == 1) {
      df.exp1_tst$ACC[ii] <- 0
   } else if ((df.exp1_tst$stim1[ii] > df.exp1_tst$stim2[ii]) & df.exp1_tst$action[ii] == 2) {
      df.exp1_tst$ACC[ii] <- 1
   }
}

### choice portion for each stimuli
### get the choices:
df.exp1_tst$choice[df.exp1_tst$action == 1] <- df.exp1_tst$stim1[df.exp1_tst$action == 1] 
df.exp1_tst$choice[df.exp1_tst$action == 2] <- df.exp1_tst$stim2[df.exp1_tst$action == 2]

df.exp1_tst.choiceRate <- df.exp1_tst %>%
      dplyr::group_by(subjID) %>%                           # group by subject
      dplyr::count(subjID,choice) %>%                       # count the number
      dplyr::mutate(prop = prop.table(n)) %>%               # calculate the proportion
      dplyr::ungroup() %>%                                  # un-group for further data manipulation in future.
      tidyr::complete(subjID,choice,fill = list(prop = 0))  # fill the missing conditions for the subjID*choice combinations

# derive the conditions for magnitude, valuence, and favorableness
df.exp1_tst.choiceRate <- df.exp1_tst.choiceRate %>%
   dplyr::mutate(
      Mag = mosaic::derivedFactor(
      'Hi' = (choice == 1 | choice == 2 | choice == 5 | choice == 6),    # derive a new factor based on condition
      'Lo' = (choice == 3 | choice == 4 | choice == 7 | choice == 8),
      method ="first",.default = NA),
      Val = mosaic::derivedFactor(
         'pos' = (choice == 1 | choice == 2 | choice == 3 | choice == 4),
         'neg' = (choice == 5 | choice == 6 | choice == 7 | choice == 8),
         method ="first",.default = NA),
      Fav = mosaic::derivedFactor(
         'Hi' = (choice == 1 | choice == 3 | choice == 5 | choice == 7),
         'Lo' = (choice == 2 | choice == 4 | choice == 6 | choice == 8),
         method ="first",.default = NA),) %>%
   dplyr::mutate_if(is.factor, as.character)

write.csv(file = 'exp1_test_prop.csv',df.exp1_tst.choiceRate,row.names = F)

##################################
###       ANOVA                ###
##################################
exp1.test.anova <- ez::ezANOVA(data = df.exp1_tst.choiceRate, dv = prop, wid = subjID, within = c('Mag','Val','Fav'))
print(exp1.test.anova)                # reproduced the results of the table 2, page 4

# ---------------------------------------------------------------------------------------------------------------
# ------ extract variables from loaded data (exp2) --------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------

# subject list
sublist <- data.frame(matrix(unlist(df2[['subjects']]), nrow=1),stringsAsFactors=FALSE)

# 8 conditions
cond_s     <- data.frame(matrix(as.numeric(unlist(df2[['con']])), nrow=160),stringsAsFactors=FALSE)

# 4 conditions 
cond_whole <- data.frame(matrix(unlist(df2[['con2']]), nrow=160),stringsAsFactors=FALSE)

# participant's choice for each trial
choice     <- data.frame(matrix(unlist(df2[['cho']]), nrow=160),stringsAsFactors=FALSE)

# feedback, partial or complete.
cond_inf   <- data.frame(matrix(unlist(df2[['con3']]), nrow=160),stringsAsFactors=FALSE)

# obtained outcome in absolute value, for each trials
out_abs    <- data.frame(matrix(unlist(df2[['out']]), nrow=160),stringsAsFactors=FALSE)

# obtained outcome in relative value, for each trials
out_relat  <- data.frame(matrix(unlist(df2[['out2']]), nrow=160),stringsAsFactors=FALSE)

# counterfactural outcome, in absolute sense
out_c_abs   <- data.frame(matrix(unlist(df2[['cou']]), nrow=160),stringsAsFactors=FALSE)

# counterfactural outcome, in relative sense
out_c_rel <- data.frame(matrix(unlist(df2[['cou2']]), nrow=160),stringsAsFactors=FALSE)

# stimuli during the test
stim_test  <- data.frame(matrix(unlist(df2[['ss']]), nrow=112),stringsAsFactors=FALSE)

# choice during the test
choice_test<- data.frame(matrix(unlist(df2[['aa']]), nrow=112),stringsAsFactors=FALSE)

saveDir <- paste0('./exp2_subj/')
rm(tmp1, tmp2,df.exp2,df.exp2_tst)
for (ii in 1:length(sublist)){
   expName <- paste(saveDir,sublist[ii],'_exp', '.csv', sep = '')
   tstName <- paste(saveDir,sublist[ii],'_test', '.csv', sep = '')
   tmp1 <- data.frame(matrix(ncol = 1, nrow = 160))
   colnames(tmp1) <- 'subjID'
   tmp1$subjID <- as.numeric(sublist[ii])
   tmp1$cond1  <- cond_s[,ii]
   tmp1$cond2  <- cond_whole[,ii]
   tmp1$infoType <- cond_inf[,ii]
   tmp1$choice <- choice[,ii]
   tmp1$outAbs <- out_abs[,ii]
   tmp1$outRel <- out_relat[,ii]
   tmp1$out_c_abs <- out_c_abs[,ii]
   tmp1$out_c_rel <- out_c_rel[,ii]
   if (!exists('df.exp2')){
      df.exp2 <- tmp1
   } else {
      df.exp2 <- rbind(df.exp2,tmp1)
   }
   write.csv(file = expName,tmp1,row.names = F)
   
   tmp2 <- data.frame(matrix(ncol = 1, nrow = 112))
   colnames(tmp2) <- 'subjID'
   tmp2$subjID <- as.numeric(sublist[ii])
   tmp2$stim1 <- stim_test[,ii*2-1]
   tmp2$stim2 <- stim_test[,ii*2]
   tmp2$action <- choice_test[,ii]
   write.csv(file = tstName,tmp2,row.names = F)
   if (!exists('df.exp2_tst')){
      df.exp2_tst <- tmp2
   } else {
      df.exp2_tst <- rbind(df.exp2_tst,tmp2)
   }
}

### traditional analysis
# add condition
###  add condition; using the derivedFactor function to get factors based on logical, then transfer to character.
df.exp2 <- df.exp2 %>%
   dplyr::mutate(
      Mag = mosaic::derivedFactor(
         'Hi' = (cond2 == 1 | cond2 == 3),                              # high magnitude (1 Euro)
         'Lo' = (cond2 == 2 | cond2 == 4),                              # low magnitude  (0.1 Euro)
         method ="first",.default = NA),
      Val = mosaic::derivedFactor(
         'gain' = (cond2 == 1 | cond2 == 2),                            # gain
         'loss' = (cond2 == 3 | cond2 == 4),                            # loss
         method ="first",.default = NA),
      Info = mosaic::derivedFactor(
         'partial' = (infoType == 0),
         'complete' = (infoType == 1),
         method ="first",.default = NA)) %>%
   dplyr::mutate_if(is.factor, as.character)                            # to character

df.exp2$ACC <- df.exp2$choice - 1

df.exp2 <- df.exp2[,c("subjID", "Mag","Val",'Info','infoType',"cond1","cond2","choice","ACC","outAbs",
                      "outRel",'out_c_abs','out_c_rel')]
write.csv(file = 'exp2_data.csv',df.exp2,row.names = F)

# ANOVA
exp2.anova <- ez::ezANOVA(df.exp2, ACC, wid = subjID,
                          within = c('Mag','Val','Info'),
                          within_full = c('Mag','Val','Info'))
print(exp2.anova)    # replicated the results of the table 1, page 3

# ---------------------------------------------------------------------------------------------------------------
# ------ testing data for model free analysis  (exp2) -----------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------
for (ii in 1:nrow(df.exp2_tst)){
   if ((df.exp2_tst$stim1[ii] < df.exp2_tst$stim2[ii]) & df.exp2_tst$action[ii] == 1) {
      df.exp2_tst$ACC[ii] <- 1
   } else if ((df.exp2_tst$stim1[ii] < df.exp2_tst$stim2[ii]) & df.exp2_tst$action[ii] == 2) {
      df.exp2_tst$ACC[ii] <- 0
   } else if ((df.exp2_tst$stim1[ii] > df.exp2_tst$stim2[ii]) & df.exp2_tst$action[ii] == 1) {
      df.exp2_tst$ACC[ii] <- 0
   } else if ((df.exp2_tst$stim1[ii] > df.exp2_tst$stim2[ii]) & df.exp2_tst$action[ii] == 2) {
      df.exp2_tst$ACC[ii] <- 1
   }
}

### choice portion for each stimuli
### get the choices:
df.exp2_tst$choice[df.exp2_tst$action == 1] <- df.exp2_tst$stim1[df.exp2_tst$action == 1] 
df.exp2_tst$choice[df.exp2_tst$action == 2] <- df.exp2_tst$stim2[df.exp2_tst$action == 2]

df.exp2_tst.choiceRate <- df.exp2_tst %>%
   dplyr::group_by(subjID) %>%                           # group by subject
   dplyr::count(subjID,choice) %>%                       # count the number
   dplyr::mutate(prop = prop.table(n)) %>%               # calculate the proportion
   dplyr::ungroup() %>%                                  # un-group for further data manipulation in future.
   tidyr::complete(subjID,choice,fill = list(prop = 0))  # fill the missing conditions for the subjID*choice combinations

# derive the conditions for magnitude, valuence, and favorableness
df.exp2_tst.choiceRate <- df.exp2_tst.choiceRate %>%
   dplyr::mutate(
      Mag = mosaic::derivedFactor(
         'Hi' = (choice == 1 | choice == 2 | choice == 5 | choice == 6),    # derive a new factor based on condition
         'Lo' = (choice == 3 | choice == 4 | choice == 7 | choice == 8),
         method ="first",.default = NA),
      Val = mosaic::derivedFactor(
         'pos' = (choice == 1 | choice == 2 | choice == 3 | choice == 4),
         'neg' = (choice == 5 | choice == 6 | choice == 7 | choice == 8),
         method ="first",.default = NA),
      Fav = mosaic::derivedFactor(
         'Hi' = (choice == 1 | choice == 3 | choice == 5 | choice == 7),
         'Lo' = (choice == 2 | choice == 4 | choice == 6 | choice == 8),
         method ="first",.default = NA),) %>%
   dplyr::mutate_if(is.factor, as.character)

write.csv(file = 'exp2_test_prop.csv',df.exp2_tst.choiceRate,row.names = F)

##################################
###       ANOVA                ###
##################################
exp2.test.anova <- ez::ezANOVA(data = df.exp2_tst.choiceRate, dv = prop, wid = subjID, within = c('Mag','Val','Fav'))
print(exp2.test.anova)                # reproduced the results of the table 2, page 4
