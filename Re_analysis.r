### This script is test to run the modeling of Bavard using R

# get the directory of the current script
curDir <- dirname(rstudioapi::getSourceEditorContext()$path) #Get the directory ofcurrent script
setwd(curDir)

#rm(list = setdiff(ls(), lsf.str()))  # remove all data but keep functions
rm(list = ls())

# load the library
library(R.matlab)
library(hBayesDM)
library(tidyverse)

# load the data
df1 <- R.matlab::readMat('Data_expe1.mat')
df2 <- R.matlab::readMat('Data_expe2.mat')

### extract variables from data list:
# subject list
sublist <- data.frame(matrix(unlist(df1[['subjects']]), nrow=1),stringsAsFactors=FALSE)

# conditions from matlab, 20 cells, each is 160 * 1 (the actual state, 4 x 2 session), into 160 * 20 dataframe
con     <- data.frame(matrix(as.numeric(unlist(df1[['con']])), nrow=160),stringsAsFactors=FALSE)

# 20 cells, each is 160 * 1 the choice, correct (2) or incorrect (1), into 160 * 20 dataframe
# cho     <- data.frame(matrix(unlist(df[['cho']]), nrow=160, byrow=T),stringsAsFactors=FALSE)
# byrow = True means that the data will be filled by row, which is not the case for current situation
cho     <- data.frame(matrix(unlist(df1[['cho']]), nrow=160),stringsAsFactors=FALSE)

# 20 cells, each is 160 * 1
con2    <- data.frame(matrix(unlist(df1[['con2']]), nrow=160),stringsAsFactors=FALSE)

# obtained outcome in absolute value, for each trials
out     <- data.frame(matrix(unlist(df1[['out']]), nrow=160),stringsAsFactors=FALSE)

# obtained outcome in relative value, for each trials
out2    <- data.frame(matrix(unlist(df1[['out2']]), nrow=160),stringsAsFactors=FALSE)


# 20 cells, each is 112 * 1 (actions in transfer test ) into a 112 * 20 datafame
aa      <- data.frame(matrix(unlist(df1[['aa']]), nrow=112),stringsAsFactors=FALSE)

# symbols in transfer test, 112 * 2 * 20 = 112 * 40 
ss      <- data.frame(matrix(unlist(df1[['ss']]), nrow=112),stringsAsFactors=FALSE)

#df.sub1_exp <- data.frame(matrix(ncol = 1, nrow = 160))
#colnames(df.sub1_exp) <- 'subjID'
#df.sub1_exp$subjID <- '201'
#df.sub1_exp$cond1  <- con[,1]
#df.sub1_exp$cond2  <- con2[,1]
#df.sub1_exp$choice <- cho[,1]

#df.sub1_test <- data.frame(matrix(ncol = 1, nrow = 112))
#colnames(df.sub1_test) <- 'subjID'
#df.sub1_test$subjID <- '201'
#df.sub1_test$stim1 <- ss[,1]
#df.sub1_test$stim2 <- ss[,2]
#df.sub1_test$action <- aa[,1]


### separate each participants' data
saveDir <- paste0('./exp1_subj/')
rm(tmp1, tmp2,df.exp1)
for (ii in 1:length(sublist)){
      expName <- paste(saveDir,sublist[ii],'_exp', '.csv', sep = '')
      tstName <- paste(saveDir,sublist[ii],'_test', '.csv', sep = '')
      tmp1 <- data.frame(matrix(ncol = 1, nrow = 160))
      colnames(tmp1) <- 'subjID'
      tmp1$subjID <- as.numeric(sublist[ii])
      tmp1$cond1  <- con[,ii]
      tmp1$cond2  <- con2[,ii]
      tmp1$choice <- cho[,ii]
      tmp1$absOut <- out[,ii]
      tmp1$rltOut <- out2[,ii]
      if (!exists('df.exp1')){
         df.exp1 <- tmp1
      } else {
         df.exp1 <- rbind(df.exp1,tmp1)
      }
      
      write.csv(file = expName,tmp1,row.names = F)
      
      tmp2 <- data.frame(matrix(ncol = 1, nrow = 112))
      colnames(tmp2) <- 'subjID'
      tmp2$subjID <- as.numeric(sublist[ii])
      tmp2$stim1 <- ss[,ii*2-1]
      tmp2$stim2 <- ss[,ii*2]
      tmp2$action <- aa[,ii]
      write.csv(file = tstName,tmp2,row.names = F)
      if (!exists('df.exp1_tst')){
         df.exp1_tst <- tmp2
      } else {
         df.exp1_tst <- rbind(df.exp1_tst,tmp2)
      }
}


### traditional analysis
# add condition
df.exp1$Mag[df.exp1$cond2 == 1 | df.exp1$cond2 == 3] <- 'high'      # magnitude =  1 euro
df.exp1$Mag[df.exp1$cond2 == 2 | df.exp1$cond2 == 4] <- 'low'       # magnitude =  0.1 euro
df.exp1$Val[df.exp1$cond2 == 1 | df.exp1$cond2 == 2] <- 'positive'  # gain
df.exp1$Val[df.exp1$cond2 == 3 | df.exp1$cond2 == 4] <- 'negative'  # loss
df.exp1$ACC <- df.exp1$choice - 1
df.exp1 <- df.exp1[,c("subjID", "Mag","Val", "cond1","cond2","choice","ACC","absOut","rltOut")]
write.csv(file = 'exp1_data.csv',df.exp1,row.names = F)

# ANOVA
exp1.anova <- ez::ezANOVA(df.exp1, ACC, wid = subjID, within = c('Mag','Val'),within_full = c('Mag','Val'))
print(exp1.anova)   # replicated the results of the table 1, page 3

# accuracy for each participant of each condition
Exp_Acc_indv <- df.exp1 %>%
   dplyr::group_by(subjID,cond2) %>%            # group data by subjID and cond2
   dplyr::summarise(ACC=mean(choice - 1)) %>%
   dplyr::ungroup()

Exp_Acc_sum <- Exp_Acc_indv %>%
   dplyr::group_by(cond2) %>%
   dplyr::summarise(ACC_m=mean(ACC),
                    ACC_sd=sd(ACC),
                    ACC_se=sd(ACC)/sqrt(length(ACC))) %>%
   dplyr::ungroup()


### correct choice rate:
# condition 1 (A): 75% of  1 Euro    0.75   fav
# condition 2 (B): 25% of  1 Euro    0.25   no-fav
# condition 3 (C): 75% of  0.1 Euro  0.075  fav
# condition 4 (D): 25% of  0.1 Euro  0.025  no-fav
# condition 5 (E): 25% of -0.1 Euro -0.025  fav
# condition 6 (F): 75% of -0.1 Euro -0.075  no-fav
# condition 7 (G): 25% of -1 Euro   -0.25   fav
# condition 8 (H): 75% of -0.1 Euro -0.75   no-fav
# if left condition # < right condition #, then left side is the correct answer
# if right condition # < left condition #, then right side is the correct answer
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
# get the choices:
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
#df.exp1_tst.choiceRate[,c('Mag','Val','Fav')] <- as.character(df.exp1_tst.choiceRate[,c('Mag','Val','Fav')])
exp1.test.anova <- ez::ezANOVA(data = df.exp1_tst.choiceRate, dv = prop, wid = subjID, within = c('Mag','Val','Fav'))
print(exp1.test.anova)                # reproduced the results of the table 2, page 4


# experiment 2
# name in
#fiting .mat
# s     (con) : the actual state (4 x 2 session)
#               1/5 - Gain 1  E
#               2/6 - Gain 10 c
#               3/7 - loss 1 E
#               4/8 - loss 10 c
# s2    (con3): whether or not this is a "complete feedback" state 
# a     (cho):  the choice (correct or incorrect): 
# r     (out):  obtained outcome in absolute value
# c     (cou):  counterfactual outcome in absolute value
# r2    (out2): obtained outcome in relative value
# c2    (cou2): counterfactual outcome in relative value
# aa    (aa):   actions in transfer test
# ss    (ss):   symbols in transfer test
#       (con2):
#       (subjects): subj no.
# subject list
sublist2 <- data.frame(matrix(unlist(df2[['subjects']]), nrow=1),stringsAsFactors=FALSE)

# stimuli presented, 8 types, 4 types of stim for each session, 160 * 40 dataframe
cond_s     <- data.frame(matrix(as.numeric(unlist(df2[['con']])), nrow=160),stringsAsFactors=FALSE)

# 4 conditions, based on magnitude by valence 
cond_whole <- data.frame(matrix(unlist(df2[['con2']]), nrow=160),stringsAsFactors=FALSE)

# feedback, partial or complete.
cond_inf   <- data.frame(matrix(unlist(df2[['con3']]), nrow=160),stringsAsFactors=FALSE)

# participant's choice for each trial
choice     <- data.frame(matrix(unlist(df2[['cho']]), nrow=160),stringsAsFactors=FALSE)

# obtained outcome in absolute value, for each trials
out_abs    <- data.frame(matrix(unlist(df2[['out']]), nrow=160),stringsAsFactors=FALSE)

# obtained outcome in relative value, for each trials
out_relat  <- data.frame(matrix(unlist(df2[['out2']]), nrow=160),stringsAsFactors=FALSE)

# counterfactural outcome, in absolute sense
out_c_abs   <- data.frame(matrix(unlist(df2[['cou']]), nrow=160),stringsAsFactors=FALSE)

# counterfactural outcome, in relative sense
out_c_rel <- data.frame(matrix(unlist(df2[['cou2']]), nrow=160),stringsAsFactors=FALSE)

# 20 cells, each is 112 * 1 (actions in transfer test ) into a 112 * 20 datafame
choice_test<- data.frame(matrix(unlist(df2[['aa']]), nrow=112),stringsAsFactors=FALSE)

# symbols in transfer test, 112 * 2 * 20 = 112 * 40 
stim_test  <- data.frame(matrix(unlist(df2[['ss']]), nrow=112),stringsAsFactors=FALSE)


saveDir <- paste0('./exp2_subj/')
rm(tmp1, tmp2,df.exp2)
for (ii in 1:length(sublist2)){
   expName <- paste(saveDir,sublist2[ii],'_exp', '.csv', sep = '')
   tstName <- paste(saveDir,sublist2[ii],'_test', '.csv', sep = '')
   tmp1 <- data.frame(matrix(ncol = 1, nrow = 160))
   colnames(tmp1) <- 'subjID'
   tmp1$subjID <- as.numeric(sublist2[ii])
   tmp1$cond1  <- cond_s[,ii]
   tmp1$cond2  <- cond_whole[,ii]
   tmp1$infoType <- cond_inf[,ii]
   tmp1$choice <- choice[,ii]
   tmp1$outAbs <- out_abs[,ii]
   tmp1$outRel <- out_relat[,ii]
   tmp1$out_c_abs <- out_c_abs[,ii]
   tmp1$out_c_rel <- out_c_rel[,ii]
   if (exists('df.exp2')){
      df.exp2 <- rbind(df.exp2,tmp1)
   } else {
      df.exp2 <- tmp1
   }
   
   write.csv(file = expName,tmp1,row.names = F)
   
   tmp2 <- data.frame(matrix(ncol = 1, nrow = 112))
   colnames(tmp2) <- 'subjID'
   tmp2$subjID <- as.numeric(sublist2[ii])
   tmp2$stim1 <- stim_test[,ii*2-1]
   tmp2$stim2 <- stim_test[,ii*2]
   tmp2$choice <- choice_test[,ii]
   write.csv(file = tstName,tmp2,row.names = F)
   if (exists('df.exp2_tst')){
      df.exp2_tst <- tmp2
   } else {
      df.exp2_tst <- rbind(df.exp2_tst,tmp2)
   }
}

### traditional analysis
# add condition
df.exp2$Mag[df.exp2$cond2 == 1 | df.exp2$cond2 == 3] <- 'high'      # magnitude =  1 euro
df.exp2$Mag[df.exp2$cond2 == 2 | df.exp2$cond2 == 4] <- 'low'       # magnitude =  0.1 euro
df.exp2$Val[df.exp2$cond2 == 1 | df.exp2$cond2 == 2] <- 'positive'  # gain
df.exp2$Val[df.exp2$cond2 == 3 | df.exp2$cond2 == 4] <- 'negative'  # loss
df.exp2$Info[df.exp2$infoType == 0] <- 'partial'
df.exp2$Info[df.exp2$infoType == 1] <- 'complete'
df.exp2$ACC <- df.exp2$choice - 1

df.exp2 <- df.exp2[,c("subjID", "Mag","Val",'Info','infoType',"cond1","cond2","choice","ACC","outAbs",
                      "outRel",'out_c_abs','out_c_rel')]
write.csv(file = 'exp2_data.csv',df.exp2,row.names = F)

# ANOVA
exp2.anova <- ez::ezANOVA(df.exp2, ACC, wid = subjID,
                          within = c('Mag','Val','Info'),
                          within_full = c('Mag','Val','Info'))
print(exp2.anova)    # replicated the results of the table 1, page 3
