install.packages("ggplot2")


library(tidyverse)
library(ggplot2)
library(janitor)
library(dplyr)
library(Hmisc)
library(ggpubr)

library(data.table)

memory.limit(100000)

#Load data set

bhdp <- read.csv("DEID_BHDP_TBI_Surveys.csv", header = TRUE, sep= ",")

bamc <- read.csv("BAMC_NSIComp_Manip.csv", header =  TRUE, sep = ",")

nicoe <- read.csv("NICOE_data_limitedCols_DEID.csv", header = TRUE, sep = ",")

demo <- read.csv("ANDE_PT_demo_03032020.csv", header = TRUE, sep = ",")

names(nicoe)[2] <- "parent_dmis_key_name"

nicoesubset <- nicoe[, c(217, 1:2, 19:20, 54,63,70, 71:94, 115, 143, 159, 170, 196)]

bhdp[ bhdp == 888 ] <- NA

bhdp$NSI.GuidanceText_V1 <- NULL

bhdp[,1:139] <- sapply(bhdp[,1:139],as.character)
bhdp[,1:139] <- sapply(bhdp[,1:139],as.numeric)


###SUM SCORES FOR ORIGINAL BHDP DATA


#Hit
bhdp<- bhdp %>%
  mutate(HIT6_Score = select(., 23:29) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(bhdp[,23:29])))%>%
  select(HIT6_Score, everything())

#phq9
bhdp<- bhdp %>%
  mutate(PHQ9_Score = select(., 31:39) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(bhdp[,31:39])))%>%
  select(PHQ9_Score, everything())

#audit
bhdp<- bhdp%>%
  mutate(AUDIT_Score = select(., 49:58) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(bhdp[,49:58])))%>%
  select(AUDIT_Score, everything())

#psqi

#ISI
bhdp<- bhdp%>%
  mutate(ISI_Score = select(., 85:91) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(bhdp[,85:91])))%>%
  select(ISI_Score, everything())


#GAD
bhdp<- bhdp%>%
  mutate(GAD_Score = select(., 93:99) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(bhdp[,93:99])))%>%
  select(GAD_Score, everything())

#PCL

bhdp<- bhdp%>%
  mutate(pcl520_tot_score = select(., 101:122) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(bhdp[,101:122])))%>%
  select(pcl520_tot_score, everything())

#QOL

#NSI vestibular
bhdp<- bhdp %>%
  mutate(NSI_F1_Vestibular = select(., c('NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1')) %>% rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(bhdp[,c('NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1')])))%>%
  select(NSI_F1_Vestibular, everything())

nsi$NSI_F1_Vestibular  <-  nsi[['NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1']].mean(axis=1)

#NSI Cognative
bhdp<- bhdp %>%
  mutate(NSI_F2_Cognative = select(., c('NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(bhdp[,c('NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1')])))%>%
  select(NSI_F2_Cognative, everything())    

nsi['NSI_F2_Cognative'] = nsi[['NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1']].mean(axis=1)

#NSI Affective
bhdp<- bhdp %>%
  mutate(NSI_F3_Affective = select(., c('NSI.Fatigue_V1','NSI.Sleep_Difficulty_V1','NSI.Anxious_V1','NSI.Depressed_V1','NSI.Irritability_V1', 'NSI.Overwhelmed_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(bhdp[,c('NSI.Fatigue_V1','NSI.Sleep_Difficulty_V1','NSI.Anxious_V1','NSI.Depressed_V1','NSI.Irritability_V1', 'NSI.Overwhelmed_V1')])))%>%
  select(NSI_F3_Affective, everything()) 

nsi['NSI_F3_Affective'] = nsi[['NSI:Fatigue_V1','NSI:Sleep_Difficulty_V1','NSI:Anxious_V1','NSI:Depressed_V1','NSI:Irritability_V1', 'NSI:Overwhelmed_V1']].mean(axis=1)


#NSI Sensory
bhdp<- bhdp %>%
  mutate(NSI_F4_Sensory = select(., c('NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(bhdp[,c('NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1')])))%>%
  select(NSI_F4_Sensory, everything())  

nsi['NSI_F4_Sensory'] = nsi[['NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1']].mean(axis=1)

#NSI Validity

bhdp<- bhdp %>%
  mutate(NSI_Validity_10 = select(., c('NSI.Dizzy_V1','NSI.Balance_V1','NSI.Clumsy_V1','NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Noise_Sensitivity_V1','NSI.Hearing_Difficulty','NSI.Taste_Smell_Change_V1','NSI.Slow_Thinking_V1','NSI.Decision_Difficulty_V1')) %>% 
           rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(bhdp[,c('NSI.Dizzy_V1','NSI.Balance_V1','NSI.Clumsy_V1','NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Noise_Sensitivity_V1','NSI.Hearing_Difficulty','NSI.Taste_Smell_Change_V1','NSI.Slow_Thinking_V1','NSI.Decision_Difficulty_V1')])))%>%
  select(NSI_Validity_10, everything())  

nsi['NSI_Validity_10'] = nsi[['NSI.Dizzy_V1','NSI.Balance_V1','NSI.Clumsy_V1','NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Noise_Sensitivity_V1','NSI.Hearing_Difficulty','NSI.Taste_Smell_Change_V1','NSI.Slow_Thinking_V1','NSI.Decision_Difficulty_V1']].sum(axis=1)
return nsi

the validity 10 is a validity score, if the validity 10 is 22 or more it is considered an over reported survey



#NSI total
bhdp<- bhdp %>%
  mutate(NSI_TotalScore = select(., 12:33) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(bhdp[,12:33])))%>%
  select(NSI_TotalScore, everything())


bhdp_sub <-  (bhdp[, c(160,154, 156, 159, 1:12 )])  

#Subset all surveys seperately into seperate dataframes

bhdp[ bhdp == 888 ] <- NA

nsi <-  (bhdp[, c(1:23, 143:149)])

hit <- (bhdp[, c(24:30, 143:149)])

phq9 <- (bhdp[, c(31:47, 143:149)])

phq9m <- (bhdp[, c(40:47, 143:149)])

audit <-  (bhdp[, c(48:57, 143:149)])

psqi <-  (bhdp[, c(58:82, 143:149)])

isi <-  (bhdp[, c(83:89, 143:149)])

gad <-  (bhdp[, c(90:96, 143:149)])

pcl <-  (bhdp[, c(97:116, 143:149)])

qol <-  (bhdp[, c(117:142, 143:149)])




#NSI Correlations and Means
nsi[,1:23] <- sapply(nsi[,1:23],as.character)
nsi[,1:23] <- sapply(nsi[,1:23],as.numeric)
nsi[ nsi == 888 ] <- NA


nsi_sub <- nsi[,1:23]  



nsi$NSI.GuidanceText_V1 <- NULL


#HIT correlations and means

hit[,1:7] <- sapply(hit[,1:7],as.character)
hit[,1:7] <- sapply(hit[,1:7],as.numeric)
hit[ hit == 888 ] <- NA


hit_sub <- hit[,1:7]  

hit_sub[ hit_sub == 888 ] <- NA

summary(hit_sub)

sapply(levels)

view(colMeans(hit_sub, na.rm = TRUE))

view(cor(hit_sub, use="pairwise.complete.obs"))

 
##PHQ9 cor and means

phq9[,1:17] <- sapply(phq9[,1:17],as.character)
phq9[,1:17] <- sapply(phq9[,1:17],as.numeric)
phq9[ phq9 == 888 ] <- NA

phq9_sub <- phq9[,1:17]  

phq9_sub[ phq9_sub == 888 ] <- NA

summary(phq9_sub)

view(colMeans(phq9_sub, na.rm = TRUE))

view(cor(phq9_sub, use="pairwise.complete.obs"))


## PHQ9M cor and means 

phq9m[,1:8] <- sapply(phq9m[,1:8],as.numeric)

phq9m_sub <- phq9[,1:8]  

phq9m_sub[ phq9m_sub == 888 ] <- NA

summary(phq9_sub)

view(colMeans(phq9_sub, na.rm = TRUE))

view(cor(phq9m_sub, use="pairwise.complete.obs"))

##audit means and cor

audit[,1:10] <- sapply(audit[,1:10],as.character)
audit[,1:10] <- sapply(audit[,1:10],as.numeric)
audit[ audit == 888 ] <- NA

audit_sub <- audit[,1:10]  

audit_sub[ audit_sub == 888 ] <- NA

summary(audit_sub)

view(colMeans(audit_sub, na.rm = TRUE))

view(cor(audit_sub, use="pairwise.complete.obs"))


##PSQI means and cor****

psqi[,c(1:2, 4:8, 10:13, 16:20, 22:23, 25)] <- sapply(psqi[,c(1:2, 4:8, 10:13, 16:20, 22:23, 25)],as.character)
psqi[,c(1:2, 4:8, 10:13, 16:20, 22:23, 25)] <- sapply(psqi[,c(1:2, 4:8, 10:13, 16:20, 22:23, 25)],as.numeric)

psqi[ psqi == 888 ] <- NA

psqi_sub <- psqi[,c(1:2, 4:8, 10:13, 16:20, 22:23, 25)]  




## ISI means and cor

isi[,1:7] <- sapply(isi[,1:7],as.character)

isi[,1:7] <- sapply(isi[,1:7],as.numeric)
isi[ isi == 888 ] <- NA


isi_sub <- isi[,1:7]  



## GAD means and cor

gad[,1:7] <- sapply(gad[,1:7],as.character)
gad[,1:7] <- sapply(gad[,1:7],as.numeric)
gad[ gad == 888 ] <- NA

gad_sub <- gad[,1:7]  


## PCL means and cor

pcl[,1:20] <- sapply(pcl[,1:20],as.character)
pcl[,1:20] <- sapply(pcl[,1:20],as.numeric)
pcl[ pcl == 888 ] <- NA

pcl_sub <- pcl[,1:20]  




## QOL means and cor

qol[,1:26] <- sapply(qol[,1:26],as.character)
qol[,1:26] <- sapply(qol[,1:26],as.numeric)
qol[ qol == 888 ] <- NA

qol_sub <- qol[,1:26]  





##plots

matplot(gad_sub, type = "p")

library(corrplot)

corrplot((cor(gad_sub, isi_sub, use ="pairwise.complete.obs" )),type = "upper", order = "hclust")


## Summation score creation


#Hit
 hit<- hit %>%
       mutate(Total = select(., 1:7) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(hit[,1:7])))%>%
       select(Total, everything())

#phq9
 phq9<- phq9 %>%
       mutate(Total = select(., 1:17) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(phq9[,1:17])))%>%
       select(Total, everything())

#audit
 audit<- audit%>%
       mutate(Total = select(., 1:10) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(audit[,1:10])))%>%
       select(Total, everything())

#psqi

#ISI
 isi<- isi %>%
       mutate(Total = select(., 1:7) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(isi[,1:7])))%>%
       select(Total, everything())


#GAD
 gad<- gad %>%
  mutate(Total = select(., 1:7) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(gad[,1:7])))%>%
    select(Total, everything())

#PCL

 pcl<- pcl %>%
       mutate(Total = select(., 1:20) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(pcl[,1:20])))%>%
       select(Total, everything())

#QOL
 
 
 ### Factor Scores
 def add_factor_score(nsi):
  
   #NSI vestibular
   nsi<- nsi %>%
   mutate(NSI_F1_Vestibular = select(., c('NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1')) %>% rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nsi[,c('NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1')])))%>%
   select(NSI_F1_Vestibular, everything())
 
    nsi$NSI_F1_Vestibular  <-  nsi[['NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1']].mean(axis=1)

#NSI Cognative
    nsi<- nsi %>%
      mutate(NSI_F2_Cognative = select(., c('NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1')) %>% 
               rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nsi[,c('NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1')])))%>%
      select(NSI_F2_Cognative, everything())    
    
  nsi['NSI_F2_Cognative'] = nsi[['NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1']].mean(axis=1)

#NSI Affective
  nsi<- nsi %>%
    mutate(NSI_F3_Affective = select(., c('NSI.Fatigue_V1','NSI.Sleep_Difficulty_V1','NSI.Anxious_V1','NSI.Depressed_V1','NSI.Irritability_V1', 'NSI.Overwhelmed_V1')) %>% 
             rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nsi[,c('NSI.Fatigue_V1','NSI.Sleep_Difficulty_V1','NSI.Anxious_V1','NSI.Depressed_V1','NSI.Irritability_V1', 'NSI.Overwhelmed_V1')])))%>%
    select(NSI_F3_Affective, everything()) 
  
  nsi['NSI_F3_Affective'] = nsi[['NSI:Fatigue_V1','NSI:Sleep_Difficulty_V1','NSI:Anxious_V1','NSI:Depressed_V1','NSI:Irritability_V1', 'NSI:Overwhelmed_V1']].mean(axis=1)

  
#NSI Sensory
  nsi<- nsi %>%
    mutate(NSI_F4_Sensory = select(., c('NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1')) %>% 
             rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nsi[,c('NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1')])))%>%
    select(NSI_F4_Sensory, everything())  
  
   nsi['NSI_F4_Sensory'] = nsi[['NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1']].mean(axis=1)

#NSI Validity
   
   nsi<- nsi %>%
     mutate(NSI_Validity_10 = select(., c('NSI.Dizzy_V1','NSI.Balance_V1','NSI.Clumsy_V1','NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Noise_Sensitivity_V1','NSI.Hearing_Difficulty','NSI.Taste_Smell_Change_V1','NSI.Slow_Thinking_V1','NSI.Decision_Difficulty_V1')) %>% 
              rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nsi[,c('NSI.Dizzy_V1','NSI.Balance_V1','NSI.Clumsy_V1','NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Noise_Sensitivity_V1','NSI.Hearing_Difficulty','NSI.Taste_Smell_Change_V1','NSI.Slow_Thinking_V1','NSI.Decision_Difficulty_V1')])))%>%
     select(NSI_Validity_10, everything())  
   
    nsi['NSI_Validity_10'] = nsi[['NSI.Dizzy_V1','NSI.Balance_V1','NSI.Clumsy_V1','NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Noise_Sensitivity_V1','NSI.Hearing_Difficulty','NSI.Taste_Smell_Change_V1','NSI.Slow_Thinking_V1','NSI.Decision_Difficulty_V1']].sum(axis=1)
 return nsi
 
 the validity 10 is a validity score, if the validity 10 is 22 or more it is considered an over reported survey
 
 #NSI total
 nsi<- nsi %>%
   mutate(NSI_TotalScore = select(., 6:27) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nsi[,6:27])))%>%
   select(NSI_TotalScore, everything())
 
 bhdp_cor <-  full_join(nsi, hit, phq9, audit, isi, gad, pcl, by= c('SubjectID','start_date', 'parent_dmis_key_name'))
 
 bhdp_cor <- rbind(c(nsi[,1:5], hit$Total, phq9$Total, audit$Total, isi$Total, gad$Total, pcl$Total))
  <- do.call("rbind", list(nsi, hit$Total, phq9$Total, audit$Total, isi$Total, gad$Total, pcl$Total))
 
 bhdp_cor <- Reduce(merge, list(nsi[,1:5], hit$Total, phq9$Total, audit$Total, isi$Total, gad$Total, pcl$Total))
 
 bhdp_cor <- Reduce(function(x, y) merge(x, y, all=TRUE), list(nsi, hit$Total, phq9$Total, audit$Total, isi$Total, gad$Total, pcl$Total))
 
 rm(bhdp_cor)
 bhdp_cor <- nsi[, c(1:6, 29)]
 
 
bhdp_cor$HIT6_Score <- hit$Total 
bhdp_cor$PHQ9_Score <- phq9$Total
bhdp_cor$AUDIT_Score <-  audit$Total
bhdp_cor$ISI_Score <-  isi$Total
bhdp_cor$GAD_Score <-  gad$Total
bhdp_cor$pcl520_tot_score <-  pcl$Total

view(cor(bhdp_cor[,1:6], bhdp_cor[,7:12 ], use ="pairwise.complete.obs" ))

## NSI factor scores in NICOE data

#NSI vestibular
nicoesubset<- nicoesubset %>%
  mutate(NSI_F1_Vestibular = select(., c('NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nicoesubset[,c('NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1')])))%>%
  select(NSI_F1_Vestibular, everything())

nsi$NSI_F1_Vestibular  <-  nsi[['NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1']].mean(axis=1)

#NSI Cognative
nicoesubset<- nicoesubset %>%
  mutate(NSI_F2_Cognative = select(., c('NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nicoesubset[,c('NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1')])))%>%
  select(NSI_F2_Cognative, everything())    

nsi['NSI_F2_Cognative'] = nsi[['NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1']].mean(axis=1)

#NSI Affective
nicoesubset<- nicoesubset %>%
  mutate(NSI_F3_Affective = select(., c('NSI.Fatigue_V1','NSI.Sleep_Difficulty_V1','NSI.Anxious_V1','NSI.Depressed_V1','NSI.Irritability_V1', 'NSI.Overwhelmed_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nicoesubset[,c('NSI.Fatigue_V1','NSI.Sleep_Difficulty_V1','NSI.Anxious_V1','NSI.Depressed_V1','NSI.Irritability_V1', 'NSI.Overwhelmed_V1')])))%>%
  select(NSI_F3_Affective, everything()) 

nsi['NSI_F3_Affective'] = nsi[['NSI:Fatigue_V1','NSI:Sleep_Difficulty_V1','NSI:Anxious_V1','NSI:Depressed_V1','NSI:Irritability_V1', 'NSI:Overwhelmed_V1']].mean(axis=1)


#NSI Sensory
nicoesubset<- nicoesubset %>%
  mutate(NSI_F4_Sensory = select(., c('NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nicoesubset[,c('NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1')])))%>%
  select(NSI_F4_Sensory, everything())  

nsi['NSI_F4_Sensory'] = nsi[['NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1']].mean(axis=1)

nicoesubset[,13:34] <- NULL


bhdp_nicoe_comb <- merge(bhdp_cor, nicoesubset, all = TRUE)

bhdp_bamc_nicoe <- merge(bhdp_nicoe_comb, bamc, all = TRUE)

combined <-   Reduce(function(x,y) merge(x,y,by="name",all=TRUE) ,list(bhdp_sub, nicoesubset, bamc))


##rename IDs for other dataframes

names(bamc)[9] <- "SubjectID"


nicoe_bhdp <-merge(bhdp_sub, nicoesubset, all = TRUE)

total <- merge(nicoe_bhdp, bamc, all= TRUE)

total_sub <- total[, c(1:15, 17:22)]

rm(bhdp_nicoe_comb)

master_cor <- bhdp_bamc_nicoe[, c(10, 1:8, 11:20)]

rm(master_cor)
