install.packages("lubridate")

install.packages("anytime")

library(lubridate)
library(anytime)

rm(nicoe)

nicoe$abc_tot_score <- NULL
nicoe$dhi_tot_score<- NULL
nicoe$epworth_tot_score<- NULL
nicoe$GAD_Score<- NULL
nicoe$HIT6_Score<- NULL
nicoe$NSI_TotalScore<- NULL
nicoe$NSI_Validity_10<- NULL
nicoe$pcl520_tot_score<- NULL
nicoe$pclm_tot_score<- NULL
nicoe$PHQ9_Score<- NULL
nicoe$PSQI_Score<- NULL


#abc
nicoe<- nicoe %>%
  mutate(abc_tot_score = select(., 3:18) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,3:18])))%>%
  select(abc_tot_score, everything())

#dhi
nicoe<- nicoe %>%
  mutate(dhi_tot_score = select(., 20:44) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,20:44])))%>%
  select(dhi_tot_score, everything())

#epworth
nicoe<- nicoe %>%
  mutate(epworth_tot_score = select(., 46:53) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,46:53])))%>%
  select(epworth_tot_score, everything())

#GAD
nicoe<- nicoe%>%
  mutate(GAD_Score = select(., 55:61) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,55:61])))%>%
  select(GAD_Score, everything())

#Hit
nicoe<- nicoe %>%
  mutate(HIT6_Score = select(., 64:69) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,64:69])))%>%
  select(HIT6_Score, everything())


#PCL

nicoe<- nicoe%>%
  mutate(pcl520_tot_score = select(., 93:112) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,93:112])))%>%
  select(pcl520_tot_score, everything())

#pclm
nicoe<- nicoe %>%
  mutate(pclm_tot_score = select(., 124:140) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,124:140])))%>%
  select(pclm_tot_score, everything())





#phq9
nicoe<- nicoe %>%
  mutate(PHQ9_Score = select(., 158:166) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,158:166])))%>%
  select(PHQ9_Score, everything())


#psqi









#NSI vestibular
nicoe<- nicoe %>%
  mutate(NSI_F1_Vestibular = select(., c('NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1')) %>% rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nicoe[,c('NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1')])))%>%
  select(NSI_F1_Vestibular, everything())

nsi$NSI_F1_Vestibular  <-  nsi[['NSI.Clumsy_V1','NSI.Dizzy_V1', 'NSI.Balance_V1']].mean(axis=1)

#NSI Cognative
nicoe<- nicoe %>%
  mutate(NSI_F2_Cognative = select(., c('NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nicoe[,c('NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1')])))%>%
  select(NSI_F2_Cognative, everything())    

nsi['NSI_F2_Cognative'] = nsi[['NSI.Distracted_V1','NSI.Forgetfulness_V1','NSI.Decision_Difficulty_V1','NSI.Slow_Thinking_V1']].mean(axis=1)

#NSI Affective
nicoe<- nicoe %>%
  mutate(NSI_F3_Affective = select(., c('NSI.Fatigue_V1','NSI.Sleep_Difficulty_V1','NSI.Anxious_V1','NSI.Depressed_V1','NSI.Irritability_V1', 'NSI.Overwhelmed_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nicoe[,c('NSI.Fatigue_V1','NSI.Sleep_Difficulty_V1','NSI.Anxious_V1','NSI.Depressed_V1','NSI.Irritability_V1', 'NSI.Overwhelmed_V1')])))%>%
  select(NSI_F3_Affective, everything()) 

nsi['NSI_F3_Affective'] = nsi[['NSI:Fatigue_V1','NSI:Sleep_Difficulty_V1','NSI:Anxious_V1','NSI:Depressed_V1','NSI:Irritability_V1', 'NSI:Overwhelmed_V1']].mean(axis=1)


#NSI Sensory
nicoe<- nicoe %>%
  mutate(NSI_F4_Sensory = select(., c('NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1')) %>% 
           rowMeans(na.rm = TRUE)*NA^!rowMeans(!is.na(nicoe[,c('NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1')])))%>%
  select(NSI_F4_Sensory, everything())  

nsi['NSI_F4_Sensory'] = nsi[['NSI.Headaches_V1', 'NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Light_Sensitivity_V1','NSI.Noise_Sensitivity_V1','NSI.Numbness_V1','NSI.Taste_Smell_Change_V1']].mean(axis=1)

#NSI Validity

nicoe<- nicoe %>%
  mutate(NSI_Validity_10 = select(., c('NSI.Dizzy_V1','NSI.Balance_V1','NSI.Clumsy_V1','NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Noise_Sensitivity_V1','NSI.Hearing_Difficulty','NSI.Taste_Smell_Change_V1','NSI.Slow_Thinking_V1','NSI.Decision_Difficulty_V1')) %>% 
           rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,c('NSI.Dizzy_V1','NSI.Balance_V1','NSI.Clumsy_V1','NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Noise_Sensitivity_V1','NSI.Hearing_Difficulty','NSI.Taste_Smell_Change_V1','NSI.Slow_Thinking_V1','NSI.Decision_Difficulty_V1')])))%>%
  select(NSI_Validity_10, everything())  

nsi['NSI_Validity_10'] = nsi[['NSI.Dizzy_V1','NSI.Balance_V1','NSI.Clumsy_V1','NSI.Nausea_V1','NSI.Vision_Problems_V1','NSI.Noise_Sensitivity_V1','NSI.Hearing_Difficulty','NSI.Taste_Smell_Change_V1','NSI.Slow_Thinking_V1','NSI.Decision_Difficulty_V1']].sum(axis=1)
return nsi

the validity 10 is a validity score, if the validity 10 is 22 or more it is considered an over reported survey



#NSI total
nicoe<- nicoe %>%
  mutate(NSI_TotalScore = select(., 79:100) %>% rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(nicoe[,79:100])))%>%
  select(NSI_TotalScore, everything())


##Add data source to each dataframe

nicoe$data_source <- "nicoe"
bhdp$data_source <- "bhdp"
bamc$data_source <- "bamc"


bhdp$start_date <-anydate(bhdp$start_date)
bamc$start_date <- anydate(bamc$start_date)
nicoe$start_date <- anydate(nicoe$start_date)

demo$start_date <- anydate(demo$start_date)

# CREATE UNFILTERED MASTER datbase

master_nicoe_bhdp <-merge(bhdp, nicoe, all = TRUE)

memory.limit(80000)

master_set <- merge(master_nicoe_bhdp, bamc, all= TRUE)

master_set$NSI_Validity_10[ master_set$NSI_Validity_10 > 21 ] <- NA

master_set$NSI_TotalScore[master_set$NSI_TotalScore > 88] <- NA

master_set[ master_set  == 888 ] <- NA


## APPEND WHOQOL

##Revalue select QOL columns

library(plyr)

master_set$QOL.Pain_Limitations_V1 <- as.numeric(revalue(as.character(master_set$QOL.Pain_Limitations_V1), c("1" = "5", "2" = "4", "3"="3", "4"="2", "5"="1")))



#QOL Physical_Health
master_set<- master_set %>%
  mutate(qol.d1 = select(., c('QOL.Pain_Limitations_V1','QOL.Medical_Treatment_V1','QOLL.Energy_Level_V1','QOL.Mobility_V1','QOL.Sleep_Satisfaction_V1','QOL.Work_Capacity_Satisfaction_V1','QOL.Daily_Activities_Satisfaction_V1'))%>% 
              rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(master_set[,c('QOL.Pain_Limitations_V1','QOL.Medical_Treatment_V1','QOLL.Energy_Level_V1','QOL.Mobility_V1','QOL.Sleep_Satisfaction_V1','QOL.Work_Capacity_Satisfaction_V1','QOL.Daily_Activities_Satisfaction_V1')])))%>%
  mutate(qol_physical_health= ((qol.d1-7)/28)*100)

#QOL Pyschological
  master_set<- master_set %>%
  mutate(qol.d2 = select(., c('QOL.Life_Enjoyment_V1','QOL.Meaningful_Life_V1','QOL.Concentration_V1','QOL.Accept_Bodily_Appearance_V1', 'QOL.Negative_Feelings_Frequency_V1','QOL.Ability_Satisfaction_V1')) %>% 
           rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(master_set[,c('QOL.Life_Enjoyment_V1','QOL.Meaningful_Life_V1','QOL.Concentration_V1','QOL.Accept_Bodily_Appearance_V1', 'QOL.Negative_Feelings_Frequency_V1','QOL.Ability_Satisfaction_V1')])))%>%
  mutate(qol_psychological= ((qol.d2-6)/24)*100)

#QOL Social_Relations
master_set<- master_set %>%
  mutate(qol.d3 = select(., c('QOL.Relationship_Satisfaction_V1','QOL.Sex_Life_Satisfaction_V1','QOL.Friend_Satisfaction_V1')) %>% 
           rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(master_set[,c('QOL.Relationship_Satisfaction_V1','QOL.Sex_Life_Satisfaction_V1','QOL.Friend_Satisfaction_V1')])))%>%
  mutate(qol_social_relations= ((qol.d3-3)/12)*100)


#QOL Enviornment
master_set<- master_set %>%
  mutate(qol.d4 = select(., c('QOL.Feel_Safe_V1','QOL.Healthy_Physcial_Environmnet_V1','QOL.Money_to_Meet_Needs_V1','QOL.Information_to_Meet_Needs_V1','QOL.Leisure_Activities_Opportunity_V1','QOL.Living_Condition_Satisfaction_V1','QOL.Health_Service_Access_V1','QOL.Transporation_Satisfaction_V1')) %>% 
           rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(master_set[,c('QOL.Feel_Safe_V1','QOL.Healthy_Physcial_Environmnet_V1','QOL.Money_to_Meet_Needs_V1','QOL.Information_to_Meet_Needs_V1','QOL.Leisure_Activities_Opportunity_V1','QOL.Living_Condition_Satisfaction_V1','QOL.Health_Service_Access_V1','QOL.Transporation_Satisfaction_V1')])))%>%
  mutate(qol_enviornment=((qol.d4-8)/32)*100)


#QOL TOTAL
master_set<- master_set %>%
  mutate(qol_total = select(., c('qol_physical_health','qol_psychological', 'qol_social_relations', 'qol_enviornment')) %>%
           rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(master_set[,c('qol_physical_health','qol_psychological', 'qol_social_relations', 'qol_enviornment')])))
 

master_set<- master_set %>%
 mutate(qol_q_count =  (rowSums(!is.na(master_set[,c( 134:159)]), na.rm = TRUE)))

master_set%>%
  filter(qol_q_count>= 21)%>%
  mutate(qol_total = select(., c('qol_physical_health','qol_psychological', 'qol_social_relations', 'qol_enviornment')) %>%
           rowSums(na.rm = TRUE)*NA^!rowSums(!is.na(master_set[,c('qol_physical_health','qol_psychological', 'qol_social_relations', 'qol_enviornment')])))


master_set <-  master_set%>%
  
  mutate(qol_total= rowSums(master_set[, c(305, 307, 309, 311)], na.rm = TRUE))  

ggplot(master_set, mapping = aes(qol_total))+
  geom_histogram()

master_set%>%
  group_by(qol_total)%>%
  summarise(count= tally(qol_total))

range(master_set$qol_total)


bhdp[ bhdp == 888 ] <- NA

master_set$qol_total[master_set$qol_q_count<22] <- NA

#applyfilters
master_set$NSI_TotalScore[master_set$NSI_TotalScore<0 | master_set$NSI_TotalScore>88] <- NA
master_set$HIT6_Score[master_set$HIT6_Score<36 | master_set$HIT6_Score>78] <- NA
master_set$PHQ9_Score[master_set$PHQ9_Score<0 | master_set$PHQ9_Score>27] <- NA
master_set$ISI_Score[master_set$ISI_Score<0 | master_set$ISI_Score>28] <- NA
master_set$GAD_Score[master_set$GAD_Score<0 | master_set$GAD_Score>10] <- NA
master_set$AUDIT_Score[master_set$AUDIT_Score<0 | master_set$AUDIT_Score>40] <- NA
master_set$abc_tot_score[master_set$abc_tot_score<0 | master_set$abc_tot_score>100] <- NA
master_set$dhi_tot_score[master_set$dhi_tot_score<0 | master_set$dhi_tot_score>100] <- NA    
master_set$epworth_tot_score[master_set$epworth_tot_score<0 | master_set$epworth_tot_score>24] <- NA    
master_set$pcl520_tot_score[master_set$pcl520_tot_score<0 | master_set$pcl520_tot_score>80] <- NA
master_set$pclm_tot_score[master_set$pclm_tot_score<17 | master_set$pclm_tot_score>85] <- NA


rm(nsi_count, nis_count)
view(t(
  
  nsi <- master_set[,c(1:6, 9, 10, 13, 18:38)]
  
  nsi_count <- (master_set[,c(1:6, 9, 13, 18:38)] %>%
       group_by(data_source)%>%
  summarise_all(funs(sum(!is.na(.)))))  ))

nsi_count <- nsi_count%>%
  adorn_totals("row")




ggplot(master_set, mapping = aes(x=start_date))+
  geom_histogram(fill= "dodgerblue4")+
  facet_wrap(~data_source)+
  labs(x= "time", y="Survey Counts")+
  ggtitle("Survey Counts over Time between Different Data Sources")


ggplot(master_set, mapping = aes(x= start_date, y= NSI_TotalScore))+
  geom_bar(stat = "identity", fill= "dodgerblue4")+
  facet_wrap(~data_source)+
  ylim(0, 1500)+
  labs(x="time", y="NSI TotalScore Count")+
  ggtitle("Count of NSI TotalScores over Time by Data Sources")

[,c(13, 9, 18:38)]

master_set %>%
  gather("value", c(NSI.Headaches_V1, NSI.Anxious_V1:NSI.Vision_Problems_V1 ))%>%
  group_by(value)%>%
  summarise_all(funs(sum(!is.na(.))))%>%
  adorn_totals("row")

rm(nsi)
nsi <-master_set[,c(18:38, 9)]

nsi <- nsi%>%
  mutate(nsi_q_count= rowSums(. ==. ))

nsi$nsi_q_count <-  (rowSums(!is.na(nsi)))


master_set[,c(18:38, 9)]%>%
  mutate(nsi_q_count =  (rowSums(!is.na(nsi))))%>%
  ggplot(mapping = aes(nsi_q_count))+
  geom_histogram(fill="dodgerblue4")+
  labs(x="Number of Questions Answered", y="Count")+
  ggtitle("Distribution of the Amount of Questions Answered in NSI")

nsi <-  master_set[,c(1, 18:38, 9)]%>%
       mutate(nsi_q_count =  (rowSums(!is.na(master_set[,c(18:38, 9)]))))%>%
  filter(NSI_TotalScore != "NA")%>%
  select(, 2:24)%>%
  group_by(nsi_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")

  

hit6 <- 
  
  master_set[,c(1, 8,  39:44)]%>%
  mutate(hit6_q_count =  (rowSums(!is.na(master_set[,c( 39:44)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore != "NA")%>%
  filter(HIT6_Score!="NA")%>%
  select(, 3:9)%>% 
  group_by(hit6_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")



gad <- 


master_set[,c(1, 16,  77:83)]%>%
mutate(gad7_q_count =  (rowSums(!is.na(master_set[,c( 77:83)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(GAD_Score!="NA")%>%
  select(, 3:10)%>% 
  group_by(gad7_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")

phq9 <- 
view(
master_set[,c(1, 17,  45:53)]%>%
  mutate(phq9_q_count =  (rowSums(!is.na(master_set[,c( 45:53)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(PHQ9_Score!="NA")%>%
  select(, 3:12)%>% 
  group_by(phq9_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")


ISI <- 
  

  master_set[,c(1, 84,  107:113)]%>%
  mutate(isi_q_count =  (rowSums(!is.na(master_set[,c( 107:113)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(ISI_Score!="NA")%>%
  select(, 3:10)%>% 
  group_by(isi_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")  
  

audit <- 
  
  master_set[,c(1, 85,  95:104)]%>%
  mutate(audit_q_count =  (rowSums(!is.na(master_set[,c( 95:104)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(AUDIT_Score!="NA")%>%
  select(, 3:13)%>% 
  group_by(audit_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")  
  

pcl5 <-  114:133
  view(
  master_set[,c(1, 7, 114:133,  216:235  )]%>%
  mutate(pcl5_q_count =  (rowSums(!is.na(master_set[,c(114:133, 216:235)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(pcl520_tot_score!="NA")%>%
  select(, 3:43)%>% 
  group_by(pcl5_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")  
  
  
pclm <- 
  
  master_set[,c(1, 14, 246:262)]%>%
  mutate(pclm_q_count =  (rowSums(!is.na(master_set[,c( 246:262)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(pclm_tot_score!="NA")%>%
  select(, 3:20)%>% 
  group_by(pclm_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")  


abc <- 
  
  
  master_set[,c(1, 165,  166:181)]%>%
  mutate(abc_q_count =  (rowSums(!is.na(master_set[,c( 166:181)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(abc_tot_score!="NA")%>%
  select(, 3:19)%>% 
  group_by(abc_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")  


dhi <- 
  
  
  master_set[,c(1, 15,  182:206)]%>%
  mutate(dhi_q_count =  (rowSums(!is.na(master_set[,c( 182:206)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(dhi_tot_score!="NA")%>%
  select(, 3:28)%>% 
  group_by(dhi_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")  


epworth <- 
  
  master_set[,c(1, 164,  207:214)]%>%
  mutate(epworth_q_count =  (rowSums(!is.na(master_set[,c( 207:214)]), na.rm = TRUE)))%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(epworth_tot_score!="NA")%>%
  select(, 3:11)%>% 
  group_by(epworth_q_count)%>%
  summarise(count= n())%>%
  adorn_totals("row")  

master_set%>%
  group_by(NSI_TotalScore)%>%
  mutate(count= n())%>%
ggplot(mapping = aes(x=NSI_TotalScore, y= count))+
  stat_summary(geom= "bar", fun = mean, fill="dodgerblue4")+
  facet_wrap(~data_source, scales = "free_y")+
  labs(x= "NSI TOTAL SCORE", y= "COUNT")+
  ggtitle("NSI Total Score Distribution Between Data Sources")



ggplot(master_set, mapping = aes(x= start_date, y= NSI_TotalScore))+
  geom_bar(stat = "identity", fill= "dodgerblue4")+
  facet_wrap(~data_source, scales = "free_y")+
  labs(x="time", y="NSI TotalScore Count")+
  ggtitle("Count of NSI TotalScores over Time by Data Sources")



view(bhdp%>%
  select(,7, 108:127))




view(master_set[,c(39:44)] %>%
 adorn_totals("col"))

  
rm(nsi)  
  
  
  
  
  

ggplot(nsi, mapping = aes(nsi_q_count))+
  geom_histogram()



view(master_set[,c(45:53)]%>%
  mutate(phq9_q_count =  (rowSums(!is.na(master_set[,c(45:53)])))))

library(viridis)

master_set%>%
  filter(NSI_Validity_10 != "NA")%>%
  ggplot(mapping = aes(x=NSI_TotalScore, y= HIT6_Score, fill= NSI_F2_Cognative))+
  geom_tile()+
  labs(x="NSI Total Score", y="HIT6 Score")+
  ggtitle("Correlation Between NSI Total, HIT6 Total and NSI Cognative")

master_set%>%
  filter(NSI_Validity_10 != "NA")%>%
  ggplot(mapping = aes(x=NSI_TotalScore, y= HIT6_Score, fill= NSI_F1_Vestibular))+
  geom_tile()+
  labs(x="NSI Total Score", y="HIT6 Score")+
  ggtitle("Correlation Between NSI Total, HIT6 Total and NSI Vestibular")

master_set%>%
  filter(NSI_Validity_10 != "NA")%>%
  ggplot(mapping = aes(x=NSI_TotalScore, y= HIT6_Score, fill= NSI_F4_Sensory))+
  geom_tile()+
  labs(x="NSI Total Score", y="HIT6 Score")+
  ggtitle("Correlation Between NSI Total, HIT6 Total and NSI Sensory")

master_set%>%
  filter(NSI_Validity_10 != "NA")%>%
  ggplot(mapping = aes(x=NSI_TotalScore, y= HIT6_Score, fill= NSI_F3_Affective))+
  geom_tile()+
  labs(x="NSI Total Score", y="HIT6 Score")+
  ggtitle("Correlation Between NSI Total, HIT6 Total and NSI Affective")




library(xtable)
  

  master_set_valid_filter <-  master_set
    
  
  master_set_valid_filter <-master_set%>%drop_na(NSI_TotalScore)
  
  residuals <-  rcorr(as.matrix(master_set_valid_filter[,c(1:6, 7:8, 14:17, 84:85, 164:165 )]), type = "pearson")

  residuals$P
  
 
 
  cor(master_matrix, use = "pairwise.complete.obs")
  
  master_matrix <-  master_set_valid_filter[,c(1:6, 7:8, 14:17, 84:85, 164:165 )]
  
  library(psych)
  
  
  
  corr.test(master_matrix)
  
master_matrix <- as.matrix(master_matrix)
    
cor.plot(cor(master_matrix[,1:6], master_matrix[,7:16], use = "pairwise.complete.obs"), order)

corrplot((cor(master_matrix[,1:6], master_matrix[,7:16], use = "pairwise.complete.obs")), method = "color")


corstar <- function(x, y = NULL, use = "pairwise", method = "pearson", round = 3, row.labels, col.labels, ...) {
  
  require(psych)
  
  ct <- corr.test(x, y, use, method)    # calculate correlation
  r <- ct$r                             # get correlation coefs
  p <- ct$p                             # get p-values
  
  stars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))) # generate significance stars
  
  m <- matrix(NA, nrow = nrow(r) * 2, ncol = ncol(r) + 1) # create empty matrix
  
  rlab <- if(missing(row.labels)) rownames(r) else row.labels # add row labels
  clab <- if(missing(col.labels)) {
    if(is.null(colnames(r)))
      deparse(substitute(y))
    else
      colnames(r)
  } else {
    col.labels # add column labels
  }
  
  rows <- 1:nrow(m)                     # row indices
  cols <- 2:ncol(m)                     # column indices
  
  odd <- rows %% 2 == 1                 # odd rows
  even <- rows %% 2 == 0                # even rows
  m[odd, 1] <- rlab                     # add variable names
  m[even, 1] <- rep("", sum(even))      # add blank
  
  m[odd, cols] <- paste(format(round(r, round), nsmall = round, ...), stars, sep = "")     # add r coefs
  m[even, cols] <- paste("(", format(round(p, round), nsmall = round, ...), ")", sep = "") # add p-values
  
  colnames(m) <- c(" ", clab)           # add colnames
  m                                     # return matrix
}


view(corstar(master_matrix))

master_cor_matrix <- as.matrix(corstar(master_matrix))

##PCL5 question renaming 108-127

names(bhdp)[108] <- "PCL.5.Q01.MEMORIES_OF_STRESS_V1"
names(bhdp)[109] <- "PCL.5.Q02.DREAM_OF_STRESS_V1"
names(bhdp)[110] <- "PCL.5.Q03.RELIVING_STRESS_V1"
names(bhdp)[111] <- "PCL.5.Q04.UPSET_V1"
names(bhdp)[112] <- "PCL.5.Q05.PHYSICAL_REACTION_V1"
names(bhdp)[113] <- "PCL.5.Q06.AVOID_STRESS_V1"
names(bhdp)[114] <- "PCL.5.Q07.AVOID_ACTIVITES_V1"
names(bhdp)[115] <- "PCL.5.Q08.TROUBLE_REMEMBERING_V1"
names(bhdp)[116] <- "PCL.5.Q09.NEGATIVE_BELIEFS_V1"
names(bhdp)[117] <- "PCL.5.Q10.BLAMING_V1"
names(bhdp)[118] <- "PCL.5.Q11.STRONG_NEGATIVE_FEELINGS_V1"
names(bhdp)[119] <- "PCL.5.Q12.LOSS_OF_INTEREST_V1"
names(bhdp)[120] <- "PCL.5.Q13.FEEL_DISTANT_V1"
names(bhdp)[121] <- "PCL.5.Q14.TROUBLE_EXPERIENCING_POSITIVE_FEELINGS_V1"
names(bhdp)[122] <- "PCL.5.Q15.IRRITABLE_V1"
names(bhdp)[123] <- "PCL.5.Q16.TAKING_RISKS_V1"
names(bhdp)[124] <- "PCL.5.Q17.ON_GUARD_V1"
names(bhdp)[125] <- "PCL.5.Q18.JUMPY_V1"
names(bhdp)[126] <- "PCL.5.Q19.CONCENTRATION_V1"
names(bhdp)[127] <- "PCL.5.Q20.DIFFICULTY_SLEEPING_V1"

##nicoe 102-121

names(nicoe)[102] <- "PCL.5.Q01.MEMORIES_OF_STRESS_V1"
names(nicoe)[103] <- "PCL.5.Q02.DREAM_OF_STRESS_V1"
names(nicoe)[104] <- "PCL.5.Q03.RELIVING_STRESS_V1"
names(nicoe)[105] <- "PCL.5.Q04.UPSET_V1"
names(nicoe)[106] <- "PCL.5.Q05.PHYSICAL_REACTION_V1"
names(nicoe)[107] <- "PCL.5.Q06.AVOID_STRESS_V1"
names(nicoe)[108] <- "PCL.5.Q07.AVOID_ACTIVITES_V1"
names(nicoe)[109] <- "PCL.5.Q08.TROUBLE_REMEMBERING_V1"
names(nicoe)[110] <- "PCL.5.Q09.NEGATIVE_BELIEFS_V1"
names(nicoe)[111] <- "PCL.5.Q10.BLAMING_V1"
names(nicoe)[112] <- "PCL.5.Q11.STRONG_NEGATIVE_FEELINGS_V1"
names(nicoe)[113] <- "PCL.5.Q12.LOSS_OF_INTEREST_V1"
names(nicoe)[114] <- "PCL.5.Q13.FEEL_DISTANT_V1"
names(nicoe)[115] <- "PCL.5.Q14.TROUBLE_EXPERIENCING_POSITIVE_FEELINGS_V1"
names(nicoe)[116] <- "PCL.5.Q15.IRRITABLE_V1"
names(nicoe)[117] <- "PCL.5.Q16.TAKING_RISKS_V1"
names(nicoe)[118] <- "PCL.5.Q17.ON_GUARD_V1"
names(nicoe)[119] <- "PCL.5.Q18.JUMPY_V1"
names(nicoe)[120] <- "PCL.5.Q19.CONCENTRATION_V1"
names(nicoe)[121] <- "PCL.5.Q20.DIFFICULTY_SLEEPING_V1"





##APPLYING CLINICAL CUT OFFS TO SURVEYS

#NSI SCORE CATEGORY
master_set$NSI_Score_Categories<- cut(master_set$NSI_TotalScore,
                                       breaks= c(-.00001, 0.00000, 4.9999, 10000),
                                       labels= c("Same", "Worse", "Clinically Worse"))

master_set%>%
  filter(NSI_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=NSI_TotalScore, fill= NSI_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="NSI Total Score", y= "Frequency Count")+
  ggtitle("NSI Total Score Distribution with Corresponding NSI Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~data_source, scales = "free_y")



#PHQ9 SCORE CATEGORIES
master_set$PHQ9_Score_Categories<- cut(master_set$PHQ9_Score,
    breaks= c(-1, 4.1, 9.1, 14.1, 19.1, 27.1),
    labels= c("Minimal", "Mild", "Moderate", "Moderately Severe", "Severe"))

master_set%>%
 filter(PHQ9_Score!= "NA")%>%
  filter(NSI_TotalScore!="NA")%>%
ggplot(mapping = aes(x=PHQ9_Score, fill= PHQ9_Score_Categories))+
  geom_histogram()+
  labs(x="PHQ9 Score", y= "Frequency Count")+
  ggtitle("PHQ9 Total Score Distribution with Corresponding NSI Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")



#EPWORTH SCORE CATEGORY
master_set$EPWORTH_Score_Categories<- cut(master_set$epworth_tot_score,
                                      breaks= c(-.00001, 0.00000, 2.9999, 10000),
                                      labels= c("Same", "Worse", "Clinically Worse"))

master_set%>%
  filter(EPWORTH_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=epworth_tot_score, fill= EPWORTH_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="EPWORTH Total Score", y= "Frequency Count")+
  ggtitle("EPWORTH Total Score Distribution with Corresponding NSI Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")


#DHI SCORE CATEGORY
master_set$DHI_Score_Categories<- cut(master_set$dhi_tot_score,
                                          breaks= c(-.00001, 0.00000, 17.9999, 10000),
                                          labels= c("Same", "Worse", "Clinically Worse"))

master_set%>%
  filter(DHI_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=dhi_tot_score, fill= DHI_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="DHI Total Score", y= "Frequency Count")+
  ggtitle("DHI Total Score Distribution Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")



#PCLM SCORE CATEGORY
master_set$PCLM_Score_Categories<- cut(master_set$pclm_tot_score,
                                      breaks= c(-.00001, 0.00000, 9.9999, 10000),
                                      labels= c("Same", "Worse", "Clinically Worse"))

master_set%>%
  filter(PCLM_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=pclm_tot_score, fill= PCLM_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="PCLM Total Score", y= "Frequency Count")+
  ggtitle("PCLM Total Score Distribution Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")



#AUDIT SCORE CATEGORY
master_set$AUDIT_Score_Categories<- cut(master_set$AUDIT_Score,
                                       breaks= c(-.00001, 0.00000, 2.9999, 10000),
                                       labels= c("Same", "Worse", "Clinically Worse"))

master_set%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(AUDIT_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=AUDIT_Score, fill= AUDIT_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="AUDIT Total Score", y= "Frequency Count")+
  ggtitle("AUDIT Total Score Distribution Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~data_source, scales = "free_y")


#ABC SCORE CATEGORY
master_set$ABC_Score_Categories<- cut(master_set$abc_tot_score,
                                        breaks= c(-.00001, 0.00000, 19.9999, 10000),
                                        labels= c("Same", "Better", "Clinically Clinically"))

master_set%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(ABC_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=abc_tot_score, fill= ABC_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="ABC Total Score", y= "Frequency Count")+
  ggtitle("ABC Total Score Distribution Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~data_source, scales = "free_y")


#GAD7 SCORE CATEGORY
master_set$GAD7_Score_Categories<- cut(master_set$GAD_Score,
                                        breaks= c(-.00001, 0.00000, 4.9999, 10000),
                                        labels= c("Same", "Worse", "Clinically Worse"))

master_set%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(GAD7_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=GAD_Score, fill= GAD7_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="GAD7 Total Score", y= "Frequency Count")+
  ggtitle("GAD7 Total Score Distribution Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~data_source, scales = "free_y")



#HIT6 SCORE CATEGORY
master_set$HIT6_Score_Categories<- cut(master_set$HIT6_Score,
                                       breaks= c(-.00001, 0.00000, 9.9999, 10000),
                                       labels= c("Same", "Worse", "Clinically Worse"))

master_set%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(HIT6_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=HIT6_Score, fill= HIT6_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="HIT6 Total Score", y= "Frequency Count")+
  ggtitle("HIT6 Total Score Distribution Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~data_source, scales = "free_y")



#PHQ8 SCORE CATEGORY
master_set$PHQ8_Score_Categories<- cut(master_set$phq8_tot_score,
                                       breaks= c(-.00001, 0.00000, 4.9999, 10000),
                                       labels= c("Same", "Worse", "Clinically Worse"))

master_set%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(PHQ8_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=phq8_tot_score, fill= PHQ8_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="PHQ8 Total Score", y= "Frequency Count")+
  ggtitle("PHQ8 Total Score Distribution Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~data_source, scales = "free_y")



#ISI SCORE CATEGORY
master_set$ISI_Score_Categories<- cut(master_set$ISI_Score,
                                       breaks= c(-.00001, 7.00001, 14.0009, 21.001, 100000),
                                       labels= c("No Clinicaly Sig. Insomnia", "Subthreshold Insomnia", "Clinical Insomnia(Moderate", "Clinical Insomnia(Severe"))

master_set%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(ISI_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=ISI_Score, fill= ISI_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="ISI Total Score", y= "Frequency Count")+
  ggtitle("ISI Total Score Distribution Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~data_source, scales = "free_y")



#PCL5 SCORE CATEGORY
master_set$PCL5_Score_Categories<- cut(master_set$pcl520_tot_score,
                                       breaks= c(-.00001, 0.00000, 32, 10000),
                                       labels= c("Same", "SubThreshold PTSD", "Probable PTSD"))

master_set%>%
  filter(NSI_TotalScore!="NA")%>%
  filter(PCL5_Score_Categories!="NA")%>%
  ggplot(mapping = aes(x=pcl520_tot_score, fill= PCL5_Score_Categories))+
  geom_histogram(bins = 50)+
  labs(x="PCL5 Total Score", y= "Frequency Count")+
  ggtitle("PCL5 Total Score Distribution Categorized by Severity")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  facet_wrap(~data_source, scales = "free_y")


###LINEAR MODELS

install.packages("sjPlot")
library(sjPlot)

summary(lm(NSI_TotalScore~HIT6_Score*NSI_F2_Cognative ,data = master_set))



nsi_count <- (master_set[,c(1:6, 9, 13, 18:38)] %>%
                group_by(data_source)%>%
                summarise_all(funs(sum(!is.na(.)))))%>%
  adorn_totals("row")
