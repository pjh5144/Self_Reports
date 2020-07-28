
total_sub <- total[, c(1:14, 15:21)]
rm(total_sub) 

total_sub$NSI_TotalScore[total_sub$NSI_TotalScore<0 | total_sub$NSI_TotalScore>88] <- NA
total_sub$HIT6_Score[total_sub$HIT6_Score<36 | total_sub$HIT6_Score>78] <- NA
total_sub$PHQ9_Score[total_sub$PHQ9_Score<0 | total_sub$PHQ9_Score>27] <- NA
total_sub$ISI_Score[total_sub$ISI_Score<0 | total_sub$ISI_Score>28] <- NA
total_sub$GAD_Score[total_sub$GAD_Score<0 | total_sub$GAD_Score>10] <- NA
total_sub$AUDIT_Score[total_sub$AUDIT_Score<0 | total_sub$AUDIT_Score>40] <- NA
total_sub$abc_tot_score[total_sub$abc_tot_score<0 | total_sub$abc_tot_score>100] <- NA
total_sub$dhi_tot_score[total_sub$dhi_tot_score<0 | total_sub$dhi_tot_score>100] <- NA    
total_sub$epworth_tot_score[total_sub$epworth_tot_score<0 | total_sub$epworth_tot_score>24] <- NA    
total_sub$pcl520_tot_score[total_sub$pcl520_tot_score<0 | total_sub$pcl520_tot_score>80] <- NA
total_sub$pclm_tot_score[total_sub$pclm_tot_score<17 | total_sub$pclm_tot_score>85] <- NA


total_sub$start_date <- as.POSIXct(total_sub$start_date,format="%m/%d/%Y")




total_sub_1 <-total_sub_1%>%drop_na(start_date)

summary(total_sub_1)

view(cor(total_sub[,4:9], total_sub[,10:21 ], use = "pairwise.complete.obs" ))

correlation <- apply(X = total_sub, MARGIN = 1, FUN = function(x) cor.test(x[4:9], x[10:21], use= "complete.obs"))

cor(total_sub$NSI_TotalScore, total_sub$AUDIT_Score, use = "complete.obs")

total_sub_filter <-  total_sub%>%
  filter(NSI_Validity_10<22)

total_sub_filter <- total_sub_filter%>%drop_na(NSI_TotalScore)
  
view(cor(total_sub_filter[,4:9], total_sub_filter[,10:21 ], use = "pairwise.complete.obs" ))

summary(total_sub_filter)


isi_cor <-total_sub[match(total_sub$NSI_TotalScore, total_sub$ISI_Score),]

rm(isi_cor)


total_sub_filter%>%
  filter(ISI_Score !="NA")%>%
    filter(NSI_TotalScore != "NA")%>%
    summarise(count= length(as.factor(ISI_Score)))

total_sub_filter %>%
  summarise_all(funs(sum(!is.na(.))))

cor(isi_cor$NSI_TotalScore, isi_cor$ISI_Score)

correlation<- (cor(total_sub_1[,4:9], total_sub_1[,10:21 ], use = "pairwise.complete.obs" ))

library(psych)
install.packages("rstatix")
library(rstatix)


residuals <-  rcorr(as.matrix(total_sub_1[,c(4:9, 10:21 )]))

residuals

cor_test(total_sub_1[,c(4:9, 10:21 )], use = "na.or.complete")

total_sub_1%>%
  filter(ISI_Score !="NA")%>%
  filter(NSI_TotalScore != "NA")%>%
  cor_test(
    vars = "NSI_TotalScore",
    vars2 = "ISI_Score"
  )

view(total_sub_1%>%
  cor_pmat(
    vars = c("NSI_TotalScore", "NSI_Validity_10", "NSI_F4_Sensory", "NSI_F3_Affective", "NSI_F2_Cognative", "NSI_F1_Vestibular"),
    vars2 = c("pcl520_tot_score", "HIT6_Score", "dhi_tot_score", "pclm_tot_score", "GAD_Score", "PHQ9_Score")
  ))



total_sub%>%
  select(c(NSI_TotalScore, GAD_Score, HIT6_Score, AUDIT_Score, pclm_tot_score, pcl520_tot_score, PHQ9_Score, ISI_Score, PSQI_Score))%>%
  cor_mat()
  




  cor_test(
    vars = "NSI_TotalScore",
    vars2 = "ISI_Score"
  )

cor.test(total_sub_1$NSI_TotalScore, total_sub_1$pcl520_tot_score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$HIT6_Score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$dhi_tot_score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$pclm_tot_score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$GAD_Score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$PHQ9_Score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$ISI_Score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$AUDIT_Score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$abc_tot_score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$epworth_tot_score)
cor.test(total_sub_1$NSI_TotalScore, total_sub_1$PSQI_Score)

summary(lm(NSI_TotalScore~GAD_Score+PHQ9_Score, data = total_sub_1))
  