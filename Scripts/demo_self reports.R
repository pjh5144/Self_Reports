#Demographic analysis

#Rename values

#race

demo$Race[demo$Race == 'C'] <- 'Caucasian'
demo$Race[demo$Race == 'M'] <- 'Asian'
demo$Race[demo$Race == 'N'] <- 'African American'
demo$Race[demo$Race == 'R'] <- 'Native American/Pacific Islander'
demo$Race[demo$Race=="X"] <- "Other"
demo$Race[demo$Race=="Z"] <- "Other"
demo$Race[demo$Race==""] <- "Other"

# SponsorService
demo$SponServAgg[demo$SponServAgg == 'A'] <- 'Army'
demo$SponServAgg[demo$SponServAgg == 'C'] <- 'Coast Guard'
demo$SponServAgg[demo$SponServAgg == 'F'] <- 'Air Force'
demo$SponServAgg[demo$SponServAgg == 'M'] <- 'Marine'
demo$SponServAgg[demo$SponServAgg == 'N'] <- 'Navy'

demo$SponServAgg[demo$SponServAgg == 'V'] <- ''
demo$SponServAgg[demo$SponServAgg == 'X'] <- ''
demo$SponServAgg[demo$SponServAgg == 'Z'] <- ''


# Marital Status
demo$Marital_Stat[demo$Marital_Stat == ''] <- 'Other'
demo$Marital_Stat[demo$Marital_Stat == 'D'] <- 'Divorced'
  demo$Marital_Stat[demo$Marital_Stat == 'M'] <- 'Married'
  demo$Marital_Stat[demo$Marital_Stat == 'S'] <-  'Single'
  demo$Marital_Stat[demo$Marital_Stat == 'W'] <- 'Widow'
  
  demo$Marital_Stat[demo$Marital_Stat == 'A'] <-
  demo$Marital_Stat[demo$Marital_Stat == 'I'] <-
  demo$Marital_Stat[demo$Marital_Stat == 'L'] <-
  demo$Marital_Stat[demo$Marital_Stat == 'Z'] <-



#Join Master Matrix and demographic information

names(demo)[1] <- "SubjectID"
master_demo <-  left_join(master_set_valid_filter, demo, by= c("SubjectID","start_date") )

master_demo_1 <-  left_join(master_set, demo, by= c("SubjectID","start_date") )

master_demo_1 <- master_demo_1[, c(1:6, 7:8, 14:17, 84:85, 164:165, 305:315)]

master_demo_filter <- master_demo[, c(1:6, 7:8, 14:17, 84:85, 164:165, 305:315)]

common <-  intersect(master_set$SubjectID, demo$SubjectID)

demo%>%
  group_by(SponServAgg)%>%
  summarise(count= n())%>%
  arrange(desc(count))

demo%>%
  group_by(Gender)%>%
  summarise(count= n())%>%
  arrange(desc(count))

demo%>%
  group_by(Race)%>%
  summarise(count= n())%>%
  arrange(desc(count))


demo%>%
  group_by(Marital_Stat)%>%
  summarise(count= n())%>%
  arrange(desc(count))


demo%>%
  group_by(BenCatCom)%>%
  summarise(count= n())%>%
  arrange(desc(count))


ggplot(demo, mapping = aes(x=Age))+
  geom_histogram()



master_set[,c(1, 18:38, 9)]%>%
  mutate(nsi_q_count =  (rowSums(!is.na(master_set[,c(18:38, 9)]))))%>%
  filter(NSI_TotalScore != "NA")%>%
  select( 3:24)%>% 
  group_by(nsi_q_count)%>%
  dplyr:: summarise(count= n())%>%
  adorn_totals("row")


residuals$p
