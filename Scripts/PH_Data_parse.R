bhdp<-data.frame(data.table::fread("DEID_BHDP_TBI_Surveys.csv"))

bhdp<-as.data.frame(bhdp)

bhdp%>%
  select(grep("NSI",names(.)))

test<-bhdp%>%
  mutate(date=as.Date(lubridate::ymd_hms(start_date)))%>%
  select(date,start_date)%>%
  head()

test<-bhdp%>%
  #filter(!is.na('NSI:Balance_V1'))%>%
  select(-NSI.GuidanceText_V1)%>%
  mutate_at(vars(contains("NSI")),as.numeric)%>%
  mutate(NSI_Total = rowSums(select(.,grep("NSI",names(.)))))%>%
  filter(!is.na(NSI_Total))%>%
  mutate(date=as.Date(lubridate::ymd_hms(start_date)))%>%
  group_by(SubjectID,date)%>%
  summarise(n=n())%>%
  filter(n>1)

test<-bhdp%>%
  mutate(date=as.Date(lubridate::ymd_hms(start_date)))%>%
  group_by(SubjectID,date)%>%
  summarise(n=n())%>%
  filter(n>1&!is.na(SubjectID)&!is.na(date))

bhdp%>%
  filter(SubjectID %in% test$SubjectID )%>%
  mutate(date=as.Date(lubridate::ymd_hms(start_date)))%>%
  select(grep("date|NSI|SubjectID",names(.)))%>%
  filter(!is.na(NSI.Anxious_V1)&NSI.Anxious_V1!="")->test2

length(unique(test2$SubjectID))

bhdp%>%
  filter(SubjectID %in% test2$SubjectID)%>%
  select(!grep("NSI",names(.)))%>%
  View()


bhdp2%>%filter(SubjectID==596374499952)%>%
  select(grep("PCL|date",names(.)))

bhdp2<-bhdp%>% 
  #filter(SurveyName=="TBI")%>% 
  select_all(~gsub("\\-|\\:","_",.))%>% 
  mutate(start_date=as.Date(lubridate::ymd_hms(start_date)))%>%
  mutate(end_date=as.Date(lubridate::ymd_hms(end_date)))

pattern<-c("AUDIT","HIT","GAD","^NSI","PCL\\.5","^PHQ9\\.","^PSQI","^QOL","^ISI") 

bhdp_df<-data.frame() 

for (i in 1:length(pattern)){ 
  print(i)
  df<-bhdp2[,grepl(paste("SubjectID","start_date","end_date","parent_dmis_key_name","child_dmis_key_name",pattern[i],sep = "|"),names(bhdp2))] 
  nnrow <- length(bhdp2[,grepl(pattern[i],names(bhdp2))])
  df<-df%>%
    mutate(na_counts = rowSums(is.na(select(.,grep(pattern[i],names(.))))|select(.,grep(pattern[i],names(.)))==" "|select(.,grep(pattern[i],names(.)))==""))%>%
    filter(na_counts!=nnrow)%>%
    select(-na_counts)
  
  
  if(length(bhdp_df)==0){ 
    bhdp_df<-df 
  } 
  
  else{ 
    bhdp_df<-merge(bhdp_df,df,by=c("SubjectID","start_date","end_date","parent_dmis_key_name","child_dmis_key_name"),all=T) 
  }   
  
} 



bhdp_df$file_source<-"bhdp" 