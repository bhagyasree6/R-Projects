setwd('D:/Data')
store_train=read.csv("store_train.csv",stringsAsFactors = F)
store_test=read.csv("store_test.csv",stringsAsFactors = F)
library(dplyr)
store_test$store=NA
store_train$data="train"
store_test$data="test"
glimpse(store_train)
store_all=rbind(store_train,store_test)
table(store_train$Areaname)

table(is.na(store_all))
colSums(is.na(store_all))

View(store_all)
store_all$population[is.na(store_all$population)]=mean(store_all$population,na.rm=T)
store_all$country[is.na(store_all$country)]=mean(store_all$country,na.rm=T)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

store_all=CreateDummies(store_all,"state_alpha",30)
store_all=CreateDummies(store_all,"store_Type",30)
store_all=CreateDummies(store_all,"Areaname",30)
store_all=CreateDummies(store_all,"countyname",30)
store_all=CreateDummies(store_all,"countytownname",30)
store_all=CreateDummies(store_all,"storecode",50)


glimpse(store_all)

store_all=dplyr::select(store_all,-c(Id))

store_train=store_all %>% filter(data=="train") %>% select(-data, -countyname_OxfordCounty,-
                                                             countyname_GraftonCounty,-countyname_AroostookCounty,-
                                                             storecode_METRO12620N23019,-storecode_NCNTY23003N23003,
                                                           -storecode_METRO14460MM1120)
store_test=store_all %>% filter(data=="test") %>% select(-data,-store, -countyname_OxfordCounty,-
                                                           countyname_GraftonCounty,-countyname_AroostookCounty,-
                                                           storecode_METRO12620N23019,-storecode_NCNTY23003N23003,
                                                         -storecode_METRO14460MM1120)

set.seed(2)
s=sample(1:nrow(store_train),0.8*nrow(store_train))
store_train1=store_train[s,]
store_train2=store_train[-s,]

for_vif=lm(store~.,data=store_train1)

library(car)

sort(vif(for_vif),decreasing =F )

attributes(alias(for_vif)$Complete)$dimnames[[1]]


for_vif=lm(store~.-sales0-sales2-sales3-Areaname_CoosCountyNH-
             Areaname_SomersetCountyME- State-sales1-state_alpha_ME 
           ,data=store_train1)



sort(vif(for_vif),decreasing =F )
formula(for_vif)



log_fit=glm(store~.-sales0-sales2-sales3-Areaname_CoosCountyNH-
              Areaname_SomersetCountyME- State-sales1-state_alpha_ME  
            -population-CouSub-state_alpha_SC
            -state_alpha_SD-state_alpha_FL-state_alpha_CO
            -state_alpha_NE-state_alpha_KS-state_alpha_VA
            -state_alpha_IA
            -Areaname_SpringfieldMAHUDMetroFMRArea
            -countyname_BerkshireCounty
            -countyname_LincolnCounty-countyname_WorcesterCounty 
            -countyname_WashingtonCounty
            -Areaname_WashingtonCountyME,data=store_train1,family = "binomial",control = list(maxit=100))


log_fit=step(log_fit)

summary(log_fit)


formula(log_fit)

library(pROC)
val.score=predict(log_fit,newdata = store_train2,type='response')

auc(roc(store_train2$store,val.score))

###Calculating on entire train data

for_vif_final=lm(store~.,data=store_train)

attributes(alias(for_vif_final)$Complete)$dimnames[[1]]

for_vif_final=lm(store~.-sales0-sales2-Areaname_CoosCountyNH-sales3
             - Areaname_SomersetCountyME-State-sales1
             -state_alpha_ME ,data=store_train)

sort(vif(for_vif_final),decreasing = F)


log.fit.final=glm(store~.-sales0-sales2-Areaname_CoosCountyNH-sales3
                  - Areaname_SomersetCountyME-State-sales1
                  -state_alpha_ME-state_alpha_SC-state_alpha_NY
                  -state_alpha_WA-state_alpha_CA-state_alpha_FL
                  -state_alpha_OK-state_alpha_PA-state_alpha_AR
                  -state_alpha_AL-state_alpha_WI-state_alpha_ND
                  -state_alpha_MO-state_alpha_MS-state_alpha_OH
                  -state_alpha_TX-countyname_SomersetCounty
                  -countyname_BerkshireCounty
                  -Areaname_SpringfieldMAHUDMetroFMRArea
                  -countyname_EssexCounty-countyname_WorcesterCounty,data = store_train,family = "binomial")

log.fit.final=step(log.fit.final)


summary(log.fit.final)

formula(log.fit.final)




test.prob.score= predict(log.fit.final,newdata = store_test,type='response')
write.csv(test.prob.score,"proper_submission_file_name.csv",row.names = F)



###quiz
val1=store_train$Areaname
val2=unique(val1)
length(val2)

shapiro.test(store_train$sales0)
qqnorm(store_train$sales3)
sales=sum(store_train$sales0,store_train$sales1,store_train$sales2,store_train$sales3,store_train$sales4,na.rm=FALSE)
sales
val3=store_train%>% group_by(store_Type) %>% summarise(sales = var(sales))
val3
val4=store_train %>%
  filter(store_Type =="Grocery Store") %>%
  filter(store ==0)
val4
