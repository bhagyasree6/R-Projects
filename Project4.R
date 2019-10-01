pro_train=read.csv("D:/Data/product_train.csv")
  
pro_test=read.csv("D:/Data/product_train.csv")
library(dplyr)
glimpse(pro_train)

pro_test$went_on_backorder=NA
pro_train$data="train"
pro_test$data="test"
pro_all=rbind(pro_train,pro_test)
glimpse(pro_all)
#table(pro_all$potential_issue)
#pro_all=pro_all %>% mutate(potential_issue=ifelse(potential_issue=="Yes",0,1)) %>% select(-potential_issue)
#table(pro_all$deck_risk)
#pro_all=pro_all %>% mutate(deck_risk=ifelse(deck_risk=="Yes",0,1)) %>% select(-deck_risk)
#pro_all=pro_all %>% mutate(oe_constraint=ifelse(oe_constraint=="Yes",0,1)) %>% select(-oe_constraint)
#pro_all=pro_all %>% mutate(ppap_risk=ifelse(ppap_risk=="Yes",0,1)) %>% select(-ppap_risk)
#pro_all=pro_all %>% mutate(stop_auto_buy=ifelse(stop_auto_buy=="Yes",0,1)) %>% select(-stop_auto_buy)
#pro_all=pro_all %>% mutate(rev_stop =ifelse(rev_stop =="Yes",0,1)) %>% select(-rev_stop)
CreateDummies=function(data,var,freq_cutoff=100){
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


pro_all=select(pro_all,-sku)
pro_train=pro_all %>% filter(data=="train") %>% select(-c(data))
pro_test=pro_all %>% filter(data=="test") %>% select(-c(data,went_on_backorder))

library(randomForest)
memory.limit()
memory.limit(size=50000)
fit_rf=randomForest(as.factor(went_on_backorder)~.,data=pro_train,na.action=na.exclude)












fit_rf
forest.pred.test=predict(fit_rf,newdata=pro_test)
table(pro_test$went_on_backorder,forest.pred.test)

importance(fit_rf)
varImpPlot(fit_rf)
write.csv(forest.pred.test,file = "Bhagyasree_Pedapudi_P3_part2.csv", row.names = F)



getmedian(pro_train$perf_6_month_avg)
median(pro_train$perf_12_month_avg)

chisq.test(pro_train$deck_risk,pro_train$went_on_backorder)


