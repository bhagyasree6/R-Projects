hr_train=read.csv("D:/Data/hr_train.csv",stringsAsFactors = F)
hr_test=read.csv("D:/Data/hr_test.csv",stringsAsFactors = F)
hr_test$left=NA
hr_train$left=as.numeric(hr_train$left)
hr_train$left=as.factor(hr_train$left)
hr_train$data="train"
hr_test$data="test"
hr_all=rbind(hr_train,hr_test)


library(randomForest)
library(dplyr)
glimpse(hr_all)
glimpse(hr_train)
glimpse(hr_test)

cat_var=names(hr_all)[sapply(hr_all,is.character)]

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

lapply(hr_all,function(x) length(unique(x)))

names(hr_all)[sapply(hr_all,function(x) is.character(x))]

cat_cols=c("sales","salary" )


for(var in cat_cols){
  hr_all=CreateDummies(hr_all,var,100)
}

hr_train=hr_all %>% filter(data=="train") %>% dplyr::select(-c(data))
hr_test=hr_all %>% filter(data=="test") %>% select(c(-data,-left))


param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

num_trials=90

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

my_params=subset_paras(param,num_trials)
my_params

myauc=0

for(i in 1:num_trials){
  #print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
}

library(cvTools)
k=cvTuning(randomForest,left~., 
           data =hr_train,
           tuning =params,
           folds = cvFolds(nrow(hr_train), K=10, type ="random"),
           cost =mycost_auc, seed =2,
           predictArgs = list(type="prob")
)


score.this=k$cv[,2]

if(score.this>myauc){
  #print(params)
  # uncomment the line above to keep track of progress
  myauc=score.this
  #print(myauc)
  # uncomment the line above to keep track of progress
  best_params=params
}

myauc
best_params

best_params=data.frame(mtry=25,
                       ntree=700,
                       maxnodes=100,
                       nodesize=10)


hr.rf.final=randomForest(left~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=hr_train
)

hr.rf.final

test.score=predict(hr.rf.final,newdata = hr_test,type='prob')[,1]
test.score
write.csv(test.score,'mysubmission.csv',row.names = F)     

## Variable IMportance

d=importance(hr.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))

## Varimp Plot

varImpPlot(hr.rf.final)

## Partial Dependence Plot

var='left'

pred.resp = predict(hr.rf.final,newdata=hr_train,type='prob')[,2]
myvar = hr_train[,var]

trend.data=data.frame(Response=pred.resp,myvar=myvar)
library(ggplot2)
trend.data %>% ggplot(aes(y=Response,x=myvar))+
  geom_smooth()              




