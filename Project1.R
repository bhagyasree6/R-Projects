setwd("D:/Data")
house_train=read.csv("housing_train.csv",stringsAsFactors = F)
house_test=read.csv("housing_test.csv",stringsAsFactors = F)
house_test$Price=NA
house_train$data="train"
house_test$data="test"
house_all=rbind(house_train,house_test)
library(dplyr)
glimpse(house_train)
house_all[house_all==0]<-NA

table(is.na(house_all))
colSums(is.na(house_all))
house_all$Bedroom2[is.na(house_all$Bedroom2)]=median(house_all$Bedroom2,na.rm = T)
house_all$Bathroom[is.na(house_all$Bathroom)]=median(house_all$Bathroom,na.rm = T)
house_all$Car[is.na(house_all$Car)]=median(house_all$Car,na.rm = T)
house_all$Landsize[is.na(house_all$Landsize)]=median(house_all$Landsize,na.rm = T)
house_all$BuildingArea[is.na(house_all$BuildingArea)]=median(house_all$BuildingArea,na.rm = T)
#house_all$month=06
#house_all$date=01
#house_all$YearBuilt=as.Date(paste(house_all$YearBuilt,house_all$month,house_all$date,sep="-"))
house_all$YearBuilt[is.na(house_all$YearBuilt)]=round(median(house_all$YearBuilt,na.rm = T))
   
  
#house_all$new_Suburb=paste(house_all$Suburb,house_all$Postcode,sep="-")
glimpse(house_all)
View(house_all)



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
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}



house_all=CreateDummies(house_all ,"Method",100)
house_all=CreateDummies(house_all ,"Type",100)
house_all=CreateDummies(house_all,"SellerG",100)
house_all=CreateDummies(house_all,"Suburb",100)
house_all=CreateDummies(house_all,"CouncilArea",100)


glimpse(house_all)

house_all=dplyr::select(house_all,-c(Address))


house_train=house_all %>% filter(data=="train") %>% dplyr::select(-data)

house_test=house_all %>% filter(data=='test') %>% 
  dplyr::select(-data,-Price)

set.seed(2)
s=sample(1:nrow(house_train),0.7*nrow(house_train))
house_train1=house_train[s,]
house_train2=house_train[-s,]

fit=lm(Price~.,data=house_train1)

library(car)
vif(fit)
sort(vif(fit),decreasing = T)
formula(fit)

fit=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
         Landsize + BuildingArea + YearBuilt + Method_SP + Method_PI + 
         Method_S + Type_u + Type_h + SellerG_Kay + SellerG_Hodges + 
         SellerG_McGrath + SellerG_Noel + SellerG_Gary + SellerG_Jas + 
         SellerG_Miles + SellerG_Greg + SellerG_Sweeney + SellerG_RT + 
         SellerG_Fletchers + SellerG_Woodards + SellerG_Brad + SellerG_Biggin + 
         SellerG_Ray + SellerG_Buxton + SellerG_Marshall + SellerG_Barry + 
         SellerG_hockingstuart + SellerG_Jellis + SellerG_Nelson + 
         Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray + 
         Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
         Suburb_Yarraville + Suburb_Balwyn + Suburb_MalvernEast + 
         Suburb_Camberwell + Suburb_Carnegie + Suburb_PortMelbourne + 
         Suburb_Bentleigh + Suburb_PascoeVale + Suburb_BrightonEast + 
         Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Coburg + Suburb_Northcote + 
         Suburb_Kew + Suburb_Brighton + Suburb_Glenroy + Suburb_GlenIris + 
         Suburb_Essendon + Suburb_Brunswick + Suburb_SouthYarra + 
         Suburb_StKilda + Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
         Suburb_Reservoir + CouncilArea_Whitehorse + CouncilArea_Manningham + 
         CouncilArea_Brimbank + CouncilArea_HobsonsBay + CouncilArea_Bayside + 
         CouncilArea_Melbourne + CouncilArea_Banyule + CouncilArea_PortPhillip + 
         CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
         CouncilArea_GlenEira + CouncilArea_Darebin + CouncilArea_MooneeValley + 
         CouncilArea_Moreland + CouncilArea_Boroondara - CouncilArea_,data = house_train1)
summary(fit)      
fit=step(fit)
formula(fit)
val.predict=predict(fit,newdata = house_train2)
#val.predict
errors=house_train2$Price-val.predict
#errors
errors**2 %>% mean() %>% sqrt()

fit.final=lm(Price~.,
             data=house_train)

sort(vif(fit.final),decreasing = T)
formula(fit.final)
fit.final=lm(Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
               Landsize + BuildingArea + YearBuilt + Method_SP + Method_PI + 
               Method_S + Type_u + Type_h + SellerG_Kay + SellerG_Hodges + 
               SellerG_McGrath + SellerG_Noel + SellerG_Gary + SellerG_Jas + 
               SellerG_Miles + SellerG_Greg + SellerG_Sweeney + SellerG_RT + 
               SellerG_Fletchers + SellerG_Woodards + SellerG_Brad + SellerG_Biggin + 
               SellerG_Ray + SellerG_Buxton + SellerG_Marshall + SellerG_Barry + 
               SellerG_hockingstuart + SellerG_Jellis + SellerG_Nelson + 
               Suburb_Doncaster + Suburb_AscotVale + Suburb_Footscray + 
               Suburb_MooneePonds + Suburb_Thornbury + Suburb_Hampton + 
               Suburb_Yarraville + Suburb_Balwyn + Suburb_MalvernEast + 
               Suburb_Camberwell + Suburb_Carnegie + Suburb_PortMelbourne + 
               Suburb_Bentleigh + Suburb_PascoeVale + Suburb_BrightonEast + 
               Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Coburg + Suburb_Northcote + 
               Suburb_Kew + Suburb_Brighton + Suburb_Glenroy + Suburb_GlenIris + 
               Suburb_Essendon + Suburb_Brunswick + Suburb_SouthYarra + 
               Suburb_StKilda + Suburb_Preston + Suburb_Richmond + Suburb_BentleighEast + 
               Suburb_Reservoir + CouncilArea_Whitehorse + CouncilArea_Manningham + 
               CouncilArea_Brimbank + CouncilArea_HobsonsBay + CouncilArea_Bayside + 
               CouncilArea_Melbourne + CouncilArea_Banyule + CouncilArea_PortPhillip + 
               CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
               CouncilArea_GlenEira + CouncilArea_Darebin + CouncilArea_MooneeValley + 
               CouncilArea_Moreland + CouncilArea_Boroondara - CouncilArea_,data = house_train)
fit.final=step(fit.final)


summary(fit.final)


test.pred=predict(fit.final,newdata=house_test)
write.csv(test.pred,"submision1_regression.csv",row.names = F)

plot(fit.final,1)
plot(fit.final,2)
plot(fit.final,3)
plot(fit.final,4)



###################quiz######################################
value1=house_train$Price
var(value1)


house_train %>%
  group_by(Type) %>%
  summarise(average=mean(Price))

value2=house_train$Postcode
val2=unique(value2)
length(val2)

shapiro.test(house_train$Distance)
qqnorm(house_train$Distance)

house_train%>% group_by(SellerG) %>% aggregate(Price = max(Price))

val3=house_train%>% group_by(CouncilArea) %>% summarise(Price = var(Price))
val3

table(is.na(house_train))
colSums(is.na(house_train))
##########################################################