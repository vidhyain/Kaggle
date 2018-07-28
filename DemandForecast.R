#loading the data
install.packages("dplyr")

install.packages("forecast")
library("forecast")
library('dplyr')
train=read.csv('train.csv')
test=read.csv('test.csv')
sample_data=read.csv('sample_submission.csv')
names(train)
str(train)
train<-train%>%mutate(store=as.factor(store),item=as.factor(item))
nrow(filter(train,(store==9&item==1)))
table(train$item)
train$date=as.Date(train$date)
colSums(is.na(train))

#No of stores
unique(train$store)
# No of items
unique(train$item)
summary(train)
(filter(train,item==1))

train%>% group_by(store,item)%>%summarise(Q1=quantile(sales,probs=0.05),Q2=quantile(sales,probs=0.10),Q3=quantile(sales,probs=0.25),Q4=quantile(sales,probs=0.50),Q5=quantile(sales,probs=0.75),Q6=quantile(sales,probs=0.90),Q7=quantile(sales,probs=0.95),Q8=quantile(sales,probs=0.99))%>%group_by(store)%>%summarise(sum_of_Q1=sum(Q1),sum_of_Q2=sum(Q2),sum_of_Q3=sum(Q3),sum_of_Q4=sum(Q4),sum_of_Q5=sum(Q5),sum_of_Q6=sum(Q6),sum_of_Q7=sum(Q7),sum_of_Q8=sum(Q8))
      
install.packages("ggmap")
library("ggmap")   
ggplot(data=filter(train,item==1))+geom_boxplot(mapping = aes(item,sales,color=store))
        
#different store sells same item in different count, it is better to split the data based on item and store and develop model for each item sold in each store.
X=split(train,list(train$store,train$item))
Y <- lapply(seq_along(X), function(x) as.data.frame(X[[x]])[, 4])
list2env(Y,envir=.GlobalEnv)
install.packages('forecast')
library('forecast')
model = list()
fit = list()
Z=list()
for (i in 1:500) 
  {
  model[[i]] <- auto.arima(Y[[i]])
  fit[[i]] <- fitted(model[[i]])
  Z[[i]]=cbind(Y[[i]],fit[[i]])
}
Y[[1]]
sapply(Z, function(x) write.table( data.frame(x), 'fit.csv'  , append= T, sep=',' ))
sapply(model, function(x) write.table(x,))
summary(model[[1]])

