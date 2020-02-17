rm(list=ls())
EPI_data <- read.csv(file.choose(),header = TRUE)
attach(EPI_data)
head(EPI_data)
#--------------------Lab2 - part1--------------------
#Central Tendency for EPI and DALY

summary(EPI_data$EPI)
mean(EPI_data$EPI,na.rm = TRUE)
median(EPI_data$EPI, na.rm = TRUE)
names(table(EPI_data$EPI))[table(EPI_data$EPI) == max(table(EPI_data$EPI))]


summary(EPI_data$DALY)
mean(EPI_data$DALY,na.rm = TRUE)
median(EPI_data$DALY, na.rm = TRUE)
names(table(EPI_data$DALY))[table(EPI_data$DALY) == max(table(EPI_data$DALY))]
#histogram for EPI and DALY
hist(EPI_data$EPI)
hist(EPI_data$DALY)

library("dplyr")

sample_n(EPI_data[!is.na(EPI_data$EPI),],5)$EPI
sample_n(EPI_data[!is.na(EPI_data$DALY),],5)$DALY

sample_frac(EPI_data[!is.na(EPI_data$EPI),],0.1)$EPI
sample_frac(EPI_data[!is.na(EPI_data$EPI),],0.1)$DALY

new_decs_EPI <- arrange(EPI_data,desc(EPI))$EPI
new_decs_DALY <- arrange(EPI_data,desc(DALY))$DALY

double_EPI <- mutate(EPI_data,MyNewColmn = EPI*2)
double_DALY <- mutate(EPI_data,MyNewColmn = DALY*2)


summarise(EPI_data,avg_EPI = mean(EPI, na.rm = TRUE))
summarise(EPI_data,avg_DALY = mean(DALY, na.rm = TRUE))
boxplot(ENVHEALTH,ECOSYSTEM)
qqplot(ENVHEALTH,ECOSYSTEM)


#Lab2b
#ENVH
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH <-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
predENVH<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
names(predENVH) <- c("DALY","AIR_H","WATER_H")
pENV<- predict(lmENVH,predENVH,interval='prediction')
cENV<- predict(lmENVH,predENVH,interval='confidence')
pENV
cENV

#AIR_E
lmAIR_E <- lm(AIR_E~DALY+AIR_H+WATER_H)
lmENVH
lmAIR_E
summary(lmAIR_E)
coefAIR_E<-coef(lmAIR_E)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
predAIR_E <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
names(predAIR_E) <- c("DALY","AIR_H","WATER_H")
pAIR_E <- predict(lmAIR_E,predAIR_E,interval='prediction')
cAIR_E<- predict(lmAIR_E,predAIR_E,interval='confidence')
pAIR_E
cAIR_E


#CLIMATE
lmCLIMATE <- lm(CLIMATE~DALY+AIR_H+WATER_H)
lmCLIMATE
summary(lmCLIMATE)
coeCLIMATE<-coef(lmCLIMATE)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
predCLIMATE <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
names(predCLIMATE) <- c("DALY","AIR_H","WATER_H")
pCLIMATE <- predict(lmAIR_E,predCLIMATE,interval='prediction')
cCLIMATE<- predict(lmAIR_E,predCLIMATE,interval='confidence')
pCLIMATE
cCLIMATE


#-----------------------Lab2 - part2 - Assignment2-----------------
rm(list=ls())
Mreg <- read.csv(choose.files())
attach(Mreg)
lmROLL <-lm(ROLL~UNEM+HGRAD)
lmROLL
summary(lmROLL)
UNEM <- 7
HGRAD <- 90000
predROLL <- data.frame(UNEM,HGRAD)
pROLL<- predict(lmROLL,predROLL,interval='prediction')
cROLL<- predict(lmROLL,predROLL,interval='confidence')
pROLL
cROLL

Mreg$INC
boxplot(ROLL,UNEM,HGRAD,INC)
lmROLL2 <- lm(ROLL~UNEM+HGRAD+INC,data=Mreg)
lmROLL2
summary(lmROLL2)
UNEM <- 7
HGRAD <- 90000
INC <-25000
predROLL2 <- data.frame(UNEM,HGRAD,INC)
pROLL2<- predict(lmROLL2,predROLL2,interval='prediction')
cROLL2<- predict(lmROLL2,predROLL2,interval='confidence')
pROLL2
cROLL2

#Classification
rm(list=ls())
aba <- read.csv(choose.files(),header = TRUE)
summary(aba$Rings)
aba$Rings <- as.numeric(aba$Rings)
aba$Rings<-cut(aba$Rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
aba$Rings <- as.factor(aba$Rings)

z <- aba
z$Sex <- NULL

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

z[1:7] <- as.data.frame(lapply(z[1:7],normalize))
ind <- sample(2,nrow(z),replace = TRUE,prob = c(0.7,0.3))
KNNtrain <- z[ind==1,]
KNNtest <- z[ind==2,]
#install.packages("class")
library("class")
sqrt(2948)
K <- 55
KNNpred<-knn(train=KNNtrain[1:7], test=KNNtest[1:7], cl=KNNtrain$Rings, k=55)
KNNpred

#clustering
library(ggplot2)
head(iris)
str(iris)
summary(iris)
newiris<- iris[,-5]
sapply(newiris,var)

ggplot(iris,aes(x=Petal.Length,y=Petal.Width,col=Species))+geom_point()
set.seed(300)
K.max <- 12
K <- 10
wss <-sapply(1:K.max,function(K){kmeans(newiris[3:4],K,nstart=20,iter.max=1000)$tot.withinss})
plot(1:K.max,wss,type="b",xlab = "Number of cluster(K)",ylab="Within cluster sum of squares")
icluster <- kmeans(newiris[,3:4],3,nstart=20)
table(icluster$cluster,iris$Species)

