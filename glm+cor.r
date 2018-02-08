data2 <- read.csv("D:/data2.csv", header=TRUE)
colSums(is.na(data2)) #missing value check
data3 <- data2
data4 <- na.omit(data3) #omit missing value
colSums(is.na(data4)) # check missing value after omit
attach(data4)
data4$bj7 <- as.integer(data4$bj7) #factor -> integer
cor(data4[,c(3,4)])

install.packages("dplyr")
library(dplyr)
install.packages("corrplot")
library(corrplot)
corrplot(cor(data4[,1:14]))
corrplot(cor(data4[,1:14]), method="circle",addCoef.col="green")
cor(data4[,1:14])
table(dbgr)
cor(DBCD,dbgr)
cor(DBCD,dbgr==0)
#plot(DBCD~dbgr==0)
cor(DBCD,dbgr==1)
cor(DBCD,dbgr==2)
cor(DBCD,dbgr==3)
cor(DBCD,dbgr==4)
cor(DBCD,dbgr==5)
cor(DBCD,dbgr==6)
cor(DBCD,dbgr==7)
cor(opre,sex==0)
cor(opre,sex==1)
cor(liab,sex==0)
cor(liab,sex==1)
cor(opre,ch==0)
cor(opre,ch==1)
cor(opre,ch==5)
cor(liab,ch==0)
cor(liab,ch==1)
cor(liab,ch==5)
#cor(liab/opre,sex)
liab/opre


cor(liab,dbgr==0)
cor(liab,dbgr==1)
cor(liab,dbgr==2)
cor(liab,dbgr==3)
cor(liab,dbgr==4)
cor(liab,dbgr==5)
cor(liab,dbgr==6)
cor(liab,dbgr==7)
cor(opre,dbgr==0)
cor(opre,dbgr==1)
cor(opre,dbgr==2)
cor(opre,dbgr==3)
cor(opre,dbgr==4)
cor(opre,dbgr==5)
cor(opre,dbgr==6)
cor(opre,dbgr==7)




data4$bj7 <- as.integer(data4$bj7)
head(data4)

#pre data
data5 <- data4
data5 <- filter(data5, opre >0, liab>0) # remove negative value
str(data5) # check data structure
data5$pl <- data5$liab/data5$opre
colSums(is.na(data5))
data6 <- filter(data5, opre>liab)
str(data6)
data6$pl <- data6$liab/data6$opre

#glm using data5
glm(pl~factor(sex),family=Gamma,data=data5)
glm(pl~sex,family=Gamma(link=log),data=data6)
