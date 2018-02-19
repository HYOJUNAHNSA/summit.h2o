dbmn <- read.csv("D:dbmn.csv", header=TRUE)
head(dbmn)



install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)

dbmn$mch <- revalue(dbmn$mch,c("GA"="0","PA"="1","TM"="2","방카"="3","홈쇼핑"="4"))

dbmn$ageg <- revalue(dbmn$ageg,c("0-9세"="0","10-19세"="1","20-29세"="2","30-39세"="3","40-49세"=4,"50-59세"="5","60-69세"="6","70-79세"="7","80-89세"="8","90-99세"="9"))

dbmn$bj7 <- revalue(dbmn$bj7,c("상해"="0","연금"="1","운전자"="2","재물"="3","저축"="4","질병"="5","통합"="6"))

dbmn$insex <- revalue(dbmn$insex,c("남"="0","여"="1"))

dbmn$dbgr <- revalue(dbmn$dbgr,c("비용불량"="0","비용우량"="1","상해불량"="2","상해우량"="3","상해질병"="4","재물기타"="5","질병불량"="6","질병우량"="7" ))

dbmn$bojo <- revalue(dbmn$bojo, c("상질통" = "0", "운전자"="1","재저연"="2"))

dbmn$agec <- revalue(dbmn$agec, c("고연령" = "2", "저연령" ="0", "중연령"="1"))

dbmn$sex <- revalue(dbmn$sex, c("남"="0","여"="1"))

dbmn$ch <- revalue(dbmn$ch,c("GA"="0","PA"="1","신채"="5"))

dbmn$pl <- dbmn$liab/dbmn$opre #l/p

head(dbmn)

colSums(is.na(dbmn))

str(dbmn)

dbmn$pl <- as.integer(dbmn$pl)

summary(dbmn$pl)

a <- filter(dbmn, pl >0)
a$dbcd <- as.factor(a$dbcd)
str(a)
head(a)
summary(a$pl)

dbmn.a <- a

 glm(pl ~ dbcd + mch + bj7 + ageg + dbgr + bojo + ch, family=Gamma(link=log), a)

sel <- glm(pl ~ dbcd + mch + bj7 + ageg + dbgr + bojo + ch, family=gaussian, a)

### test train data 분리

#str(data5.a.s)
n=nrow(dbmn.a) #test train data 분리
dbmn.a.index <- sample(1:n,round(0.75*n))
dbmn.a.tr <- dbmn.a[dbmn.a.index,]
dbmn.a.te <- dbmn.a[-dbmn.a.index,]


## data frame 변환


## glm fit - select variable

dbmn.gl.fit <- glm(pl ~ dbcd + mch + bj7 + ageg + insex + dbgr + bojo, family = gaussian, dbmn.a)
step(dbmn.gl.fit)

## glm fit - fitting
dbmn.glm.fit.tr <- glm(pl ~ dbcd + bj7 + ageg, family = gaussian, data = dbmn.a.tr)

pred.glm <- predict(dbmn.glm.fit.tr, dbmn.a.te)
summary(pred.glm)
str(pred.glm)

str(dbmn.a.te$pl)
str(dbmn.a.tr$pl)

##mse


## glm mape value

install.packages("MLmetrics")
library(MLmetrics)
MAPE(pred.glm,dbmn.a.te$pl)
MSE(pred.glm,dbmn.a.te$pl)
install.packages("Metrics")
library(Metrics)
mape(dbmn.a.te$pl,pred.glm)
mse(dbmn.a.te$pl,pred.glm)

##neural net data scale

dbmn.a <- as.double(dbmn.a)
dbmn.a.i <- dbmn.a
dbmn.a.i$dbcd <- as.integer(dbmn.a.i$dbcd)
dbmn.a.i$mch <- as.integer(dbmn.a.i$mch)
dbmn.a.i$bj7 <- as.integer(dbmn.a.i$bj7)
dbmn.a.i$ageg <- as.integer(dbmn.a.i$ageg)
dbmn.a.i$insex <- as.integer(dbmn.a.i$insex)
dbmn.a.i$dbgr <- as.integer(dbmn.a.i$dbgr)
dbmn.a.i$bojo <- as.integer(dbmn.a.i$bojo)
dbmn.a.i$agec <- as.integer(dbmn.a.i$agec)
dbmn.a.i$sex <- as.integer(dbmn.a.i$sex)
dbmn.a.i$ch <- as.integer(dbmn.a.i$ch)
maxs <- apply(dbmn.a.i, 2, max)
mins <- apply(dbmn.a.i, 2, min)
dbmn.a.i.s <- as.data.frame(scale(dbmn.a.i ,center=mins, scale=maxs - mins))
head(dbmn.a.i.s)
str(dbmn.a.i.s)
### data split

n=nrow(dbmn.a.i.s) #test train data 분리
dbmn.a.i.s.index <- sample(1:n,round(0.75*n))
dbmn.a.i.s.tr <- dbmn.a.i.s[dbmn.a.i.s.index,]
dbmn.a.i.s.te <- dbmn.a.i.s[-dbmn.a.i.s.index,]

str(dbmn.a.i.s.tr)
##neural net
library(neuralnet)


dbmn.ne.fit.tr <- neuralnet(pl ~ dbcd + bj7 + ageg, data= dbmn.a.i.s.tr,hidden = c(1,2), linear.output=TRUE)
print(dbmn.ne.fit.tr)

#dbmn.ne.fit.te <- compute(dbmn.ne.fit.tr,dbmn.a.i.s.te)

##mse 

dbmn.ne.fit.te <- compute(dbmn.ne.fit.tr,dbmn.a.i.s.te[,c("dbcd","bj7","ageg")])

ne.te <- dbmn.ne.fit.te$net.result * (max(dbmn.a.i.s.tr$pl)-min(dbmn.a.i.s.tr$pl))+min(dbmn.a.i.s.tr$pl)

ne.te.r <- (dbmn.a.i.s.te$pl)*(max(dbmn.a.i.s.tr$pl)-min(dbmn.a.i.s.tr$pl))+ min(dbmn.a.i.s.tr$pl)


## variable

# dbcd, bj7, ageg


#mse value <- 식 계산 

mse.neu <- sum((ne.te.r - ne.te)^2)/nrow(dbmn.a.i.s.te)

install.packages("MLmetrics")
library(MLmetrics)
install.packages("Metrics")
library(Metrics)

##Neural net
#mape

mape(dbmn.a.i.s.te$pl,dbmn.ne.fit.te$net.result)
MAPE(dbmn.ne.fit.te$net.result,dbmn.a.i.s.te$pl)


#mse

mse(dbmn.a.i.s.te$pl,dbmn.ne.fit.te$net.result)
MSE(dbmn.ne.fit.te$net.result,dbmn.a.i.s.te$pl)
#0.0003287714932



## glm mape value


MAPE(pred.glm,dbmn.a.te$pl)
MSE(pred.glm,dbmn.a.te$pl)

mape(dbmn.a.te$pl,pred.glm)
mse(dbmn.a.te$pl,pred.glm)

