install.packages("dplyr")
library(dplyr)
#data loading

data2 <- read.csv("D:/data2.csv", header=TRUE)
colSums(is.na(data2)) #missing value check
data3 <- data2
data4 <- na.omit(data3) #omit missing value
colSums(is.na(data4)) # check missing value after omit
attach(data4)
data4$bj7 <- as.integer(data4$bj7) #factor -> integer

#glm
#data scailing
data5 <- filter(data4, opre >0, liab>0) # remove negative value
#str(data5) # check data structure
data5$pl <- data5$liab/data5$opre #발생손해액/원보험료
colSums(is.na(data5)) #결측치 확인
head(data5)

data5.va <- c("DBCD","mcn","isd","bj7","ageg","isex","dbgr","bjg","agec","sex","ch","pl") #사용 변수 선택
data5.a <- data5[,data5.va]
data5.a.s <- scale(data5.a)
head(data5.a)
head(data5.a.s)
#data5.a.s
#str(data5.a.s)
n=nrow(data5.a.s) #test train data 분리
data5.index <- sample(1:n,round(0.75*n))
data5.a.s.tr <- data5.a.s[data5.index,]
data5.a.s.te <- data5.a.s[-data5.index,]

#neural net
install.packages("neuralnet")
library(neuralnet)
fit.neural <- neuralnet(pl~ch,data=data5.a.s.tr, hidden=c(1,2), linear.output = TRUE)
pred.neural <- compute(fit.neural, data5.a.s.te[,11])
summary(pred.neural)
pred.neural
#glm

data5.a.s.te.df <- data.frame(data5.a.s.te)
data5.a.s.tr.df <- data.frame(data5.a.s.tr)
fit.glm <- glm(pl~DBCD+mcn+isd+bj7+ageg+isex+dbgr+bjg+ch, data=data5.a.s.tr.df)

pred.glm <- predict(fit.glm,data5.a.s.te.df)
summary(pred.glm)
head(pred.glm)
pred.glm
summary(fit.glm)
