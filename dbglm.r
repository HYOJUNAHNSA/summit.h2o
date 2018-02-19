db06 <- read.csv("D:/db06040903.csv",header=TRUE)
head(db06)

###data coding

install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)

db06$mch <- revalue(db06$mch,c("GA"="0","PA"="1","TM"="2","방카"="3","홈쇼핑"="4"))

db06$ageg <- revalue(db06$ageg,c("0-9세"="0","10-19세"="1","20-29세"="2","30-39세"="3","40-49세"=4,"50-59세"="5","60-69세"="6","70-79세"="7","80-89세"="8","90-99세"="9"))

db06$bj7 <- revalue(db06$bj7,c("상해"="0","연금"="1","운전자"="2","재물"="3","저축"="4","질병"="5","통합"="6"))

db06$insex <- revalue(db06$insex,c("남"="0","여"="1"))

db06$dbgr <- revalue(db06$dbgr,c("비용불량"="0","비용우량"="1","상해불량"="2","상해우량"="3","상해질병"="4","재물기타"="5","질병불량"="6","질병우량"="7" ))

db06$bojo <- revalue(db06$bojo, c("상질통" = "0", "운전자"="1","재저연"="2"))

db06$agec <- revalue(db06$agec, c("고연령" = "2", "저연령" ="0", "중연령"="1"))

db06$sex <- revalue(db06$sex, c("남"="0","여"="1"))

db06$ch <- revalue(db06$ch,c("GA"="0","PA"="1","신채"="5"))

db06$pl <- db06$liab/db06$opre

#db07 데이터 사용

db07 <- db06

head(db07)

db07 <- filter(db07, pl ==!Inf)

head(db07)

#db06 <- filter(db06, liab > 0)

#db06$pl <- db06$liab/db06$opre

table(is.infinite(db07$pl))

#db06$liab <- filter(db06, db06$liab>0)

head(db06)

head(db06$liab)

db06$pl <- db06$liab/db06$opre

########

str(db06)
head(db06)

########


######

db06.int <- as.integer(as.character(db06))





table(db06$mch)
table(db06$mch == "방카")
gsub("방카","ba",db06$mch)
gsub("홈쇼핑","hs",db06$mch)
sapply(db06,class)
sapply(db06, function(x) gsub("방카","ba",db06$mch))
recode(db06$mch,"방카","ba")

install.packages("Epiccalc")

sub('방카','ba',db06)

str(db06$mch)
#check factor
levels(db06$mch)

#check variable name
names(db06$mch)

#############################
#factor data 변경


#levels(db06$mch)[levels(db06$mch)=="방카"] <- "ba"

#다른 방법


#db06$mch <- revalue(db06$mch,c("홈쇼핑"="hs"))

db06$mch <- revalue(db06$mch,c("GA"="0","PA"="1","TM"="2","방카"="3","홈쇼핑"="4"))

db06$ageg <- revalue(db06$ageg,c("0-9세"="0","10-19세"="1","20-29세"="2","30-39세"="3","40-49세"=4,"50-59세"="5","60-69세"="6","70-79세"="7","80-89세"="8","90-99세"="9"))

db06$bj7 <- revalue(db06$bj7,c("상해"="0","연금"="1","운전자"="2","재물"="3","저축"="4","질병"="5","통합"="6"))

db06$insex <- revalue(db06$insex,c("남"="0","여"="1"))

db06$dbgr <- revalue(db06$dbgr,c("비용불량"="0","비용우량"="1","상해불량"="2","상해우량"="3","상해질병"="4","재물기타"="5","질병불량"="6","질병우량"="7" ))

db06$bojo <- revalue(db06$bojo, c("상질통" = "0", "운전자"="1","재저연"="2"))

db06$agec <- revalue(db06$agec, c("고연령" = "2", "저연령" ="0", "중연령"="1"))

db06$sex <- revalue(db06$sex, c("남"="0","여"="1"))

db06$ch <- revalue(db06$ch,c("GA"="0","PA"="1","신채"="5"))


str(db06)
db06 <- na.omit(db06)
colSums(is.na(db06))
##############################


#target data 만들기


db06$pl <- db06$liab/db06$opre

colSums(is.na(db06))

db07 <- filter(db06, opre>0,liab>0)
db07$pl <- db07$liab/db07$opre

db07.glm <- glm(pl~mch+bj7+ageg+insex+bojo,family=Gamma(link=log),data=db07)

colSums(is.na(db07))
names(db07)

as.Date(db07$fy)

test.db07 <- db07[1:10,]

test.db07 <- as.integer(test.db07)

ab <- test.db07[,c("dbcd","mch","isdt","bj7","ageg","insex")]
ab <- as.logical(ab)
ab

test.db07

head(test.db07)
str(test.db07)


#data summary

write.csv(table(db06$dbcd),"D:/dbo6dbcd.csv")
write.csv(table(db06$dbcd),"D:/dbo6dbcd.csv")
write.csv(table(db06$mch),"D:/dbo6mch.csv")
write.csv(table(db06$bj7),"D:/dbo6bj7.csv")
write.csv(table(db06$ageg),"D:/dbo6ageg.csv")
write.csv(table(db06$insex),"D:/dbo6insex.csv")


summary(db06$opre)
quantile(db06$opre, probs = c(0, 1, 5, 10, 25, 50, 75, 90, 95, 99, 100, NA)/100)

summary(db06$liab)
quantile(db06$liab, probs = c(0, 1, 5, 10, 25, 50, 75, 90, 95, 99, 100, NA)/100)
table(is.na(db06$liab))


#####glm

glm(pl~mch,family=poisson(link=log),data=db07)

