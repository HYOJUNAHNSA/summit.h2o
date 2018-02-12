db06 <- read.csv("D:/db06040903.csv",header=TRUE)
head(db06)
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

levels(db06$mch)[levels(db06$mch)=="방카"] <- "ba"

#다른 방법
install.packages("plyr")
library(plyr)

db06$mch <- revalue(db06$mch,c("홈쇼핑"="hs"))

db06$ageg <- revalue(db06$ageg,c("0-9세"="0","10-19세"="1","20-29세"="2","30-39세"="3","40-49세"=4,"50-59세"="5","60-69세"="6","70-79세"="7","80-89세"="8","90-99세"="9"))

db06$bj7 <- revalue(db06$bj7,c("상해"="0","연금"="1","운전자"="2","재물"="3","저축"="4","질병"="5","통합"="6"))

db06$insex <- revalue(db06$insex,c("남"="0","여"="1"))

db06$dbgr <- revalue(db06$dbgr,c("비용불량"="0","비용우량"="1","상해불량"="2","상해우량"="3","상해질병"="4","재물기타"="5","질병불량"="6","질병우량"="7" ))
##############################


#target data 만들기


db06$pl <- db06$liab/db06$opre

colSums(is.na(db06))

db07 <- filter(db06, opre>0,liab>0)
db07$pl <- db07$liab/db07$opre

db07.glm <- glm(pl~mch+bj7+ageg+insex+bojo,family=Gamma(link=log),data=db07)

