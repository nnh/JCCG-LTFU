#JACLS_ALL02_csvファイル整え
#Mamiko Yonejima
#2016/10/07

setwd("../rawdata")
DF <- read.csv("症例基本情報一覧(提出用)_150911pw.csv",as.is=T)

#1,2行目削除
DF1 <- DF[c(3:1254),]

#必要列抽出
DF2 <- DF1[,c(1,2,6,45,46)]
#変数名変更
colnames(DF2) <- c("ALLNo","JACLS登録コード","診断年月日","治療終了日","終了種別")

ALL02_DS<-DF2
setwd("../programs")