#JACLS_ALL02_csvファイル整え
#Risa Watanabe
#2016/10/07

setwd("//Rinken-sv2/プロトコール別/長期フォロー/LTFU/rawdata")
follow <- read.csv("JACLS_followup2012_160512_1004.csv",as.is=T)
regi<- read.csv("JACLS_registration_160512_1004.csv",as.is=T)


#必要列抽出
follow1<-follow[,c(15,84,86,88)]
regi1<-regi[,c(11,15,21:23)]

#変数名の変更
colnames(follow1)[2:4] <- c("JACLS最終確認日","JACLS生死","JACLS死亡日")

#ALL02_DSとDF3をマージしてALLnoのあるひとのみにする
ALL02_DS1<-merge(ALL02_DS,follow1,by.x="JACLS登録コード",by.y="登録コード",all.x=TRUE)
mergedata<-merge(ALL02_DS1,regi1,by.x="JACLS登録コード",by.y="登録コード",all.x=TRUE)

#最終転帰の列を追加
mergedata$ALIVE_DEATH<-ifelse(is.na(mergedata$JACLS最終確認日)&mergedata$最終確認日=="","LOST",mergedata$生死)
length(mergedata$JACLS登録コード)

#日付をcharacterからdate型に変換
mergedata$治療終了日 <- as.Date(mergedata$治療終了日,format="%Y/%m/%d") 
mergedata$最終確認日 <- as.Date(mergedata$最終確認日,format="%Y/%m/%d") 
mergedata$死亡日 <- as.Date(mergedata$死亡日,format="%Y/%m/%d") 
mergedata$生年月日 <- as.Date(mergedata$生年月日,format="%Y/%m/%d") 
mergedata$診断年月日<- as.Date(mergedata$診断年月日,format="%Y/%m/%d") 


#定義
str1 = mergedata$治療終了日
str2 = mergedata$最終確認日
str3 = mergedata$死亡日
str4 = mergedata$生年月日
str00 = mergedata$診断年月日

#macro
    datedif <- function(starting, ending) {
 y <- as.integer((as.integer(format(ymd(ending),"%Y%m%d")) - as.integer(format(ymd(starting),"%Y%m%d")))/10000)
 months <- as.numeric((as.integer(format(ymd(ending),"%m%d")) - as.integer(format(ymd(starting),"%m%d")))/100)
 ym <- ifelse(months<0, as.integer(months + 12), as.integer(months))
 m <- ym + y*12
 d <- as.integer(ymd(ending) - ymd(starting))
 days <- as.integer(format(ymd(ending),"%d")) - as.integer(format(ymd(starting),"%d"))
 yd <- as.integer(ymd(ending) - (ymd(starting) %m+% months(12*y)))
 md <- ifelse(days<0, ymd(ending) - (ymd(starting) %m+% months(m)), days)
 return(data.frame(y,m,d,ym,yd,md))
}
  library(lubridate)　　#必須
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

#診断時年齢計算
age <- datedif(str4,str00)
  mergedata$age_diagnosis <- age$y

#NAのあるデータ確認
mergedata$END_NA_CD <- ifelse(is.na(str1),"0","1")   #0はNA、1はデータあり
mergedata$FINAL_NA_CD <- ifelse(is.na(str2),"0","1")
mergedata$DEATH_NA_CD <- ifelse(is.na(str3),"0","1")

#治療終了-最終確認の差分を年で算出
Diff1 <-subset(mergedata,mergedata$END_NA_CD=="1"& mergedata$FINAL_NA_CD=="1")
str5 = Diff1$治療終了日
str6 = Diff1$最終確認日
age <- datedif(str5,str6)

  Diff1$DIFF_END_FINAL <- age$y

groupNA1 <-subset(mergedata,mergedata$END_NA_CD=="0"|mergedata$FINAL_NA_CD=="0")
groupNA1$DIFF_END_FINAL <- ""

mergedata <- rbind(Diff1,groupNA1)
length(mergedata$JACLS登録コード)

#生年月日-最終確認の差分算出で最後に確認された年齢算出
Diff0 <-subset(mergedata,mergedata$FINAL_NA_CD=="1")
str11 = Diff0$生年月日
str12 = Diff0$最終確認日
age <- datedif(str11,str12)

  Diff0$AGE_FINAL <- age$y

groupNA0 <-subset(mergedata,mergedata$FINAL_NA_CD=="0")
groupNA0$AGE_FINAL <- ""

mergedata <- rbind(Diff0,groupNA0)
length(mergedata$JACLS登録コード)


#死亡例は治療終了後何年で死んだかを算出、治療終了-死亡日
Diff2 <-subset(mergedata,mergedata$DEATH_NA_CD=="1"& mergedata$END_NA_CD=="1")
str7 = Diff2$治療終了日
str8 = Diff2$死亡日
age <- datedif(str7,str8)

  Diff2$DIFF_END_DEATH <- age$y

groupNA2 <-subset(mergedata,mergedata$DEATH_NA_CD=="0"|mergedata$END_NA_CD=="0")
groupNA2$DIFF_END_DEATH <- ""

mergedata <- rbind(Diff2,groupNA2)
length(mergedata$JACLS登録コード)


#生年月日-死亡日で死亡した年齢を算出
Diff3 <-subset(mergedata,mergedata$DEATH_NA_CD=="1")
str9  = Diff3$生年月日
str10 = Diff3$死亡日
age <- datedif(str9,str10)

  Diff3$AGE_DEATH <- age$y

groupNA3 <-subset(mergedata,mergedata$DEATH_NA_CD=="0")
groupNA3$AGE_DEATH <- ""

mergedata <- rbind(Diff3,groupNA3)
length(mergedata$JACLS登録コード)


colnames(mergedata)[1]<- c("登録コード")

LTFU_DATASET <- mergedata

setwd("../output")
write.csv(LTFU_DATASET,"ALL02data.csv",row.names=F)

setwd("../programs")




