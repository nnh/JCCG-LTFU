
#JPLSG_AML05_のデータから、ALIVE_DEATH、最終確認日データの作成
#Sahoko Ando
#2016/10/13


#################################

#死亡 0:なし, 1:あり
#1=死亡=TRUE
#0=生存=FALSE

#############################

#AML05 
#1=死亡=TRUE
#0=生存=FALSE
#JPLSGの転帰（生死）
#死亡=TRUE
#生存=FALSE

############################

#ALIVE_DEATHの列を作成する
#「最終確認日」　の列を作成する


#05死亡のグループ　05死亡ーALIVE_DEATH「TRUE」
#「最終確認日」=05の最終確認日
str1 <-subset(merge_J05,merge_J05$死亡=="1")
str1$ALIVE_DEATH <- "TRUE"
str1$最終確認日 <-　str1$最終確認日AML05
str1$死亡日 <-　str1$最終確認日AML05


#05生存 & JP死亡のグループ　ALIVE_DEATH「TRUE」
#「最終確認日」　＝　JPの最終確認日
str2 <-subset(merge_J05,merge_J05$死亡=="0" & merge_J05$生死=="true" &merge_J05$最終確認日JPLSG!="" )
str2$ALIVE_DEATH <- "TRUE"
str2$最終確認日 <-　str2$最終確認日JPLSG
str2$死亡日 <-　str2$最終確認日JPLSG

#05生存 & JP死亡 & 最終確認日JPLSGが不明　ALIVE_DEATH「LOST」
#「最終確認日」　＝　JPの最終確認日
str5 <-subset(merge_J05,merge_J05$死亡=="0" & merge_J05$生死=="true" & merge_J05$最終確認日JPLSG=="" )
str5$ALIVE_DEATH <- "LOST"
str5$最終確認日 <-　str5$最終確認日JPLSG
str5$死亡日 <-　str5$最終確認日JPLSG


#05生存 & JP生存のグループ　ALIVE_DEATH「FALSE」
#「最終確認日」　＝　JPの最終確認日
str3 <-subset(merge_J05,merge_J05$死亡=="0" & merge_J05$生死=="false")
str3$ALIVE_DEATH <- "FALSE"
str3$最終確認日 <-　str3$最終確認日JPLSG
str3$死亡日 <-　""

#05生存 & JP「生死不明」のグループ　ALIVE_DEATH「LOST」
#「最終確認日」　＝　JPの最終確認日
str4 <-subset(merge_J05,merge_J05$死亡=="0" & merge_J05$生死=="")
str4$ALIVE_DEATH <- "LOST"
str4$最終確認日 <-　str4$最終確認日AML05
str4$死亡日 <-　""


#streのDFをひとつのDFにまとめる
str1234 <- rbind(str1,str2,str3,str4,str5)
length(str1234$JPLSG登録コード)



#日付をcharacterからdate型に変換
str1234$終了日 <- as.Date(str1234$終了日,format="%Y/%m/%d") 
str1234$最終確認日 <- as.Date(str1234$最終確認日,format="%Y/%m/%d") 
str1234$死亡日 <- as.Date(str1234$死亡日,format="%Y/%m/%d") 
str1234$生年月日 <- as.Date(str1234$生年月日,format="%Y/%m/%d") 
str1234$診断年月日<- as.Date(str1234$診断年月日,format="%Y/%m/%d") 

length(str1234$JPLSG登録コード)


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


#定義
aa1 = str1234$終了日
aa2 = str1234$最終確認日
aa3 = str1234$死亡日
aa4 = str1234$生年月日
aa00 = str1234$診断年月日

#診断時年齢計算
age <- datedif(aa4,aa00)
  str1234$age_diagnosis <- age$y

#治療終了-最終確認の差分を年で算出
Diff1 <-subset(str1234,str1234$ALIVE_DEATH=="TRUE" | str1234$ALIVE_DEATH=="FALSE")
aa5 = Diff1$終了日
aa6 = Diff1$最終確認日
age <- datedif(aa5,aa6)

  Diff1$DIFF_END_FINAL <- age$y

groupNA1 <-subset(str1234,str1234$ALIVE_DEATH=="LOST")

groupNA1$DIFF_END_FINAL <- ""

str1234 <- rbind(Diff1,groupNA1)
length(str1234$JPLSG登録コード)


#生年月日-最終確認の差分算出で最後に確認された年齢算出
Diff0 <-subset(str1234,str1234$AML05No!=361)
str11 = Diff0$生年月日
str12 = Diff0$最終確認日
age <- datedif(str11,str12)
  Diff0$AGE_FINAL <- age$y
groupNA0 <-subset(str1234,str1234$AML05No==361)
groupNA0$AGE_FINAL <- ""
str1234 <- rbind(Diff0,groupNA0)
length(str1234$JPLSG登録コード)


#死亡例は治療終了後何年で死んだかを算出、治療終了-死亡日
Diff2 <-subset(str1234,str1234$ALIVE_DEATH=="TRUE")
aa7 = Diff2$終了日
aa8 = Diff2$死亡日
age <- datedif(aa7,aa8)

  Diff2$DIFF_END_DEATH <- age$y

groupNA2 <-subset(str1234,str1234$ALIVE_DEATH=="FALSE" | str1234$ALIVE_DEATH=="LOST")
groupNA2$DIFF_END_DEATH <- ""

str1234 <- rbind(Diff2,groupNA2)
length(str1234$JPLSG登録コード)


#生年月日-死亡日で死亡した年齢を算出
Diff3 <-subset(str1234,str1234$ALIVE_DEATH=="TRUE")
aa9  = Diff3$生年月日
aa10 = Diff3$死亡日
age <- datedif(aa9,aa10)

  Diff3$AGE_DEATH <- age$y

groupNA3 <-subset(str1234,str1234$ALIVE_DEATH=="FALSE" | str1234$ALIVE_DEATH=="LOST")
groupNA3$AGE_DEATH <- ""

str1234 <- rbind(Diff3,groupNA3)
length(str1234$JPLSG登録コード)

write.csv(str1234,"//Rinken-sv2/プロトコール別/長期フォロー/LTFU/output/AML05dataset.csv",row.names=F)

