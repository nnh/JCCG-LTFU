#JPLSG_AML05_csvファイル整え
#Sahoko Ando
#2016/10/11

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(dirname(frame_files[[length(frame_files)]]))

setwd(paste(PATH,"/rawdata",sep=""))
#setwd("../rawdata")
DF <- read.csv("AML-05データ_20161011.csv",as.is=T,skip=11,header=T)
#DF <- read.csv("AML-05データ_20161011.csv",as.is=T)
#変数名削除
#DF1 <- DF[c(2:485),]
#length(DF1$AML05.No.)

#変数名変更
colnames(DF1) <- c("AML05No","JPLSG登録コード","事後不適格","解析対象外",
                   "診断年月日","生年月日","生年月日_入力不可_1：あり※ダミーの日付入力",
                  "中止届有無","中止届に記載された中止日","therapy最終投薬日",
                  "リスク",
                  "移植有無","移植日","死亡","最終確認日")

length(DF1$AML05No)

#事後不適格を外す
DF2 <- subset(DF1,DF1$事後不適格!=3)

#解析対象外を外す
DF3 <- subset(DF2,DF2$解析対象外!=1)

AML05_DF<-DF3

#データの出力
#write.csv(AML05_DF,"//Rinken-sv2/プロトコール別/長期フォロー/LTFU/output/AML05_DF.csv",row.names=F)

#####################
#中止届有無
#0：なし、
#1：あり

#移植有無
#0：なし、1：あり

#リスク 1：低リスク　
#2：中間リスク　
#3：高リスク　　
#0：治療中止
#7：リスク判定時に中止
#8：リスク判定間違いで中止
#9：リスク判定前中止

#死亡 0:なし, 1:あり
#######################

#終了日の作成
#試験中止＝中止日
sample <-subset(AML05_DF,AML05_DF$中止届有無==1)
sample$終了日 <-　sample$中止届に記載された中止

#高リスク(高リスク=3),移植あり＝移植日
sample1 <- subset(AML05_DF,AML05_DF$リスク==3 & AML05_DF$移植有無==1 & AML05_DF$中止届有無!=1) #中止届が出ているものは除く
sample1$終了日 <-　sample1$移植日

#高リスク(高リスク=3),移植なし＝therapy最終投薬日
sample2 <- subset(AML05_DF,AML05_DF$リスク==3 & AML05_DF$移植有無==0　& AML05_DF$中止届有無!=1) #中止届が出ているものは除く
sample2$終了日 <-　sample2$therapy最終投薬日

#高リスク以外の人＝therapy最終投薬日
sample3_1 <-subset(AML05_DF,AML05_DF$中止届有無!=1)#中止届が出ているものは除く
sample3 <- subset(sample3_1,sample3_1$リスク!=3)
sample3$終了日 <-　sample3$therapy最終投薬日

#データの出力
#write.csv(sample,"//Rinken-sv2/プロトコール別/長期フォロー/LTFU/output/AML05_sample.csv",row.names=F)
#write.csv(sample1,"//Rinken-sv2/プロトコール別/長期フォロー/LTFU/output/AML05_sample1.csv",row.names=F)
#write.csv(sample2,"//Rinken-sv2/プロトコール別/長期フォロー/LTFU/output/AML05_sample2.csv",row.names=F)
#write.csv(sample3,"//Rinken-sv2/プロトコール別/長期フォロー/LTFU/output/AML05_sample3.csv",row.names=F)

#sampleのDFをひとつのDFにまとめる
sample_1 <- rbind(sample,sample1)
sample_12 <- rbind(sample_1,sample2)
sample_123 <- rbind(sample_12,sample3)

length(sample_123$AML05No)

#出力する
#write.csv(sample_123,"//Rinken-sv2/プロトコール別/長期フォロー/LTFU/output/AML05_DF_ALL.csv",row.names=F)

#データフレーム名の書き換え
AML05_DF_ALL <- sample_123

#AML05のデータから必要な変数名のみ抽出
AML05 <- AML05_DF_ALL[,c(1,2,5,14,15,16)]

#JPLSGデータの読み込み
JP_DF <- read.csv("JPLSG_registration_161005_0955.csv",as.is=T)

#JPデータ　必要な変数名のみ抽出
JP_DF1 <- JP_DF[,c("登録コード","生年月日","生死","最終確認日")]

#マージする
merge_J05 <- merge(AML05,JP_DF1,by.x="JPLSG登録コード",by.y="登録コード",all.x=T)


#変数名の書き換え
names(merge_J05)[5] <- c("最終確認日AML05")
names(merge_J05)[9] <- c("最終確認日JPLSG")

setwd("../output")

#出力する
write.csv(merge_J05,"JP_AML05.csv",row.names=F)

setwd("../programs")
