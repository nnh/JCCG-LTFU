# LTFU
# mamiko yonejima
# 2017/1/13
##################

YearDif <- function(starting, ending) {
# 満xx年を作る関数
  if (starting != "" && ending != "") {
    period <- as.integer((as.integer(format(as.Date(ending), "%Y%m%d"))
                          - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
  } else {
    period <- ""
  }
  return(period)
}

FollowupRate <- function(dataframe) {
# フォローアップ率を作る関数
    sum(dataframe$followup.in.2y == T) / sum(dataframe$death.before.2y == F)
}

kFixDateAml05 <- "2016/10/05"
kFixDateAll02 <- "2016/05/12"

# Read external data
setwd("./input")
prefecture <- read.csv('Prefecture.csv', as.is=T, fileEncoding='UTF-8-BOM')

# Read CSV rawdata, ファイル名の最初の5文字をdataframe名にして読み込む
setwd("../rawdata")
filenames <- list.files()
for (i in 1:length(filenames)) {
  assign(substr(filenames[i], 1, 5), read.csv(filenames[i], as.is=T, fileEncoding='CP932'))
}

# JACLS-ALL-02
jacls.registration <- regis[c("現施設名", "登録コード")]
all02.pick0 <- ALL02[c("JACLS登録コード", "診断年月日", "治療終了日")]
# 現施設名をマージする(regitrationから施設名はとる)
all02.pick <- merge(all02.pick0, jacls.registration, by.x="JACLS登録コード", by.y="登録コード", all.x=T)
# 現施設名と施設コードをマージして、SCSTRESC（県コード）を作成
merge.02.facil <- merge(all02.pick, facil, by.x="現施設名", by.y="施設名", all.x=T)
merge.JACLS <- merge(merge.02.facil, JACLS, by.x="JACLS登録コード", by.y="登録コード", all.x=T)
merge.JACLS$SCSTRESC <- floor(as.numeric(merge.JACLS$施設CD) / 10000000)  # 施設コードの上2桁が県コード
merge1 <- merge.JACLS[c("JACLS登録コード", "診断年月日", "治療終了日", "生年月日", "生死", "死亡日", "最終確認日", "SCSTRESC")]
# merge
# merge0 <- merge(all02.pick, jacls.pick, by="JACLS登録コード", all=T)
# merge1 <- merge0[,　c(1:3,5:9)]
names(merge1)[c(1:7)] <- c("SUBJID", "MHSTDTC", "date.end.trt", "BRTHDTC", "DTHFL", "DTHDTC", "DSSTDTC")
merge1$STUDYID <- "ALL02"
merge1$DTHFL[merge1$DTHFL == "true"] <- T
merge1$DTHFL[merge1$DTHFL == "false"] <- F

# JPLSG-AML-05
# Pick up and proccessing data from AML05(終了日の列)
aml05.pick <- subset(AML05, is.na(AML05$解析対象外))
aml05.pick[is.na(aml05.pick)] <- "-"  # Replace NA to "-"

for (i in 1:length(aml05.pick$J_CD)) {
  if (aml05.pick$中止届有無[i] == 1) {
    aml05.pick$date.end.trt[i] <- aml05.pick$中止日[i]
  } else if (aml05.pick$移植有無[i] == 1) {
    aml05.pick$date.end.trt[i] <- aml05.pick$移植日[i]
  } else {
    aml05.pick$date.end.trt[i] <- aml05.pick$therapy最終投薬日[i]
  }
}

aml05.pick1 <- aml05.pick[c("J_CD", "診断日", "死亡.有り無し", "最終確認日", "date.end.trt")]
names(aml05.pick1)[4] <- "AML05最終確認日"
#現施設名と施設コードをマージして、SCSTRESC(県コード)を作成
merge.JPLSG <- merge(JPLSG, facil, by.x="現施設名", by.y="施設名", all.x=T)
merge.JPLSG$SCSTRESC <- floor(as.numeric(merge.JPLSG$施設CD) / 10000000)  # 施設コードの上2桁が県コード

# Pick up data from JPLSG
jplsg.pick <- merge.JPLSG[c("登録コード", "生年月日", "生死", "死亡日", "最終確認日", "SCSTRESC")]
merge2 <- merge(aml05.pick1, jplsg.pick, by.x="J_CD", by.y="登録コード", all.x=T)

# Proccessing data from merge data(生死の列)
merge2$DTHFL <- ifelse(merge2$死亡.有り無し == "1", T, merge2$生死)
merge2$DTHFL[merge2$DTHFL == "true"] <- T
merge2$DTHFL[merge2$DTHFL == "false"] <- F

for (i in 1:length(merge2$J_CD)) {
  if ((merge2$DTHFL[i] == T) & (merge2$死亡日[i] == "")) {
    merge2$DTHDTC[i] <- merge2$AML05最終確認日[i]
  } else if (merge2$DTHFL[i] == T) {
    merge2$DTHDTC[i] <- merge2$死亡日[i]
  } else {
    merge2$DTHDTC[i] <- ""
  }
}

# TODO(yonejima): ここ修正の必要あり
merge2$DSSTDTC <- ifelse((merge2$最終確認日 == ""), merge2$AML05最終確認日, merge2$最終確認日)

merge2.1 <- merge2[c("J_CD", "診断日", "date.end.trt", "生年月日", "DTHFL", "DTHDTC", "DSSTDTC", "SCSTRESC")]
names(merge2.1)[c(1, 2, 4)] <- c("SUBJID", "MHSTDTC", "BRTHDTC")
merge2.1$STUDYID <- "AML05"

# JACLS-ALL-02 + JPLSG-AML-05
data.set <- rbind(merge1, merge2.1)

#解析対象集団の抽出
data.set[is.na(data.set)] <- ""  # Replace NA to ""
data.set$fix.date <- ifelse(data.set$STUDYID == "AML05", kFixDateAml05, kFixDateAll02)
# for (i in 1:length(data.set$SUBJID)) {
#   if ((data.set$DTHDTC[i]! = "") & (data.set$DTHDTC[i] <= fix.date)) {
#     data.set$anal.obj[i] <- "death prev.20141031"
#   } else if (data.set$DSSTDTC[i] == "") {
#     data.set$anal.obj[i] <- "unknown DSSTDTC"
#   } else if ((data.set$DTHFL[i] == T) & (data.set$DTHDTC[i] == "")) {
#     data.set$anal.obj[i] <- "unknown DTHDTC"
#   } else if (data.set$date.end.trt[i] == "") {
#     data.set$anal.obj[i] <- "unknown date end treat"
#   } else if (data.set$DTHFL[i] == "") {
#     data.set$anal.obj[i] <- "unknown DTHFL"
#   } else {
#     data.set$anal.obj[i] <- "A"
#   }
# }
# breakdown <- data.matrix(table(data.set$anal.obj))  # 内訳
# ads <- subset(data.set,data.set$anal.obj == "A")  # 解析対象のみ抽出
ads <- data.set

#ads$fix.date <- ifelse(ads$STUDYID == "AML05", kFixDateAml05, kFixDateAll02)

ads$y.from.last.update <- YearDif(ads$DSSTDTC, ads$fix.date)  #y.from.last.updateにはデータ固定日-最終確認日が入る
for (i in 1:length(ads$SUBJID)) {
  ads$y.from.death[i] <- YearDif(ads$DTHDTC[i], ads$fix.date[i])  # y.from.deathにはデータ固定日-死亡日が入る
}
#ads$followup.in.2y <- ifelse(ads$y.from.last.update <= 2, T, F)  # 2年以内の転帰確認
ads$followup.in.2y <- ifelse((is.na(as.numeric(ads$y.from.last.update)) | as.numeric(ads$y.from.last.update) > 2),
                             F, T)  # 2年以内の転帰確認
ads$death.before.2y <- ifelse((is.na(as.numeric(ads$y.from.death)) | as.numeric(ads$y.from.death) <= 2),
                              F, T)  # 2年時点の死亡確認
ads$y.from.end.trt <- YearDif(ads$date.end.trt, ads$fix.date)  # 治療終了後年数
ads$age.fixed <- YearDif(ads$BRTHDTC, ads$fix.date)  #データ固定時の年齢

# 横軸に治療後年数、縦軸にフォローアップ率のグラフを記述する
ads1 <- ads[!is.na(ads$y.from.end.trt), ]
follow.up.rate1 <- NULL
for (i in 1:max(ads1$y.from.end.trt)) {
  follow.up.rate1[i] <- FollowupRate(ads1[ads1$y.from.end.trt == i, ])
}
barplot(follow.up.rate1, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
        main="Follow up rate by years after end of treatment",
        xlab="Years after end of treatment", ylab="Follow up rate")

# 横軸にデータ固定時年齢、縦軸にフォローアップ率のグラフを記述する
ads2 <- ads[!is.na(ads$age.fixed), ]
follow.up.rate2 <- NULL
for (i in 1:max(ads2$age.fixed)) {
  follow.up.rate2[i] <- FollowupRate(ads2[ads2$age.fixed == i, ])
}
barplot(follow.up.rate2, ylim=c(0:1), names.arg=c(1:max(ads2$age.fixed)), family="sans",
        main="Follow up rate by age", xlab="Age at data fix", ylab="Follow up rate")

#県別のdataframeでフォローアップ率を出す
follow.up.rate3 <- NULL
for (i in 1:47) {
  follow.up.rate3[i] <- FollowupRate(ads[ads$SCSTRESC == i, ])
}
barplot(follow.up.rate3, names.arg=prefecture$Prefecture, family="sans", las=3, ylim=c(0:1),
        main="Follow-up rate by prefecture", xlab="", ylab="Follow up rate", cex.names=0.7)

setwd("../output")
write.csv(ads, "LTFU dataset.csv", row.names = T)
setwd("..")
