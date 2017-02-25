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
    sum(dataframe$followup.in.2y == T) / sum(dataframe$no.death.before.2y == T)
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

# Make ADS(Analysis Data Set) for JACLS-ALL-02
jacls.registration <- regis[c("現施設名", "登録コード")]
all02.pick0 <- ALL02[c("JACLS登録コード", "診断年月日", "治療終了日")]
all02.pick <- merge(all02.pick0, jacls.registration, by.x="JACLS登録コード", by.y="登録コード", all.x=T)
merge.02.facil <- merge(all02.pick, facil, by.x="現施設名", by.y="施設名", all.x=T)
merge.JACLS <- merge(merge.02.facil, JACLS, by.x="JACLS登録コード", by.y="登録コード", all.x=T)
merge.JACLS$SCSTRESC <- floor(as.numeric(merge.JACLS$施設CD) / 10000000)  # 施設コードの上2桁が県コード
ads.all02 <- merge.JACLS[c("JACLS登録コード", "診断年月日", "治療終了日", "生年月日", "生死", "死亡日", "最終確認日", "SCSTRESC")]
names(ads.all02)[c(1:7)] <- c("SUBJID", "MHSTDTC", "date.end.trt", "BRTHDTC", "DTHFL", "DTHDTC", "DSSTDTC")
ads.all02$DTHFL[ads.all02$DTHFL == "true"] <- T
ads.all02$DTHFL[ads.all02$DTHFL == "false"] <- F
ads.all02$STUDYID <- "ALL02"
ads.all02$fix.date <- kFixDateAll02

# Make ADS(Analysis Data Set) for JPLSG-AML-05
aml05.pick <- subset(AML05, is.na(AML05$解析対象外))
aml05.pick[is.na(aml05.pick)] <- "-"  # TODO(tosh):なぜ必要？
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
merge.JPLSG <- merge(JPLSG, facil, by.x="現施設名", by.y="施設名", all.x=T)
merge.JPLSG$SCSTRESC <- floor(as.numeric(merge.JPLSG$施設CD) / 10000000)  # 施設コードの上2桁が県コード
jplsg.pick <- merge.JPLSG[c("登録コード", "生年月日", "生死", "死亡日", "最終確認日", "SCSTRESC")]
merge2 <- merge(aml05.pick1, jplsg.pick, by.x="J_CD", by.y="登録コード", all.x=T)
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
merge2$DSSTDTC <- ifelse((merge2$最終確認日 == ""), merge2$AML05最終確認日, merge2$最終確認日)
ads.aml05 <- merge2[c("J_CD", "診断日", "date.end.trt", "生年月日", "DTHFL", "DTHDTC", "DSSTDTC", "SCSTRESC")]
names(ads.aml05)[c(1, 2, 4)] <- c("SUBJID", "MHSTDTC", "BRTHDTC")
ads.aml05$STUDYID <- "AML05"
ads.aml05$fix.date <- kFixDateAml05

# JACLS-ALL-02 + JPLSG-AML-05
ads <- rbind(ads.all02, ads.aml05)
ads$age.at.followup <- YearDif(ads$BRTHDTC, ads$DSSTDTC)  # 最終転帰更新日時点の年齢
ads[is.na(ads)] <- ""  # TODO(tosh):なぜ必要？
missing.value <- ads[ads$DSSTDTC == "" | ads$BRTHDTC == "", ]  # 最終転帰更新日または生年月日が無い症例
# ADS(analysis data set, 解析用データセット) 作成完了

# follow-up率算出用変数作成
ads$base.date <- ads$fix.date
ads$y.from.last.update <- YearDif(ads$DSSTDTC, ads$base.date)  #フォローアップ率算出基準日-最終確認日が入る
for (i in 1:length(ads$SUBJID)) {
  ads$y.from.death[i] <- YearDif(ads$DTHDTC[i], ads$base.date[i])  # フォローアップ率算出基準日-死亡日が入る
}
ads$followup.in.2y <- ifelse((is.na(as.numeric(ads$y.from.last.update)) | as.numeric(ads$y.from.last.update) > 2),
                             F, T)  # 2年以内の転帰確認
ads$no.death.before.2y <- ifelse((is.na(as.numeric(ads$y.from.death)) | as.numeric(ads$y.from.death) <= 2),
                                 T, F)  # 2年時点の死亡確認
ads$y.from.end.trt <- YearDif(ads$date.end.trt, ads$base.date)  # 治療終了後年数
ads$age.at.datafix <- YearDif(ads$BRTHDTC, ads$base.date)  # データ固定時の年齢

# AML05カテゴリー化
denom.aml05 <- xtabs(no.death.before.2y ~ y.from.end.trt + age.at.datafix, data = ads[ads$STUDYID == "AML05", ])
numer.aml05 <- xtabs(followup.in.2y ~ y.from.end.trt + age.at.datafix, data = ads[ads$STUDYID == "AML05", ])
rates.aml05 <- round(addmargins(numer.aml05) / addmargins(denom.aml05), 2)*100
ads$cat2.age.at.datafix <- cut(ads$age.at.datafix, breaks = c(6,10,14,18,22,27),
                              labels= c("6-9","10-13","14-17","18-21","22-26"), right=FALSE)
ads$cat2.y.from.end.trt<- cut(ads$y.from.end.trt, breaks = c(4,6,8,10),
                             labels= c("4-5","6-7","8-9"), right=FALSE)
denom.aml05.cat <- xtabs(no.death.before.2y ~ cat2.y.from.end.trt + cat2.age.at.datafix, data = ads[ads$STUDYID == "AML05", ])
numer.aml05.cat <- xtabs(followup.in.2y ~ cat2.y.from.end.trt + cat2.age.at.datafix, data = ads[ads$STUDYID == "AML05", ])
rates.aml05.cat <- round(addmargins(numer.aml05.cat) / addmargins(denom.aml05.cat), 2)*100

# ALL02カテゴリー化
denom.all02 <- xtabs(no.death.before.2y ~ y.from.end.trt + age.at.datafix, data = ads[ads$STUDYID == "ALL02", ])
numer.all02 <- xtabs(followup.in.2y ~ y.from.end.trt + age.at.datafix, data = ads[ads$STUDYID == "ALL02", ])
rates.all02 <- round(addmargins(numer.all02) / addmargins(denom.all02), 2)*100
ads$cat.age.at.datafix <- cut(ads$age.at.datafix, breaks = c(6,9,13,17,21,25,30),
                              labels= c("6-8","9-12","13-16","17-20","21-24","25-29"), right=FALSE)
ads$cat.y.from.end.trt<- cut(ads$y.from.end.trt, breaks = c(4,7,9,11,14),
                             labels= c("4-6","7-8","9-10","11-13"), right=FALSE)
denom.all02.cat <- xtabs(no.death.before.2y ~ cat.y.from.end.trt + cat.age.at.datafix, data = ads[ads$STUDYID == "ALL02", ])
numer.all02.cat <- xtabs(followup.in.2y ~ cat.y.from.end.trt + cat.age.at.datafix, data = ads[ads$STUDYID == "ALL02", ])
rates.all02.cat <- round(addmargins(numer.all02.cat) / addmargins(denom.all02.cat), 2)*100

# New Algorithmを用いてflow-up率の棒グラフを表示する
# x.lab <- dimnames(rates.all02)[["y.from.end.trt"]]
# rates.all02["Sum", -length(rates.all02["Sum",])]  # Sum行を除いたfollow-up率
barplot(rates.all02['Sum',], family="sans",
        main="Follow-up rate by years after end of treatment, ALL02, NEW",
        xlab="Years after end of treatment", ylab="Follow up rate")
barplot(rates.all02[, 'Sum'], family="sans",
        main="Follow-up rate by age at data fix, ALL02, NEW",
        xlab="Age at data fix", ylab="Follow up rate")

# 横軸に治療後年数、縦軸にフォローアップ率のグラフを記述する
ads1 <- ads[!is.na(ads$y.from.end.trt), ]
rate.end.trt <- NULL
rate.end.trt.all02 <- NULL
rate.end.trt.aml05 <- NULL
denom.end.trt <- NULL
denom.end.trt.all02 <- NULL
denom.end.trt.aml05 <- NULL
for (i in 1:max(ads1$y.from.end.trt)) {
  rate.end.trt[i] <- FollowupRate(ads1[ads1$y.from.end.trt == i, ])
  denom.end.trt[i] <- sum(ads1[ads1$y.from.end.trt == i, ]$no.death.before.2y == T)
}
for (i in 1:max(ads1$y.from.end.trt)) {
  rate.end.trt.all02[i] <- FollowupRate(ads1[ads1$y.from.end.trt == i & ads1$STUDYID == "ALL02", ])
  denom.end.trt.all02[i] <- sum(ads1[ads1$y.from.end.trt == i & ads1$STUDYID == "ALL02", ]$no.death.before.2y == T)
}
for (i in 1:max(ads1$y.from.end.trt)) {
  rate.end.trt.aml05[i] <- FollowupRate(ads1[ads1$y.from.end.trt == i & ads1$STUDYID == "AML05", ])
  denom.end.trt.aml05[i] <- sum(ads1[ads1$y.from.end.trt == i & ads1$STUDYID == "AML05", ]$no.death.before.2y == T)
}
barplot(rate.end.trt, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
        main="Follow-up rate by years after end of treatment",
        xlab="Years after end of treatment", ylab="Follow up rate")
barplot(rate.end.trt.all02, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
        main="Follow-up rate by years after end of treatment, ALL02",
        xlab="Years after end of treatment", ylab="Follow up rate")
barplot(rate.end.trt.aml05, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
        main="Follow-up rate by years after end of treatment, AML05",
        xlab="Years after end of treatment", ylab="Follow up rate")

# 横軸にデータ固定時年齢、縦軸にフォローアップ率のグラフを記述する
ads2 <- ads[!is.na(ads$age.at.datafix), ]
rate.age.datafix <- NULL
rate.age.datafix.all02 <- NULL
rate.age.datafix.aml05 <- NULL
denom.age.datafix <- NULL
denom.age.datafix.all02 <- NULL
denom.age.datafix.aml05 <- NULL
for (i in 1:max(ads2$age.at.datafix)) {
  rate.age.datafix[i] <- FollowupRate(ads2[ads2$age.at.datafix == i, ])
  denom.age.datafix[i] <- sum(ads2[ads2$age.at.datafix == i, ]$no.death.before.2y == T)
}
for (i in 1:max(ads2$age.at.datafix)) {
  rate.age.datafix.all02[i] <- FollowupRate(ads2[ads2$age.at.datafix == i & ads2$STUDYID == "ALL02", ])
  denom.age.datafix.all02[i] <- sum(ads2[ads2$age.at.datafix == i & ads2$STUDYID == "ALL02", ]$no.death.before.2y == T)
}
for (i in 1:max(ads2$age.at.datafix)) {
  rate.age.datafix.aml05[i] <- FollowupRate(ads2[ads2$age.at.datafix == i & ads2$STUDYID == "AML05", ])
  denom.age.datafix.aml05[i] <- sum(ads2[ads2$age.at.datafix == i & ads2$STUDYID == "AML05", ]$no.death.before.2y == T)
}
barplot(rate.age.datafix, ylim=c(0:1), names.arg=c(1:max(ads2$age.at.datafix)), family="sans",
        main="Follow-up rate by age at data fix", xlab="Age at data fix", ylab="Follow up rate")
barplot(rate.age.datafix.all02, ylim=c(0:1), names.arg=c(1:max(ads2$age.at.datafix)), family="sans",
        main="Follow-up rate by age at data fix, ALL02", xlab="Age at data fix", ylab="Follow up rate")
barplot(rate.age.datafix.aml05, ylim=c(0:1), names.arg=c(1:max(ads2$age.at.datafix)), family="sans",
        main="Follow-up rate by age at data fix, AML05", xlab="Age at data fix", ylab="Follow up rate")

#県別のdataframeでフォローアップ率を出す
f.u.rate3 <- NULL
denom3 <- NULL
for (i in 1:47) {
  f.u.rate3[i] <- FollowupRate(ads[ads$SCSTRESC == i, ])
  denom3[i] <- sum(ads[ads$SCSTRESC == i, ]$no.death.before.2y == T)
}
barplot(f.u.rate3, names.arg=prefecture$Prefecture, family="sans", las=3, ylim=c(0:1),
        main="Follow-up rate by prefecture", xlab="", ylab="Follow up rate", cex.names=0.7)

# 横軸に最終転帰更新日時点年齢、縦軸にフォローアップ率のグラフを記述する
ads4 <- ads[!is.na(ads$age.at.followup), ]
rate.age.followup <- NULL
rate.age.followup.all02 <- NULL
rate.age.followup.aml05 <- NULL
denom.age.followup <- NULL
denom.age.followup.all02 <- NULL
denom.age.followup.aml05 <- NULL
x.max <- max(as.integer(ads4$age.at.followup), na.rm=T)
for (i in 1:x.max) {
  rate.age.followup[i] <- FollowupRate(ads4[ads4$age.at.followup == i, ])
  denom.age.followup[i] <- sum(ads4[ads4$age.at.followup == i, ]$no.death.before.2y == T)
}
for (i in 1:x.max) {
  rate.age.followup.all02[i] <- FollowupRate(ads4[ads4$age.at.followup == i & ads4$STUDYID == "ALL02", ])
  denom.age.followup.all02[i] <- sum(ads4[ads4$age.at.followup == i & ads4$STUDYID == "ALL02", ]$no.death.before.2y == T)
}
for (i in 1:x.max) {
  rate.age.followup.aml05[i] <- FollowupRate(ads4[ads4$age.at.followup == i & ads4$STUDYID == "AML05", ])
  denom.age.followup.aml05[i] <- sum(ads4[ads4$age.at.followup == i & ads4$STUDYID == "AML05", ]$no.death.before.2y == T)
}
barplot(rate.age.followup, ylim=c(0:1), names.arg=c(1:x.max), family="sans",
        main="Follow-up rate by age at follow-up", xlab="Age at follow-up", ylab="Follow up rate")
barplot(rate.age.followup.all02, ylim=c(0:1), names.arg=c(1:x.max), family="sans",
        main="Follow-up rate by age at follow-up, ALL02", xlab="Age at follow-up", ylab="Follow up rate")
barplot(rate.age.followup.aml05, ylim=c(0:1), names.arg=c(1:x.max), family="sans",
        main="Follow-up rate by age at follow-up, AML05", xlab="Age at follow-up", ylab="Follow up rate")

setwd("../output")
write.csv(ads, "LTFU dataset.csv", row.names=T)
write.csv(missing.value, "LTFU missing value.csv", row.names=T)
setwd("..")
