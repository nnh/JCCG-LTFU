FollowupRate <- function(dataframe) {
  # フォローアップ率を作る関数
  sum(dataframe$followup.in.2y == T) / sum(dataframe$no.death.before.2y == T)
}

# follow-up率算出用変数作成
ads$y.from.last.update <- YearDif(ads$DSSTDTC, ads$base.date)  #フォローアップ率算出基準日-最終確認日が入る
for (i in 1:length(ads$SUBJID)) {
  ads$y.from.death[i] <- YearDif(ads$DTHDTC[i], ads$base.date[i])  # フォローアップ率算出基準日-死亡日が入る
}
ads$followup.in.2y <- ifelse((is.na(as.numeric(ads$y.from.last.update)) | as.numeric(ads$y.from.last.update) > 2),
                             F, T)  # 2年以内の転帰確認
ads$no.death.before.2y <- ifelse((is.na(as.numeric(ads$y.from.death)) | as.numeric(ads$y.from.death) <= 2),
                                 T, F)  # 2年時点の死亡確認
ads$y.from.end.trt <- YearDif(ads$date.end.trt, ads$base.date)  # 治療終了後年数

# AML05カテゴリー化
denom.aml05 <- xtabs(no.death.before.2y ~ y.from.end.trt + age.at.datafix, data = ads[ads$STUDYID == "AML05", ])
numer.aml05 <- xtabs(followup.in.2y ~ y.from.end.trt + age.at.datafix, data = ads[ads$STUDYID == "AML05", ])
rates.aml05 <- round(addmargins(numer.aml05) / addmargins(denom.aml05), 2)*100
ads$cat2.age.at.datafix <- cut(ads$age.at.datafix, breaks = c(6,10,14,18,22,27),
                              labels= c("6-9","10-13","14-17","18-21","22-26"), right=FALSE)
ads$cat2.y.from.end.trt<- cut(ads$y.from.end.trt, breaks = c(2,4,6,8,10),
                             labels= c("2-3","4-5","6-7","8-9"), right=FALSE)
denom.aml05.cat <- xtabs(no.death.before.2y ~ cat2.y.from.end.trt + cat2.age.at.datafix, data = ads[ads$STUDYID == "AML05", ])
numer.aml05.cat <- xtabs(followup.in.2y ~ cat2.y.from.end.trt + cat2.age.at.datafix, data = ads[ads$STUDYID == "AML05", ])
rates.aml05.cat <- round(addmargins(numer.aml05.cat) / addmargins(denom.aml05.cat), 2)*100

# ALL02カテゴリー化
denom.all02 <- xtabs(no.death.before.2y ~ y.from.end.trt + age.at.datafix, data = ads[ads$STUDYID == "ALL02", ])
numer.all02 <- xtabs(followup.in.2y ~ y.from.end.trt + age.at.datafix, data = ads[ads$STUDYID == "ALL02", ])
rates.all02 <- round(addmargins(numer.all02) / addmargins(denom.all02), 2)*100
ads$cat.age.at.datafix <- cut(ads$age.at.datafix, breaks = c(6,9,13,17,21,25,30),
                              labels= c("6-8","9-12","13-16","17-20","21-24","25-29"), right=FALSE)
ads$cat.y.from.end.trt<- cut(ads$y.from.end.trt, breaks = c(3,5,7,9,11,14),
                             labels= c("3-4","5-6","7-8","9-10","11-13"), right=FALSE)
denom.all02.cat <- xtabs(no.death.before.2y ~ cat.y.from.end.trt + cat.age.at.datafix, data = ads[ads$STUDYID == "ALL02", ])
numer.all02.cat <- xtabs(followup.in.2y ~ cat.y.from.end.trt + cat.age.at.datafix, data = ads[ads$STUDYID == "ALL02", ])
rates.all02.cat <- round(addmargins(numer.all02.cat) / addmargins(denom.all02.cat), 2)*100


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

#県別のdataframeでフォローアップ率を出す
ads5 <- ads[!is.na(ads$SCSTRESC), ]
f.u.rate3 <- NULL
denom3 <- NULL
for (i in 1:47) {
  f.u.rate3[i] <- FollowupRate(ads[ads5$SCSTRESC == i, ])
  denom3[i] <- sum(ads[ads5$SCSTRESC == i, ]$no.death.before.2y == T)
}

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
