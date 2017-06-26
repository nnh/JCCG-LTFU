FollowupRate <- function(dataframe) {
  # フォローアップ率を作る関数
  sum(dataframe$followup.in.2y == T) / sum(dataframe$no.death.before.2y == T)
}

RatesAml05 <- function(dataframe) {
  # AML05カテゴリー化
  denom.aml05 <- xtabs(no.death.before.2y ~ y.from.end.trt + age.at.datafix, data = dataframe)
  numer.aml05 <- xtabs(followup.in.2y ~ y.from.end.trt + age.at.datafix, data = dataframe)
  dataframe$cat2.age.at.datafix <- cut(dataframe$age.at.datafix, breaks = c(6,10,14,18,22,27),
                                 labels= c("6-9","10-13","14-17","18-21","22-26"), right=FALSE)
  dataframe$cat2.y.from.end.trt<- cut(dataframe$y.from.end.trt, breaks = c(2,4,6,8,10),
                                labels= c("2-3","4-5","6-7","8-9"), right=FALSE)
  denom.aml05.cat <- xtabs(no.death.before.2y ~ cat2.y.from.end.trt + cat2.age.at.datafix, data = dataframe)
  numer.aml05.cat <- xtabs(followup.in.2y ~ cat2.y.from.end.trt + cat2.age.at.datafix, data = dataframe)
  rates.aml05.cat <- round(addmargins(numer.aml05.cat) / addmargins(denom.aml05.cat), 2)*100
  print("denom.aml05.cat", quote = F)
  print(addmargins(denom.aml05.cat))
  print("", quote = F)
  print("numer.aml05.cat", quote = F)
  print(addmargins(numer.aml05.cat))
  print("", quote = F)
  print("rates.aml05.cat", quote = F)
  print(rates.aml05.cat)
}

RatesAll02 <- function(dataframe) {
  # ALL02カテゴリー化
  denom.all02 <- xtabs(no.death.before.2y ~ y.from.end.trt + age.at.datafix, data = dataframe)
  numer.all02 <- xtabs(followup.in.2y ~ y.from.end.trt + age.at.datafix, data = dataframe)
  rates.all02 <- round(addmargins(numer.all02) / addmargins(denom.all02), 2)*100
  dataframe$cat.age.at.datafix <- cut(dataframe$age.at.datafix, breaks = c(6,9,13,17,21,25,30),
                                labels= c("6-8","9-12","13-16","17-20","21-24","25-29"), right=FALSE)
  dataframe$cat.y.from.end.trt <- cut(dataframe$y.from.end.trt, breaks = c(3,5,7,9,11,14,16),
                               labels= c("3-4","5-6","7-8","9-10","11-13","14-16"), right=FALSE)
  denom.all02.cat <- xtabs(no.death.before.2y ~ cat.y.from.end.trt + cat.age.at.datafix, data = dataframe)
  numer.all02.cat <- xtabs(followup.in.2y ~ cat.y.from.end.trt + cat.age.at.datafix, data = dataframe)
  rates.all02.cat <- round(addmargins(numer.all02.cat) / addmargins(denom.all02.cat), 2)*100
  print("denom.all02.cat", quote = F)
  print(addmargins(denom.all02.cat))
  print("", quote = F)
  print("numer.all02.cat", quote = F)
  print(addmargins(numer.all02.cat))
  print("", quote = F)
  print("rates.all02.cat", quote = F)
  print(rates.all02.cat)
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


setwd(".")
