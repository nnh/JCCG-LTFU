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
setwd("..")

# Make ADS(Analysis Data Set) for JACLS-ALL-02
jacls.registration <- regis[c("現施設名", "登録コード", "生年月日", "最終確認日")]
all02.pick0 <- ALL02[c("JACLS登録コード", "診断年月日", "治療終了日")]
all02.pick <- merge(all02.pick0, jacls.registration, by.x="JACLS登録コード", by.y="登録コード", all.x=T)
merge.02.facil <- merge(all02.pick, facil, by.x="現施設名", by.y="施設名", all.x=T)
merge.JACLS <- merge(merge.02.facil, JACLS, by.x="JACLS登録コード", by.y="登録コード", all.x=T)
merge.JACLS$SCSTRESC <- floor(as.numeric(merge.JACLS$施設CD) / 10000000)  # 施設コードの上2桁が県コード
ads.all02 <- merge.JACLS[c("JACLS登録コード", "診断年月日", "治療終了日", "生年月日.x", "生死", "死亡日", "最終確認日.x", "SCSTRESC")]
names(ads.all02)[c(1:7)] <- c("SUBJID", "MHSTDTC", "date.end.trt", "BRTHDTC", "DTHFL", "DTHDTC", "DSSTDTC")
ads.all02$DTHFL[ads.all02$DTHFL == "true"] <- T
ads.all02$DTHFL[ads.all02$DTHFL == "false"] <- F
ads.all02$STUDYID <- "ALL02"
ads.all02$fix.date <- kFixDateAll02

# Make ADS(Analysis Data Set) for JPLSG-AML-05
aml05.pick <- subset(AML05, is.na(AML05$解析対象外))
aml05.pick$中止届有無[is.na(aml05.pick$中止届有無)] <- 0
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
ads$age.at.datafix <- YearDif(ads$BRTHDTC, ads$fix.date)  # データ固定時の年齢
ads$DTHFL <- as.logical(ads$DTHFL)
missing.value <- ads[is.na(ads$DSSTDTC) | is.na(ads$BRTHDTC), ]  # 最終転帰更新日または生年月日が無い症例
ads <- ads[!is.na(ads$DSSTDTC) & !is.na(ads$BRTHDTC), ]

# Save ADS(analysis data set)
setwd("./output")
write.csv(ads, "LTFU dataset.csv", row.names=T)
write.csv(missing.value, "LTFU missing value.csv", row.names=T)
setwd("..")
