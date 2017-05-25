# LTFU Make dataset ALL02 background factors
# mamiko yonejima
# 2017/4/11
##################

source("./programs/make_ads.R", encoding = "UTF-8")
ads$base.date <- ads$fix.date
source("./programs/followup_rate.R", encoding = "UTF-8")

#adsからALL-02のデータだけ取り出す、診断時年齢追加
dxt.ads <- ads[ads$STUDYID == "ALL02", ]
dxt.ads$age.diagnosis <- YearDif(dxt.ads$BRTHDTC, dxt.ads$MHSTDTC) 

#follow up提出状況確認
JACLS$submit.fu <- "o"
dxt.JACLS <- JACLS[, c(15, 193)]
#ALL02から背景因子用にadsに不足しているデータを取り出し
dxt.all02 <- ALL02[, c(2, 6, 8, 10, 29)]
#マージ
ds.all02.bf.0 <- merge(dxt.ads, dxt.all02, by.x = "SUBJID", by.y = "JACLS登録コード", all.x = T)
ds.all02.bf.0 <- merge(ds.all02.bf.0, dxt.JACLS, by.x = "SUBJID", by.y = "登録コード", all.x = T)
#診断時からのデータ固定までの期間の列の作成
ds.all02.bf.0$y.from.diagnosis <- YearDif(ds.all02.bf.0$診断年月日, ds.all02.bf.0$DSSTDTC)
                                                                                           
#解析対象集団（データ固定2年前以前に死亡した人を除いた集団）の作成・計算
ds.all02.bf <- ds.all02.bf.0[((ds.all02.bf.0$no.death.before.2y ==T) || (ds.all02.bf.0$date.end.trt != "")), ]
age.diagnosis <- summary(ds.all02.bf$age.diagnosis)
sex <- table(ds.all02.bf$性別)
marker <- table(ds.all02.bf$マーカー)
initial.wbc <- summary(ds.all02.bf$WBC..μl.)
age.at.data.fixed <- summary(ds.all02.bf$age.at.datafix)
y.from.diagnosis <- summary(ds.all02.bf$y.from.diagnosis)

#解析対象外集団の作成
missing.value.bf <- ds.all02.bf.0[ds.all02.bf.0$no.death.before.2y ==F | ds.all02.bf.0$date.end.trt == "", ]
missing.value.age.diagnosis <- summary(missing.value.bf $age.diagnosis)
missing.value.sex <- table(missing.value.bf$性別)
missing.value.age.at.data.fixed <- summary(missing.value.bf$age.at.datafix)
missing.value.y.from.diagnosis <- summary(missing.value.bf$y.from.diagnosis)
#追跡調査未回答集団の作成
not.sbmt.fu.bf <- ds.all02.bf.0[is.na(ds.all02.bf.0$submit.fu), ]
not.sbmt.fu.bf.age.diagnosis <- summary(not.sbmt.fu.bf$age.diagnosis)
not.sbmt.fu.bf.sex <- table(not.sbmt.fu.bf$性別)
not.sbmt.fu.age.at.data.fixed <- summary(not.sbmt.fu.bf$age.at.datafix)
not.sbmt.fu.y.from.diagnosis <- summary(not.sbmt.fu.bf$y.from.diagnosis)
#facititiesの詳細
number.by.facilities <- table(ds.all02.bf$hospital.type, useNA="always")
shisetsu <- xtabs( ~ SCSTRESC + hospital.type, data = ds.all02.bf)
shisetsu.mat <- matrix(shisetsu, nrow(shisetsu), ncol(shisetsu))
colnames(shisetsu.mat) <- colnames(shisetsu)
shisetsu.mat.0 <- ifelse(shisetsu.mat != 0, 1, 0)
count.facilities <- apply(shisetsu.mat.0, 2, sum)

setwd("./output")
write.csv(ds.all02.bf.0, "background.csv", row.names = F)

setwd("..")