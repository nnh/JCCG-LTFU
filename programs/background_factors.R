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
#ALL02から背景因子用にadsに不足しているデータを取り出し、マージ
dxt.all02 <- ALL02[, c(2, 8, 10, 29)]
ds.all02.bf.0 <- merge(dxt.ads, dxt.all02, by.x = "SUBJID", by.y = "JACLS登録コード", all.x = T)

#データ固定2年前以前に死亡した人を除いた集団の作成・計算
ds.all02.bf <- ds.all02.bf.0[((ds.all02.bf.0$no.death.before.2y ==T) || (ds.all02.bf.0$date.end.trt != "")), ]
age.diagnosis <- summary(ds.all02.bf$age.diagnosis)
sex <- table(ds.all02.bf$性別)
marker <- table(ds.all02.bf$マーカー)
initial.wbc <- summary(ds.all02.bf$WBC..μl.)

#解析対象外集団の作成
missing.value.bf <- ds.all02.bf.0[ds.all02.bf.0$no.death.before.2y ==F | ds.all02.bf.0$date.end.trt == "", ]
missing.value.age.diagnosis <- summary(missing.value.bf $age.diagnosis)
missing.value.sex <- table(missing.value.bf $性別)

setwd("..")