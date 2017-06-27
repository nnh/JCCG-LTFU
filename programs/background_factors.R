# LTFU Make dataset ALL02 background factors
# mamiko yonejima
# 2017/4/11
##################



#3桁コード、Ptoshコード、地区コード、地区名のデータセットを作成
m.df <- merge(sites.all02, area, by.x = "site.code", by.y = "施設CD", all.x = T)
m.df1 <- merge(m.df, facilities, by.x = "site.code", by.y = "code_3digit", all.x = T)
area.cd <- m.df1[,c("site.code", "code_9digit", "地区CD", "area")]
area.cd <- area.cd[-27, ]  #  成育医療センターが二個できてしまうため、削除するためのコード
#adsからALL-02のデータだけ取り出す、診断時年齢追加
dxt.ads <- ads[ads$STUDYID == "ALL02", ]
dxt.ads$age.diagnosis <- YearDif(dxt.ads$BRTHDTC, dxt.ads$MHSTDTC) 

#follow up提出状況確認
JACLS$submit.fu <- TRUE
dxt.JACLS <- JACLS[, c(15, 193)]
#ALL02から背景因子用にadsに不足しているデータを取り出し
dxt.all02 <- ALL02[, c(2, 8, 10, 29)]

#マージ
ds.all02.bf.0 <- merge(dxt.ads, dxt.all02, by.x = "SUBJID", by.y = "JACLS登録コード", all.x = T)
ds.all02.bf.0 <- merge(ds.all02.bf.0, dxt.JACLS, by.x = "SUBJID", by.y = "登録コード", all.x = T)
#診断時からのデータ固定までの期間の列の作成
ds.all02.bf.0$y.from.diagnosis <- YearDif(ds.all02.bf.0$MHSTDTC, ds.all02.bf.0$fix.date)
#age.at.datafixを21歳未満と、21歳以上に分ける   
ds.all02.bf.0$cat.age.datafix <- cut(ds.all02.bf.0$age.at.datafix, breaks = c(0,21,150),
                                               labels= c("<21", "21 <="), right=FALSE)

#地区コードマージする
ds.all02.bf.0 <- merge(ds.all02.bf.0, area.cd, by.x = "SITEID", by.y = "code_9digit", all.x = T)  # ds.all02.bf.0は1252例

#解析対象集団（データ固定2年前以前に死亡した人を除いた集団）の作成・計算
ds.all02.bf <- ds.all02.bf.0[ds.all02.bf.0$no.death.before.2y ==T & ds.all02.bf.0$date.end.trt != "", ] #  ds.all02.bfは962例
age.diagnosis <- summary(ds.all02.bf$age.diagnosis)
sex <- table(ds.all02.bf$性別)
marker <- table(ds.all02.bf$マーカー)
initial.wbc <- summary(ds.all02.bf$WBC..μl.)
age.at.data.fixed <- summary(ds.all02.bf$age.at.datafix)
y.from.diagnosis <- summary(ds.all02.bf$y.from.diagnosis)

#解析対象外集団の作成
missing.value.bf <- ds.all02.bf.0[ds.all02.bf.0$date.end.trt == "" | ds.all02.bf.0$no.death.before.2y ==F, ]
missing.value.age.diagnosis <- summary(missing.value.bf $age.diagnosis)
missing.value.sex <- table(missing.value.bf$性別)
missing.value.age.at.data.fixed <- summary(missing.value.bf$age.at.datafix)
missing.value.y.from.diagnosis <- summary(missing.value.bf$y.from.diagnosis)
#追跡調査未回答集団の作成
fu.missing.v <- table(missing.value.bf$submit.fu, useNA = 'always')
fu.ads <-table(ds.all02.bf$submit.fu, useNA = 'always')
not.sbmt.fu.bf <- ds.all02.bf.0[is.na(ds.all02.bf.0$submit.fu), ]
not.sbmt.fu.bf.age.diagnosis <- summary(not.sbmt.fu.bf$age.diagnosis)
not.sbmt.fu.bf.sex <- table(not.sbmt.fu.bf$性別)
not.sbmt.fu.age.at.data.fixed <- summary(not.sbmt.fu.bf$age.at.datafix)
not.sbmt.fu.y.from.diagnosis <- summary(not.sbmt.fu.bf$y.from.diagnosis)
#facititiesの詳細
number.by.facilities <- table(ds.all02.bf$hospital.type, useNA="always")
shisetsu <- xtabs( ~ SITEID + hospital.type, data = ds.all02.bf)
shisetsu.mat <- matrix(shisetsu, nrow(shisetsu), ncol(shisetsu))
colnames(shisetsu.mat) <- colnames(shisetsu)
shisetsu.mat.0 <- ifelse(shisetsu.mat != 0, 1, 0)
count.facilities <- apply(shisetsu.mat.0, 2, sum)

#地区別のdataframeでフォローアップ率を出す(all cases)
area <- c("A", "B", "C", "D", "E", "F")
ds.all02.bf.1 <- ds.all02.bf[ds.all02.bf$area != "Other", ]
## denom/numberを計算
denom.all02.area.all <- xtabs(no.death.before.2y ~ no.death.before.2y + area, data = ds.all02.bf.1)
numer.all02.area.all <- xtabs(followup.in.2y ~ followup.in.2y + area, data = ds.all02.bf.1)
## フォローアップ率
f.u.rate.chiku.all <- NULL
denom.chiku.all <- NULL
for (i in 1:6) {
  f.u.rate.chiku.all[i] <- FollowupRate(ds.all02.bf.1[ds.all02.bf.1$地区CD == i, ])
}
rates.all <- round(f.u.rate.chiku.all*100)


# 地区別のdataframeでフォローアップ率を出す(21未満)
ads.chiku.less.than21 <- ds.all02.bf.1[ds.all02.bf.1$cat.age.datafix == "<21", ]
## denom/numberを計算
denom.all02.area.less.than21 <- xtabs(no.death.before.2y ~ no.death.before.2y + area, data = ads.chiku.less.than21)
numer.all02.area.less.than21 <- xtabs(followup.in.2y ~ followup.in.2y + area, data = ads.chiku.less.than21)
## フォローアップ率
f.u.rate.chiku <- NULL
denom.chiku <- NULL
for (i in 1:6) {
  f.u.rate.chiku[i] <- FollowupRate(ads.chiku.less.than21[ads.chiku.less.than21$地区CD == i, ])
}
rates.less.than21 <- round(f.u.rate.chiku*100)

#地区別のdataframeでフォローアップ率を出す(21以上)
ads.chiku.over21 <- ds.all02.bf.1[ds.all02.bf.1$cat.age.datafix == "21 <=", ]
## denom/numberを計算
denom.all02.area.over21 <- xtabs(no.death.before.2y ~ no.death.before.2y + area, data = ads.chiku.over21)
numer.all02.area.over21 <- xtabs(followup.in.2y ~ followup.in.2y + area, data = ads.chiku.over21)
## フォローアップ率
f.u.rate.chiku1 <- NULL
denom.chiku1 <- NULL
for (i in 1:6) {
  f.u.rate.chiku1[i] <- FollowupRate(ads.chiku.over21[ads.chiku.over21$地区CD == i, ])
}
rates.over.than21 <- round(f.u.rate.chiku1*100)

setwd("./output")
write.csv(ds.all02.bf.0, "background.csv", row.names = F)

setwd("..")