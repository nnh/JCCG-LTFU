FollowupRate <- function(dataframe) {
  # フォローアップ率を作る関数
  sum(dataframe$followup.in.2y == T) / sum(dataframe$no.death.before.2y == T)
}
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

# # New Algorithmを用いてflow-up率の棒グラフを表示する
# # x.lab <- dimnames(rates.all02)[["y.from.end.trt"]]
# # rates.all02["Sum", -length(rates.all02["Sum",])]  # Sum行を除いたfollow-up率
# barplot(rates.all02['Sum',], family="sans",
#         main="Follow-up rate by years after end of treatment, ALL02, NEW",
#         xlab="Years after end of treatment", ylab="Follow up rate")
# barplot(rates.all02[, 'Sum'], family="sans",
#         main="Follow-up rate by age at data fix, ALL02, NEW",
#         xlab="Age at data fix", ylab="Follow up rate")
# 
# barplot(rate.end.trt, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
#         main="Follow-up rate by years after end of treatment",
#         xlab="Years after end of treatment", ylab="Follow up rate")
# barplot(rate.end.trt.all02, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
#         main="Follow-up rate by years after end of treatment, ALL02",
#         xlab="Years after end of treatment", ylab="Follow up rate")
# barplot(rate.end.trt.aml05, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
#         main="Follow-up rate by years after end of treatment, AML05",
#         xlab="Years after end of treatment", ylab="Follow up rate")
# 
# barplot(rate.age.datafix, ylim=c(0:1), names.arg=c(1:max(ads2$age.at.datafix)), family="sans",
#         main="Follow-up rate by age at data fix", xlab="Age at data fix", ylab="Follow up rate")
# barplot(rate.age.datafix.all02, ylim=c(0:1), names.arg=c(1:max(ads2$age.at.datafix)), family="sans",
#         main="Follow-up rate by age at data fix, ALL02", xlab="Age at data fix", ylab="Follow up rate")
# barplot(rate.age.datafix.aml05, ylim=c(0:1), names.arg=c(1:max(ads2$age.at.datafix)), family="sans",
#         main="Follow-up rate by age at data fix, AML05", xlab="Age at data fix", ylab="Follow up rate")
# 
# barplot(f.u.rate3, names.arg=prefecture$Prefecture, family="sans", las=3, ylim=c(0:1),
#         main="Follow-up rate by prefecture", xlab="", ylab="Follow up rate", cex.names=0.7)
# 
# barplot(rate.age.followup, ylim=c(0:1), names.arg=c(1:x.max), family="sans",
#         main="Follow-up rate by age at follow-up", xlab="Age at follow-up", ylab="Follow up rate")
# barplot(rate.age.followup.all02, ylim=c(0:1), names.arg=c(1:x.max), family="sans",
#         main="Follow-up rate by age at follow-up, ALL02", xlab="Age at follow-up", ylab="Follow up rate")
# barplot(rate.age.followup.aml05, ylim=c(0:1), names.arg=c(1:x.max), family="sans",
#         main="Follow-up rate by age at follow-up, AML05", xlab="Age at follow-up", ylab="Follow up rate")


#横軸に地区、縦軸にFU率のグラフを記述する
#地区別のdataframeでフォローアップ率を出す(21未満)
ds.all02.bf.1 <- ds.all02.bf.0[ds.all02.bf.0$area3 != "Other", ]
ads.chiku.less.than21 <- ds.all02.bf.1[ds.all02.bf.1$cat.age.datafix == "<21", ]
f.u.rate.chiku <- NULL
denom.chiku <- NULL
for (i in 1:6) {
  f.u.rate.chiku[i] <- FollowupRate(ads.chiku.less.than21[ads.chiku.less.than21$地区CD == i, ])
 # denom.chiku[i] <- sum(ds.all02.bf.0[ads.chiku.less.than21$地区CD == i, ]$no.death.before.2y == T)
}
area <- c("A", "B", "C", "D", "E", "F")
# barplot(f.u.rate.chiku, names.arg = area, family="sans", las=3, ylim=c(0:1),
#         main="Follow-up rate by area, less than 21years old", xlab="", ylab="Follow up rate", cex.names=0.7)
#地区別のdataframeでフォローアップ率を出す(21以上)
ads.chiku.over21 <- ds.all02.bf.1[ds.all02.bf.1$cat.age.datafix == "21 <=", ]
f.u.rate.chiku1 <- NULL
denom.chiku1 <- NULL
for (i in 1:6) {
  f.u.rate.chiku1[i] <- FollowupRate(ads.chiku.over21[ads.chiku.over21$地区CD == i, ])
  # denom.chiku1[i] <- sum(ds.all02.bf.0[ads.chiku.over21$地区CD == i, ]$no.death.before.2y == T)
}
# area <- c("Hokkaido", "Tokai", "Kansai", "Chugoku_Shikoku", "Kyusyu", "Kyoto", "Tohoku", "Other")
# barplot(f.u.rate.chiku1, names.arg = area, family="sans", las=3, ylim=c(0:1),
#         main="Follow-up rate by area, over 21years old", xlab="", ylab="Follow up rate", cex.names=0.7)
