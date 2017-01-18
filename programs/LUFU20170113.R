# LUFU
# mamiko yonejima
# 2017/1/13
##################

YearDif <- function(starting, ending) {
# 満xx年を作る関数
  if (starting != "" && ending != "") {
    period <- as.integer((as.integer(format(as.Date(ending),"%Y%m%d"))
                          - as.integer(format(as.Date(starting),"%Y%m%d")))/10000)
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

setwd("./rawdata")

# Making File List
list <- as.data.frame(list.files())
list$no <- c(1:nrow(list))
list$DFname <- substr(list.files(), 1, 5)
names(list)[1] <- "file_name"

# Read CSV data
for (i in 1:length(list$no)) {
  eval(
    parse(
      text = paste0(list$DFname[i], "<- read.csv('", list$file_name[i], "', as.is=T, fileEncoding='CP932')")
      )
    )
}

# JACLS-ALL-02
# Pick up data from ALL02
all02.pick <- ALL02[, c(2, 6, 45)]  # JACLS登録コード,診断年月日,治療終了日※3
# Pick up data from JACLS
jacls.pick <- JACLS[, c(11,15, 21, 22, 84)]  # 生年月日,登録コード,生死,死亡日,最終確認日
# merge
merge1 <- merge(all02.pick, jacls.pick, by.x="JACLS登録コード", by.y="登録コード", all.x=T)
names(merge1) <- c("SUBJID", "MHSTDTC", "DATE_END_TRT","BRTHDTC", "DTHFL", "DTHDTC", "DSSTDTC")
merge1$STUDYID <- "ALL02"  # ALL02のデータセット作成終わり

# JPLSG-AML-05
# Pick up and proccessing data from AML05(終了日の列)
aml05.pick <- subset(AML05, is.na(AML05$解析対象外))
aml05.pick[is.na(aml05.pick)] <- "-"  # Replace NA to "-"

for (i in 1:length(aml05.pick$J_CD)) {
  if (aml05.pick$中止届有無0なし.1あり[i] == 1) {
    aml05.pick$DATE_END_TRT[i] <- aml05.pick$中止届に記載された中止日[i]
  } else if (aml05.pick$移植有無0.なし.1.あり[i] == 1) {
    aml05.pick$DATE_END_TRT[i] <- aml05.pick$移植日[i]
  } else {
    aml05.pick$DATE_END_TRT[i] <- aml05.pick$therapy最終投薬日[i]
  }
}

aml05.pick1 <- aml05.pick[, c(2, 5, 14:16)]
names(aml05.pick1)[4] <- "AML05最終確認日"

# Pick up data from JPLSG
jplsg.pick <- JPLSG[, c(11,15, 21:23)]
merge2 <- merge(aml05.pick1, jplsg.pick, by.x="J_CD", by.y="登録コード", all.x=T)

# proccessing data from merge data(生死の列)
merge2$DTHFL <- ifelse(merge2$死亡.0.なし..1.あり == "1", T, merge2$生死)

for (i in 1:length(merge2$J_CD)) {
  if ((merge2$DTHFL[i] == T) & (merge2$死亡日[i] == "")) {
    merge2$DTHDTC[i] <- merge2$AML05最終確認日[i]
  } else if (merge2$DTHFL[i] == T) {
    merge2$DTHDTC[i] <- merge2$死亡日[i]
  } else {
    merge2$DTHDTC[i] <- ""
  }
}

merge2$DSSTDTC <- ifelse(merge2$最終確認日 == "", merge2$AML05最終確認日, merge2$最終確認日)

merge2 <- merge2[, c(1, 2, 5, 6, 9:11)]
names(merge2)[c(1, 2, 4, 5)] <- c("SUBJID", "MHSTDTC", "BRTHDTC", "DSSTDTC")
merge2$STUDYID <- "AML05"
merge2 <- merge2[, c(1:4, 6, 7, 5, 8)]

# JACLS-ALL-02 + JPLSG-AML-05
data.set <- rbind(merge1, merge2)

#解析対象集団の抽出
data.set[is.na(data.set)] <- ""  # Replace NA to ""
data.set$fix.date <- ifelse(data.set$STUDYID == "AML05", kFixDateAml05, kFixDateAll02)
# for (i in 1:length(data.set$SUBJID)) {
#   str.a <- data.set$DTHDTC[i]
#   str.b <- data.set$DSSTDTC[i]
#   str.c <- data.set$DTHFL[i]
#   str.d <- data.set$DATE_END_TRT[i]
#   str.r <- ""
#   if ((str.a! = "") & (str.a <= fix.date)) {
#     str.r <- "death prev.20141031"
#   } else if (str.b == "") {
#     str.r <- "unknown DSSTDTC"
#   } else if ((str.c == T) & (str.a == "")) {
#     str.r <- "unknown DTHDTC"
#   } else if (str.d == "") {
#     str.r <- "unknown date end treat"
#   } else if (str.c == "") {
#     str.r <- "unknown DTHFL"
#   } else {
#     str.r <- "A"
#   }
#   data.set$anal.obj[i] <- str.r
# }

# breakdown <- data.matrix(table(data.set$anal.obj))  #内訳
ads <- data.set
# ads <- subset(data.set,data.set$anal.obj == "A")  # 解析対象のみ抽出

#ads$fix.date <- ifelse(ads$STUDYID == "AML05", kFixDateAml05, kFixDateAll02)
ads$y.from.last.update <- YearDif(ads$DSSTDTC, ads$fix.date) #y.from.last.updateにはデータ固定日-最終確認日が入る
for (i in 1:length(ads$SUBJID)) {
  ads$y.from.death[i] <- YearDif(ads$DTHDTC[i], ads$fix.date[i])  # y.from.deathにはデータ固定日-死亡日が入る
}
#ads$followup.in.2y <- ifelse(ads$y.from.last.update <= 2, T, F)  # 2年以内の転帰確認
ads$followup.in.2y <- ifelse((is.na(as.numeric(ads$y.from.last.update)) | as.numeric(ads$y.from.last.update) > 2),
                             F, T)  # 2年以内の転帰確認
ads$death.before.2y <- ifelse((is.na(as.numeric(ads$y.from.death)) | as.numeric(ads$y.from.death) <2),
                              F, T)  # 2年時点の死亡確認
ads$y.end.trt <- YearDif(ads$DATE_END_TRT, ads$fix.date)  # 治療終了後年数
ads$age.fixed <- YearDif(ads$BRTHDTC, ads$fix.date)  #データ固定時の年齢

# 横軸に治療後年数、縦軸にフォローアップ率のグラフを記述する
# max <- max(ads$y.end.trt)
for (i in 1:20) {
  eval(parse(text = paste0("aa<- subset(ads, ads$y.end.trt == ", i, ")")))
  治療終了後年数 <- i
  フォローアップ率 <- FollowupRate(aa)
  eval(parse(text = paste0("df", i, "<-data.frame(治療終了後年数, フォローアップ率)")))
}
df.number <- paste("df", c(1:20), sep="", collapse=",")
eval(parse(text = paste0("result1 <- data.matrix(rbind(", df.number, "))")))

# setwd("../output")
# png("Figure1.png", width=800, height=600)
barplot(result1[,2], ylim=c(0:1), names.arg=c(1:20), family="sans",
        main="Follow up rate by years after end of treatment",
        xlab="Years after end of treatment", ylab="Follow up rate")
# dev.off()

# 横軸にデータ固定時年齢、縦軸にフォローアップ率のグラフを記述する
# max <- max(ads$age.fixed)
for (i in 1:35) {
  eval(parse(text = paste0("aa <- subset(ads, ads$age.fixed == ", i, ")")))
  データ固定時年齢 <- i
  フォローアップ率 <- FollowupRate(aa)
  eval(parse(text = paste0("df", i, "<-data.frame(データ固定時年齢, フォローアップ率)")))
}
df.number <- paste("df", c(1:35), sep="", collapse=",")
eval(parse(text=paste0("result2 <- data.matrix(rbind(", df.number, "))")))
# png("Figure2.png", width=800, height=600)
barplot(result2[,2],  ylim=c(0:1), names.arg=c(1:35), family="sans",
        main="Follow up rate by age", xlab="Age at data fix", ylab="Follow up rate")
# dev.off()
setwd("..")
