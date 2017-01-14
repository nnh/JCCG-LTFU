# LUFU 
# mamiko yonejima
# 2017/1/13
#########

fix.date <- "2014/10/31"
setwd("./rawdata")

# Making File List
list <- as.data.frame(list.files())
list$no  <- c(1:nrow(list))
list$DFname <- substr(list.files(), 1, 5)
names(list)[1] <- "file_name"

# Read CSVdata
for(i in 1:length(list$no)){
  eval(
    parse(
      text=paste0(list$DFname[i], "<- read.csv('", list$file_name[i], "', as.is=T, fileEncoding='CP932')")
    )
  )
}
# Pick up data from ALL02
ALL02pick <- ALL02[, c(2, 6, 45)]
# Pick up data from JACLS
JACLSpick <- JACLS[, c(15, 21:23)]
# merge
merge1 <- merge(ALL02pick, JACLSpick, by.x="JACLS登録コード", by.y="登録コード", all.x=T)
names(merge1) <- c("SUBJID", "MHSTDTC", "DATE_END_TRT", "DTHFL", "DTHDTC", "DSSTDTC")
merge1$CMTRT <- "ALL02"  # ALL02のデータセット作成終わり

# Pick up and proccessing data from AML05(終了日の列)
AML05pick <- subset(AML05, AML05$事後不適格 == "#N/A")
AML05pick[is.na(AML05pick)] <- "-"  # Replace NA to "-"

for (i in 1:length(AML05pick$J_CD)) {
  str.a <- AML05pick$中止届有無0なし.1あり[i]
  srt.b <- AML05pick$移植有無0.なし.1.あり[i]
  str.date <- ""

  if  (str.a == 1) {
    str.date <- AML05pick$中止届に記載された中止日 [i]
  } else if (srt.b == 1) {
    str.date <- AML05pick$移植日 [i]
  } else {
    str.date  <- AML05pick$therapy最終投薬日[i]}

  AML05pick$DATE_END_TRT[i]= str.date  # 結果を入れる列の指定
}
AML05pick1 <- AML05pick[, c(2, 5, 14:16)]
names(AML05pick1)[4] <- "AML05最終確認日"

# Pick up data from JPLSG
JPLSGpick <- JPLSG[, c(15, 21:23)]
merge2 <- merge(AML05pick1, JPLSGpick, by.x="J_CD", by.y="登録コード", all.x=T)

# proccessing data from merge data(生死の列)
for (i in 1:length(merge2$J_CD)){
  str.a <- merge2$死亡.0.なし..1.あり[i]
  srt.b <- merge2$生死[i]
  str.tenki <- ""

  if  (str.a==1) {
    str.tenki <- "true"
  } else {
    str.tenki  <- srt.b
  }

  merge2$DTHFL[i] <- str.tenki
}

for (i in 1:length(merge2$J_CD)) {
  str.a <- merge2$DTHFL[i]
  str.b <- merge2$死亡日[i]
  str.c <- merge2$AML05最終確認日[i]
  str.date <- ""

  if ((str.a == "true") & (srt.b == "")) {
    str.date <- str.c
  } else if (str.a == "true") {
    str.date <- str.b
  } else {
    str.date <- ""
  }
  merge2$DTHDTC[i] <- str.date
}

for (i in 1:length(merge2$J_CD)) {
  str.a <- merge2$最終確認日[i]
  str.b <- merge2$AML05最終確認日[i]
  str.date <- ""

  ifelse(str.a == "", str.date <- str.b, str.date <- str.a)

  merge2$DSSTDTC[i] <- str.date
}

merge2 <- merge2[, c(1, 2, 5, 9:11)]
names(merge2)[1:2] <- c("SUBJID", "MHSTDTC")
merge2$CMTRT <- "AML05"

calc.data <- rbind(merge1, merge2)
