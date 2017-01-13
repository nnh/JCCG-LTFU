#JACLS_ALL02_csvファイル整え
#Mamiko Yonejima
#2016/10/12

#治療終了後年数-生死-total表1)
#生
ALIVE <- subset(LTFU_DATASET,LTFU_DATASET$ALIVE_DEATH=="false")
cnt_ALIVE <- as.data.frame(table(ALIVE$DIFF_END_FINAL))
colnames(cnt_ALIVE) <- c("FU_years_AfterEND","alive")

#死
DEATH <- subset(LTFU_DATASET,LTFU_DATASET$ALIVE_DEATH=="true")
cnt_DEATH <- as.data.frame(table(DEATH$DIFF_END_DEATH))
colnames(cnt_DEATH) <- c("FU_years_AfterEND","death")


#マージ
table1 <- merge(cnt_ALIVE,cnt_DEATH,by="FU_years_AfterEND",all=T)


#総数とFUされているパーセントの計算
total = length(LTFU_DATASET$登録コード)

table1$number_登録総数マイナス死亡 <- ifelse(is.na(table1$death),total,total-table1$death)
table1$percent_FU <-  floor((table1$alive/table1$number_登録総数マイナス死亡)*100*10^(1-1)+0.5)/10^(1-1)

table1_1<-table1[-1,]
table1_1$FU_years_AfterEND<- as.numeric(as.character(table1_1$FU_years_AfterEND))
table1_1 <- table1_1[order(table1_1$FU_years_AfterEND),]

#不明
LOST <- subset(LTFU_DATASET,LTFU_DATASET$ALIVE_DEATH=="LOST")
unknown <-　length(LOST$登録コード)
DFLOST <- data.frame(
       FU_years_AfterEND = "転帰不明",
       alive = unknown ,
       death = "",
       number_登録総数マイナス死亡  = "",
       percent_FU = ""
                     )

cnt_DEATH <- as.data.frame(table(DEATH$DIFF_END_DEATH))
colnames(cnt_DEATH) <- c("FU_years_AfterEND","death")

table1_2<-table1[1,]
table1_2$FU_years_AfterEND <-"終了日もしくは最終確認日不明"

table1 <- rbind(table1_1,table1_2,DFLOST)

#dropoutした年齢 表2)
#生
ALIVE2 <- subset(LTFU_DATASET,LTFU_DATASET$ALIVE_DEATH=="false"|LTFU_DATASET$ALIVE_DEATH=="LOST")
cnt_ALIVE2 <- as.data.frame(table(ALIVE2$AGE_FINAL))
colnames(cnt_ALIVE2) <- c("AGE_dropout","alive")
cnt_ALIVE2_1 <- cnt_ALIVE2[-1,]


#死
DEATH2 <- subset(LTFU_DATASET,LTFU_DATASET$ALIVE_DEATH=="true")
cnt_DEATH2 <- as.data.frame(table(DEATH$AGE_DEATH))
colnames(cnt_DEATH2) <- c("AGE_dropout","death")

#診断時年齢
Diagnosis<- as.data.frame(table(LTFU_DATASET$age_diagnosis))
colnames(Diagnosis) <- c("age_diagnosis","diagnosis")

#マージ
table2 <- merge(cnt_ALIVE2_1,cnt_DEATH2,by="AGE_dropout",all=T)
table2 <- merge(table2,Diagnosis,by.x="AGE_dropout",by.y="age_diagnosis",all=T)

#年で並べ替え
table2$AGE_dropout<-as.numeric(as.character(table2$AGE_dropout))
table2 <- table2[order(table2$AGE_dropout),]

cnt_ALIVE2_2 <- cnt_ALIVE2[1,]
cnt_ALIVE2_2$AGE_dropout <- "最終確認日不明"
cnt_ALIVE2_2$death       <- "0"
cnt_ALIVE2_2$diagnosis   <- ""
table2 <- rbind(table2,cnt_ALIVE2_2)

setwd("../config")
source("LTFUconfig.R")
Output1 <- paste0(UsingDataName,"table1.csv")
Output2 <- paste0(UsingDataName,"table2.csv")
setwd("../output")
write.csv(table1,Output1,row.names=F)
write.csv(table2,Output2,row.names=F)

