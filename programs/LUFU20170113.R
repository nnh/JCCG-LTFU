#LUFU 
#mamiko yonejima
#2017/1/13
#########
Fix_Date = "2014/10/31"
setwd("./rawdata")

#Making List
list <- as.data.frame(list.files())
list$no  <- c(1:nrow(list))
list$DFname <- substr(list.files(), 1, 5)
names(list)[1] <- "file_name"

#Read CSVdata
for(i in 1:length(list$no)){
  eval(
    parse(
      text=paste0(list$DFname[i],"<- read.csv('",list$file_name[i],"',as.is=T,fileEncoding='CP932')")
    )
  )
}
#Pick up data from ALL02
ALL02pick <-ALL02[,c(2,6,45)]
#Pick up data from JACLS
JACLSpick<-JACLS[,c(15,21:23)]
#merge
merge1 <- merge(ALL02pick,JACLSpick,by.x="JACLS登録コード",by.y="登録コード",all.x=T)
names(merge1) <- c("SUBJID","MHSTDTC","DATE_END_TRT","DTHFL","DTHDTC","DSSTDTC")
merge1$CMTRT <- "ALL02"    #ALL02のデータセット作成終わり

#Pick up and proccessing data from AML05(終了日の列)
AML05pick <- subset(AML05,AML05$事後不適格=="#N/A")
AML05pick[is.na(AML05pick)]<-"-"            #Replace NA to "-"

for (i in 1:length(AML05pick$J_CD)){
  strA = AML05pick$中止届有無0なし.1あり[i] 　　　　　
  srtB = AML05pick$移植有無0.なし.1.あり[i]  　　　
  strDATE = ""       　　　　　　　
  
  if  (strA==1){ 
    strDATE <- AML05pick$中止届に記載された中止日 [i]
  }else if(srtB==1){
    strDATE <- AML05pick$移植日 [i]
  }else{
    strDATE  <- AML05pick$therapy最終投薬日[i]}
  
  AML05pick$DATE_END_TRT[i]= strDATE  　　　 #結果を入れる列の指定
}
AML05pick1 <- AML05pick[,c(2,5,14:16)]
names(AML05pick1)[4] <- "AML05最終確認日"

#Pick up data from JPLSG
JPLSGpick<-JPLSG[,c(15,21:23)]
merge2 <- merge(AML05pick1,JPLSGpick,by.x="J_CD",by.y="登録コード",all.x=T)

#proccessing data from merge data(生死の列)
for (i in 1:length(merge2$J_CD)){
  strA = merge2$死亡.0.なし..1.あり[i] 　　　　　
  srtB = merge2$生死[i]  　　　
  strTENKI = ""       　　　　　　　
  
  if  (strA==1){ 
    strTENKI <- "true"
  }else{
    strTENKI  <- srtB}
  
  merge2$DTHFL[i]= strTENKI  　　　 
}

for (i in 1:length(merge2$J_CD)){
  strA = merge2$DTHFL[i]　　　　　
  strB = merge2$死亡日[i]  
  strC = merge2$AML05最終確認日[i]
  strDATE = ""       　　　　　　　
  
  if  ((strA=="true")&(srtB=="")){ 
     strDATE <- strC
  }else if(strA=="true"){
     strDATE <- strB
  }else{
    strDATE  <- ""}
  
  merge2$DTHDTC[i]= strDATE  　　　 
}

for (i in 1:length(merge2$J_CD)){
  strA = merge2$最終確認日[i]　　　　
  strB = merge2$AML05最終確認日[i]  
  strDATE = ""       　　　　　　　
  
  ifelse(strA=="", 
          strDATE <- strB,
          strDATE <- strA)
  
  merge2$DSSTDTC[i]= strDATE  　　　 
}

merge2 <- merge2[,c(1,2,5,9:11)]
names(merge2)[1:2] <- c("SUBJID","MHSTDTC")
merge2$CMTRT <- "AML05"

calc_DATA <- rbind(merge1,merge2)
