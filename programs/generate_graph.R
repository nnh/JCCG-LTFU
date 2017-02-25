# New Algorithmを用いてflow-up率の棒グラフを表示する
# x.lab <- dimnames(rates.all02)[["y.from.end.trt"]]
# rates.all02["Sum", -length(rates.all02["Sum",])]  # Sum行を除いたfollow-up率
barplot(rates.all02['Sum',], family="sans",
        main="Follow-up rate by years after end of treatment, ALL02, NEW",
        xlab="Years after end of treatment", ylab="Follow up rate")
barplot(rates.all02[, 'Sum'], family="sans",
        main="Follow-up rate by age at data fix, ALL02, NEW",
        xlab="Age at data fix", ylab="Follow up rate")

barplot(rate.end.trt, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
        main="Follow-up rate by years after end of treatment",
        xlab="Years after end of treatment", ylab="Follow up rate")
barplot(rate.end.trt.all02, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
        main="Follow-up rate by years after end of treatment, ALL02",
        xlab="Years after end of treatment", ylab="Follow up rate")
barplot(rate.end.trt.aml05, ylim=c(0:1), names.arg=c(1:max(ads1$y.from.end.trt)), family="sans",
        main="Follow-up rate by years after end of treatment, AML05",
        xlab="Years after end of treatment", ylab="Follow up rate")

barplot(rate.age.datafix, ylim=c(0:1), names.arg=c(1:max(ads2$age.at.datafix)), family="sans",
        main="Follow-up rate by age at data fix", xlab="Age at data fix", ylab="Follow up rate")
barplot(rate.age.datafix.all02, ylim=c(0:1), names.arg=c(1:max(ads2$age.at.datafix)), family="sans",
        main="Follow-up rate by age at data fix, ALL02", xlab="Age at data fix", ylab="Follow up rate")
barplot(rate.age.datafix.aml05, ylim=c(0:1), names.arg=c(1:max(ads2$age.at.datafix)), family="sans",
        main="Follow-up rate by age at data fix, AML05", xlab="Age at data fix", ylab="Follow up rate")

barplot(f.u.rate3, names.arg=prefecture$Prefecture, family="sans", las=3, ylim=c(0:1),
        main="Follow-up rate by prefecture", xlab="", ylab="Follow up rate", cex.names=0.7)

barplot(rate.age.followup, ylim=c(0:1), names.arg=c(1:x.max), family="sans",
        main="Follow-up rate by age at follow-up", xlab="Age at follow-up", ylab="Follow up rate")
barplot(rate.age.followup.all02, ylim=c(0:1), names.arg=c(1:x.max), family="sans",
        main="Follow-up rate by age at follow-up, ALL02", xlab="Age at follow-up", ylab="Follow up rate")
barplot(rate.age.followup.aml05, ylim=c(0:1), names.arg=c(1:x.max), family="sans",
        main="Follow-up rate by age at follow-up, AML05", xlab="Age at follow-up", ylab="Follow up rate")
