# It is quit possible that at the time someone wants to reproduce the results
# using this code file, some of the codes/packages are not working due to updates.
# We hope this code file could at least reproduce the econometric results.
# Ying Wang, faust1987@tamu.edu

setwd("E:/PhD/heating and green innovation/data") # Please put your file path to the data set here


library(readxl)
library(geosphere)
library(rdrobust)
library(ggplot2)
library(jtools)
library(huxtable)
library(dplyr)
library(arsenal)
library(MatchIt)

rm(list=ls())

all_data=read_excel("green innovation data_English.xlsx", sheet = "All Data")
all_data=all_data[complete.cases(all_data),]
all_data=all_data[all_data$`Number of Green Innovations`<=all_data$`Number of Total Innovations`,]
all_data$`Total Asset`=all_data$`Total Asset`/1000000000
all_data$`Growth Rate of Main Business Income`=all_data$`Growth Rate of Main Business Income`/100
all_data$Distance=all_data$Distance/1000
all_data=mutate(all_data, heating_distance=all_data$Distance*all_data$Heating)
all_data=mutate(all_data, heating_negative=all_data$Heating )
all_data$heating_negative[all_data$heating_negative==0]=-1
all_data=mutate(all_data, distance_running=all_data$Distance*all_data$heating_negative)

longest_dis=4
point_est=matrix(0,nrow=longest_dis,ncol=5)

# green innovation/asset =================================================================== 

all_data=mutate(all_data, gi_cap=all_data$`Number of Green Innovations`/all_data$`Total Asset`)

# non-green/asset
all_data=mutate(all_data, ng=all_data$`Number of Total Innovations`-all_data$`Number of Green Innovations`)
all_data=mutate(all_data, ng_cap=all_data$ng/all_data$`Total Asset`)




# Figure 2 Kernal density  =======================================================================================================
library(sm)
library(kde1d)

data_100km=all_data[all_data$Distance<100,]

data_200km=all_data[all_data$Distance<200,]


data_100km_N=data_100km[data_100km$Heating==1,]
data_100km_S=data_100km[data_100km$Heating==0,]


gi_100km_N=data.frame(data_100km_N$gi_cap, Groups="Cities with public heating")
gi_100km_S=data.frame(data_100km_S$gi_cap, Groups="Cities without public heating")

names(gi_100km_N)[1] ="gi_cap"
names(gi_100km_S)[1] = "gi_cap"

hist_data_100km=rbind(gi_100km_N,gi_100km_S)


ggplot(gi_100km_N, aes(gi_cap, fill=Groups, colour=Groups)) +
  geom_histogram(aes(y=..count../sum(..count..)), alpha=0.2, 
                 position="identity", show.legend =F,breaks=seq(0,6,0.2)) +
  ggtitle("Normalized")

ggplot(gi_100km_S, aes(gi_cap, fill=Groups, colour=Groups)) +
  geom_histogram(aes(y=..count../sum(..count..)), alpha=0.2, 
                 position="identity", show.legend =F,breaks=seq(0,6,0.2)) +
  ggtitle("Normalized")


ggplot(hist_data_100km, aes(gi_cap, fill=Groups, colour=Groups)) +
  geom_histogram(aes(y=..count../sum(..count..)), alpha=0.2, 
                 position="identity", show.legend =T,breaks=seq(0,6,0.2)) +
  ggtitle("Normalized")


ggplot(hist_data_100km, aes(gi_cap, fill=Groups, colour=Groups)) +
  stat_density(aes(group = Groups, color = Groups),position="identity",geom="line",size=1.5)+
  xlab("The number of green innovations per billion RMB in total assets ") + ylab("Density")+
  ggtitle("Kernal densities for green innovations")



# Table 1 summary statistics================================================================
reg_sample=data.frame(all_data$`Number of Green Innovations`,
                      all_data$`Number of Total Innovations`,
                      all_data$`Total Asset`,
                      all_data$gi_cap,  
                      all_data$`Return on Net Assets`,
                      all_data$`Earnings per Share`, 
                      all_data$`Growth Rate of Main Business Income`,
                      all_data$`Number of Directors`,
                      all_data$Distance,
                      all_data$Heating,
                      all_data$ng,
                      all_data$ng_cap)

names(reg_sample)[names(reg_sample)=="all_data..Number.of.Green.Innovations."] = "No. of Green Innovation"
names(reg_sample)[names(reg_sample)=="all_data..Number.of.Total.Innovations."] = "No. of Total Innovation"
names(reg_sample)[names(reg_sample)=="all_data.gi_cap"] = "No. of Green Innovation per Asset"
names(reg_sample)[names(reg_sample)=="all_data..Total.Asset."] = "Total Asset"
names(reg_sample)[names(reg_sample)=="all_data..Return.on.Net.Assets."] = "RoNA"
names(reg_sample)[names(reg_sample)=="all_data..Earnings.per.Share."] = "Eps"
names(reg_sample)[names(reg_sample)=="all_data..Growth.Rate.of.Main.Business.Income."] = "GRoMBI"
names(reg_sample)[names(reg_sample)=="all_data..Number.of.Directors."] = "NoD"
names(reg_sample)[names(reg_sample)=="all_data.ng"] = "No. of Non-Green Innovation"
names(reg_sample)[names(reg_sample)=="all_data.ng_cap"] = "No. of Non-Green Innovation per Asset"
names(reg_sample)[names(reg_sample)=="all_data.Heating"] = "Heating"
names(reg_sample)[names(reg_sample)=="all_data.Distance"] = "Distance"



table_controls <- tableby.control(
  test = T,
  total = F,
  numeric.test = "anova", 
  numeric.stats = c("meansd",  "range"),
  stats.labels = list(
    meansd = "Mean (SD)",
    range = "Min - Max"
  )
)

sample_100km= reg_sample[reg_sample$Distance<100, ]
table_100km <- tableby(Heating ~ ., data =sample_100km,control = table_controls)

sample_200km= reg_sample[reg_sample$Distance<200, ]
table_200km <- tableby(Heating ~ ., data =sample_200km,control = table_controls)

sample_300km= reg_sample[reg_sample$Distance<300, ]
table_300km <- tableby(Heating ~ ., data =sample_300km,control = table_controls)

sample_400km= reg_sample[reg_sample$Distance<400, ]
table_400km <- tableby(Heating ~ ., data =sample_400km,control = table_controls)


write2word(table_100km , file = "summary of statistics_100km")
write2word(table_200km , file = "summary of statistics_200km")
write2word(table_300km , file = "summary of statistics_300km")
write2word(table_400km , file = "summary of statistics_400km")


# Table 2 data on industrial distribution ===================================================================================================

indu=data.frame(all_data$Industry_Chinese ,all_data$distance_running)
indu=fastDummies::dummy_cols(indu, select_columns = "all_data.Industry_Chinese")


names(indu)[names(indu)=="all_data.Industry_Chinese_采矿"] = "Mining"
names(indu)[names(indu)=="all_data.Industry_Chinese_电力"] = "Power"
names(indu)[names(indu)=="all_data.Industry_Chinese_纺织"] = "Textile"
names(indu)[names(indu)=="all_data.Industry_Chinese_钢铁"] = "Steel"
names(indu)[names(indu)=="all_data.Industry_Chinese_化工"] = "Chemical"
names(indu)[names(indu)=="all_data.Industry_Chinese_石化"] = "Petrochemical"
names(indu)[names(indu)=="all_data.Industry_Chinese_水泥"] = "Cement "
names(indu)[names(indu)=="all_data.Industry_Chinese_冶金"] = "Metallurgical"
names(indu)[names(indu)=="all_data.Industry_Chinese_医药"] = "Pharmaceutical"
names(indu)[names(indu)=="all_data.Industry_Chinese_造纸"] = "Paper"

indu=distinct(indu)
indu_N=indu[indu$all_data.distance_running>0,]
indu_S=indu[indu$all_data.distance_running<0,]
indu_T=matrix(0, nrow = 10,ncol =9)
indu_T[1,1]="Mining"
indu_T[2,1]="Power"
indu_T[3,1]="Textile"
indu_T[4,1]="Steel"
indu_T[5,1]="Chemical"
indu_T[6,1]="Petrochemical"
indu_T[7,1]="Cement"
indu_T[8,1]="Metallurgical"
indu_T[9,1]="Pharmaceutical"
indu_T[10,1]="Paper"

indu_N_100km=indu_N[abs(indu_N$all_data.distance_running)<100,]
indu_N_200km=indu_N[abs(indu_N$all_data.distance_running)<200,]
indu_N_300km=indu_N[abs(indu_N$all_data.distance_running)<300,]
indu_N_400km=indu_N[abs(indu_N$all_data.distance_running)<400,]

indu_S_100km=indu_S[abs(indu_S$all_data.distance_running)<100,]
indu_S_200km=indu_S[abs(indu_S$all_data.distance_running)<200,]
indu_S_300km=indu_S[abs(indu_S$all_data.distance_running)<300,]
indu_S_400km=indu_S[abs(indu_S$all_data.distance_running)<400,]



indu_T[1,2]=sum(indu_N_100km$Mining)
indu_T[2,2]=sum(indu_N_100km$Power)
indu_T[3,2]=sum(indu_N_100km$Textile)
indu_T[4,2]=sum(indu_N_100km$Steel)
indu_T[5,2]=sum(indu_N_100km$Chemical)
indu_T[6,2]=sum(indu_N_100km$Petrochemical)
indu_T[7,2]=sum(indu_N_100km$Cement)
indu_T[8,2]=sum(indu_N_100km$Metallurgical)
indu_T[9,2]=sum(indu_N_100km$Pharmaceutical)
indu_T[10,2]=sum(indu_N_100km$Paper)

indu_T[1,3]=sum(indu_S_100km$Mining)
indu_T[2,3]=sum(indu_S_100km$Power)
indu_T[3,3]=sum(indu_S_100km$Textile)
indu_T[4,3]=sum(indu_S_100km$Steel)
indu_T[5,3]=sum(indu_S_100km$Chemical)
indu_T[6,3]=sum(indu_S_100km$Petrochemical)
indu_T[7,3]=sum(indu_S_100km$Cement)
indu_T[8,3]=sum(indu_S_100km$Metallurgical)
indu_T[9,3]=sum(indu_S_100km$Pharmaceutical)
indu_T[10,3]=sum(indu_S_100km$Paper)

indu_T[1,4]=sum(indu_N_200km$Mining)
indu_T[2,4]=sum(indu_N_200km$Power)
indu_T[3,4]=sum(indu_N_200km$Textile)
indu_T[4,4]=sum(indu_N_200km$Steel)
indu_T[5,4]=sum(indu_N_200km$Chemical)
indu_T[6,4]=sum(indu_N_200km$Petrochemical)
indu_T[7,4]=sum(indu_N_200km$Cement)
indu_T[8,4]=sum(indu_N_200km$Metallurgical)
indu_T[9,4]=sum(indu_N_200km$Pharmaceutical)
indu_T[10,4]=sum(indu_N_200km$Paper)

indu_T[1,5]=sum(indu_S_200km$Mining)
indu_T[2,5]=sum(indu_S_200km$Power)
indu_T[3,5]=sum(indu_S_200km$Textile)
indu_T[4,5]=sum(indu_S_200km$Steel)
indu_T[5,5]=sum(indu_S_200km$Chemical)
indu_T[6,5]=sum(indu_S_200km$Petrochemical)
indu_T[7,5]=sum(indu_S_200km$Cement)
indu_T[8,5]=sum(indu_S_200km$Metallurgical)
indu_T[9,5]=sum(indu_S_200km$Pharmaceutical)
indu_T[10,5]=sum(indu_S_200km$Paper)

indu_T[1,6]=sum(indu_N_300km$Mining)
indu_T[2,6]=sum(indu_N_300km$Power)
indu_T[3,6]=sum(indu_N_300km$Textile)
indu_T[4,6]=sum(indu_N_300km$Steel)
indu_T[5,6]=sum(indu_N_300km$Chemical)
indu_T[6,6]=sum(indu_N_300km$Petrochemical)
indu_T[7,6]=sum(indu_N_300km$Cement)
indu_T[8,6]=sum(indu_N_300km$Metallurgical)
indu_T[9,6]=sum(indu_N_300km$Pharmaceutical)
indu_T[10,6]=sum(indu_N_300km$Paper)

indu_T[1,7]=sum(indu_S_300km$Mining)
indu_T[2,7]=sum(indu_S_300km$Power)
indu_T[3,7]=sum(indu_S_300km$Textile)
indu_T[4,7]=sum(indu_S_300km$Steel)
indu_T[5,7]=sum(indu_S_300km$Chemical)
indu_T[6,7]=sum(indu_S_300km$Petrochemical)
indu_T[7,7]=sum(indu_S_300km$Cement)
indu_T[8,7]=sum(indu_S_300km$Metallurgical)
indu_T[9,7]=sum(indu_S_300km$Pharmaceutical)
indu_T[10,7]=sum(indu_S_300km$Paper)

indu_T[1,8]=sum(indu_N_400km$Mining)
indu_T[2,8]=sum(indu_N_400km$Power)
indu_T[3,8]=sum(indu_N_400km$Textile)
indu_T[4,8]=sum(indu_N_400km$Steel)
indu_T[5,8]=sum(indu_N_400km$Chemical)
indu_T[6,8]=sum(indu_N_400km$Petrochemical)
indu_T[7,8]=sum(indu_N_400km$Cement)
indu_T[8,8]=sum(indu_N_400km$Metallurgical)
indu_T[9,8]=sum(indu_N_400km$Pharmaceutical)
indu_T[10,8]=sum(indu_N_400km$Paper)

indu_T[1,9]=sum(indu_S_400km$Mining)
indu_T[2,9]=sum(indu_S_400km$Power)
indu_T[3,9]=sum(indu_S_400km$Textile)
indu_T[4,9]=sum(indu_S_400km$Steel)
indu_T[5,9]=sum(indu_S_400km$Chemical)
indu_T[6,9]=sum(indu_S_400km$Petrochemical)
indu_T[7,9]=sum(indu_S_400km$Cement)
indu_T[8,9]=sum(indu_S_400km$Metallurgical)
indu_T[9,9]=sum(indu_S_400km$Pharmaceutical)
indu_T[10,9]=sum(indu_S_400km$Paper)

indu_T_sub=data.frame(indu_T)
indu_T_sub=data.frame(indu_T[,2:9])
indu_T_sub=as.numeric(as.matrix(indu_T_sub)) 
indu_T_sub=matrix(indu_T_sub, ncol =8,nrow=10)
indu_T_sub=data.frame(indu_T_sub)

indu_T_sub$X1=indu_T_sub$X1/sum(indu_T_sub$X1)
indu_T_sub$X2=indu_T_sub$X2/sum(indu_T_sub$X2)
indu_T_sub$X3=indu_T_sub$X3/sum(indu_T_sub$X3)
indu_T_sub$X4=indu_T_sub$X4/sum(indu_T_sub$X4)
indu_T_sub$X5=indu_T_sub$X5/sum(indu_T_sub$X5)
indu_T_sub$X6=indu_T_sub$X6/sum(indu_T_sub$X6)
indu_T_sub$X7=indu_T_sub$X7/sum(indu_T_sub$X7)
indu_T_sub$X8=indu_T_sub$X8/sum(indu_T_sub$X8)

chisq_1=chisq.test(indu_T_sub$X1,indu_T_sub$X2)
chisq_2=chisq.test(indu_T_sub$X3,indu_T_sub$X4)
chisq_3=chisq.test(indu_T_sub$X5,indu_T_sub$X6)
chisq_4=chisq.test(indu_T_sub$X7,indu_T_sub$X8)

chisq_1=as.matrix(chisq_1$p.value)
chisq_2=as.matrix(chisq_2$p.value)
chisq_3=as.matrix(chisq_3$p.value)
chisq_4=as.matrix(chisq_4$p.value)

chisq.table=matrix(0,nrow=1,ncol=4)
chisq.table[1,1]=chisq_1[1,1]
chisq.table[1,2]=chisq_2[1,1]
chisq.table[1,3]=chisq_3[1,1]
chisq.table[1,4]=chisq_4[1,1]



# Figure 3 RD plot =================================================================== 
# The distance we used is 200km, instead of 100km. This is to show some robustness.
all_200km=all_data[all_data$Distance<200,]

gi_cap=all_200km$gi_cap


rdplot(gi_cap,dis_all,p=1,x.label="Distance(KM)", y.label="The number of green innovations/total asset",ci=95)
ggsave("rd plot using 1st order polynomial.png")

rdplot(gi_cap,dis_all,p=2,x.label="Distance(KM)", y.label="The number of green innovations/total asset",ci=95)
ggsave("rd plot using 2st order polynomial.png")

rdplot(gi_cap,dis_all,p=3,x.label="Distance(KM)", y.label="The number of green innovations/total asset",ci=95)
ggsave("rd plot using 3st order polynomial.png")



# OLS results ===================================================================
names(all_data)[names(all_data)=="Return on Net Assets"] = "RoNA"
names(all_data)[names(all_data)=="Earnings per Share"] = "EpS"
names(all_data)[names(all_data)=="Growth Rate of Main Business Income"] = "GRoMBI"
names(all_data)[names(all_data)=="Number of Directors"] = "NoD"

ols_100km=lm(gi_cap ~ Heating + RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<100))
ols_200km=lm(gi_cap ~ Heating + RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<200))
ols_300km=lm(gi_cap ~ Heating + RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<300))
ols_400km=lm(gi_cap ~ Heating + RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<400))

ols_table=huxreg("100km"=ols_100km, "200km"=ols_200km,"300km"=ols_300km,"400km"=ols_400km,statistics = "nobs")
quick_docx(ols_table, file = "ols_table.docx")

# RD results ===================================================================

# The Distance variable in the paper tables is NOT the Distance variable in this 
# data, but distance_running.

reg_100km=lm(gi_cap ~ Heating + distance_running + heating_distance + RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<100))
reg_200km=lm(gi_cap ~ Heating + distance_running + heating_distance + RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<200))
reg_300km=lm(gi_cap ~ Heating + distance_running + heating_distance + RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<300))
reg_400km=lm(gi_cap ~ Heating + distance_running + heating_distance + RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<400))

reg_table=huxreg("100km"=reg_100km, "200km"=reg_200km,"300km"=reg_300km,"400km"=reg_400km,statistics = "nobs")
quick_docx(reg_table, file = "reg_table.docx")

for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~ Heating + distance_running + heating_distance + RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<d))
  est_sum=summary(reg_ikm)
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,3]
  
}
point_est=data.frame(point_est)

ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Dependent variable: The Number of Green Innovations per Billion RMB of Asset")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("regression using only valid independent variables_point est.png")

# non-green innovations
for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(ng_cap~ Heating +distance_running+heating_distance++ RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<d))
  est_sum=summary(reg_ikm)
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,3]
  
}
point_est=data.frame(point_est)

ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Dependent variable: The Number of Non-Green Innovations per Billion RMB of Asset")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("non-green innovation behavior.png")

reg_ng_100km=lm(ng_cap~ Heating +distance_running+heating_distance++ RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<100))
reg_ng_200km=lm(ng_cap~ Heating +distance_running+heating_distance++ RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<200))
reg_ng_300km=lm(ng_cap~ Heating +distance_running+heating_distance++ RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<300))
reg_ng_400km=lm(ng_cap~ Heating +distance_running+heating_distance++ RoNA + EpS + GRoMBI + NoD, data=subset(all_data,all_data$Distance<400))

reg_ng_table=huxreg("100km"=reg_ng_100km, "200km"=reg_ng_200km,"300km"=reg_ng_300km,"400km"=reg_ng_400km,statistics = "nobs")
quick_docx(reg_ng_table, file = "reg_ng_table.docx")

# discontinuity in covariates ====================================================
dis_NoD=matrix(NA,nrow=4,ncol=2)

# number.of.directors_est
number.of.directors_est=matrix(0,nrow=longest_dis,ncol=5)
for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(NoD ~ Heating +distance_running+heating_distance, data=subset(all_data,all_data$Distance<d))
  est_sum=summary(reg_ikm)
  dis_NoD[i,1]=est_sum$coef[2,1]
  dis_NoD[i,2]=est_sum$coef[2,4]
  number.of.directors_est[i,1]=d
  number.of.directors_est[i,2]=est_sum$coef[2,1]
  number.of.directors_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  number.of.directors_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  number.of.directors_est[i,5]=est_sum$coef[2,3]
}

number.of.directors_est=data.frame(number.of.directors_est)

ggplot() + geom_pointrange(number.of.directors_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Number of Directors")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("discontinuity_number.of.directors.png")


# RoNA_est
dis_RoNA=matrix(NA,nrow=4,ncol=2)

RoNA_est=matrix(0,nrow=longest_dis,ncol=5)
for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(RoNA ~ Heating +distance_running+heating_distance, data=subset(all_data,all_data$Distance<d))
  est_sum=summary(reg_ikm)
  dis_RoNA[i,1]=est_sum$coef[2,1]
  dis_RoNA[i,2]=est_sum$coef[2,4]
  RoNA_est[i,1]=d
  RoNA_est[i,2]=est_sum$coef[2,1]
  RoNA_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  RoNA_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  RoNA_est[i,5]=est_sum$coef[2,3]
}

RoNA_est=data.frame(RoNA_est)

ggplot() + geom_pointrange(RoNA_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Return on Net Assets")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("discontinuity_RoNA.png")


# GRoMBI_est
dis_GRoMBI=matrix(NA,nrow=4,ncol=2)

GRoMBI_est=matrix(0,nrow=longest_dis,ncol=5)
for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(GRoMBI ~ Heating +distance_running+heating_distance, data=subset(all_data,all_data$Distance<d))
  est_sum=summary(reg_ikm)
  dis_GRoMBI[i,1]=est_sum$coef[2,1]
  dis_GRoMBI[i,2]=est_sum$coef[2,4]
  number.of.directors_est[i,1]=d
  number.of.directors_est[i,2]=est_sum$coef[2,1]
  number.of.directors_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  number.of.directors_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  number.of.directors_est[i,5]=est_sum$coef[2,3]
}

number.of.directors_est=data.frame(number.of.directors_est)

ggplot() + geom_pointrange(number.of.directors_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Growth Rate of Main Business Income")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("discontinuity_GRoMBI.png")



# EpS_est

dis_EpS=matrix(NA,nrow=4,ncol=2)

EpS_est=matrix(0,nrow=longest_dis,ncol=5)
for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(EpS ~ Heating +distance_running+heating_distance, data=subset(all_data,all_data$Distance<d))
  est_sum=summary(reg_ikm)
  dis_EpS[i,1]=est_sum$coef[2,1]
  dis_EpS[i,2]=est_sum$coef[2,4]
  number.of.directors_est[i,1]=d
  number.of.directors_est[i,2]=est_sum$coef[2,1]
  number.of.directors_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  number.of.directors_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  number.of.directors_est[i,5]=est_sum$coef[2,3]
}

number.of.directors_est=data.frame(number.of.directors_est)


ggplot() + geom_pointrange(number.of.directors_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Earnings per Share")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("discontinuity_EpS.png")


dis_covs=cbind.data.frame(dis_RoNA, dis_EpS,dis_GRoMBI,dis_NoD)

# erasing between-province compound effect======================================================

e_106_data=all_data[all_data$lon > 106.1896258,]  

e_106_100km=lm(gi_cap ~  Heating + distance_running + heating_distance + RoNA + EpS + GRoMBI + NoD, data=subset(e_106_data,e_106_data$Distance<100))
e_106_200km=lm(gi_cap ~  Heating + distance_running + heating_distance + RoNA + EpS + GRoMBI + NoD, data=subset(e_106_data,e_106_data$Distance<200))
e_106_300km=lm(gi_cap ~  Heating + distance_running + heating_distance + RoNA + EpS + GRoMBI + NoD, data=subset(e_106_data,e_106_data$Distance<300))
e_106_400km=lm(gi_cap ~  Heating + distance_running + heating_distance + RoNA + EpS + GRoMBI + NoD, data=subset(e_106_data,e_106_data$Distance<400))


e_106_table=huxreg("100km"=e_106_100km, "200km"=e_106_200km,"300km"=e_106_300km,"400km"=e_106_400km,statistics = "nobs")
quick_docx(e_106_table, file = "regression using only the sample at the east of 106 E.docx")



for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~  Heating +distance_running+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(e_106_data,e_106_data$Distance<d))
  est_sum=summary(reg_ikm)
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,3]
}

point_est=data.frame(point_est)

ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("regression using only the sample at the east of 106 E.png")



# functional form  =====================================================================================================
all_data=mutate(all_data, distance_square=all_data$distance_running*all_data$distance_running*sign(all_data$distance_running))
all_data=mutate(all_data, distance_cubic=all_data$distance_running*all_data$distance_running*all_data$distance_running)



for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~ Heating +distance_running+distance_square+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<d))
  est_sum=summary(reg_ikm)
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,3]
  
}
point_est=data.frame(point_est)


ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Dependent variable: The Number of Green Innovations per Billion RMB of Asset")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("regression with square term.png")


reg_square_100km=lm(gi_cap ~ Heating +distance_running+distance_square+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<100))
reg_square_200km=lm(gi_cap ~ Heating +distance_running+distance_square+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<200))
reg_square_300km=lm(gi_cap ~ Heating +distance_running+distance_square+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<300))
reg_square_400km=lm(gi_cap ~ Heating +distance_running+distance_square+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<400))

reg_square_table=huxreg("100km"=reg_square_100km, "200km"=reg_square_200km,"300km"=reg_square_300km,"400km"=reg_square_400km,statistics = "nobs")
quick_docx(reg_square_table, file = "reg_square_table.docx")



for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~ Heating +distance_running+distance_square+distance_cubic+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<d))
  est_sum=summary(reg_ikm)
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,3]
  
}
point_est=data.frame(point_est)

ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Dependent variable: The Number of Green Innovations per Billion RMB of Asset")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("regression with square and cubic term.png")


reg_cubic_100km=lm(gi_cap ~ Heating +distance_running+distance_square+distance_cubic+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<100))
reg_cubic_200km=lm(gi_cap ~ Heating +distance_running+distance_square+distance_cubic+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<200))
reg_cubic_300km=lm(gi_cap ~ Heating +distance_running+distance_square+distance_cubic+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<300))
reg_cubic_400km=lm(gi_cap ~ Heating +distance_running+distance_square+distance_cubic+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=subset(all_data,all_data$Distance<400))

reg_cubic_table=huxreg("100km"=reg_cubic_100km, "200km"=reg_cubic_200km,"300km"=reg_cubic_300km,"400km"=reg_cubic_400km,statistics = "nobs")
quick_docx(reg_cubic_table, file = "reg_cubic_table.docx")
# placebo test ====================================================================================================
# 

# move boundary to the north 100km
N100_distance_running=all_data$distance_running +100
N100_Heating=matrix(sign(N100_distance_running))
N100_distance=abs(N100_distance_running)

for(i in 1:nrow(N100_Heating)){
  if (N100_Heating[i,1]==-1){
    N100_Heating[i,1]=0
  }
}

N100_heating_distance=N100_Heating*N100_distance_running

N100_data=cbind.data.frame(all_data,N100_distance_running)
N100_data=cbind.data.frame(N100_data,N100_Heating)
N100_data=cbind.data.frame(N100_data,N100_heating_distance)
N100_data=cbind.data.frame(N100_data,N100_distance)

placebo_N100km=matrix(NA,nrow=4,ncol=2)

for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~ N100_Heating +N100_heating_distance+N100_heating_distance+ RoNA + EpS + GRoMBI + NoD, data=subset(N100_data,N100_data$N100_distance<d))
  placebo_N100km[i,1]=est_sum$coef[2,1]
  placebo_N100km[i,2]=est_sum$coef[2,4]
  est_sum=summary(reg_ikm)
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,4] # this is to get p-value, instead of t-test
  
}
point_est=data.frame(point_est)

ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Placebo Test North 100 km")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("Placebo Test North 100 km.png")

# move boundary to the north 200km
N200_distance_running=all_data$distance_running +200
N200_Heating=matrix(sign(N200_distance_running))
N200_distance=abs(N200_distance_running)

for(i in 1:nrow(N200_Heating)){
  if (N200_Heating[i,1]==-1){
    N200_Heating[i,1]=0
  }
}

N200_heating_distance=N200_Heating*N200_distance_running

N200_data=cbind.data.frame(all_data,N200_distance_running)
N200_data=cbind.data.frame(N200_data,N200_Heating)
N200_data=cbind.data.frame(N200_data,N200_heating_distance)
N200_data=cbind.data.frame(N200_data,N200_distance)

placebo_N200km=matrix(NA,nrow=4,ncol=2)

for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~ N200_Heating +N200_heating_distance+N200_heating_distance+RoNA+ EpS+ GRoMBI+ NoD , data=subset(N200_data,N200_data$N200_distance<d))
  est_sum=summary(reg_ikm)
  placebo_N200km[i,1]=est_sum$coef[2,1]
  placebo_N200km[i,2]=est_sum$coef[2,4]
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,4]
  
}
point_est=data.frame(point_est)



ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Placebo Test North 200 km")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("Placebo Test North 200 km.png")



# move boundary to the north 300km
N300_distance_running=all_data$distance_running +300
N300_Heating=matrix(sign(N300_distance_running))
N300_distance=abs(N300_distance_running)

for(i in 1:nrow(N300_Heating)){
  if (N300_Heating[i,1]==-1){
    N300_Heating[i,1]=0
  }
}

N300_heating_distance=N300_Heating*N300_distance_running

N300_data=cbind.data.frame(all_data,N300_distance_running)
N300_data=cbind.data.frame(N300_data,N300_Heating)
N300_data=cbind.data.frame(N300_data,N300_heating_distance)
N300_data=cbind.data.frame(N300_data,N300_distance)

placebo_N300km=matrix(NA,nrow=4,ncol=2)

for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~ N300_Heating +N300_heating_distance+N300_heating_distance+RoNA+ EpS+ GRoMBI+ NoD , data=subset(N300_data,N300_data$N300_distance<d))
  est_sum=summary(reg_ikm)
  placebo_N300km[i,1]=est_sum$coef[2,1]
  placebo_N300km[i,2]=est_sum$coef[2,4]
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,4]
  
}
point_est=data.frame(point_est)


ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Placebo Test North 300 km")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("Placebo Test North 300 km.png")



# move boundary to the south 100km
S100_distance_running=all_data$distance_running -100
S100_Heating=matrix(sign(S100_distance_running))
S100_distance=abs(S100_distance_running)

for(i in 1:nrow(S100_Heating)){
  if (S100_Heating[i,1]==-1){
    S100_Heating[i,1]=0
  }
}

S100_heating_distance=S100_Heating*S100_distance_running

S100_data=cbind.data.frame(all_data,S100_distance_running)
S100_data=cbind.data.frame(S100_data,S100_Heating)
S100_data=cbind.data.frame(S100_data,S100_heating_distance)
S100_data=cbind.data.frame(S100_data,S100_distance)

placebo_S100km=matrix(NA,nrow=4,ncol=2)

for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~ S100_Heating +S100_heating_distance+S100_heating_distance+RoNA+ EpS+ GRoMBI+ NoD , data=subset(S100_data,S100_data$S100_distance<d))
  est_sum=summary(reg_ikm)
  placebo_S100km[i,1]=est_sum$coef[2,1]
  placebo_S100km[i,2]=est_sum$coef[2,4]
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,4]
  
}
point_est=data.frame(point_est)


ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Placebo Test South 100 km")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("placebo test South 100km.png")

# move boundary to the south 200km
S200_distance_running=all_data$distance_running -200
S200_Heating=matrix(sign(S200_distance_running))
S200_distance=abs(S200_distance_running)

for(i in 1:nrow(S200_Heating)){
  if (S200_Heating[i,1]==-1){
    S200_Heating[i,1]=0
  }
}

S200_heating_distance=S200_Heating*S200_distance_running

S200_data=cbind.data.frame(all_data,S200_distance_running)
S200_data=cbind.data.frame(S200_data,S200_Heating)
S200_data=cbind.data.frame(S200_data,S200_heating_distance)
S200_data=cbind.data.frame(S200_data,S200_distance)

placebo_S200km=matrix(NA,nrow=4,ncol=2)

for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~ S200_Heating +S200_heating_distance+S200_heating_distance+RoNA+ EpS+ GRoMBI+ NoD , data=subset(S200_data,S200_data$S200_distance<d))
  est_sum=summary(reg_ikm)
  placebo_S200km[i,1]=est_sum$coef[2,1]
  placebo_S200km[i,2]=est_sum$coef[2,4]
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,4]
  
}
point_est=data.frame(point_est)


ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Placebo Test South 200 km")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("placebo test South 200km.png")


# move boundary to the south 300km
S300_distance_running=all_data$distance_running -300
S300_Heating=matrix(sign(S300_distance_running))
S300_distance=abs(S300_distance_running)

for(i in 1:nrow(S300_Heating)){
  if (S300_Heating[i,1]==-1){
    S300_Heating[i,1]=0
  }
}

S300_heating_distance=S300_Heating*S300_distance_running

S300_data=cbind.data.frame(all_data,S300_distance_running)
S300_data=cbind.data.frame(S300_data,S300_Heating)
S300_data=cbind.data.frame(S300_data,S300_heating_distance)
S300_data=cbind.data.frame(S300_data,S300_distance)

placebo_S300km=matrix(NA,nrow=4,ncol=2)

for (i in 1:longest_dis) {
  d=i*100
  reg_ikm=lm(gi_cap ~ S300_Heating +S300_heating_distance+S300_heating_distance+RoNA+ EpS+ GRoMBI+ NoD , data=subset(S300_data,S300_data$S300_distance<d))
  est_sum=summary(reg_ikm)
  placebo_S300km[i,1]=est_sum$coef[2,1]
  placebo_S300km[i,2]=est_sum$coef[2,4]
  point_est[i,1]=d
  point_est[i,2]=est_sum$coef[2,1]
  point_est[i,3]=est_sum$coef[2,1] - qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,4]=est_sum$coef[2,1] + qt(0.975, df = est_sum$df[2]) * est_sum$coef[2, 2]
  point_est[i,5]=est_sum$coef[2,4]
  
}
point_est=data.frame(point_est)

ggplot() + geom_pointrange(point_est, mapping=aes(x=X1, y=X2, ymin=X4, ymax=X3), size=1,  fill="white", shape=21)+
  xlab("Distance(KM)") + ylab("Point Estimate")+
  ggtitle("Placebo Test South 300 km")+ 
  geom_hline(yintercept=0,linetype="dashed", color = "red")
ggsave("placebo test South 300km.png")



# matching=====================================================================================================
library(MatchIt)
library(fastDummies)
library(stargazer)
ind_dummies=dummy_columns(all_data$Industry_English)


names(ind_dummies)[names(ind_dummies)==".data_Mining"] = "Mining"
names(ind_dummies)[names(ind_dummies)==".data_Power Generation"] = "Power"
names(ind_dummies)[names(ind_dummies)==".data_Textile"] = "Textile"
names(ind_dummies)[names(ind_dummies)==".data_Steel"] = "Steel"
names(ind_dummies)[names(ind_dummies)==".data_Chemical"] = "Chemical"
names(ind_dummies)[names(ind_dummies)==".data_Petrochemical"] = "Petrochemical"
names(ind_dummies)[names(ind_dummies)==".data_Cement"] = "Cement"
names(ind_dummies)[names(ind_dummies)==".data_Metallurgical"] = "Metallurgical"
names(ind_dummies)[names(ind_dummies)==".data_Pharmaceutical"] = "Pharmaceutical"
names(ind_dummies)[names(ind_dummies)==".data_Paper"] = "Paper"

match_data=cbind.data.frame(all_data,ind_dummies)

data_100km=match_data[match_data$Distance<100,]
names(data_100km)[names(data_100km)=="Distance"] = "distance_variable"
match_result <- matchit(Heating ~ `Total Asset`+Industry_English+ Mining+Power
                        +Textile+Steel+Chemical+Petrochemical+ Cement +Metallurgical+Pharmaceutical, data = data_100km, method="nearest", ratio=1)
matched_data_100km= match.data(match_result)[1:ncol(data_100km)]
match_reg_100km=lm(gi_cap ~ Heating +distance_running+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=matched_data_100km)


data_200km=match_data[match_data$Distance<200,]
names(data_200km)[names(data_200km)=="Distance"] = "distance_variable"
match_result <- matchit(Heating ~ `Total Asset`+Industry_English+ Mining+Power
                        +Textile+Steel+Chemical+Petrochemical+ Cement +Metallurgical+Pharmaceutical, data = data_200km, method="nearest", ratio=1)
matched_data_200km= match.data(match_result)[1:ncol(data_200km)]
match_reg_200km=lm(gi_cap ~ Heating +distance_running+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=matched_data_200km)

data_300km=match_data[match_data$Distance<300,]
names(data_300km)[names(data_300km)=="Distance"] = "distance_variable"
match_result <- matchit(Heating ~ `Total Asset`+Industry_English+ Mining+Power
                        +Textile+Steel+Chemical+Petrochemical+ Cement +Metallurgical+Pharmaceutical, data = data_300km, method="nearest", ratio=1)
matched_data_300km= match.data(match_result)[1:ncol(data_300km)]
match_reg_300km=lm(gi_cap ~ Heating +distance_running+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=matched_data_300km)

data_400km=match_data[match_data$Distance<400,]
names(data_400km)[names(data_400km)=="Distance"] = "distance_variable"
match_result <- matchit(Heating ~ `Total Asset`+Industry_English+ Mining+Power
                        +Textile+Steel+Chemical+Petrochemical+ Cement +Metallurgical+Pharmaceutical, data = data_400km, method="nearest", ratio=1)
matched_data_400km= match.data(match_result)[1:ncol(data_400km)]
match_reg_400km=lm(gi_cap ~ Heating +distance_running+heating_distance+RoNA+ EpS+ GRoMBI+ NoD, data=matched_data_400km)

reg_table=stargazer(match_reg_100km, match_reg_200km, match_reg_300km,match_reg_400km,  type = "text",intercept.bottom = F, intercept.top = T)


matching_table=huxreg("100km"=match_reg_100km, "200km"=match_reg_200km,"300km"=match_reg_300km,"400km"=match_reg_400km,statistics = "nobs")
quick_docx(matching_table, file = "matching_table.docx")

