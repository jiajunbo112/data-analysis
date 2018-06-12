library(ggplot2)
#library(dplyr)
library(plyr)
library(cowplot)

setwd("C:\\Users\\Administrator\\Desktop\\A题\\A题")
data <- read.csv("2018年过敏统计信息表.csv")
data <- data[c(1:ncol(data)-1)]


###清理数据

#提取数值
abstract_num <- function(s){
  s=as.character(s)         #转换成字符串
  s=substr(s,1,1)     #提取第一个数值字符
  i=as.numeric(s)    #把提取的字符转换成数值
  return(i)
}

data0=data[c(1:7)]
data1=sapply(data[c(8:ncol(data))],abstract_num)
data=cbind(data0,data1)
####
#问题一：试分析过敏性疾病与时间，季节，性别，年龄分别有什么样的关系；

keshi <- c("儿科","耳鼻喉科","耳专病门诊","妇科","骨科","呼吸科","灰甲趾","康复医学科",
           "口腔科","泌尿科","内分泌科","皮肤科","普内科","普外科","神经科","肾内科",
            "体检科","体检中心","推拿科","消化科","心内科","血液科", "眼科","中医",
           "中医科","肿瘤科" )

ills <- c("儿科","耳鼻喉科","呼吸科","内分泌科","皮肤科",
          "普内科","肾内科","眼科")

#-------------季节---------------------

data$Season=NA
data$Season[which(data$month>11 | data$month<3)]="Winter"
data$Season[which(data$month>2 & data$month<6)]="Spring"
data$Season[which(data$month>5 & data$month<9)]="Summer"
data$Season[which(data$month>8 & data$month<12)]="Autumn"

func <- function(abc,ylims){
  shuju <- data.frame()
  for(j in ills){
    nums <- c()
    for(i in abc){
      dt <- data[which(data$age1==i & data$科室==j),]
      dt <- nrow(dt)
      nums <-c(nums,dt)
    }
    nums <- nums/bili
    shuju <- rbind(shuju,nums)
  }
  colnames(shuju) <- abc
  cols <- terrain.colors(length(abc))
  
  shuju <- t(as.matrix(shuju))
  barplot(shuju,  offset = 0, axis.lty = 1, ylim =c(0,ylims), 
          names.arg = ills, col = cols, beside = TRUE) 
  box() 
  legend("topright", legend = label, fill = cols,ncol=1,cex=1) 
  title(main = "过敏性疾病与年龄", xlab = "疾病类型", ylab = "人数") 
  
}

jijie <- c("Spring","Summer","Autumn","Winter")
func(jijie,320)


#--------性别---------------
xingbie <- c("女","男")
func(xingbie,550)

#---------年龄-------------
age <- c(0,10,20,30,40,50,60,70,80,90,100)
label <- c("0-9岁","10-19岁","20-29岁","30-39岁","40-49岁","50-59岁","60-69岁","70-100岁")
hist(data$年龄,main = "过敏性疾病与年龄", xlab = "年龄", ylab = "人数",col = "yellow")
data$age1=0
for(i in 1:length(age)-1){
  data$age1[data$年龄>=age[i]]=age[i+1]
}
a <- c()
age <- c(10,20,30,40,50,60,70,100)
for(i in 1:10){
  t <- data[which(data$age==age[i]),]
  a <- c(a,nrow(t))
}

bili <- c(0.109856627,0.131153109,0.171391556,0.161440733,0.17283377,
         0.120099531,0.074866777,0.058357897)
bili <- bili*10
jiaquan <- a/bili*10 
label <- c("0-9岁","10-19岁","20-29岁","30-39岁","40-49岁","50-59岁",
           "60-69岁","70-79岁","80-89岁","90-99岁" )
barplot(jiaquan,names.arg = label)

##----时间----------------

time <- c(2013,2014,2015,2016,2017)
func(time)


##-----huiguifenxi----------------
data$Season=as.factor(data$Season)
data$性别=as.factor(data$性别)

r <- data.frame()
for(i in ills){
  data$y=0
  data$y[which(data$科室==i)]=1
  model <- lm(y~性别+age+Season+Year,data=data)
  model <- summary(model)
  r <- rbind(r,model$coefficients)
}

r <- round(r,3)
write.csv(r,"diyiwen.csv")

###问题二：结合第一问的相关影响因素，分析不同类型疾病的病人过敏原检测
#          结果之间的差异；

keshi <- data.frame(table(data$科室))    #观察不同类型疾病病人数
write.csv(keshi,"keshi.csv")
#选取人数超过三十的疾病类型进行分析

ills <- c("儿科","耳鼻喉科","呼吸科","内分泌科","皮肤科",
          "普内科","肾内科","眼科")
func6 <- function(ill,main){
  dt <- data[which(data$科室==ill),]
  n <- c()
  for(i in 8:24){
    sums <- sum(dt[i]>0,na.rm = T)
    n <- c(n,sums)
  }
  x <- as.vector(colnames(dt)[8:24])
  H <- as.vector(n)
  #H=n/nrow(dt)
  barplot(H,names.arg=x,col = "blue",main=main,)
  
  
}
opar<-par(no.readonly=T)
par(mfrow=c(3,3))

for(i in ills){
  func6(i,i)
}


cor <- data.frame()
for(i in ills){
  data$bing=0
  data$bing[which(data$科室==i)]=1
  x<-data[c(29)]
  y<-data[c(8:24)]
  corr <- round(cor(x,y),2)
  cor <- rbind(cor,corr)
}

###问题三：定量分析从2013年到2017年过敏原检测结果的变化趋势，选取一个环境保护的指标
#         （譬如PM2.5浓度），从网上查找相关数据分析两者之间的相关性；


#求每月过敏数据的总值

#data$youbing[which(is.na(data$youbing))]=0
dt <- ddply(data,.(Year,month),summarize,
            全部=sum(!is.na(Year),na.rm = T),
            树组合=sum(树组合>0,na.rm = T),
            普通豚草=sum(普通豚草>0,na.rm = T),
            艾蒿=sum(艾蒿>0,na.rm = T),
            尘螨组合=sum(尘螨组合>0,na.rm = T),
            屋尘=sum(屋尘>0,na.rm = T),
            猫毛=sum(猫毛>0,na.rm = T),
            狗上皮=sum(狗上皮>0,na.rm = T),
            蟑螂=sum(蟑螂>0,na.rm = T),
            霉菌组合=sum(霉菌组合>0,na.rm = T),
            律草=sum(律草>0,na.rm = T),
            鸡蛋白=sum(鸡蛋白>0,na.rm = T),
            海鱼组合=sum(海鱼组合>0,na.rm = T),
            虾=sum(虾>0,na.rm = T),
            蟹=sum(蟹>0,na.rm = T),
            牛奶=sum(牛奶>0,na.rm = T),
            花生=sum(花生>0,na.rm = T),
            黄豆=sum(黄豆>0,na.rm = T)
)
t <- c(1:nrow(dt))
dt$t <- t
dt$label <- paste(dt$Year,dt$month,sep="")

dt1 <- ddply(data,.(Year),summarize,
            全部=sum(!is.na(Year),na.rm = T),
            树组合=sum(树组合>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            普通豚草=sum(普通豚草>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            艾蒿=sum(艾蒿>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            尘螨组合=sum(尘螨组合>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            屋尘=sum(屋尘>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            猫毛=sum(猫毛>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            狗上皮=sum(狗上皮>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            蟑螂=sum(蟑螂>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            霉菌组合=sum(霉菌组合>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            律草=sum(律草>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            鸡蛋白=sum(鸡蛋白>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            海鱼组合=sum(海鱼组合>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            虾=sum(虾>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            蟹=sum(蟹>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            牛奶=sum(牛奶>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            花生=sum(花生>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            黄豆=sum(黄豆>0,na.rm = T)/sum(!is.na(Year),na.rm = T)
)
#t <- c(1:nrow(dt1))
#dt1$t <- t
#dt1$label <- paste(dt1$Year,dt1$month,sep="")
dtt <- dt1[c(3:19)]

func <- function(abc,ylims){
  shuju <- dtt
  cols <- terrain.colors(length(abc))
  shuju <- as.matrix(shuju)
  barplot(shuju,  offset = 0, axis.lty = 1, ylim =c(0,ylims), 
          names.arg = colnames(shuju), col = cols, beside = TRUE) 
  box() 
  legend("topright", legend = abc, fill = cols,ncol=2,cex=1) 
  title(main = "过敏性疾病与季节", xlab = "疾病类型", ylab = "人数") 
  
}
abc <- c(2013,2014,2015,2016,2017)
func(abc,0.5)


#求每月环保数据均值

huanbao <- read.csv("徐州空气质量日数据.csv")

dt1 <- ddply(huanbao,.(Year,month),summarize,
             AQI指数=mean(AQI指数,na.rm = T),
             PM2.5指数=mean(PM2.5指数,na.rm = T),
             PM10指数=mean(PM10指数,na.rm = T),
             SO2浓度=mean(SO2浓度,na.rm = T),
             NO2浓度=mean(NO2浓度,na.rm = T),
             CO浓度=mean(CO浓度,na.rm = T),
             O3浓度=mean(O3浓度,na.rm = T)
)

data2 <- merge(dt,dt1,level=0)

##时间序列图
a <- seq(0,60,12)
huanjing <- c(4:13)
shiwu <- c(14:20)
func7 <- function(abc,main){
  tt <- data.frame(dt1[c(abc[1],21,22)])
  colnames(tt)=c("y","x","label")
  cols <- rainbow(length(abc))
  plot(tt$x,tt$y,type="l",main=main,xlab = "",col=cols[1],pch=1,
       ylab = "人数",ylim=c(0,2),xaxt="n",lty=1)
  label=colnames(dt1)[abc]
  for(j in 2:length(abc)){
    tt <- data.frame(dt1[c(abc[j],21,22)])
    colnames(tt)=c("y","x","label")
    lines(tt$x,tt$y,type="l",pch=j,lty=j,col=cols[j])
  }
  axis(1,labels=tt$label[a],at=tt$x[a])
  legend("topright","topright", legend = label, lty = c(1:length(abc)),
         ncol=3,cex=1,col=cols)
}
func7(huanjing,"环境")
func7(shiwu,"食物")
opar<-par(no.readonly=T)
op <- par(mar = rep(2, 4))   
par(fig=c(0,0.2,0,0.2))
par(mfrow=c(1,1))
n <- c()
for(i in 2013:2017){
  n <- c(n,sum(data$Year==i))
}

for(i in 3:20){
  func7(i)
}

shiwu <- c()

par(opar)

#进行相关性分析

huanbao <- read.csv("广州.csv")
huanbao_dt <- merge(dt,huanbao,level=1)
huanbao_dt <- huanbao_dt[order(huanbao_dt$t),]
x<-huanbao_dt[c(4:20)]
y<-huanbao_dt[c(22:28)]
corr <- round(cor(x,y),2)
write.csv(corr,"corrcehng.csv")

plot(huanbao_dt$t,huanbao_dt$PM2.5,type="l",col="blue",ylim = c(0,140))
lines(huanbao_dt$t,huanbao_dt$普通豚草,type="l",col="red")


ml <- lm(普通豚草~t+PM2.5,data=huanbao_dt)



dt1 <- ddply(data,.(Year,month,科室),summarize,
             n_keshi=sum(!is.na(Year))
)
dt1 <- dt1[which(dt1$科室 %in% ills),]
data_keshi <- merge(dt1,huanbao,level=1)
res <- data.frame()
cor <- data.frame()
for(i in ills){
  lmm <- data_keshi[which(data_keshi$科室==i),]
  #ml <- lm(n_keshi~PM2.5,data=lmm)
  #re <- summary(ml)$coefficients
  #res <- rbind(res,re)
  x<-lmm[c(4)]
  y<-lmm[c(5:11)]
  corr <- round(cor(x,y),2)
  cor <- rbind(cor,corr)
}

row.names(cor)=ills














