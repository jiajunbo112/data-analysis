library(ggplot2)
#library(dplyr)
library(plyr)
library(cowplot)

setwd("C:\\Users\\Administrator\\Desktop\\A��\\A��")
data <- read.csv("2018�����ͳ����Ϣ��.csv")
data <- data[c(1:ncol(data)-1)]


###��������

#��ȡ��ֵ
abstract_num <- function(s){
  s=as.character(s)         #ת�����ַ���
  s=substr(s,1,1)     #��ȡ��һ����ֵ�ַ�
  i=as.numeric(s)    #����ȡ���ַ�ת������ֵ
  return(i)
}

data0=data[c(1:7)]
data1=sapply(data[c(8:ncol(data))],abstract_num)
data=cbind(data0,data1)
####
#����һ���Է��������Լ�����ʱ�䣬���ڣ��Ա�����ֱ���ʲô���Ĺ�ϵ��

keshi <- c("����","���Ǻ���","��ר������","����","�ǿ�","������","�Ҽ�ֺ","����ҽѧ��",
           "��ǻ��","�����","�ڷ��ڿ�","Ƥ����","���ڿ�","�����","�񾭿�","���ڿ�",
            "����","�������","���ÿ�","������","���ڿ�","ѪҺ��", "�ۿ�","��ҽ",
           "��ҽ��","������" )

ills <- c("����","���Ǻ���","������","�ڷ��ڿ�","Ƥ����",
          "���ڿ�","���ڿ�","�ۿ�")

#-------------����---------------------

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
      dt <- data[which(data$age1==i & data$����==j),]
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
  title(main = "�����Լ���������", xlab = "��������", ylab = "����") 
  
}

jijie <- c("Spring","Summer","Autumn","Winter")
func(jijie,320)


#--------�Ա�---------------
xingbie <- c("Ů","��")
func(xingbie,550)

#---------����-------------
age <- c(0,10,20,30,40,50,60,70,80,90,100)
label <- c("0-9��","10-19��","20-29��","30-39��","40-49��","50-59��","60-69��","70-100��")
hist(data$����,main = "�����Լ���������", xlab = "����", ylab = "����",col = "yellow")
data$age1=0
for(i in 1:length(age)-1){
  data$age1[data$����>=age[i]]=age[i+1]
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
label <- c("0-9��","10-19��","20-29��","30-39��","40-49��","50-59��",
           "60-69��","70-79��","80-89��","90-99��" )
barplot(jiaquan,names.arg = label)

##----ʱ��----------------

time <- c(2013,2014,2015,2016,2017)
func(time)


##-----huiguifenxi----------------
data$Season=as.factor(data$Season)
data$�Ա�=as.factor(data$�Ա�)

r <- data.frame()
for(i in ills){
  data$y=0
  data$y[which(data$����==i)]=1
  model <- lm(y~�Ա�+age+Season+Year,data=data)
  model <- summary(model)
  r <- rbind(r,model$coefficients)
}

r <- round(r,3)
write.csv(r,"diyiwen.csv")

###���������ϵ�һ�ʵ����Ӱ�����أ�������ͬ���ͼ����Ĳ��˹���ԭ���
#          ���֮��Ĳ��죻

keshi <- data.frame(table(data$����))    #�۲첻ͬ���ͼ���������
write.csv(keshi,"keshi.csv")
#ѡȡ����������ʮ�ļ������ͽ��з���

ills <- c("����","���Ǻ���","������","�ڷ��ڿ�","Ƥ����",
          "���ڿ�","���ڿ�","�ۿ�")
func6 <- function(ill,main){
  dt <- data[which(data$����==ill),]
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
  data$bing[which(data$����==i)]=1
  x<-data[c(29)]
  y<-data[c(8:24)]
  corr <- round(cor(x,y),2)
  cor <- rbind(cor,corr)
}

###������������������2013�굽2017�����ԭ������ı仯���ƣ�ѡȡһ������������ָ��
#         ��Ʃ��PM2.5Ũ�ȣ��������ϲ���������ݷ�������֮�������ԣ�


#��ÿ�¹������ݵ���ֵ

#data$youbing[which(is.na(data$youbing))]=0
dt <- ddply(data,.(Year,month),summarize,
            ȫ��=sum(!is.na(Year),na.rm = T),
            �����=sum(�����>0,na.rm = T),
            ��ͨ���=sum(��ͨ���>0,na.rm = T),
            ����=sum(����>0,na.rm = T),
            �������=sum(�������>0,na.rm = T),
            �ݳ�=sum(�ݳ�>0,na.rm = T),
            èë=sum(èë>0,na.rm = T),
            ����Ƥ=sum(����Ƥ>0,na.rm = T),
            ���=sum(���>0,na.rm = T),
            ù�����=sum(ù�����>0,na.rm = T),
            �ɲ�=sum(�ɲ�>0,na.rm = T),
            ������=sum(������>0,na.rm = T),
            �������=sum(�������>0,na.rm = T),
            Ϻ=sum(Ϻ>0,na.rm = T),
            з=sum(з>0,na.rm = T),
            ţ��=sum(ţ��>0,na.rm = T),
            ����=sum(����>0,na.rm = T),
            �ƶ�=sum(�ƶ�>0,na.rm = T)
)
t <- c(1:nrow(dt))
dt$t <- t
dt$label <- paste(dt$Year,dt$month,sep="")

dt1 <- ddply(data,.(Year),summarize,
            ȫ��=sum(!is.na(Year),na.rm = T),
            �����=sum(�����>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            ��ͨ���=sum(��ͨ���>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            ����=sum(����>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            �������=sum(�������>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            �ݳ�=sum(�ݳ�>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            èë=sum(èë>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            ����Ƥ=sum(����Ƥ>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            ���=sum(���>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            ù�����=sum(ù�����>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            �ɲ�=sum(�ɲ�>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            ������=sum(������>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            �������=sum(�������>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            Ϻ=sum(Ϻ>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            з=sum(з>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            ţ��=sum(ţ��>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            ����=sum(����>0,na.rm = T)/sum(!is.na(Year),na.rm = T),
            �ƶ�=sum(�ƶ�>0,na.rm = T)/sum(!is.na(Year),na.rm = T)
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
  title(main = "�����Լ����뼾��", xlab = "��������", ylab = "����") 
  
}
abc <- c(2013,2014,2015,2016,2017)
func(abc,0.5)


#��ÿ�»������ݾ�ֵ

huanbao <- read.csv("���ݿ�������������.csv")

dt1 <- ddply(huanbao,.(Year,month),summarize,
             AQIָ��=mean(AQIָ��,na.rm = T),
             PM2.5ָ��=mean(PM2.5ָ��,na.rm = T),
             PM10ָ��=mean(PM10ָ��,na.rm = T),
             SO2Ũ��=mean(SO2Ũ��,na.rm = T),
             NO2Ũ��=mean(NO2Ũ��,na.rm = T),
             COŨ��=mean(COŨ��,na.rm = T),
             O3Ũ��=mean(O3Ũ��,na.rm = T)
)

data2 <- merge(dt,dt1,level=0)

##ʱ������ͼ
a <- seq(0,60,12)
huanjing <- c(4:13)
shiwu <- c(14:20)
func7 <- function(abc,main){
  tt <- data.frame(dt1[c(abc[1],21,22)])
  colnames(tt)=c("y","x","label")
  cols <- rainbow(length(abc))
  plot(tt$x,tt$y,type="l",main=main,xlab = "",col=cols[1],pch=1,
       ylab = "����",ylim=c(0,2),xaxt="n",lty=1)
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
func7(huanjing,"����")
func7(shiwu,"ʳ��")
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

#��������Է���

huanbao <- read.csv("����.csv")
huanbao_dt <- merge(dt,huanbao,level=1)
huanbao_dt <- huanbao_dt[order(huanbao_dt$t),]
x<-huanbao_dt[c(4:20)]
y<-huanbao_dt[c(22:28)]
corr <- round(cor(x,y),2)
write.csv(corr,"corrcehng.csv")

plot(huanbao_dt$t,huanbao_dt$PM2.5,type="l",col="blue",ylim = c(0,140))
lines(huanbao_dt$t,huanbao_dt$��ͨ���,type="l",col="red")


ml <- lm(��ͨ���~t+PM2.5,data=huanbao_dt)



dt1 <- ddply(data,.(Year,month,����),summarize,
             n_keshi=sum(!is.na(Year))
)
dt1 <- dt1[which(dt1$���� %in% ills),]
data_keshi <- merge(dt1,huanbao,level=1)
res <- data.frame()
cor <- data.frame()
for(i in ills){
  lmm <- data_keshi[which(data_keshi$����==i),]
  #ml <- lm(n_keshi~PM2.5,data=lmm)
  #re <- summary(ml)$coefficients
  #res <- rbind(res,re)
  x<-lmm[c(4)]
  y<-lmm[c(5:11)]
  corr <- round(cor(x,y),2)
  cor <- rbind(cor,corr)
}

row.names(cor)=ills













