###主成分分析
x<-read.csv("数据数据.csv")
x
rowname = x[,1]
colnames(x)<-c(paste("X",0:10,sep=""))
x<-x[,-11]
x<-x[,-1]
x
round((a<-cor(x)),3)
write.csv(a,"相关系数改.csv")
install.packages("psych")
library(psych)
cortest.bartlett(a,n=nrow(x))
pca<-princomp(covmat=a)
summary(pca,loadings=T)
write.csv(pca$loadings[1:9,],"载荷.csv")
screeplot(pca,type="l",main="PCA")###画主成分的碎石图
abline(h=1,lwd=2,col=2)##碎石图中添加y=1直线
#主成分综合评价
pca<-princomp(x,cor=T,scores=T)
b<-pca$scores###计算主成分得分
c<-as.data.frame(b[ ,c(1,2,3)])
#作图
# 导入包
library(rgl)
Sys.setenv(LANGUAGE = "en") #显示英文报错信息
options(stringsAsFactors = FALSE) #禁止chr转成factor
plot3d(c[,1:3], # 取前三个主成分
       xlab="FA1", ylab="FA2", zlab="FA3", 
       col=c(1:11), # 按groups填充颜色
       type="s", # 画球，'p' for points, 's' for spheres, 'l' for lines, 'h' for line segments 
       size=1, #球的大小
       lwd=5, box=T,cex=100)

rgl.snapshot("PCA01.png")
#
d<-c[,1]*0.644+c[,2]*0.133+c[,3]*0.116
c$scores = d
c$排名<-rank(-d)
rownames(c)<-rowname
write.csv(round(c,3),"排名.csv")
##聚类分析
x<-read.csv("数据数据.csv")
x
colnames(x)<-c(paste("X",0:10,sep=""))
b<-x
rownames(b)<-b[,1]
b<-b[,-1]
f<-b
b<-scale(b)
par(mfrow=c(1,3))
hc1.1<-hclust(dist(b),method="single")
plot(hc1.1,hang=-1)
hc2.1<-hclust(dist(b),method="complete")
plot(hc2.1,hang=-1)
hc3.1<-hclust(dist(b),method="ave")
plot(hc3.1,hang=-1)
method1=c("single","complete","average")
cc1<-numeric(0)
for (m in method1){
dc<-cophenetic(hclust(dist(b),m))
cc1[m]<-cor(dist(b),dc)
}
cc1
plot(hc1.1,hang=-1)
rect.hclust(hc1.1,k=3)
plot(hc2.1,hang=-1)
rect.hclust(hc2.1,k=3)
plot(hc3.1,hang=-1)
rect.hclust(hc3.1,k=3)
####描述性统计图
x<-read.csv("数据数据.csv")
x
colnames(x)<-c(paste("X",0:10,sep=""))
names = x[,1]
rownames(x)<-x[,1]
x<-x[,-1]
x
Mean=sapply(x,mean)							
Min=sapply(x,min)							
Median=sapply(x,median)
Max=sapply(x,max)
SD=sapply(x,sd)
round(cbind(Mean,Min,Median,Max,SD),3)	
write.csv(round(cbind(Mean,Min,Median,Max,SD),3),"1234.csv")

par(mfrow=c(2,2))
plot(x[,1],ylab = "地区生产总值",xlab ="地区",main = "地区生产总值",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
plot(x[,2],ylab = "工业企业数",xlab ="地区",main = "工业企业数",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
plot(x[,3],ylab = "实际利用外资",xlab ="地区",main = "实际利用外资",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
plot(x[,4],ylab = "进出口总额",xlab ="地区",main = "进出口总额",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
par(mfrow=c(2,2))
plot(x[,5],ylab = "财政科技支出",xlab ="地区",main = "财政科技支出",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
plot(x[,6],ylab = "普通高等学校数",xlab ="地区",main = "普通高等学校数",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,,cex.axis=0.8)
plot(x[,7],ylab = "卫生机构数",xlab ="地区",main = "卫生机构数",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,,cex.axis=0.8)
plot(x[,8],ylab = "学前教育学校数",xlab ="地区",main = "学前教育学校数",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
par(mfrow=c(2,2))
plot(x[,9],ylab = "人均可支配收入",xlab ="地区",main = "人均可支配收入",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,,cex.axis=0.8)
plot(x[,10],ylab = "人才总量",xlab ="地区",main = "人才总量",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
par(mfrow=c(1,1))
boxplot(scale(x),main = "数据标准化后箱型图")
g<-scale(x)
g<-g[1:11,]
g<-as.data.frame(g)
library(GGally)
ggpairs(g)
stars(g,full = T,key.loc=c(7,2))

