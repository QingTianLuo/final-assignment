w<-read.csv("数据.csv")
head(w)
###描述性分析
names<-w[,1]
w1<-w[,-1]
colnames(w1)<-c(paste("x",1:9,sep=""),"y")
head(w1)
Mean=sapply(w1,mean)							
Min=sapply(w1,min)							
Median=sapply(w1,median)
Max=sapply(w1,max)
SD=sapply(w1,sd)
round(cbind(Mean,Min,Median,Max,SD),3)	
write.csv(round(cbind(Mean,Min,Median,Max,SD),3),"describe.csv")
##折线图
par(mfrow=c(2,2))
plot(w1[,1],ylab = "普通高校在校学生人数（万人）",xlab ="地区",main = "普通高校在校学生人数",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
plot(w1[,2],ylab = "人均地区生产总值（元）",xlab ="地区",main = "人均地区生产总值",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
plot(w1[,3],ylab = "规模以上工业企业数（个）",xlab ="地区",main = "规模以上工业企业数",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
plot(w1[,4],ylab = "民用汽车拥有量（万）",xlab ="地区",main = "民用汽车拥有量",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
par(mfrow=c(2,2))
plot(w1[,5],ylab = "商品房销售额（亿）",xlab ="地区",main = "商品房销售额",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
plot(w1[,6],ylab = "国内旅游收入（亿）",xlab ="地区",main = "国内旅游收入",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,,cex.axis=0.8)
plot(w1[,7],ylab = "出口额（亿）",xlab ="地区",main = "出口额",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,,cex.axis=0.8)
plot(w1[,8],ylab = "专利授权（个）",xlab ="地区",main = "专利授权",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
par(mfrow=c(2,2))
plot(w1[,9],ylab = "人均可支配收入（元）",xlab ="地区",main = "人均可支配收入",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,,cex.axis=0.8)
plot(w1[,10],ylab = "人才总量（万人）",xlab ="地区",main = "人才总量",xaxt="n",type="l")
axis(1,at=c(1:11),labels=names,cex.axis=0.8)
##数据标准化箱线图
par(mfrow=c(1,1))
boxplot(scale(w1),main = "数据标准化后箱型图")
g<-scale(w1)[1:11,]
g<-as.data.frame(g)
library(GGally)
ggpairs(g)
##
round(cor(w1),3)
write.csv(round(cor(w1),3),"2.csv")
##
###lasso回归建模
install.packages("lars")
library(lars)
w2<-scale(w1)
head(w2)
X<-as.matrix(w2[,1:9])
Y<-as.matrix(w2[,10])
laa<-lars(X,Y)
summary(laa)
laa
plot(laa)
###lasso选择出的变量做线性回归
w2<-as.data.frame(w2)
lm1=lm(y~x1+x2+x3+x4+x5+x6+x7+x9,data=w2)
summary(lm1)
lm2=lm(y~x2+x3+x6+x7+x9,data = w2)
summary(lm2)
install.packages("flextable")
library(flextable)
as_flextable(lm2)
##多重共线性检测
install.packages(car)
library(car)
library(carData)
vif(lm2)
###主成分分析
install.packages("psych")
library(psych)
cortest.bartlett(cor(w2),n=nrow(w2))
pca<-princomp(covmat=cor(w2))
summary(pca,loadings=T)
write.csv(pca$loadings[1:9,],"载荷.csv")
screeplot(pca,type="l",main="PCA")###画主成分的碎石图
abline(h=1,lwd=2,col=2)##碎石图中添加y=1直线
#主成分综合评价
pca<-princomp(w1,cor=T,scores=T)
b<-pca$scores###计算主成分得分
c<-as.data.frame(b[ ,c(1,2)])
d<-c[,1]*0.751+c[,2]*0.106
c$scores = d
c$排名<-rank(-d)
rownames(c)<-names
names(c)<-c("FA1","FA2","得分","排名")
write.csv(round(c,3),"排名.csv")
##聚类
rownames(w1)<-names
stars(w1,full = T,key.loc=c(7,2))
b<-scale(w1)
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
