
rm(list = ls())

library("dummies")
library("AER")
library("plotly")
library('RColorBrewer')
library("rgl")
library("data.table")
library("mlogit")
library("gmnl")
library("data.table")

### read data 
data <- read.csv('kiwi_bubbles_P2.csv')
demo <- read.csv('demo_P2.csv')
### data cleaning
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

### Define variables
id=data$id
purchaseKB=1*(data$choice=="KB") #Define an indicator of purchase
priceKB=data$price.KB
priceKR=data$price.KR
priceMB=data$price.MB

################################ Question 3: Logit model without segmantation

### Convert data to mlogit.data form. 
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")

#Run MLE.
mle= gmnl(choice ~  price, data = mlogitdata)
summary(mle)

coef=mle$coefficients

beta1 <- coef[4]
beta0KB <- coef[1]
beta0KR <- coef[2]
beta0MB <- coef[3]

### Simulate choice probability
### Write choice probability as a function
demand1 = function(priceKB,priceKR,priceMB,beta0KB,beta0KR,beta0MB,beta1){
  prob1=exp(beta0KB+beta1*priceKB)/(1+exp(beta0KB+beta1*priceKB)+exp(beta0KR+beta1*priceKR)+exp(beta0MB+beta1*priceMB))
  return(prob1)
}

demand2 = function(priceKB,priceKR,priceMB,beta0KB,beta0KR,beta0MB,beta1){
  prob2=exp(beta0KR+beta1*priceKR)/(1+exp(beta0KB+beta1*priceKB)+exp(beta0KR+beta1*priceKR)+exp(beta0MB+beta1*priceMB))
  return(prob2)
}

demand3 = function(priceKB,priceKR,priceMB,beta0KB,beta0KR,beta0MB,beta1){
  prob3=exp(beta0MB+beta1*priceMB)/(1+exp(beta0KB+beta1*priceKB)+exp(beta0KR+beta1*priceKR)+exp(beta0MB+beta1*priceMB))
  return(prob3)
}


### Own-price elasticity of KB/KR/MB
average.KB <- mean(priceKB) # 1.381332  
average.KR <- mean(priceKR) # 1.378087
average.MB <- mean(priceMB) # 1.345585

elac.KB <- -beta1*average.KB*(1-demand1(average.KB,average.KR,average.MB,beta0KB,beta0KR,beta0MB,beta1))
elac.KB
elac.KR <- -beta1*average.KR*(1-demand2(average.KB,average.KR,average.MB,beta0KB,beta0KR,beta0MB,beta1))
elac.KR
elac.MB <- -beta1*average.MB*(1-demand3(average.KB,average.KR,average.MB,beta0KB,beta0KR,beta0MB,beta1))
elac.MB

### Cross-price elacticity
elac.KBKR <- -beta1*average.KR*(demand2(average.KB,average.KR,average.MB,beta0KB,beta0KR,beta0MB,beta1))
elac.KBKR
elac.KBMB <- -beta1*average.MB*(demand3(average.KB,average.KR,average.MB,beta0KB,beta0KR,beta0MB,beta1))
elac.KBMB
elac.KRMB <- -beta1*average.MB*(demand3(average.KB,average.KR,average.MB,beta0KB,beta0KR,beta0MB,beta1))
elac.KRMB
elac.KRKB <- -beta1*average.KB*(demand1(average.KB,average.KR,average.MB,beta0KB,beta0KR,beta0MB,beta1))
elac.KRKB
elac.MBKB <- -beta1*average.KB*(demand1(average.KB,average.KR,average.MB,beta0KB,beta0KR,beta0MB,beta1))
elac.MBKB
elac.MBKR <- -beta1*average.KR*(demand2(average.KB,average.KR,average.MB,beta0KB,beta0KR,beta0MB,beta1))
elac.MBKR

### Finding optimal price 
# Unit cost
uc=0.5;

# Optimal price of KR and KB
profit=function(priceKB,priceKR,priceMB,beta0KB,beta0KR,beta0MB,beta1){
  profitKB=1000*demand1(priceKB,priceKR,priceMB,beta0KB,beta0KR,beta0MB,beta1)*(priceKB-0.5)
  profitKR=1000*demand2(priceKB,priceKR,priceMB,beta0KB,beta0KR,beta0MB,beta1)*(priceKR-0.5)
  return(cbind(profitKB,profitKR))
}
# Choose space of prices to search for the optimal price over
pricespace=seq(0,3,0.01)
# Because we search over two dimensions, create complete combination of the two prices
pricespace2=expand.grid(pricespace,pricespace)
profitmat=matrix(0L,nrow(pricespace2),1)
for (i in 1:nrow(pricespace2)){
  profitmat[i]=sum(profit(pricespace2[i,1],pricespace2[i,2],1.43,beta0KB,beta0KR,beta0MB,beta1))  
}

xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit")
p=plot_ly(x=pricespace2[,1],y=pricespace2[,2],z=as.numeric(profitmat),
          type="scatter3d",mode="markers",
          marker = list(color = as.numeric(profitmat), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
  layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
  config(mathjax = 'cdn')
p
pricespace2[profitmat==max(profitmat)]
max(profitmat)
# Maximized price for KB and MB are the same at 1.16, the profit is 393.4082.




################################ Question 4: Logit model with segmantation

### We need the judge the best number of segments
require("cluster")
require("fpc")
require("factoextra")
require("gridExtra")
library(cluster)
library(fpc)
library(factoextra)
library(gridExtra)

set.seed(0)
fviz_nbclust(demo[,2:18],kmeans,method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")
fviz_nbclust(demo[,2:18],kmeans,method ="silhouette",iter.max=100,nstart=1000,k.max=15)

# Number of individuals
N = 359

# Clustering
demo_cluster = kmeans(x=demo[, 2:18], centers = 4, nstart = 1000)
# Combine cluster identity into the raw data
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
data = merge(data, cluster_id, by = "id", all.x = T)

# For those who don't fit in any cluster, group them into one additional cluster
data$cluster[is.na(data$cluster)] = 5

# Segment share
seg.share = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N	

# Store the coefficients 
coef.est = data.frame(segment = 1:5, intercept.KB = NA, intercept.KR = NA, 
                      intercept.MB = NA, price.coef = NA) 

# Write a for-loop. 
for (seg in 1:5) {
  # During each loop, pick subset of data of consumers from each segment.
  data.sub = subset(data, cluster == seg)
  
  # Using that data, the rest remains the same.
  mlogitdata=mlogit.data(data.sub,id="id",varying=4:7,choice="choice",shape="wide")
  
  # Run MLE.
  mle= gmnl(choice ~  price, data = mlogitdata)
  mle
  # Store the outcome in the coef.est matrix.
  coef.est[seg, 2:5] = mle$coefficients
}

# Demand function 
demand4=function(priceKB,priceKR,priceMB,para){
  prob4=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(prob4)
}

demand5=function(priceKB,priceKR,priceMB,para){
  prob5=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(prob5)
}

demand6=function(priceKB,priceKR,priceMB,para){
  prob6=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(prob6)
}

# Aggregate choice probability for KB
agg_choice1=function(priceKB,priceKR,priceMB) {
  
  agg_choice1=seg.share[1]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))
    return(agg_choice1)
}

# Aggregate choice probability for KR
agg_choice2=function(priceKB,priceKR,priceMB) {
  
  agg_choice2=seg.share[1]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))
  return(agg_choice2)
}

# Aggregate choice probability for MB
agg_choice3=function(priceKB,priceKR,priceMB) {
  
  agg_choice3=seg.share[1]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))
  return(agg_choice3)
}

# Remaining part used for own price elasticity of KB
att1 <- function(priceKB,priceKR,priceMB) {
  
  att1=seg.share[1]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))*as.numeric(coef.est[1,5])*(1-demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))) +
    seg.share[2]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))*as.numeric(coef.est[2,5])*(1-demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5])))+
    seg.share[3]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))*as.numeric(coef.est[3,5])*(1-demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5])))+
    seg.share[4]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))*as.numeric(coef.est[4,5])*(1-demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5])))+
    seg.share[5]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))*as.numeric(coef.est[5,5])*(1-demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5])))
  return(att1)
}

# Remaing part used for own price elasticity of KR
att2 <- function(priceKB,priceKR,priceMB) {
  
  att2=seg.share[1]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))*as.numeric(coef.est[1,5])*(1-demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))) +
    seg.share[2]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))*as.numeric(coef.est[2,5])*(1-demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5])))+
    seg.share[3]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))*as.numeric(coef.est[3,5])*(1-demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5])))+
    seg.share[4]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))*as.numeric(coef.est[4,5])*(1-demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5])))+
    seg.share[5]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))*as.numeric(coef.est[5,5])*(1-demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5])))
  return(att2)
}

# Remaing part used for own price elasticity of MB
att3 <- function(priceKB,priceKR,priceMB) {
  
  att3=seg.share[1]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))*as.numeric(coef.est[1,5])*(1-demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))) +
    seg.share[2]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))*as.numeric(coef.est[2,5])*(1-demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5])))+
    seg.share[3]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))*as.numeric(coef.est[3,5])*(1-demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5])))+
    seg.share[4]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))*as.numeric(coef.est[4,5])*(1-demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5])))+
    seg.share[5]*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))*as.numeric(coef.est[5,5])*(1-demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5])))
  return(att3)
}


### Own-price elasticity of KB/KR/MB
elac.KB1 <- -average.KB/agg_choice1(average.KB,average.KR,average.MB)*att1(average.KB,average.KR,average.MB)
elac.KB1
elac.KR1 <- -average.KR/agg_choice2(average.KB,average.KR,average.MB)*att2(average.KB,average.KR,average.MB)
elac.KR1
elac.MB1 <- -average.MB/agg_choice3(average.KB,average.KR,average.MB)*att3(average.KB,average.KR,average.MB)
elac.MB1

### Percentile difference
diff.KB <- elac.KB - elac.KB1
diff.KR <- elac.KR - elac.KR1 
diff.MB <- elac.MB - elac.MB1 

### Cross-price elacticity
# Remaining part used for cross price elasticity of KBKR
att.KBKR <- function(priceKB,priceKR,priceMB) {
  
  att.KBKR=seg.share[1]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))*as.numeric(coef.est[1,5])*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5])) +
    seg.share[2]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))*as.numeric(coef.est[2,5])*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))*as.numeric(coef.est[3,5])*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))*as.numeric(coef.est[4,5])*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))*as.numeric(coef.est[5,5])*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))
  return(att.KBKR)
}

# Remaing part used for own price elasticity of KBMB
att.KBMB <- function(priceKB,priceKR,priceMB) {
  
  att.KBMB=seg.share[1]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))*as.numeric(coef.est[1,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5])) +
    seg.share[2]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))*as.numeric(coef.est[2,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))*as.numeric(coef.est[3,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))*as.numeric(coef.est[4,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand4(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))*as.numeric(coef.est[5,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))
  return(att.KBMB)
}

# Remaing part used for own price elasticity of KRMB
att.KRMB <- function(priceKB,priceKR,priceMB) {
  
  att.KRMB=seg.share[1]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5]))*as.numeric(coef.est[1,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[1,2:5])) +
    seg.share[2]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))*as.numeric(coef.est[2,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))*as.numeric(coef.est[3,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))*as.numeric(coef.est[4,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand5(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))*as.numeric(coef.est[5,5])*demand6(priceKB,priceKR,priceMB,as.numeric(coef.est[5,2:5]))
  return(att.KRMB)
}

elac.KBKR1 <- -average.KR/agg_choice1(average.KB,average.KR,average.MB)*att.KBKR(average.KB,average.KR,average.MB)
elac.KBKR1
elac.KBMB1 <- -average.MB/agg_choice1(average.KB,average.KR,average.MB)*att.KBMB(average.KB,average.KR,average.MB)
elac.KBMB1
elac.KRMB1 <- -average.MB/agg_choice2(average.KB,average.KR,average.MB)*att.KRMB(average.KB,average.KR,average.MB)
elac.KRMB1
elac.KRKB1 <- -average.KB/agg_choice2(average.KB,average.KR,average.MB)*att.KBKR(average.KB,average.KR,average.MB)
elac.KRKB1
elac.MBKB1 <- -average.KB/agg_choice3(average.KB,average.KR,average.MB)*att.KBMB(average.KB,average.KR,average.MB)
elac.MBKB1
elac.MBKR1 <- -average.KR/agg_choice3(average.KB,average.KR,average.MB)*att.KRMB(average.KB,average.KR,average.MB)
elac.MBKR1

# Percentile difference
diff.KBKR <- elac.KBKR - elac.KBKR1
diff.KBMB <- elac.KBMB - elac.KBMB1
diff.KRMB <- elac.KRMB - elac.KRMB1
diff.KRKB <- elac.KRKB - elac.KRKB1
diff.MBKB <- elac.MBKB - elac.MBKB1
diff.MBKR <- elac.MBKR - elac.MBKR1


### Profit Maximize price

### without KB
uc=0.5
demand.noKB=function(priceKR,priceMB,para){
  prob.noKB=exp(para[2]+para[4]*priceKR)/(1+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(prob.noKB)
}

agg_choice.noKB=function(priceKR,priceMB) {
  
  agg_choice.noKB=seg.share[1]*demand.noKB(priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demand.noKB(priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand.noKB(priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand.noKB(priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand.noKB(priceKR,priceMB,as.numeric(coef.est[5,2:5]))
  return(agg_choice.noKB)
}

profit.noKB=1000*agg_choice.noKB(pricespace,1.43)*(pricespace-0.5)
plot(pricespace,profit.noKB,
     type='l',xlab='Prices',ylab='Profit.noKB',col="blue",lwd=2,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
pricespace[profit.noKB==max(profit.noKB)]
max(profit.noKB)
## KR max price without KB is 1.06, and the max profit is 295.4891.

# Considering the profit of MB.
demand.MB=function(priceKR,priceMB,para){
  prob.MB=exp(para[3]+para[4]*priceMB)/(1+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(prob.MB)
}

agg_choice.MB=function(priceKR,priceMB) {
  
  agg_choice.MB=seg.share[1]*demand.MB(priceKR,priceMB,as.numeric(coef.est[1,2:5]))+
    seg.share[2]*demand.MB(priceKR,priceMB,as.numeric(coef.est[2,2:5]))+
    seg.share[3]*demand.MB(priceKR,priceMB,as.numeric(coef.est[3,2:5]))+
    seg.share[4]*demand.MB(priceKR,priceMB,as.numeric(coef.est[4,2:5]))+
    seg.share[5]*demand.MB(priceKR,priceMB,as.numeric(coef.est[5,2:5]))
  return(agg_choice.MB)
}
profit.MB.noKB <- 1000*agg_choice.MB(pricespace[profit.noKB==max(profit.noKB)],1.43)*(1.43-0.5)
profit.MB.noKB
# Profit of MB without lauching KB is 106.9014.

### with KB

# Optimal price of KR and KB
profit.KB=function(priceKB,priceKR,priceMB){
  profitKB=1000*agg_choice1(priceKB,priceKR,priceMB)*(priceKB-0.5)
  profitKR=1000*agg_choice2(priceKB,priceKR,priceMB)*(priceKR-0.5)
  return(cbind(profitKB,profitKR))
}

pricespace=seq(0,3,0.01)

# Because we search over two dimensions, create complete combination of the two prices
pricespace2=expand.grid(pricespace,pricespace)
profitmat.KB=matrix(0L,nrow(pricespace2),1)
for (i in 1:nrow(pricespace2)){
  profitmat.KB[i]=sum(profit.KB(pricespace2[i,1],pricespace2[i,2],1.43))  
}

xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit with KB")
p.KB=plot_ly(x=pricespace2[,1],y=pricespace2[,2],z=as.numeric(profitmat.KB),
          type="scatter3d",mode="markers",
          marker = list(color = as.numeric(profitmat.KB), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
  layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
  config(mathjax = 'cdn')
p.KB
pricespace2[profitmat.KB==max(profitmat.KB)]
max(profitmat.KB) # 393.7793
# the max profit price for KB is 1.16 and the max profit price for KR is 1.16, and the max profit of KB and KR is 394.9407.

# Considering the profit of MB.
profit.MB.KB <- 1000*agg_choice3(1.16,1.16,1.43)*(1.43-uc)
profit.MB.KB   # 92.00578 
### Profit of MB when lauching the KB is smaller then the profit without lauching the KB.




################################ Question 5: Strategic Responce

# Now we consider Mange's new price strategy according to our's
###################### Here we consider without segmentation scenario

# Step 1
pricespace = seq(0,3,0.01)
profit1 = 1000*demand3(1.16,1.16,pricespace,beta0KB,beta0KR,beta0MB,beta1)*(pricespace-uc)
pricespace[profit1==max(profit1)]
max(profit1)
# Mange will decrease its price to 0.95.
# Step 2
profit2 = 1000*demand1(pricespace,pricespace,0.95,beta0KB,beta0KR,beta0MB,beta1)*(pricespace-uc)+1000*demand2(pricespace,pricespace,0.95,beta0KB,beta0KR,beta0MB,beta1)*(pricespace-uc)
pricespace[profit2==max(profit2)]   #1.04
max(profit2)
# Repeat step 1 or step 2
profit3 = 1000*demand3(1.04,1.04,pricespace,beta0KB,beta0KR,beta0MB,beta1)*(pricespace-uc)
pricespace[profit3==max(profit3)]
max(profit3)   # 0.91

profit4 = 1000*demand1(pricespace,pricespace,0.91,beta0KB,beta0KR,beta0MB,beta1)*(pricespace-uc)+1000*demand2(pricespace,pricespace,0.91,beta0KB,beta0KR,beta0MB,beta1)*(pricespace-uc)
pricespace[profit4==max(profit4)]   #1.03
max(profit4)

profit5 = 1000*demand3(1.03,1.03,pricespace,beta0KB,beta0KR,beta0MB,beta1)*(pricespace-uc)
pricespace[profit5==max(profit5)]
max(profit5)      #0.91

# So the optimal price for KB and KR are both 1.03, the optimal price for MB is 0.91.

###################### Here we consider segmentation scenario
# Step 1
profit11 <- 1000*agg_choice3(1.16,1.16,pricespace)*(pricespace-uc)
pricespace[profit11==max(profit11)]   #0.95
max(profit11)  #178.5712

# Step 2
pricespace=seq(0,3,0.01)
# Because we search over two dimensions, create complete combination of the two prices
pricespace2=expand.grid(pricespace,pricespace)
profit22=matrix(0L,nrow(pricespace2),1)
for (i in 1:nrow(pricespace2)){
  profit22[i]=sum(profit.KB(pricespace2[i,1],pricespace2[i,2],0.95))  
}

xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit22")
p22=plot_ly(x=pricespace2[,1],y=pricespace2[,2],z=as.numeric(profit22),
             type="scatter3d",mode="markers",
             marker = list(color = as.numeric(profit22), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
  layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
  config(mathjax = 'cdn')
p22
pricespace2[profit22==max(profit22)]  #KB = 1.04, KR = 1.05
max(profit22) #275.6912

# Repeat step 1 or step 2
profit33 <- 1000*agg_choice3(1.04,1.05,pricespace)*(pricespace-uc)
pricespace[profit33==max(profit33)]   #0.92
max(profit33)  #147.9704


pricespace=seq(0,3,0.01)
# Because we search over two dimensions, create complete combination of the two prices
pricespace2=expand.grid(pricespace,pricespace)
profit44=matrix(0L,nrow(pricespace2),1)
for (i in 1:nrow(pricespace2)){
  profit44[i]=sum(profit.KB(pricespace2[i,1],pricespace2[i,2],0.92))  
}

xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit44")
p44=plot_ly(x=pricespace2[,1],y=pricespace2[,2],z=as.numeric(profit44),
            type="scatter3d",mode="markers",
            marker = list(color = as.numeric(profit44), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
               layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
               config(mathjax = 'cdn')
p44
pricespace2[profit44==max(profit44)]  # KB = 1.03, KR = 1.04
max(profit44) #  265.5745

profit55 <- 1000*agg_choice3(1.03,1.04,pricespace)*(pricespace-uc)
pricespace[profit55==max(profit55)]   #0.91
max(profit55)   # 145.3087


pricespace=seq(0,3,0.01)
# Because we search over two dimensions, create complete combination of the two prices
pricespace2=expand.grid(pricespace,pricespace)
profit66=matrix(0L,nrow(pricespace2),1)
for (i in 1:nrow(pricespace2)){
  profit66[i]=sum(profit.KB(pricespace2[i,1],pricespace2[i,2],0.91))  
}

xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit66")
p66=plot_ly(x=pricespace2[,1],y=pricespace2[,2],z=as.numeric(profit66),
            type="scatter3d",mode="markers",
            marker = list(color = as.numeric(profit66), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
  layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
  config(mathjax = 'cdn')
p66
pricespace2[profit66==max(profit66)]  # KB = 1.03, KR = 1.03
max(profit66)        #262.1776


profit77 <- 1000*agg_choice3(1.03,1.03,pricespace)*(pricespace-uc)
pricespace[profit77==max(profit77)]   #0.91
max(profit77)  #143.9374

# So the optimal price for KB and KR are both 1.03, the optimal price for MB is 0.91. The same as the scenario without segmentation.

