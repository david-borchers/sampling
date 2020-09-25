
# Caribou
# =========
count = c(1,50,21,98,2,36,4,29,7,15,86,10,21,5,4)
n = length(count)
transect = 1:n
transect.length = rep(10,n)
set.seed(1)
stratum = rep(1,n)
stratum[count>20] = 2
area = rep(900,n)
area[stratum==2] = 600
N = rep(100,n)
N[stratum==2] = 186
caribou = data.frame(transect=transect,length=transect.length,stratum=stratum,count=count, area=area, N=N)
caribou

save(caribou,file="./data/caribou.RData")

data(caribou)
N=286
n = dim(caribou)[1]
library(survey)
srs <- svydesign(id=~1,data=caribou,fpc=rep(n/N,n))
ybar <- svymean(~count,srs)
ybar
confint(ybar)
ybar*N
ytot <- svytotal(~Leave,srs)
ytot
confint(ytot)/10^6

# Check
s2 = sum((count-ybar)^2)/(n-1)
f = n/N
var.ybar = (1-f)*s2/n
var.ybar
tcrit = abs(qt(0.025,n-1))
ci.t = ybar[1] + c(-1,1)*tcrit*sqrt(var.ybar)
ci.t
ci.z = ybar[1] + c(-1,1)*1.96*sqrt(var.ybar)
ci.z



# Brexit sample
# ==============
brexdat = readRDS("/Users/dlb/git/MT4608/datasets/brexdat.Rds")
head(brexdat)
N = dim(brexdat)[1]
n=38

library(sampling)
set.seed(1234)
swor <- srswor(n,N)
brexitsample <- brexdat[swor==1,c(1:3,5:8,11)]
brexitsample
save(brexitsample, file="./data/brexitsample.RData")
