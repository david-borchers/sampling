library(sampling)

data(ackroyd) # get ackroyd data

# simple random sample of sales, of size 4
samp<-take.sample(ackroyd,y.name="sales",n=4)
summary(samp) # summarize sample data

# All simple random samples of sales, of size 4
all.samp<-take.sample(ackroyd,y.name="sales",n=4,take.all=TRUE) 
summary(all.samp) # summarize sample data
ests = point.est(all.samp,type="ybar")
hist(ests, breaks=seq(min(ests),max(ests),length=20))

# All simple random samples of sales, of size 4, with auxiliary variable "mplyees"
all.samp<-take.sample(ackroyd,y.name="sales",n=4,aux.name="mplyees",take.all=TRUE) 
summary(all.samp) # summarize sample data
ests = point.est(all.samp,type="ybar")
hist(ests, breaks=seq(min(ests),max(ests),length=20))
ests = point.est(all.samp,type="ratio.mean")
hist(ests, breaks=seq(min(ests),max(ests),length=20))


# All stratified random samples of sales, of size 4, with auxiliary variable "mplyees"
# First need to define strata:
strat.ackroyd<-define.subunit(ackroyd,aux.name="nature",type="strat")
# Then take samples:
all.strat.samp<-take.sample(strat.ackroyd,type="strat", y.name="sales",n=c(2,2),aux.name="mplyees",take.all=TRUE) 
summary(all.strat.samp) # summarize sample data

# Example with strata defined by breaks
strat.ackroyd<-define.subunit(ackroyd,aux.name="firm",breaks=c(0,3.5,7),type="strat")
# Then take samples:
all.strat.samp<-take.sample(strat.ackroyd,type="strat", y.name="sales",n=c(2,2),aux.name="mplyees",take.all=TRUE) 
summary(all.strat.samp) # summarize sample data

ests = point.est(all.strat.samp,type="strat.mean")
hist(ests, breaks=seq(min(ests),max(ests),length=20))
