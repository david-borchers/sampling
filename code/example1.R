library(mt4608)
help(package="mt4608")

data("classeg")

mu=mean(classeg$mark)
S2=var(classeg$mark)  # note this is the population variance

mu
S2

samp=take.sample(classeg,y.name="mark",n=2,type="srs",take.all=TRUE)

summary(samp)

samp

samp=add.point.est(samp,type="ybar")

summary(samp)

plot(samp)



data("stclasses")
stclass = stclasses[stclasses$class==2007,]
samp=take.sample(stclass,y.name="math.grade",n=1,type="srs",take.all=TRUE)
summary(samp)
samp=add.point.est(samp,type="ybar")
summary(samp)
plot(samp)
