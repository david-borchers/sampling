t.dbn.ci<-function(point,var,level,df)
{
  sig<-(1-level)/2
  lower<-point+qt(sig,df=df)*sqrt(var)
  upper<-point+qt((1-sig),df=df)*sqrt(var)
  list(ci.lower=lower,ci.upper=upper)
}