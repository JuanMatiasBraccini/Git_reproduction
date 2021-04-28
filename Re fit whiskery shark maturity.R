if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

Dat=read.csv(handl_OneDrive("Data/Life history parameters/Maturity_whisk_Simpfendorferetal1998.csv"))

mod <- nls(Prop.Mat~1/(1+exp(-log(19)*(FL-p50)/(p95-p50))), start=c(p50=115, p95=120), data=Dat)

fit=summary(mod)$parameters[,1:2]

fn.logis=function(dat,p50,p95) 1/(1+exp(-log(19)*(dat-p50)/(p95-p50)))

plot(Dat$FL,Dat$Prop.Mat,pch=19)
SEQ=80:140
lines(SEQ,fn.logis(SEQ,fit[1,1],fit[2,1]),col=2) 



