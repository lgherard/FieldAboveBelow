library(ggplot2)
library(tgp)
library(MASS)
library(doBy)
library(cowplot)
library(nlme)
library(egg)

# Source data cleanning script
source("data_cleanning.R")
#
# Mixed Model Analyses and Figures
# F-BNPP ------------------------------------------------------------------
mm1<-lme(fbnpp~ppt,random=~ppt|site/year,data=na.omit(ALL_dt[,c("fbnpp","ppt","site","year","plot")]),na.action=na.exclude)
summary(mm1)

mm2<-lme(fbnpp~log(ppt),random=~ppt|site/year,data=na.omit(ALL_dt[,c("fbnpp","ppt","site","year")]),na.action=na.exclude)
summary(mm2)
intervals(mm2)
AIC(mm1,mm2)

newdat <- data.frame(ppt=sample(ALL_dt$ppt,150),site=rep(c("SGS","KNZ","JRN"),50),year=rep(c("2016","2017"),75))
newdat<-na.omit(newdat)
newdat$fbnpp<-predict(mm2,newdat=newdat,level=0)

Designmat <- model.matrix(eval(eval(mm1$call$fixed)[-2]), newdat[-ncol(newdat)])
predvar <- diag(Designmat %*% mm1$varFix %*% t(Designmat))
newdat$SE <- sqrt(predvar)
newdat$SE2 <- sqrt(predvar+mm1$sigma^2)

png(paste("figures/Mixed Model fbnpp PPT-Year V5.png"),res = 350, pointsize = 12,height=6,width=5,units= "in")
ggplot(na.omit(ALL_dt[,c("fbnpp","ppt","site","year")]), aes(x=ppt, y=fbnpp))+geom_point(size=1,col="grey")+
  # geom_line(aes(y=predict(mm2),x=ppt,group=interaction(site,year),col=site),size=2) +
  geom_line(data=newdat,aes(y=predict(mm2,level=0,newdat=newdat),x=ppt),size=3)+
  geom_ribbon(data=newdat,aes(ymin=fbnpp-SE,ymax=fbnpp+SE),alpha=0.2,fill="palegreen4")+
  theme_bw(base_size=18)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(.8, 0.85))+
  labs(x = "Growing-Season Precipitation (mm)", y=expression(paste("Fraction BNPP (%)")))
dev.off()

# BNPP --------------------------------------------------------------------
mm1<-lme(bnpp~ppt,random=~1|site/year,data=na.omit(ALL_dt[,c("bnpp","ppt","site","year")]),na.action=na.exclude)
summary(mm1)
mm2<-lme(bnpp~ppt,random=~ppt|site/year,data=na.omit(ALL_dt[,c("bnpp","ppt","site","year")]),na.action=na.exclude)
summary(mm2)
mm3<-lme(bnpp~log(ppt),random=~1|site/year,data=na.omit(ALL_dt[,c("bnpp","ppt","site","year")]),na.action=na.exclude)
summary(mm3)
mm4<-lme(bnpp~log(ppt),random=~log(ppt)|site/year,data=na.omit(ALL_dt[,c("bnpp","ppt","site","year")]),na.action=na.exclude)
summary(mm4)
intervals(mm3)
AIC(mm1,mm2,mm3,mm4)

newdat <- data.frame(ppt=sample(ALL_dt$ppt,180),site=rep(c("SGS","KNZ","JRN"),60),year=rep(c("2016","2017"),90))
newdat<-na.omit(newdat)
newdat$bnpp<-predict(mm2,newdat=newdat,level=0)

Designmat <- model.matrix(eval(eval(mm2$call$fixed)[-2]), newdat[-ncol(newdat)])
predvar <- diag(Designmat %*% mm2$varFix %*% t(Designmat))
newdat$SE <- sqrt(predvar)
newdat$SE2 <- sqrt(predvar+mm2$sigma^2)

png(paste("figures/Mixed Model bnpp PPT-year V5.png"),res = 350, pointsize = 12,height=6,width=5,units= "in")
ggplot(na.omit(ALL_dt[,c("bnpp","ppt","site","year")]), aes(x=ppt, y=bnpp))+geom_point(size=1,col="grey")+
  geom_line(aes(y=predict(mm2),x=ppt,group=interaction(site,year),col=site),size=2) +
  geom_line(data=newdat,aes(y=predict(mm2,level=0,newdat=newdat),x=ppt),size=3)+
  geom_ribbon(data=newdat,aes(ymin=bnpp-SE,ymax=bnpp+SE),alpha=0.2,fill="palegreen4")+
  theme_bw(base_size=18)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(.8, 0.85))+
  labs(x = "Growing-Season Precipitation (mm)", y=expression(paste("BNPP (g  ",m^-2," ",yr^-1,")",sep=" ")))
dev.off()

# ANPP --------------------------------------------------------------------
mm1<-lme(anpp~ppt,random=~ppt|site/year,data=ALL_dt,na.action=na.exclude)
summary(mm1)
mm2<-lme(anpp~log(ppt),random=~ppt|site/year,data=ALL_dt,na.action=na.exclude)
summary(mm2)
intervals(mm2)
AIC(mm1,mm2)

newdat <- data.frame(ppt=sample(ALL_dt$ppt,60),site=rep(c("SGS","KNZ","JRN"),20),year=rep(c("2016","2017"),30))
newdat<-na.omit(newdat)
newdat$anpp<-predict(mm1,newdat=newdat,level=0)

Designmat <- model.matrix(eval(eval(mm1$call$fixed)[-2]), newdat[-ncol(newdat)])
predvar <- diag(Designmat %*% mm1$varFix %*% t(Designmat))
newdat$SE <- sqrt(predvar)
newdat$SE2 <- sqrt(predvar+mm1$sigma^2)

png(paste("figures/Mixed Model anpp PPT V5.png"),res = 350, pointsize = 12,height=6,width=5,units= "in")
ggplot(ALL_dt, aes(x=ppt, y=anpp))+geom_point(size=1,col="grey")+
  geom_line(aes(y=predict(mm1),x=ppt,group=interaction(site,year),col=site),size=2) +
  geom_line(data=newdat,aes(y=predict(mm1,level=0,newdat=newdat),x=ppt),size=3)+
  geom_ribbon(data=newdat,aes(ymin=anpp-SE,ymax=anpp+SE),alpha=0.2,fill="palegreen4")+
  theme_bw(base_size=18)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(.2, 0.85))+
  labs(x = "Growing-Season Precipitation (mm)", y=expression(paste("ANPP (g  ",m^-2," ",yr^-1,")",sep=" ")))
dev.off()

### PPT by Nematode abundance interaction
ALL_dt$npp<-ALL_dt$bnpp+ALL_dt$anpp
# SGS ---------------------------------------------------------------------
SGS2017_dt<-ALL_dt[which(ALL_dt$site=="SGS"&ALL_dt$year==2017),]
SGS2016_dt<-na.omit(ALL_dt[which(ALL_dt$site=="SGS"&ALL_dt$year==2016),])

SGSlm2log<-lm(log(root.feeders)~ppt,data=SGS2016_dt)
 summary(SGSlm2log)
SGSlm1log<-lm(log(root.feeders)~ppt,data=SGS2017_dt)
# summary(SGSlm1log)

Fig3a<-ggplot(SGS2017_dt, aes(y=root.feeders, x=ppt))+geom_point(size=1,col="grey")+
  geom_point(data=SGS2016_dt,size=1,col="grey")+
  
  geom_smooth(aes(y=exp(predict(SGSlm1log)), x=ppt),method="lm",lty=2,lwd=1,se = F,col="palegreen4")+
  
  geom_smooth(data=SGS2016_dt,aes(y=exp(predict(SGSlm2log)), x=ppt),method="lm",lty=2,lwd=1,col="steelblue" ,se=F)+
  # theme(plot.margin = unit(c(0.1,0,0,1), "cm"))+
  # theme_bw(base_size=10)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        ylim(0, 13000)+
  annotate("text",x=50,y=13000,label="D")+
  labs( y=expression(paste("Root Feeders (ind. ",Kg^-1, "dry soil)",sep=" ")),x =expression(paste("Precipitation (mm ",yr^-1,")",sep=" ")))
  Fig3a

SGSlm1a<-lm(fbnpp~ppt,data=SGS2017_dt)
# summary(SGSlm1a)
SGSlm2a<-lm(fbnpp~ppt,data=na.omit(SGS2016_dt))
# summary(SGSlm2a)

Fig3d<-ggplot(SGS2017_dt, aes(y=fbnpp, x=ppt))+geom_point(size=1,col="grey")+
  geom_point(data=SGS2016_dt,size=1,col="grey")+
  
  geom_smooth(aes(y=fbnpp, x=ppt),method="lm",lwd=1,col="palegreen4")+
  
  geom_smooth(data=SGS2016_dt,aes(y=fbnpp, x=ppt),method="lm",lwd=1,col="steelblue",se=T)+
  # theme_bw(base_size=10)+
  # theme(plot.margin = unit(c(0,0,1,1), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+ylim(0,85)+
  
  annotate("text",x=50,y=85,label="A")+
  labs( y="F-BNPP (%)",x =expression(paste("Precipitation (mm ",yr^-1,")",sep=" ")))
 Fig3d

# JRN ---------------------------------------------------------------------
JRN2017_dt<-na.omit(ALL_dt[which(ALL_dt$site=="JRN"&ALL_dt$year==2017),])
JRN2016_dt<-na.omit(ALL_dt[which(ALL_dt$site=="JRN"&ALL_dt$year==2016),])
JRNlm2log<-lm(log(root.feeders)~ppt,data=JRN2016_dt)
 summary(JRNlm2log)
JRNlm1log<-lm(log(root.feeders)~ppt,data=JRN2017_dt)
 summary(JRNlm1log)

Fig3b<-ggplot(JRN2017_dt, aes(y=root.feeders, x=ppt))+geom_point(size=1,col="grey")+
  
  geom_point(data=JRN2016_dt,size=1,col="grey")+
  
  geom_smooth(aes(y=exp(predict(JRNlm1log)), x=ppt),method="lm",lty=2,lwd=1,se = F,col="palegreen4")+
  
  geom_smooth(data=JRN2016_dt,aes(y=exp(predict(JRNlm2log)), x=ppt),method="lm",lty=2,lwd=1,col= "steelblue",se=F)+
  # theme_bw(base_size=10)+
  # theme(plot.margin = unit(c(0.1,0,0,0), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.y=element_blank(),axis.text.y=element_blank())+ylim(0, 13000)+
  annotate("text",x=10,y=13000,label="E")+
  labs( y=expression(paste("Root Feeders (ind. ",Kg^-1, "dry soil)",sep=" ")),x =expression(paste("Precipitation (mm ",yr^-1,")",sep=" ")))
 Fig3b

JRNlm1a<-lm(fbnpp~ppt,data=na.omit(JRN2017_dt))
 summary(JRNlm1a)
JRNlm2a<-lm(fbnpp~ppt,data=JRN2016_dt)
 summary(JRNlm2a)

Fig3e<-ggplot(JRN2017_dt, aes(y=fbnpp, x=ppt))+geom_point(size=1,col="grey")+
  geom_point(data=JRN2016_dt,size=1,col="grey")+
  
  geom_smooth(aes(y=fbnpp, x=ppt),method="lm",lwd=1,col="palegreen4")+
  
  geom_smooth(data=JRN2016_dt,aes(y=fbnpp, x=ppt),method="lm",lwd=1,col="steelblue",se=T)+
  # theme_bw(base_size=10)+
  # theme(plot.margin = unit(c(0,0,1,0), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),axis.text.y=element_blank())+ylim(0, 85)+
  annotate("text",x=10,y=85,label="B")+
  labs( y="F-BNPP (%)",x =expression(paste("Precipitation (mm ",yr^-1,")",sep=" ")))
  Fig3e

# KNZ ---------------------------------------------------------------------
KNZ2017_dt<-na.omit(ALL_dt[which(ALL_dt$site=="KNZ"&ALL_dt$year==2017),])
KNZ2016_dt<-na.omit(ALL_dt[which(ALL_dt$site=="KNZ"&ALL_dt$year==2016),])

knzlm1log<-lm(log(root.feeders)~ppt,data=KNZ2017_dt)
summary(knzlm1log)
knzlm2log<-lm(log(root.feeders)~ppt,data=KNZ2016_dt)
summary(knzlm2log)

Fig3c<-ggplot(KNZ2017_dt, aes(y=root.feeders, x=ppt))+geom_point(size=1,col="grey")+
  geom_point(data=KNZ2016_dt,aes(y=root.feeders, x=ppt),size=1,col="grey")+
  
  geom_smooth(aes(y=exp(predict(knzlm1log)), x=ppt),method="lm",lty=2,lwd=1,se = T,col="palegreen4")+
  
  
  geom_smooth(data=KNZ2016_dt,aes(y=exp(predict(knzlm2log)), x=ppt),method="lm",se=T,lwd=1,col= "steelblue")+
  geom_ribbon(data=KNZ2016_dt,aes(ymin=exp(predict(knzlm2log))-exp(summary(knzlm2log)$coef[2,2])*mean(ppt),
                                  ymax=exp(predict(knzlm2log))+exp(summary(knzlm2log)$coef[2,2])*mean(ppt)),alpha=0.3,fill="grey")+
  # theme_bw(base_size=10)+
  # theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.y=element_blank(),axis.text.y=element_blank())+ylim(0, 13000)+
  annotate("text",x=200,y=13000,label="F")+
  labs( y=expression(paste("Root Feeders (ind. ",Kg^-1, "dry soil)",sep=" ")),x =expression(paste("Precipitation (mm ",yr^-1,")",sep=" ")))
  Fig3c

knzlm1a<-lm(fbnpp~ppt,data=na.omit(KNZ2017_dt))
 summary(knzlm1a)
knzlm2a<-lm(fbnpp~ppt,data=na.omit(KNZ2016_dt))
summary(knzlm2a)

Fig3f<-ggplot(KNZ2016_dt, aes(y=fbnpp, x=ppt))+geom_point(size=1,col="grey")+
  geom_smooth(aes(y=fbnpp, x=ppt),col="steelblue",method="lm",lty=2,lwd=1,se=F)+
  
  geom_point(data=KNZ2017_dt, aes(y=fbnpp, x=ppt),size=1,col="grey")+
  geom_smooth(data=KNZ2017_dt, aes(y=fbnpp, x=ppt),col="palegreen4",method="lm",lty=1,lwd=1,se=T)+
  
  # geom_smooth(aes(y=fbnpp, x=ppt,col="palegreen4"),method="lm",lwd=1)+
  # theme_bw(base_size=10)+
  scale_colour_manual(values=c("steelblue","palegreen4"),guide="legend",labels=c("year 1","year 2"))+

  # theme(plot.margin = unit(c(2,6,1,2), "cm"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),axis.text.y=element_blank(),legend.position=c(.8, 0.85))+ylim(0,85)+
  annotate("text",x=200,y=85,label="C")+
  theme(legend.title=element_blank())+
  labs( y="F-BNPP (%)",x =expression(paste("Precipitation (mm ",yr^-1,")",sep=" ")))
  Fig3f

# Figure 4 ----------------------------------------------------------------
png(filename = paste("figures/Figure_3_6panels_V6.png",sep=""),res = 200,height=15.25,width=22.5,units="cm")
ggarrange(Fig3d,Fig3e,Fig3f,Fig3a,Fig3b,Fig3c,nrow=2)
dev.off()

# Regional plant vss trophic mechanism figure -----------------------------
tableKNZ2017<-data.frame(site="KNZ",meanppt=mean(KNZ2017_dt$ppt,na.rm=T)
                         ,RFslope=summary(knzlm1log)$coef[2,1],RFlower=confint(knzlm1log)[2,1],RFupper=confint(knzlm1log)[2,2]
                         ,PPTslope=summary(knzlm1a)$coef[2,1],PPTlower=confint(knzlm1a)[2,1],PPTupper=confint(knzlm1a)[2,2])
tableKNZ2016<-data.frame(site="KNZ",meanppt=mean(KNZ2016_dt$ppt,na.rm=T)
                         ,RFslope=summary(knzlm2log)$coef[2,1],RFlower=confint(knzlm2log)[2,1],RFupper=confint(knzlm2log)[2,2]
                         ,PPTslope=summary(knzlm2a)$coef[2,1],PPTlower=confint(knzlm2a)[2,1],PPTupper=confint(knzlm2a)[2,2])
tableSGS2017<-data.frame(site="SGS",meanppt=mean(SGS2017_dt$ppt,na.rm=T)
                         ,RFslope=summary(SGSlm1log)$coef[2,1],RFlower=confint(SGSlm1log)[2,1],RFupper=confint(SGSlm1log)[2,2]
                         ,PPTslope=summary(SGSlm1a)$coef[2,1],PPTlower=confint(SGSlm1a)[2,1],PPTupper=confint(SGSlm1a)[2,2])
tableSGS2016<-data.frame(site="SGS",meanppt=mean(SGS2016_dt$ppt,na.rm=T)
                         ,RFslope=summary(SGSlm2log)$coef[2,1],RFlower=confint(SGSlm2log)[2,1],RFupper=confint(SGSlm2log)[2,2]
                         ,PPTslope=summary(SGSlm2a)$coef[2,1],PPTlower=confint(SGSlm2a)[2,1],PPTupper=confint(SGSlm2a)[2,2])
tableJRN2017<-data.frame(site="JRN",meanppt=mean(JRN2017_dt$ppt,na.rm=T)
                         ,RFslope=summary(JRNlm1log)$coef[2,1],RFlower=confint(JRNlm1log)[2,1],RFupper=confint(JRNlm1log)[2,2]
                         ,PPTslope=summary(JRNlm1a)$coef[2,1],PPTlower=confint(JRNlm1a)[2,1],PPTupper=confint(JRNlm1a)[2,2])
tableJRN2016<-data.frame(site="JRN",meanppt=mean(JRN2016_dt$ppt,na.rm=T)
                         ,RFslope=summary(JRNlm2log)$coef[2,1],RFlower=confint(JRNlm2log)[2,1],RFupper=confint(JRNlm2log)[2,2]
                         ,PPTslope=summary(JRNlm2a)$coef[2,1],PPTlower=confint(JRNlm2a)[2,1],PPTupper=confint(JRNlm2a)[2,2])

slopes<-rbind(tableKNZ2016,tableKNZ2017,tableSGS2016,tableSGS2017,tableJRN2016,tableJRN2017)

non_linear_RF<-nls(RFslope ~ b+c*log(meanppt),data=slopes, start=list(b=0,c=0), trace=FALSE)
summary(non_linear_RF)
Fun_RF<- function(meanppt) coef(non_linear_RF)[1]+coef(non_linear_RF)[2]*log(meanppt)

linear_RF<-lm(RFslope ~ meanppt,data=slopes)
summary(linear_RF)
AIC(non_linear_RF,linear_RF)

non_linear_PPT<-nls(PPTslope ~ a+b*log(meanppt),data=slopes, start=list(a=0,b=0), trace=FALSE)
summary(non_linear_PPT)
Fun_PPT<- function(meanppt) coef(non_linear_PPT)[1]+coef(non_linear_PPT)[2]*log(meanppt)
linear_PPT<-lm(PPTslope ~ meanppt,data=slopes)
summary(linear_PPT)
AIC(non_linear_PPT,linear_PPT)

png(filename = paste("figures/Figure_5_V5.png",sep=""),res = 300, pointsize = 10,height=18.5,width=10.5,units="cm")
par(las=1,oma=c(4,7,1,0),mar=c(.5,0,1,0.5), mgp=c(2,0.75,0),ps=12,tck=-0.01,bty="n",cex.lab=1,mfrow=c(2,1),cex.axis=.8)
plot(RFslope~meanppt,data=slopes,col=1,xlim=c(100,900),ylim=c(0.0015,-0.0005),
     ylab = "",xlab="Mean Annual Precipitation (mm yr-1)",
     pch=c(16,21,21,21,21,21),cex=3,bty="l",xaxt="n")
mtext(text = "Trophic Cascade Hypothesis Effect",side=2,las=0,line=4,cex=1.1,col=1)
abline(h=0,col="grey")
abline(linear_RF,lwd=2,col=1,lty=2)
mtext(text="A",3,adj=0.05,line=-0.5,cex=1.25)
plot(PPTslope~meanppt,data=slopes,yaxt="s",col=1,ylab="",ylim=c(0.01,-0.082),
     xlim=c(100,900),xlab="Annual Precipitation (mm yr-1)",
     pch=c(21,16,16,16,16,16),cex=3,bty="l")
abline(h=0,col="grey")
axis(2)
curve(Fun_PPT,from=min(slopes$meanppt)*.5,to=max(slopes$meanppt)*1.1,add=T,lwd=2,col=1)
mtext(text = "Plant Hypothesis Effect",side=2,las=0,line=4,cex=1.1,col=1)
mtext(text = "Annual Precipitation (mm)",side=1,las=0,line=3,cex=1.25,col=)
mtext(text="B",3,adj=0.05,line=-0.2,cex=1.25)
dev.off()