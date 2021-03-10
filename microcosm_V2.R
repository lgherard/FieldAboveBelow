MC_KNZ<-read.csv("data/microcosm_data_V3.csv")
MC_KNZ$site<-"KNZ"
MC_KNZ<-merge(MC_KNZ,ALL_dt[,c(1:3,7)])
MC_KNZ$treat<-ifelse(MC_KNZ$sample=="a","Native Community",NA)
MC_KNZ$treat<-ifelse(MC_KNZ$sample=="b","Native community + root feeders",MC_KNZ$treat)
MC_KNZ$treat<-ifelse(MC_KNZ$sample=="c","Root feeders",MC_KNZ$treat)
MC_KNZ$treat<-ifelse(MC_KNZ$sample=="d","Defaunated",MC_KNZ$treat)
plots<-c(1,4,6,7,8,9,12,13,15,16,17,20,23,24,25,26,28,29,30,31,32,33,34,38,40)
MC_KNZ<-MC_KNZ[MC_KNZ$plot %in% plots,]
MC_KNZ$mass<-ifelse(is.na(MC_KNZ$mass),0,MC_KNZ$mass)

summary(aov(mass~treat*ppt,data=MC_KNZ))


library(ggplot2)

dodge <- position_dodge(50)
png(paste("figures/Microcosm Fig_V1.tiff"),res = 350, pointsize = 12,height=8,width=8,units= "in")
ggplot(MC_KNZ, aes(x = ppt, y = mass,color=treat,fill=treat)) + #geom_point(aes(color=treat))+
  geom_line(data = MC_KNZ, aes(y=predict(m4.germ, newdata = MC_KNZ), x= ppt,group=treat), size = 3) +
  stat_summary(fun.y = mean, geom = "point", size = 5,position=dodge) + 
  stat_summary(fun.data = mean_se, geom = "errorbar",size=1,width=32,position=dodge) +
  xlab("Growing Season Precipitation (mm)") + ylab(expression(paste("BNPP (g  ",m^-2," ",yr^-1,")",sep=" "))) + labs(colour="")+
  theme_classic(base_size = 18) +
  theme(legend.position = c(0.275,1.05) ) +
  coord_cartesian(ylim = c(0, 1.25)) 
dev.off()

ggplot(MC_KNZ, aes(x = ppt, y = mass,color=treat,fill=treat)) + #geom_point(aes(color=treat))+
  # stat_summary(fun.y = mean, geom = "point", size = 3) +
  # stat_summary(fun.data = mean_se, geom = "errorbar",size=1,width=22,position="dodge") +
  geom_bar(stat="summary",position="dodge", fun.y = "mean")+
  xlab("PPT ") +
  # geom_line(data = MC_KNZ, aes(y=predict(m4.germ, newdata = MC_KNZ), x= ppt,group=treat), size = 3) +
  # geom_line(data=sub.all.PPT_16.17.msq,aes(y=exp(fit)/5*100,x=PPT_mm),col="black")+
  ylab(expression(paste("BNPP (g  ",m^-2," ",yr^-1,")",sep=" "))) +
  theme_classic(base_size = 18) +
  theme(legend.position = c(0.35,0.92) ) +
  coord_cartesian(ylim = c(0, 1.2)) 
