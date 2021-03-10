# Load libraries
my_packages<-c("reshape2","MASS","dplyr","nlme","gdata")
for (package in my_packages) {
  if (!require(package, character.only=T, quietly=T)) {
    try(install.packages(package))
    library(package, character.only=T)
  }
}

# Input data
PPT<-read.csv("data/ppt_all.csv")

# Jornada data ------------------------------------------------------------
JRN_anpp_2016 <- read.csv("data/AboveBelow_JRN_aboveground_biomass_2016.csv")
treat<-read.csv("data/JRN_treatments.csv")
JRN_bnpp_2016 <- read.csv("data/bnpp_IC_JRN_V2.csv")
JRN_srb_2016 <- read.csv("data/2016/JRN 2016 SRB AboveBelow_V2.csv")


JRN_anpp_2017 <- read.csv("data/2017/AboveBelow_JRN_aboveground_biomass_2017.csv")
JRN_bnpp_2017 <- read.csv("data/2017/JRN_IC_2017RW.csv")
JRN_srb_2017 <- read.csv("data/2017/JRN 2017 SRB AboveBelow_V2.csv")


# Konza data --------------------------------------------------------------
KNZ_bnpp_2016 <- read.csv("data/bnpp_IC_KNZ_V2.csv")
KNZ_treat <- read.csv("data/KNZ_plots.csv")
KNZ_anpp_2016 <- read.csv("data/AboveBelow_KNZ_anpp_2016.csv")
KNZ_srb_2016 <- read.csv("data/2016/KNZ 2016 SRB AboveBelow_V2.csv")

KNZ_bnpp_2017 <- read.csv("data/2017/KNZ_IC_2017RW.csv")
KNZ_anpp_2017 <- read.csv("data/2017/AboveBelow_KNZ_aboveground_biomass_2017.csv")
KNZ_srb_2017 <- read.csv("data/2017/KNZ 2017 SRB AboveBelow_V2.csv")


# SGRC data ---------------------------------------------------------------
SGS_bnpp_2016 <- read.csv("data/bnpp_IC_SGS_V2.csv")
SGS_treat <- read.csv("data/SGS_plots.csv")
SGS_anpp_2016 <- read.csv("data/AboveBelow_SGS_aboveground_biomass_2016.csv")
SGS_srb_2016 <- read.csv("data/2016/SGS 2016 SRB AboveBelow_V2.csv")

SGS_bnpp_2017 <- read.csv("data/2017/SGS_IC_2017RW.csv")
SGS_anpp_2017 <- read.csv("data/2017/AboveBelow_SGS_aboveground_biomass_2017.csv")
SGS_srb_2017 <- read.csv("data/2017/SGS 2017 SRB AboveBelow_V2.csv")


# # Nematodes data all sites ----------------------------------------------
nem_dt<-read.csv("data/nematode data.csv")

#### Jornada ####
# Clean and aggregate data by plot
# JRN_bnpp_2016<-subset(JRN_bnpp_2016,JRN_bnpp_2016$EastorWest=="E")
# bnpp_2016<-JRN_bnpp_2016[,-c(1,8,9,11,12)]
# bnpp16<-aggregate(.~plot,bnpp_2016,FUN=sum,na.action=NULL)
bnpp16<-JRN_bnpp_2016
bnpp16$year<-2016

bnpp17<-JRN_bnpp_2017
bnpp17$year<-2017

bnpp_JRN<-rbind(bnpp16,bnpp17)
# Convert bnpp values from core area to m2 
bnpp_JRN$bnpp<-(bnpp_JRN$bnpp/0.002)

# Standing Root Biomass
JRN_srb_2016$year<-2016
JRN_srb_2017$year<-2017
srb_JRN<-rbind(JRN_srb_2016,JRN_srb_2017)

# Convert anpp from quadrat area to m2
anpp16<-subset(JRN_anpp_2016[,-c(4,6)],JRN_anpp_2016$category=="GREEN")
anpp16<-aggregate(anpp~plot+site+year,data=anpp16,sum)
anpp16$anpp<-anpp16$anpp*5

anpp17<-subset(JRN_anpp_2017[,-c(4,6)],JRN_anpp_2017$category=="GREEN")
anpp17<-aggregate(anpp~plot+site+year,data=anpp17,sum)
anpp17$anpp<-anpp17$anpp*5
anpp17$anpp[anpp17$plot==27]<-anpp17$anpp[anpp17$plot==27]/2
anpp17$anpp[anpp17$plot==10]<-anpp17$anpp[anpp17$plot==10]/2
anpp_JRN<-rbind(anpp16,anpp17)

dt<-merge(anpp_JRN,treat)

dt<-merge(dt,bnpp_JRN)
dt<-merge(dt,srb_JRN)
dt$treat<-ifelse(dt$treat==3,"control",dt$treat)
dt$ppt_treat<-ifelse(dt$treat==1, 0.2, NA)
dt$ppt_treat<-ifelse(dt$treat==2, 0.5, dt$ppt_treat)
dt$ppt_treat<-ifelse(dt$treat=="control", 1, dt$ppt_treat)
dt$ppt_treat<-ifelse(dt$treat==4, 1.5, dt$ppt_treat)
dt$ppt_treat<-ifelse(dt$treat==5, 1.8, dt$ppt_treat)

# Growing season PPT
# WEST WELL rain gauge 2016 196.85 mm 2017 181.86
# RABBIT rain gauge 2016 130.30 mm 2017 171.96
ppt2016<-sum(PPT[which(PPT$site=="JRN"& PPT$Year==2016 & PPT$Month >= 7& PPT$Month<=10),"Total.Precip"])
ppt2017<-sum(PPT[which(PPT$site=="JRN"& PPT$Year==2017 & PPT$Month >= 7& PPT$Month<=10),"Total.Precip"])

# Annual PPT 
# WEST WELL rain gauge 2016 282.44 mm 2017 315.98 mm
# RABBIT rain gauge 2016 211.84 mm 2017 284.98 mm

dt$ppt<-ifelse(dt$year==2016,dt$ppt_treat*ppt2016,NA)
dt$ppt<-ifelse(dt$year==2017,dt$ppt_treat*ppt2017,dt$ppt)

dt$srb<-dt$srb/0.002
dt$npp<-dt$anpp+dt$bnpp
dt$fbnpp<-dt$bnpp/dt$npp*100


#### Konza data ####
KNZ_anpp_2016<-KNZ_anpp_2016[KNZ_anpp_2016$category=="Green",]

KNZ_anpp_2017<-KNZ_anpp_2017[KNZ_anpp_2017$category=="GREEN",-6]
KNZ_anpp<-rbind(KNZ_anpp_2016,KNZ_anpp_2017)

KNZ_bnpp_2016$year<- 2016
KNZ_bnpp_2017$year<- 2017
KNZ_bnpp<-rbind(KNZ_bnpp_2016,KNZ_bnpp_2017)

# Standing Root Biomass
KNZ_srb_2016$year<-2016
KNZ_srb_2017$year<-2017
srb_KNZ<-rbind(KNZ_srb_2016,KNZ_srb_2017)

dtKNZ<-merge(KNZ_bnpp,KNZ_anpp)
dtKNZ<-merge(dtKNZ,srb_KNZ)
dtKNZ<-merge(dtKNZ,KNZ_treat)
dtKNZ$anpp<-dtKNZ$anpp*10
dtKNZ$bnpp<-dtKNZ$bnpp/0.002
dtKNZ$npp<-dtKNZ$anpp+dtKNZ$bnpp
dtKNZ$srb<-dtKNZ$srb/0.002

dtKNZ$ppt_treat<-ifelse(dtKNZ$treat==-60, 0.4, NA)
dtKNZ$ppt_treat<-ifelse(dtKNZ$treat==-30, 0.7, dtKNZ$ppt_treat)
dtKNZ$ppt_treat<-ifelse(dtKNZ$treat== "control", 1, dtKNZ$ppt_treat)
dtKNZ$ppt_treat<-ifelse(dtKNZ$treat==30, 1.3, dtKNZ$ppt_treat)
dtKNZ$ppt_treat<-ifelse(dtKNZ$treat==60, 1.6, dtKNZ$ppt_treat)

# Annual PPT KNZ
# KNZppt2016<-991 
# KNZppt2017<-726 

# Growing season PPT
# KNZppt2016<-868.7
# KNZppt2017<-582.6

KNZppt2016<-sum(PPT[which(PPT$site=="KNZ"& PPT$Year==2016 & PPT$Month >= 4& PPT$Month<=9),"Total.Precip"])
KNZppt2017<-sum(PPT[which(PPT$site=="KNZ"& PPT$Year==2017 & PPT$Month >= 4& PPT$Month<=9),"Total.Precip"])

dtKNZ$ppt<-ifelse(dtKNZ$year==2016,dtKNZ$ppt_treat*KNZppt2016,NA)
dtKNZ$ppt<-ifelse(dtKNZ$year==2017,dtKNZ$ppt_treat*KNZppt2017,dtKNZ$ppt)

dtKNZ$fbnpp<-dtKNZ$bnpp/dtKNZ$npp*100

#### SGS data ####
SGS_anpp16<-SGS_anpp_2016[which(SGS_anpp_2016$category=="GREEN"|SGS_anpp_2016$category=="YE LLOW"),-6]
SGS_anpp16<-aggregate(anpp~plot+site+year,data=SGS_anpp16,sum)

SGS_anpp_2017<-SGS_anpp_2017[which(SGS_anpp_2017$category=="GREEN"|SGS_anpp_2017$category=="YE LLOW"),-6]
SGS_anpp_2017<-aggregate(anpp~plot+site+year,data=SGS_anpp_2017,sum)

SGS_anpp<-rbind(SGS_anpp16,SGS_anpp_2017)

SGS_bnpp_2016$year<- 2016
SGS_bnpp_2017$year<- 2017
SGS_bnpp<-rbind(SGS_bnpp_2016,SGS_bnpp_2017)

# Standing Root Biomass
SGS_srb_2016$year<-2016
SGS_srb_2017$year<-2017
srb_SGS<-rbind(SGS_srb_2016,SGS_srb_2017)

dtSGS<-merge(SGS_bnpp,SGS_anpp)
dtSGS<-merge(dtSGS,srb_SGS)
dtSGS<-merge(dtSGS,SGS_treat)
dtSGS$anpp<-dtSGS$anpp*10
dtSGS$bnpp<-dtSGS$bnpp/0.002
dtSGS$npp<-dtSGS$anpp+dtSGS$bnpp
dtSGS$srb<-dtSGS$srb/0.002

dtSGS$ppt_treat<-ifelse(dtSGS$treat==-70, 0.3, NA)
dtSGS$ppt_treat<-ifelse(dtSGS$treat==-40, 0.6, dtSGS$ppt_treat)
dtSGS$ppt_treat<-ifelse(dtSGS$treat== "control", 1, dtSGS$ppt_treat)
dtSGS$ppt_treat<-ifelse(dtSGS$treat==40, 1.4, dtSGS$ppt_treat)
dtSGS$ppt_treat<-ifelse(dtSGS$treat==70, 1.7, dtSGS$ppt_treat)

SGSppt2016<-sum(PPT[which(PPT$site=="SGS"& PPT$Year==2016 & PPT$Month >= 3& PPT$Month<=9),"Total.Precip"])
SGSppt2017<-sum(PPT[which(PPT$site=="SGS"& PPT$Year==2017 & PPT$Month >= 3& PPT$Month<=9),"Total.Precip"])

dtSGS$ppt<-ifelse(dtSGS$year==2016,dtSGS$ppt_treat*SGSppt2016,NA)
dtSGS$ppt<-ifelse(dtSGS$year==2017,dtSGS$ppt_treat*SGSppt2017,dtSGS$ppt)

dtSGS$fbnpp<-dtSGS$bnpp/dtSGS$npp*100

columns<-c("plot","year","site","anpp","bnpp","treat","ppt","fbnpp","srb")
ALL_dt<-rbind(dtSGS[,columns],dtKNZ[,columns],dt[,columns])

# Merge Nematode abundance data
ALL_dt<-merge(ALL_dt,nem_dt[,c(1,2,4,9,11,13)])
ALL_dt$root.feeders[ALL_dt$root.feeders>13000]<-NA
keep(ALL_dt,sure=T)

library(dplyr)
agg_dt<-ALL_dt %>%
  group_by(site,year,treat) %>%
  # filter(treat=="control")%>%
  summarize(anpp=mean(anpp,na.rm=T),bnpp=mean(bnpp,na.rm=T),fbnpp=mean(fbnpp,na.rm=T),root.feeders=mean(root.feeders,na.rm=T),ppt=mean(ppt))
write.csv(agg_dt,paste(Sys.Date(),"mean_anpp_site.csv",sep="_"))
