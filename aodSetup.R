#load packages
install.packages("googlesheets")
install.packages("Hmisc")

library("googlesheets")
library("Hmisc")

#Import data
aodimport<-gs_title("Analysis of Demand")
aod<-gs_read(aodimport, ws=4)
ad<-as.data.frame(aod)

#Explore data
describe(aod)

#Create Simple Calculated Variables

aod$tabReq<-aod$trtAppr*2.8
aod$tabRec<-aod$botRec*500
aod$calcTabReq<-ifelse(is.na(aod$tabReq)==T, aod$tabShip, aod$tabReq)
aod$tabAvail<-aod$tabInStock+aod$calcTabReq
aod$diffTrt


tabRem[i]<-tabInStock[i+1]
pctRem<-tabRem/tabAvail
tabChg1y[i]<-tabReq[i]-tabReq[i-1]
pctChg1y[i]<-tabChg1y[i]/tabReq[i-1]
diffTrtJA<-popTarg-trtAppr
