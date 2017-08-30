#load packages
install.packages("googlesheets")
install.packages("Hmisc")
install.packages("data.table")

library("googlesheets")
library("Hmisc")
library("data.table")

#Import data
aodimport<-gs_title("Analysis of Demand")
ad<-gs_read(aodimport, ws=4)
aood<-ad[order(ad$country, ad$yearTrt), ]
aod<-as.data.table(aood)

#Explore data
describe(aod)

#Create Simple Calculated Variables
aod$tabReq<-aod$trtAppr*2.8
aod$tabRec<-aod$botRec*500
aod$calcTabReq<-ifelse(is.na(aod$tabReq)==T, aod$tabShip, aod$tabReq)
aod$tabAvail<-aod$tabInStock+aod$calcTabReq
aod$diffTrtJA<-aod$popTarg-aod$trtAppr


#Create variables dependent on next or previous year
aod<-aod[, tabRem:=shift(tabInStock,n=1, fill=NA, type="lead"), by=country]
aod$pctRem<-aod$tabRem/aod$tabAvail
aod<-aod[, tabReq1ya:=shift(tabReq,n=1, fill=NA, type="lag"), by=country]
aod$tabChg1y<-aod$tabReq-aod$tabReq1ya
aod$pctChg1y<-aod$tabChg1y/aod$tabReq1ya

#Dependent Variable Definitions
#tabRem[i]<-tabInStock[i+1]
#pctRem<-tabRem/tabAvail
#tabChg1y[i]<-tabReq[i]-tabReq[i-1]
#pctChg1y[i]<-tabChg1y[i]/tabReq[i-1]




#Create variable to assess consistency of requests, usage and reported leftover

#Create variable to assess the estimated number of treatments based on leftovers




