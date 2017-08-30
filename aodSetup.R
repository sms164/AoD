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
#describe(aod)

#Create Simple Calculated Variables
aod$calcTrtAppr<-ifelse(is.na(aod$trtAppr)==T, aod$popTarg, aod$trtAppr)
aod$tabReq<-aod$calcTrtAppr*2.8
aod$tabRec<-aod$botRec*500
aod$calcTabRec<-ifelse(is.na(aod$tabRec)==T, aod$tabShip, aod$tabRec)
aod$tabAvail<-aod$tabInStock+aod$calcTabRec
aod$diffTrtJA<-aod$popTarg-aod$trtAppr


#Create variables dependent on next or previous year
aod<-aod[, tabRem:=shift(tabInStock,n=1, fill=NA, type="lead"), by=country]
aod$pctRem<-aod$tabRem/aod$tabAvail
aod<-aod[, tabReq1ya:=shift(tabReq,n=1, fill=NA, type="lag"), by=country]
aod$tabChg1y<-aod$tabReq-aod$tabReq1ya
aod$pctChg1y<-aod$tabChg1y/aod$tabReq1ya

#Create variable to assess consistency of requests, usage and reported leftover
aod$tabvstrt<-NULL
aod$diffTrtAva<-aod$tabAvail-aod$tabReq
aod$sdifTrtAva<-ifelse(abs(aod$diffTrtAva)>500, aod$diffTrtAva, 0)

#Create variable to assess the estimated number of treatments based on leftovers
aod$tabTrtRate<-round((aod$tabAvail-aod$tabRem)/2.8,0)

#Compare performed treatments vs requested treatments
aod$trtComp<-aod$calcTrtAppr-aod$tabTrtRate
aod$pctTrtComp<-aod$tabTrtRate/aod$calcTrtAppr


#Create Desired Table for a Country


