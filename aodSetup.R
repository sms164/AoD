#load packages
install.packages("googlesheets")
install.packages("Hmisc")
install.packages("data.table")
install.packages("rtf")
install.packages("scales")
install.packages("htmlTable")
install.packages("knitr")

library("googlesheets")
library("Hmisc")
library("data.table")
library("rtf")
library("scales")
library("htmlTable")
library("knitr")

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
aod$pctUsed<-(aod$tabAvail-aod$tabRem)/aod$tabAvail
aod<-aod[, tabReq1ya:=shift(tabReq,n=1, fill=NA, type="lag"), by=country]
aod$tabChg1y<-round(aod$tabReq-aod$tabReq1ya,0)
aod$pctChg1y<-aod$tabChg1y/aod$tabReq1ya

#Create variable to assess consistency of requests, usage and reported leftover
aod$diffTrtAva<-aod$tabAvail-aod$tabReq
aod$sdifTrtAva<-ifelse(abs(aod$diffTrtAva)>500, aod$diffTrtAva, 0)

#Create variable to assess the estimated number of treatments based on leftovers
aod$tabTrtRate<-round((aod$tabAvail-aod$tabRem)/2.8,0)

#Compare performed treatments vs requested treatments
aod$trtComp<-aod$calcTrtAppr-aod$tabTrtRate
aod$pctTrtComp<-aod$tabTrtRate/aod$calcTrtAppr

#Create Desired Table for a Country
pickcountry<-function(ds,country){
  which<-which(ds$country==country)
  countryds<-as.data.frame(ds[which,])
  return(countryds)
}

#Success
Togo<-pickcountry(ds=aod,country="Togo")

Togo$pctRemfmt<-ifelse(is.na(Togo$pctRem)==T, "NA", paste(100*round(Togo$pctRem,2),"%", sep=""))
Togo$pctChg1yfmt<-ifelse(is.na(Togo$pctChg1y)==T, "NA", paste(100*round(Togo$pctChg1y,2),"%", sep=""))


myvars<-c("yearTrt", "calcTrtAppr", "tabReq", "tabInStock", "tabShip", "tabRem", "pctRemfmt", "tabChg1y", "pctChg1yfmt")
tab<-Togo[myvars]
colnames(tab)<-c("Treatment Year", "Treatments Approved", "Tablets Required for MDA", "Tablets Left in Stock from Previous Year", "Tablets Shipped", "Tablets Remaining After MDA", "Percent of Availible Tablets Remaining After MDA", "Change in Demand of Tablets from Previous Year", "Percent Change in Demand of Tablets from Previous Year")
tabs<-t(tab[,2:9])
colnames(tabs)<-tab$`Treatment Year`


#-------------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------------

#Import Data function
aodimportf<-function(title, ws){
  aodimport<-gs_title(title)
  ad<-gs_read(aodimport, ws=ws)
  aood<-ad[order(ad$country, ad$yearTrt), ]
  aod<-as.data.table(aood)
  return(aod)
}

#Create Calculated Variables function 
calcvar<-function(ds){
  #simple
  ds$calcTrtAppr<-ifelse(is.na(ds$trtAppr)==T, ds$popTarg, ds$trtAppr)
  ds$tabReq<-ds$calcTrtAppr*2.8
  ds$tabRec<-ds$botRec*500
  ds$calcTabRec<-ifelse(is.na(ds$tabRec)==T, ds$tabShip, ds$tabRec)
  ds$tabAvail<-ds$tabInStock+ds$calcTabRec
  ds$diffTrtJA<-ds$popTarg-ds$trtAppr
  #lead/lag
  ds<-ds[, tabRem:=shift(tabInStock,n=1, fill=NA, type="lead"), by=country]
  ds$pctRem<-ds$tabRem/ds$tabAvail
  ds<-ds[, tabReq1ya:=shift(tabReq,n=1, fill=NA, type="lag"), by=country]
  ds$tabChg1y<-round(ds$tabReq-ds$tabReq1ya,0)
  ds$pctChg1y<-ds$tabChg1y/ds$tabReq1ya
  #compare
  ds$diffTrtAva<-ds$tabAvail-ds$tabReq
  ds$sdifTrtAva<-ifelse(abs(ds$diffTrtAva)>500, ds$diffTrtAva, 0)
  ds$tabTrtRate<-round((ds$tabAvail-ds$tabRem)/2.8,0)
  ds$trtComp<-ds$calcTrtAppr-ds$tabTrtRate
  ds$pctTrtComp<-ds$tabTrtRate/ds$calcTrtAppr
  return(ds)
}

#Pick country and years 
pickcountry<-function(ds,country,startyr=NA, endyr=NA){
  which<-which(ds$country==country)
  countryds<-as.data.frame(ds[which,])
  if (is.na(startyr)==F) {
    countryds<-countryds[which(countryds$yearTrt>=startyr),]
  }
  if (is.na(endyr)==F) {
    countryds<-countryds[which(countryds$yearTrt<=endyr),]
  }
  return(countryds)
}


tablefmt<-function(table){
  table$pctRemfmt<-percent(round(table$pctRem,3))
  table$pctChg1yfmt<-percent(round(table$pctChg1y,3))
  table$calcTrtAppr<-format(table$calcTrtAppr, big.mark=",")
  table$tabReq<-format(table$tabReq, big.mark=",")
  table$tabInStock<-format(table$tabInStock, big.mark=",")
  table$tabShip<-format(table$tabShip, big.mark=",")
  table$tabRem<-format(table$tabRem, big.mark=",")
  table$tabChg1y<-format(table$tabChg1y, big.mark=",")  
  myvars<-c("yearTrt", "calcTrtAppr", "tabReq", "tabInStock", "tabShip", "tabRem", "pctRemfmt", "tabChg1y", "pctChg1yfmt")
  tab<-table[myvars]
  colnames(tab)<-c("Treatment Year", "Treatments Approved", "Tablets Required for MDA", "Tablets Left in Stock from Previous Year", "Tablets Shipped", "Tablets Remaining After MDA", "Percent of Availible Tablets Remaining After MDA", "Change in Demand of Tablets from Previous Year", "Percent Change in Demand of Tablets from Previous Year")
  tabs<-t(tab[,-1])
  colnames(tabs)<-tab$`Treatment Year`
  tabs<-ifelse(is.na(tabs)==T, "-", tabs)
  return(tabs)
}


#Master Function:

countryaod<-function(titlem, wsm, countrym, startyrm, endyrm){
  rawaod<-aodimportf(title=titlem, ws=wsm)
  aodcalc<-calcvar(rawaod)
  country<-pickcountry(aodcalc, countrym, startyrm, endyrm)
  finaltab<-tablefmt(country)
}

#Run
countryX<-countryaod("Analysis of Demand", 4, "Togo", 2014, 2016)


