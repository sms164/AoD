#-------------------------------------------------------------------------------------
# Analysis of Demand Functions
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
  table$pctRemfmt<-ifelse(is.na(table$pctRem)==F,percent(round(table$pctRem,3)), "-")
  table$pctChg1yfmt<-ifelse(is.na(table$pctChg1y)==F,percent(round(table$pctChg1y,3)), "-")
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
  tabs<-ifelse(tabs=="       NA", "-", tabs)
  return(tabs)
}


#Master Function:

countryaod<-function(titlem, wsm, countrym, startyrm=NA, endyrm=NA){
  rawaod<-aodimportf(title=titlem, ws=wsm)
  aodcalc<-calcvar(rawaod)
  country<-pickcountry(aodcalc, countrym, startyrm, endyrm)
  finaltab<-tablefmt(country)
}

