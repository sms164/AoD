#load packages
install.packages("googlesheets")
install.packages("Hmisc")
install.packages("data.table")
install.packages("rtf")

library("googlesheets")
library("Hmisc")
library("data.table")
library("rtf")

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
tabs<-t(tab[,2:9])
colnames(tabs)<-tab$yearTrt[]



pdf("dataout.pdf", height=11, width=8.5)
grid.table(tabs)
dev.off()

library(rtf)
rtffile <- RTF("rtf.doc")  # this can be an .rtf or a .doc
addParagraph(rtffile, "This is the table of tablets:\n")
addTable(rtffile, tabs)
addParagraph(rtffile, "\n\nThis is the nicer looking table we made above:\n")
addTable(rtffile, cbind(rownames(outtab), outtab))
done(rtffile)

