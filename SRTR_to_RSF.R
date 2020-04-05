## R code to import SRTR SAF file, process data, and grow RSF for MELD40 manuscript

library(haven)
library(randomForestSRC)

d <- read_sas('tx_li.sas7bdat')
d <- as.data.frame(d)
d <- subset(d, difftime(strptime(REC_TX_DT, format='%Y-%m-%d'),strptime('2001-05-01', format='%Y-%m-%d'))>=0)
d <- subset(d, strptime("2017-01-01", "%Y-%m-%d")-strptime(REC_TX_DT, "%Y-%m-%d")>0)
d$MELD.last <- d$CAN_LAST_SRTR_LAB_MELD - 6200
d <- subset(d, REC_TX_ORG_TY %in% c('LI','KI LI'))
d <- subset(d, REC_AGE_AT_TX>=18)
d <- subset(d, DON_TY=='C')
for(i in 1:nrow(d)){
    if(d$CAN_LAST_STAT[i] %in% c(6010,6011,6012)) d$meldcat[i] <- 'Status 1'
    else if(d$CANHX_MPXCPT_HCC_APPROVE_IND[i]==1) d$meldcat[i] <- 'HCC exception'
    else if (is.na(d$MELD.last[i])) d$meldcat[i] <- NA
    else if (d$MELD.last[i] < 15) d$meldcat[i] <- '<15'
    else if (d$MELD.last[i] < 29) d$meldcat[i] <- '15-28'
    else if (d$MELD.last[i] < 33) d$meldcat[i] <- '29-32'
    else if (d$MELD.last[i] < 37) d$meldcat[i] <- '33-36'
    else if (d$MELD.last[i] < 41) d$meldcat[i] <- '37-40'
    else d$meldcat[i] <- '>40'
}
d$meldcat <- factor(d$meldcat, levels=c('HCC exception','<15','15-28','29-32','33-36','37-40','>40','Status 1'))
d <- subset(d, meldcat=="37-40" & MELD.last==40)
##
## n=5309
##
## MELD=40
## excludes Status 1 and HCC exception
## 2001-05-01 through 2016-12-31
## recipient age >= 18y
## LI or KI LI
## deceased donors
##
##
d$txyr <- as.numeric(substr(d$REC_TX_DT, 1, 4))
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_ANGINA[i]) | d$CAN_ANGINA[i]==998) d$angina[i] <- NA
    else if (d$CAN_ANGINA[i]==1) d$angina[i] <- 'No'
    else if (d$CAN_ANGINA[i] %in% c(6, 7, 30)) d$angina[i] <- 'Yes'
    else d$angina[i] <- 'ERROR'
}
d$angina <- factor(d$angina)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_CEREB_VASC[i]) | d$CAN_CEREB_VASC[i]=='' | d$CAN_CEREB_VASC[i]=='U') d$cereb.vasc[i] <- NA
    else if (d$CAN_CEREB_VASC[i]=='Y') d$cereb.vasc[i] <- 'Yes'
    else if (d$CAN_CEREB_VASC[i]=='N') d$cereb.vasc[i] <- 'No'
    else d$cereb.vasc[i] <- 'ERROR'
}
d$cereb.vasc <- factor(d$cereb.vasc)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_DRUG_TREAT_COPD[i]) | d$CAN_DRUG_TREAT_COPD[i]=='' | d$CAN_DRUG_TREAT_COPD[i]=='U') d$drug.trt.copd[i] <- NA
    else if (d$CAN_DRUG_TREAT_COPD[i]=='Y') d$drug.trt.copd[i] <- 'Yes'
    else if (d$CAN_DRUG_TREAT_COPD[i]=='N') d$drug.trt.copd[i] <- 'No'
    else d$drug.trt.copd[i] <- 'ERROR'
}
d$drug.trt.copd <- factor(d$drug.trt.copd)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_DRUG_TREAT_HYPERTEN[i]) | d$CAN_DRUG_TREAT_HYPERTEN[i]=='' | d$CAN_DRUG_TREAT_HYPERTEN[i]=='U') d$drug.trt.hyperten[i] <- NA
    else if (d$CAN_DRUG_TREAT_HYPERTEN[i]=='Y') d$drug.trt.hyperten[i] <- 'Yes'
    else if (d$CAN_DRUG_TREAT_HYPERTEN[i]=='N') d$drug.trt.hyperten[i] <- 'No'
    else d$drug.trt.hyperten[i] <- 'ERROR'
}
d$drug.trt.hyperten <- factor(d$drug.trt.hyperten)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_PERIPH_VASC[i]) | d$CAN_PERIPH_VASC[i]=='' | d$CAN_PERIPH_VASC[i]=='U') d$periph.vasc[i] <- NA
    else if (d$CAN_PERIPH_VASC[i]=='Y') d$periph.vasc[i] <- 'Yes'
    else if (d$CAN_PERIPH_VASC[i]=='N') d$periph.vasc[i] <- 'No'
    else d$periph.vasc[i] <- 'ERROR'
}
d$periph.vasc <- factor(d$periph.vasc)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_MALIG[i]) | d$CAN_MALIG[i]=='' | d$CAN_MALIG[i]=='U') d$can.malig[i] <- NA
    else if (d$CAN_MALIG[i]=='Y') d$can.malig[i] <- 'Yes'
    else if (d$CAN_MALIG[i]=='N') d$can.malig[i] <- 'No'
    else d$can.malig[i] <- 'ERROR'
}
d$can.malig <- factor(d$can.malig)
##
for(i in 1:nrow(d)){
    if (is.na(d$REC_VARICEAL_BLEEDING[i]) | d$REC_VARICEAL_BLEEDING[i]=='' | d$REC_VARICEAL_BLEEDING[i]=='U') d$varic.bleed[i] <- NA
    else if (d$REC_VARICEAL_BLEEDING[i]=='Y') d$varic.bleed[i] <- 'Yes'
    else if (d$REC_VARICEAL_BLEEDING[i]=='N') d$varic.bleed[i] <- 'No'
    else d$varic.bleed[i] <- 'ERROR'
}
d$varic.bleed <- factor(d$varic.bleed)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_LAST_DIAL_PRIOR_WEEK[i]) | d$CAN_LAST_DIAL_PRIOR_WEEK[i]=='' | d$CAN_LAST_DIAL_PRIOR_WEEK[i]=='U' | d$CAN_LAST_DIAL_PRIOR_WEEK[i]=='A') d$last.dial.prior.wk[i] <- NA
    else if (d$CAN_LAST_DIAL_PRIOR_WEEK[i]=='Y') d$last.dial.prior.wk[i] <- 'Yes'
    else if (d$CAN_LAST_DIAL_PRIOR_WEEK[i]=='N') d$last.dial.prior.wk[i] <- 'No'
    else d$last.dial.prior.wk[i] <- 'ERROR'
}
d$last.dial.prior.wk <- factor(d$last.dial.prior.wk)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_LAST_ASCITES[i]) | d$CAN_LAST_ASCITES[i]==4) d$can.ascites[i] <- NA
    else if (d$CAN_LAST_ASCITES[i]==1) d$can.ascites[i] <- 'Absent'
    else if (d$CAN_LAST_ASCITES[i]==2) d$can.ascites[i] <- 'Slight'
    else if (d$CAN_LAST_ASCITES[i]==3) d$can.ascites[i] <- 'Moderate'
    else d$can.ascites[i] <- 'ERROR'
}
d$can.ascites <- factor(d$can.ascites, levels=c('Absent','Slight','Moderate'))
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_LAST_ENCEPH[i]) | d$CAN_LAST_ENCEPH[i]==4) d$enceph[i] <- NA
    else if (d$CAN_LAST_ENCEPH[i]==1) d$enceph[i] <- 'None'
    else if (d$CAN_LAST_ENCEPH[i]==2) d$enceph[i] <- '1-2'
    else if (d$CAN_LAST_ENCEPH[i]==3) d$enceph[i] <- '3-4'
    else d$enceph[i] <- 'ERROR'
}
d$enceph <- factor(d$enceph, levels=c('None','1-2','3-4'))
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_BACTERIA_PERIT[i]) | d$CAN_BACTERIA_PERIT[i]=='' | d$CAN_BACTERIA_PERIT[i]=='U') d$can.bact.perit[i] <- NA
    else if (d$CAN_BACTERIA_PERIT[i]=='Y') d$can.bact.perit[i] <- 'Yes'
    else if (d$CAN_BACTERIA_PERIT[i]=='N') d$can.bact.perit[i] <- 'No'
    else d$can.bact.perit[i] <- 'ERROR'
}
d$can.bact.perit <- factor(d$can.bact.perit)
##
for(i in 1:nrow(d)){
    if (is.na(d$REC_VENTILATOR[i])) d$rec.vent[i] <- NA
    else if (d$REC_VENTILATOR[i]==0) d$rec.vent[i] <- 'No'
    else if (d$REC_VENTILATOR[i]==1) d$rec.vent[i] <- 'Yes'
    else d$rec.vent[i] <- 'ERROR'
}
d$rec.vent <- factor(d$rec.vent)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_PREV_ABDOM_SURG[i]) | d$CAN_PREV_ABDOM_SURG[i]=='' | d$CAN_PREV_ABDOM_SURG[i]=='U') d$can.prev.abdom.surg[i] <- NA
    else if (d$CAN_PREV_ABDOM_SURG[i]=='Y') d$can.prev.abdom.surg[i] <- 'Yes'
    else if (d$CAN_PREV_ABDOM_SURG[i]=='N') d$can.prev.abdom.surg[i] <- 'No'
    else d$can.prev.abdom.surg[i] <- 'ERROR'
}
d$can.prev.abdom.surg <- factor(d$can.prev.abdom.surg)
##
for(i in 1:nrow(d)){
    if (is.na(d$REC_PREV_ABDOM_SURG[i]) | d$REC_PREV_ABDOM_SURG[i]=='' | d$REC_PREV_ABDOM_SURG[i]=='U') d$rec.prev.abdom.surg[i] <- NA
    else if (d$REC_PREV_ABDOM_SURG[i]=='Y') d$rec.prev.abdom.surg[i] <- 'Yes'
    else if (d$REC_PREV_ABDOM_SURG[i]=='N') d$rec.prev.abdom.surg[i] <- 'No'
    else d$rec.prev.abdom.surg[i] <- 'ERROR'
}
d$rec.prev.abdom.surg <- factor(d$rec.prev.abdom.surg)
##
for(i in 1:nrow(d)){
    if(!is.na(d$can.prev.abdom.surg[i]) & d$can.prev.abdom.surg[i]=='Yes') d$prevabdomsurg[i] <- 'Yes'
    else if(!is.na(d$rec.prev.abdom.surg[i]) & d$rec.prev.abdom.surg[i]=='Yes') d$prevabdomsurg[i] <- 'Yes'
    else if(is.na(d$rec.prev.abdom.surg[i]) & is.na(d$can.prev.abdom.surg[i])) d$prevabdomsurg[i] <- NA
    else d$prevabdomsurg[i] <- 'No'
}
d$prevabdomsurg <- factor(d$prevabdomsurg)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_PORTAL_VEIN[i]) | d$CAN_PORTAL_VEIN[i]=='' | d$CAN_PORTAL_VEIN[i]=='U') d$can.portal.vein[i] <- NA
    else if (d$CAN_PORTAL_VEIN[i]=='Y') d$can.portal.vein[i] <- 'Yes'
    else if (d$CAN_PORTAL_VEIN[i]=='N') d$can.portal.vein[i] <- 'No'
    else d$can.portal.vein[i] <- 'ERROR'
}
d$can.portal.vein <- factor(d$can.portal.vein)
##
for(i in 1:nrow(d)){
    if (is.na(d$REC_PORTAL_VEIN[i]) | d$REC_PORTAL_VEIN[i]=='' | d$REC_PORTAL_VEIN[i]=='U') d$rec.portal.vein[i] <- NA
    else if (d$REC_PORTAL_VEIN[i]=='Y') d$rec.portal.vein[i] <- 'Yes'
    else if (d$REC_PORTAL_VEIN[i]=='N') d$rec.portal.vein[i] <- 'No'
    else d$rec.portal.vein[i] <- 'ERROR'
}
d$rec.portal.vein <- factor(d$rec.portal.vein)
##
for(i in 1:nrow(d)){
    if(!is.na(d$can.portal.vein[i]) & d$can.portal.vein[i]=='Yes') d$portalvein[i] <- 'Yes'
    else if(!is.na(d$rec.portal.vein[i]) & d$rec.portal.vein[i]=='Yes') d$portalvein[i] <- 'Yes'
    else if(is.na(d$rec.portal.vein[i]) & is.na(d$can.portal.vein[i])) d$portalvein[i] <- NA
    else d$portalvein[i] <- 'No'
}
d$portalvein <- factor(d$portalvein)
##
for(i in 1:nrow(d)){
    if (is.na(d$CAN_TIPSS[i]) | d$CAN_TIPSS[i]=='' | d$CAN_TIPSS[i]=='U') d$can.tipss[i] <- NA
    else if (d$CAN_TIPSS[i]=='Y') d$can.tipss[i] <- 'Yes'
    else if (d$CAN_TIPSS[i]=='N') d$can.tipss[i] <- 'No'
    else d$can.tipss[i] <- 'ERROR'
}
d$can.tipss <- factor(d$can.tipss)
##
for(i in 1:nrow(d)){
    if (is.na(d$REC_TIPSS[i]) | d$REC_TIPSS[i]=='' | d$REC_TIPSS[i]=='U') d$rec.tipss[i] <- NA
    else if (d$REC_TIPSS[i]=='Y') d$rec.tipss[i] <- 'Yes'
    else if (d$REC_TIPSS[i]=='N') d$rec.tipss[i] <- 'No'
    else d$rec.tipss[i] <- 'ERROR'
}
d$rec.tipss <- factor(d$rec.tipss)
##
for(i in 1:nrow(d)){
    if(!is.na(d$can.tipss[i]) & d$can.tipss[i]=='Yes') d$tipss[i] <- 'Yes'
    else if(!is.na(d$rec.tipss[i]) & d$rec.tipss[i]=='Yes') d$tipss[i] <- 'Yes'
    else if(is.na(d$rec.tipss[i]) & is.na(d$can.tipss[i])) d$tipss[i] <- NA
    else d$tipss[i] <- 'No'
}
d$tipss <- factor(d$tipss)
##
for(i in 1:nrow(d)){
    if (is.na(d$REC_HOSP_90_DAYS[i]) | d$REC_HOSP_90_DAYS[i]=='' | d$REC_HOSP_90_DAYS[i]=='U') d$rec.hosp90[i] <- NA
    else if (d$REC_HOSP_90_DAYS[i]=='Y') d$rec.hosp90[i] <- 'Yes'
    else if (d$REC_HOSP_90_DAYS[i]=='N') d$rec.hosp90[i] <- 'No'
    else d$rec.hosp90[i] <- 'ERROR'
}
d$rec.hosp90 <- factor(d$rec.hosp90)
##
for(i in 1:nrow(d)) if(d$DON_RACE_SRTR[i]=='') d$DON_RACE_SRTR[i] <- NA
d$don.race <- factor(d$DON_RACE_SRTR, levels=c('WHITE','ASIAN','BLACK','MULTI','NATIVE','PACIFIC'))
##
for(i in 1:nrow(d)){
    if (is.na(d$DON_CAD_DON_COD[i]) | d$DON_CAD_DON_COD[i]==998) d$don.cod[i] <- NA
    else if (d$DON_CAD_DON_COD[i]==1) d$don.cod[i] <- 'Anoxia'
    else if (d$DON_CAD_DON_COD[i]==2) d$don.cod[i] <- 'Cerebrovascular/Stroke'
    else if (d$DON_CAD_DON_COD[i]==3) d$don.cod[i] <- 'Head Trauma'
    else if (d$DON_CAD_DON_COD[i]==4) d$don.cod[i] <- 'CNS Tumor'
    else if (d$DON_CAD_DON_COD[i]==999) d$don.cod[i] <- 'Other'
    else d$don.cod[i] <- 'ERROR'
}
d$don.cod <- factor(d$don.cod)
##
for(i in 1:nrow(d)){
    if (is.na(d$DON_INOTROP_AGENT_GE3[i]) | d$DON_INOTROP_AGENT_GE3[i]=='' | d$DON_INOTROP_AGENT_GE3[i]=='U') d$don.inotrop.ge3[i] <- NA
    else if (d$DON_INOTROP_AGENT_GE3[i]=='Y') d$don.inotrop.ge3[i] <- 'Yes'
    else if (d$DON_INOTROP_AGENT_GE3[i]=='N') d$don.inotrop.ge3[i] <- 'No'
    else d$don.inotrop.ge3[i] <- 'ERROR'
}
d$don.inotrop.ge3 <- factor(d$don.inotrop.ge3)
##
for(i in 1:nrow(d)){
    if (is.na(d$DON_INOTROP_SUPPORT[i]) | d$DON_INOTROP_SUPPORT[i]=='' | d$DON_INOTROP_SUPPORT[i]=='U') d$don.inotrop.supp[i] <- NA
    else if (d$DON_INOTROP_SUPPORT[i]=='Y') d$don.inotrop.supp[i] <- 'Yes'
    else if (d$DON_INOTROP_SUPPORT[i]=='N') d$don.inotrop.supp[i] <- 'No'
    else d$don.inotrop.supp[i] <- 'ERROR'
}
d$don.inotrop.supp <- factor(d$don.inotrop.supp)
##
for(i in 1:nrow(d)){
    if (is.na(d$DON_MEET_CDC_HIGH_RISK[i]) | d$DON_MEET_CDC_HIGH_RISK[i]=='' | d$DON_MEET_CDC_HIGH_RISK[i]=='U') d$don.cdc.high.risk[i] <- NA
    else if (d$DON_MEET_CDC_HIGH_RISK[i]=='Y') d$don.cdc.high.risk[i] <- 'Yes'
    else if (d$DON_MEET_CDC_HIGH_RISK[i]=='N') d$don.cdc.high.risk[i] <- 'No'
    else d$don.cdc.high.risk[i] <- 'ERROR'
}
d$don.cdc.high.risk <- factor(d$don.cdc.high.risk)
##
for(i in 1:nrow(d)){
    if (is.na(d$DON_NON_HR_BEAT[i]) | d$DON_NON_HR_BEAT[i]=='' | d$DON_NON_HR_BEAT[i]=='U') d$don.non.hr.beat[i] <- NA
    else if (d$DON_NON_HR_BEAT[i]=='Y') d$don.non.hr.beat[i] <- 'Yes'
    else if (d$DON_NON_HR_BEAT[i]=='N') d$don.non.hr.beat[i] <- 'No'
    else d$don.non.hr.beat[i] <- 'ERROR'
}
d$don.non.hr.beat <- factor(d$don.non.hr.beat)
##
d$CAN_ETHNICITY_SRTR <- factor(d$CAN_ETHNICITY_SRTR, levels=c('NLATIN','LATINO'))
d$can.eth <- d$CAN_ETHNICITY_SRTR
d$CAN_RACE_SRTR <- factor(d$CAN_RACE_SRTR, levels=c('WHITE','ASIAN','BLACK','MULTI','NATIVE','PACIFIC'))
d$can.race <- d$CAN_RACE_SRTR
##
for(i in 1:nrow(d)){
    if(is.na(d$CAN_DIAB_TY[i]) | d$CAN_DIAB_TY[i]=='998') d$diabetes[i] <- NA
    else if(d$CAN_DIAB_TY[i] %in% c(2,3,4,5)) d$diabetes[i] <- 'Yes'
    else if(d$CAN_DIAB_TY[i]==1) d$diabetes[i] <- 'No'
    else d$diabetes[i] <- 'ERROR'
}
d$diabetes <- factor(d$diabetes)
##
d$can.gender <- factor(d$CAN_GENDER)
d$don.age <- d$DON_AGE
d$don.gender <- factor(d$DON_GENDER)
d$don.eth <- factor(d$DON_ETHNICITY_SRTR, levels=c('NLATIN','LATINO'))
##
for(i in 1:nrow(d)){
    if (is.na(d$DON_ORG_SHARED[i])) d$don.org.shared[i] <- NA
    else if (d$DON_ORG_SHARED[i]==0) d$don.org.shared[i] <- 'No'
    else if (d$DON_ORG_SHARED[i]==1) d$don.org.shared[i] <- 'Yes'
    else d$don.org.shared[i] <- 'ERROR'
}
d$don.org.shared <- factor(d$don.org.shared)
d$donor.bmi <- 10000 * d$DON_WGT_KG / d$DON_HGT_CM^2
d$donor.bmi[d$donor.bmi>100] <- NA
d$rec.age <- d$REC_AGE_AT_TX
d$kili <- factor(d$REC_TX_ORG_TY, levels=c('LI','KI LI'))
##
for(i in 1:nrow(d)){
    if (is.na(d$REC_PREV_LI[i])) d$rec.prev.li[i] <- NA
    else if (d$REC_PREV_LI[i]==0) d$rec.prev.li[i] <- 'No'
    else if (d$REC_PREV_LI[i]==1) d$rec.prev.li[i] <- 'Yes'
    else d$rec.prev.li[i] <- 'ERROR'
}
d$rec.prev.li <- factor(d$rec.prev.li)
##
for(i in 1:nrow(d)){
    if(is.na(d$CAN_HGT_CM[i]) | d$CAN_HGT_CM[i] < 25) d$ht[i] <- d$REC_HGT_CM[i]
    else d$ht[i] <- d$CAN_HGT_CM[i]
}
for(i in 1:nrow(d)){
    if(is.na(d$CAN_WGT_KG[i])) d$wt[i] <- d$REC_WGT_KG[i]
    else d$wt[i] <- d$CAN_WGT_KG[i]
    if(!is.na(d$CAN_WGT_KG[i]) & !is.na(d$REC_WGT_KG[i]) & d$CAN_WGT_KG[i] > 2 * d$REC_WGT_KG[i]) d$wt[i] <- d$REC_WGT_KG[i]
}
d$bmi <- 10000 * d$wt / d$ht^2
##
for(i in 1:nrow(d)){
    if(!is.na(d$ht[i]) & d$ht[i] < 100){
        d$ht[i] <- NA
        d$bmi[i] <- NA
    }
}
##
d$cold.isch.time <- d$REC_COLD_ISCH_TM
d$albumin <- d$CAN_LAST_ALBUMIN
d$bili <- d$CAN_LAST_BILI
d$creat <- d$CAN_LAST_SERUM_CREAT
d$inr <- d$CAN_LAST_INR
d$don.ast <- d$DON_SGOT
d$sodium <- d$CAN_LAST_SERUM_SODIUM
d$alt <- d$REC_SGPT
##
for(i in 1:nrow(d)){
    d$death_date_final[i] <- NA
    if(!is.na(d$TFL_DEATH_DT[i])) d$death_date_final[i] <- as.character(d$TFL_DEATH_DT[i])
    else if(!is.na(d$PERS_OPTN_DEATH_DT[i])) d$death_date_final[i] <- as.character(d$PERS_OPTN_DEATH_DT[i])
    else if(!is.na(d$PERS_SSA_DEATH_DT[i])) d$death_date_final[i] <- as.character(d$PERS_SSA_DEATH_DT[i])
}
##
for(i in 1:nrow(d)){
    d$surv_time_final[i] <- as.numeric(min(difftime(strptime(d$TFL_LAFUDATE[i], '%Y-%m-%d'), strptime(d$REC_TX_DT[i], '%Y-%m-%d'), units='days'), difftime(strptime(d$death_date_final[i], '%Y-%m-%d'), strptime(d$REC_TX_DT[i], '%Y-%m-%d'), units='days'), na.rm=T))
}
##
d$death <- rep(0, nrow(d))
for(i in 1:nrow(d)){
    if(!is.na(d$death_date_final[i]) & as.numeric(difftime(strptime(d$death_date_final[i], '%Y-%m-%d'), strptime(d$REC_TX_DT[i], '%Y-%m-%d'), units='days')) == d$surv_time_final[i]) d$death[i] <- 1
}
##
for(i in 1:nrow(d)){
    if(is.na(d$TFL_GRAFT_DT[i]) & is.na(d$PERS_RETX[i])) d$graft_date_final[i] <- NA
    else if(is.na(d$PERS_RETX[i])) d$graft_date_final[i] <- as.character(d$TFL_GRAFT_DT[i])
    else if(is.na(d$TFL_GRAFT_DT[i])) d$graft_date_final[i] <- as.character(d$PERS_RETX[i])
    else d$graft_date_final[i] <- as.character(min(strptime(d$TFL_GRAFT_DT[i], '%Y-%m-%d'), strptime(d$PERS_RETX[i], '%Y-%m-%d')))
}
##
for(i in 1:nrow(d)){
    d$time_graft_fail[i] <- as.numeric(difftime(min(strptime(d$TFL_LAFUDATE[i], '%Y-%m-%d'), strptime(d$death_date_final[i], '%Y-%m-%d'), d$graft_date_final[i], na.rm=T), strptime(d$REC_TX_DT[i], '%Y-%m-%d'), units='days'))
}
##
d$graft_fail <- d$graft_fail_dc <- rep(0, nrow(d))
for(i in 1:nrow(d)){
    if(!is.na(d$graft_date_final[i]) & as.numeric(difftime(d$graft_date_final[i], strptime(d$REC_TX_DT[i], '%Y-%m-%d'), units='days')) == d$time_graft_fail[i]) d$graft_fail[i] <- 1
    if(!is.na(d$graft_date_final[i]) & as.numeric(difftime(d$graft_date_final[i], strptime(d$REC_TX_DT[i], '%Y-%m-%d'), units='days')) == d$time_graft_fail[i]) d$graft_fail_dc[i] <- 1
    if(d$death[i] == 1) d$graft_fail[i] <- 1
}
##
d$pf1 <- d$death
d$tpf1 <- d$surv_time_final
for(i in 1:nrow(d)){
    if (d$surv_time_final[i] > 365){
        d$tpf1[i] <- 365
        d$pf1[i] <- 0
    }
}
##
d$tpf1 <- round(d$tpf1)
##
d$tpf1 <- pmax(0.5, d$tpf1)



d -> d.all.columns
##
d <- d[, c(2,5,73,74,308,310:372)]



func.imp <- function() {
    impute(Surv(tpf1, pf1)
           ~ txyr
           + rec.prev.li
           + cold.isch.time
           + rec.age
           + can.gender
           + can.eth
           + can.race
           + bmi
           + diabetes
           + can.malig
           + last.dial.prior.wk
           + albumin
           + bili
           + creat
           + inr
           + can.ascites
           + enceph
           + can.bact.perit
           + rec.vent
           + prevabdomsurg
           + portalvein
           + tipss
           + rec.hosp90
           + don.age
           + don.gender
           + don.eth
           + don.race
           + donor.bmi
           + don.cod
           + don.inotrop.ge3
           + don.inotrop.supp
           + don.cdc.high.risk
           + don.non.hr.beat
           + don.ast
           + don.org.shared
           + kili
           + angina
           + cereb.vasc
           + drug.trt.copd
           + drug.trt.hyperten
           + periph.vasc
           + varic.bleed
           + sodium
           + alt,
           data=d,
           ntree=2000,
           nodesize=15,
           nsplit=10,
           ntime=100
           )
}

func.fit <- function(data, seed) {
    rfsrc(Surv(tpf1, pf1)
          ~ txyr
          + rec.prev.li
          + cold.isch.time
          + rec.age
          + can.gender
          + can.eth
          + can.race
          + bmi
          + diabetes
          + can.malig
          + last.dial.prior.wk
          + albumin
          + bili
          + creat
          + inr
          + can.ascites
          + enceph
          + can.bact.perit
          + rec.vent
          + prevabdomsurg
          + portalvein
          + tipss
          + rec.hosp90
          + don.age
          + don.gender
          + don.eth
          + don.race
          + donor.bmi
          + don.cod
          + don.inotrop.ge3
          + don.inotrop.supp
          + don.cdc.high.risk
          + don.non.hr.beat
          + don.ast
          + don.org.shared
          + kili
          + angina
          + cereb.vasc
          + drug.trt.copd
          + drug.trt.hyperten
          + periph.vasc
          + varic.bleed
          + sodium
          + alt,
          data=data,
          ntree=1000,
          block.size=1,
          nodesize=15,
          nsplit=10,
          ntime=100,
          importance='permute',
          seed=seed
          )
}


set.seed(7006)
imp6 <- func.imp()
save(imp6, file="imp6_obj")
##
f6 <- func.fit(data=imp6, seed=-8006)
save(f6, file="f6_obj")
sub.f6 <- subsample(f6, B=1000)
save(sub.f6, file="sub_f6_obj")
##
e6 <- extract.subsample(sub.f6)


e6.full <- row.names(subset(e6$var.jk.sel.Z, pvalue <= 1))
e6.200 <- row.names(subset(e6$var.jk.sel.Z, pvalue < 0.20))
e6.100 <- row.names(subset(e6$var.jk.sel.Z, pvalue < 0.10))
e6.050 <- row.names(subset(e6$var.jk.sel.Z, pvalue < 0.05))
e6.025 <- row.names(subset(e6$var.jk.sel.Z, pvalue < 0.025))
e6.010 <- row.names(subset(e6$var.jk.sel.Z, pvalue < 0.01))

set.seed(7777)
##
me6.full <- rfsrc(as.formula(paste('Surv(tpf1, pf1)', paste(e6.full, collapse=" + "), sep=" ~ ")),
data=imp6,
ntree=1000,
nodesize=15,
nsplit=10,
ntime=100
)
##
me6.200 <- rfsrc(as.formula(paste('Surv(tpf1, pf1)', paste(e6.200, collapse=" + "), sep=" ~ ")),
data=imp6,
ntree=1000,
nodesize=15,
nsplit=10,
ntime=100
)
##
me6.100 <- rfsrc(as.formula(paste('Surv(tpf1, pf1)', paste(e6.100, collapse=" + "), sep=" ~ ")),
data=imp6,
ntree=1000,
nodesize=15,
nsplit=10,
ntime=100
)
##
me6.050 <- rfsrc(as.formula(paste('Surv(tpf1, pf1)', paste(e6.050, collapse=" + "), sep=" ~ ")),
data=imp6,
ntree=1000,
nodesize=15,
nsplit=10,
ntime=100
)
##
me6.025 <- rfsrc(as.formula(paste('Surv(tpf1, pf1)', paste(e6.025, collapse=" + "), sep=" ~ ")),
data=imp6,
ntree=1000,
nodesize=15,
nsplit=10,
ntime=100
)
##
me6.010 <- rfsrc(as.formula(paste('Surv(tpf1, pf1)', paste(e6.010, collapse=" + "), sep=" ~ ")),
data=imp6,
ntree=1000,
nodesize=15,
nsplit=10,
ntime=100
)
##

fm <- me6.200

## fm is final RSF object with 22 predictors used for partial dependence figure and example predictions figure in manuscript
