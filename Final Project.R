# Project - Advanced Data Analytics 
# SPACs

#Change Working Directory 
rm(list = ls())
setwd("/Users/kristalabovich/Desktop/Advanced Data Analytics/Final Project/Data Files")


#Read in Files with Excel Reader - Files are in Google Drive 
#install.packages('Rcpp') #You may need to install 
require(Rcpp)
require(readxl)
itrk <- as.data.frame(read_excel("ITRK LN.xlsx",range="C5:N737")) #Intertek 
head(itrk)

clvt <- as.data.frame(read_excel("CLVT US.xlsx",range="C5:O737")) #Clarivate 
head(clvt)

be <- as.data.frame(read_excel("BE US.xlsx",range="C5:N737")) #Bloom Energy 
head(be)

vrt <- as.data.frame(read_excel("VRT US.xlsx",range="C5:N737")) #Vertiv Holdings
head(vrt)

hri <- as.data.frame(read_excel("HRI US.xlsx",range="C5:N737")) #Herc Holdings
head(hri)

wsc <- as.data.frame(read_excel("WSC US.xlsx",range="C5:N737")) #WIllscot Corp 
head(wsc)

spr <- as.data.frame(read_excel("SPR US.xlsx",range="C5:N737")) #Spirit Aerosystems
head(spr)

spce <- as.data.frame(read_excel("SPCE US.xlsx",range="C5:N737")) #Virgin Galctic 
head(wsc)

psec <- as.data.frame(read_excel("PSEC US.xlsx",range="C5:N737")) #Prospect Capital Group 
head(psec)

nkla <- as.data.frame(read_excel("NKLA US.xlsx",range="C5:N737")) #Nikola Corp  
head(nkla)


#Looking into NAs 
sapply(be, function(x) sum(is.na(x))) 
sapply(clvt, function(x) sum(is.na(x))) 
#68 NAs for % of EPS - do more work to figure out what to do 
sapply(hri, function(x) sum(is.na(x)))
sapply(itrk, function(x) sum(is.na(x)))
sapply(nkla, function(x) sum(is.na(x)))
#203 NAs for Estimate EPS/Target price - work on solution 
sapply(psec[,], function(x) sum(is.na(x)))
#211/107 NAs for Target Price/ Estimate EPS - work on solution 
sapply(spce, function(x) sum(is.na(x)))
sapply(spr, function(x) sum(is.na(x)))
sapply(vrt, function(x) sum(is.na(x)))
#91/120 NAs for target price/estimate EPS - work on solution 
sapply(wsc, function(x) sum(is.na(x)))

#TO DO for NAs
#Try to remove 
na.omit(be)
na.omit(clvt)
na.omit(hri)
na.omit(itrk)
na.omit(psec)
na.omit(spce)
na.omit(spr)
na.omit(vrt)
na.omit(wsc)

#TO DO - Remove First Row of Data from All Datasets 
be <- be[-1,]
clvt <- clvt[-1,]
hri <- hri[-1,]
itrk <- itrk[-1,]
nkla <- nkla[-1,]
psec <- psec[-1,]
spce <- spce[-1,]
spr <- spr[-1,]
vrt <- vrt[-1,]
wsc <- wsc[-1,]

#Remove 0s 
be <- subset(be, be$`Percent Change Price`!=0)
clvt <- subset(clvt, clvt$`Percent Change Price`!=0)
hri <- subset(hri, hri$`Percent Change Price`!=0)
itrk <- subset(itrk, itrk$`Percent Change Price`!=0)
nkla <- subset(nkla, nkla$`Percent Change Price`!=0)
psec <- subset(psec, psec$`Percent Change Price`!=0)
spce <- subset(spce, be$`Percent Change Price`!=0)
spr <- subset(spr, spr$`Percent Change Price`!=0)
vrt <- subset(vrt, vrt$`Percent Change Price`!=0)
wsc <- subset(wsc, be$`Percent Change Price`!=0)

#Create Dummy Variables 
#Create Dummy Variable Column for if RSI < 30 
be$rsi30 <- ifelse(be$RSI<=30,1,0)
clvt$rsi30 <- ifelse(clvt$RSI<=30,1,0)
hri$rsi30 <- ifelse(hri$RSI<=30,1,0)
itrk$rsi30 <- ifelse(itrk$RSI<=30,1,0)
nkla$rsi30 <- ifelse(nkla$RSI<=30,1,0)
psec$rsi30 <- ifelse(psec$RSI<=30,1,0)
spr$rsi30 <- ifelse(spr$RSI<=30,1,0)
spce$rsi30 <- ifelse(spce$RSI<=30,1,0)
vrt$rsi30 <- ifelse(vrt$RSI<=30,1,0)
wsc$rsi30 <- ifelse(wsc$RSI<=30,1,0)

#Create Dummy Variable Column for if RSI > 70
be$rsi70 <- ifelse(be$RSI>=70,1,0)
clvt$rsi70 <- ifelse(clvt$RSI>=70,1,0)
hri$rsi70 <- ifelse(hri$RSI>=70,1,0)
itrk$rsi70 <- ifelse(itrk$RSI>=70,1,0)
nkla$rsi70 <- ifelse(nkla$RSI>=70,1,0)
psec$rsi70 <- ifelse(psec$RSI>=70,1,0)
spr$rsi70 <- ifelse(spr$RSI>=70,1,0)
spce$rsi70 <- ifelse(spce$RSI>=70,1,0)
vrt$rsi70 <- ifelse(vrt$RSI>=70,1,0)
wsc$rsi70 <- ifelse(wsc$RSI>=70,1,0)
#Create Variable Up/Down if Stock Price % Change Went "UP" or "DOWN" with ifelse() for > | < 0
be$pricechg_binary <- ifelse(be$`Percent Change Price`>=0,1,0)
clvt$pricechg_binary <- ifelse(clvt$`Percent Change Price`>=0,1,0)
hri$pricechg_binary <- ifelse(hri$`Percent Change Price`>=0,1,0)
itrk$pricechg_binary <- ifelse(itrk$`Percent Change Price`>=0,1,0)
nkla$pricechg_binary <- ifelse(nkla$`Percent Change Price`>=0,1,0)
psec$pricechg_binary <- ifelse(psec$`Percent Change Price`>=0,1,0)
spr$pricechg_binary <- ifelse(spr$`Percent Change Price`>=0,1,0)
spce$pricechg_binary <- ifelse(spce$`Percent Change Price`>=0,1,0)
vrt$pricechg_binary <- ifelse(vrt$`Percent Change Price`>=0,1,0)
wsc$pricechg_binary <- ifelse(wsc$`Percent Change Price`>=0,1,0)

# Rename Column to Date instead of ...1
be$date<- be$...1
clvt$date <- clvt$...1
hri$date <- hri$...1
itrk$date <- itrk$...1
nkla$date <- nkla$...1
psec$date <- psec$...1
spr$date <- spr$...1
spce$date <- spce$...1
vrt$date <- vrt$...1
wsc$date <- wsc$...1

be$...1 <- NULL
clvt$...1 <- NULL
hri$...1 <- NULL
itrk$...1 <- NULL
nkla$...1 <- NULL
psec$...1 <- NULL 
spr$...1 <- NULL
spce$...1 <- NULL
vrt$...1 <- NULL 
wsc$...1 <- NULL 

# Change News Publication Count NAs to 0s for all Datasets 
be$`News Publication Count` <- ifelse(is.na(be$`News Publication Count`),0,be$`News Publication Count`)
clvt$`News Publication Count` <- ifelse(is.na(clvt$`News Publication Count`),0,clvt$`News Publication Count`)
hri$`News Publication Count` <- ifelse(is.na(hri$`News Publication Count`),0,hri$`News Publication Count`)
itrk$`News Publication Count` <- ifelse(is.na(itrk$`News Publication Count`),0,itrk$`News Publication Count`)
nkla$`News Publication Count` <- ifelse(is.na(nkla$`News Publication Count`),0,nkla$`News Publication Count`)
psec$`News Publication Count` <- ifelse(is.na(psec$`News Publication Count`),0,psec$`News Publication Count`)
spce$`News Publication Count` <- ifelse(is.na(spce$`News Publication Count`),0,spce$`News Publication Count`)
spr$`News Publication Count` <- ifelse(is.na(spr$`News Publication Count`),0,spr$`News Publication Count`)
vrt$`News Publication Count` <- ifelse(is.na(vrt$`News Publication Count`),0,vrt$`News Publication Count`)
wsc$`News Publication Count` <- ifelse(is.na(wsc$`News Publication Count`),0,wsc$`News Publication Count`)



#create train/test set 
be_train  <- be[1:(nrow(be) * 0.8),]
be_test <- be[-(1:(nrow(be) * 0.8)) ,]

clvt_train  <- clvt[1:(nrow(clvt) * 0.8),]
clvt_test <- clvt[-(1:(nrow(clvt) * 0.8)) ,]

hri_train  <- hri[1:(nrow(hri) * 0.8),]
hri_test <- hri[-(1:(nrow(hri) * 0.8)) ,]

itrk_train  <- itrk[1:(nrow(itrk) * 0.8),]
itrk_test <- itrk[-(1:(nrow(itrk) * 0.8)) ,]

nkla_train  <- nkla[1:(nrow(nkla) * 0.8),]
nkla_test <- nkla[-(1:(nrow(nkla) * 0.8)) ,]

psec_train  <- psec[1:(nrow(psec) * 0.8),]
psec_test <- psec[-(1:(nrow(psec) * 0.8)) ,]

spce_train  <- spce[1:(nrow(spce) * 0.8),]
spce_test <- spce[-(1:(nrow(spce) * 0.8)) ,]

spr_train  <- spr[1:(nrow(spr) * 0.8),]
spr_test <- spr[-(1:(nrow(spr) * 0.8)) ,]


vrt_train  <- vrt[1:(nrow(vrt) * 0.8),]
vrt_test <- vrt[-(1:(nrow(vrt) * 0.8)) ,]

wsc_train  <- vrt[1:(nrow(wsc) * 0.8),]
wsc_test <- wsc[-(1:(nrow(wsc) * 0.8)) ,]

#Run logistic regression 

    """Spacs:
          - clvt - RSI significant (+0.05) neg intercept (-2.9) (61% accuracy)
          - vrt RSI +0.05, -3.15
          - wsc RSI +0.05, int -3.15
          - vrt RSI + 0.006, int +0.4, 
          - nkla RSI +0.04, int -2.5

    #Non-Spac:
      - itrk RSI 0.04, int -2
      - be RSI +0.05, int -3 (60% accuracy)
      - hri RSI + 0.04, int = -2.1 (62% accuracy)
      - spr RSI +0.04, int -2.6, News (not significant - +0.003)
      - psec RSI +0.06, int -2.9
  """
head(be)
lm.mod_be <- glm(pricechg_binary~`RSI`,data=be_train,family=binomial)
summary(lm.mod_be)


lm.mod_clvt <- glm(pricechg_binary~ RSI,data=clvt_train,family=binomial)
summary(lm.mod_clvt)

lm.mod_hri <- glm(pricechg_binary~RSI,data=hri_train,family=binomial)
summary(lm.mod_hri)

lm.mod_itrk <- glm(pricechg_binary~`RSI`,data=itrk_train,family=binomial)
summary(lm.mod_itrk)

lm.mod_nkla <- glm(pricechg_binary~`RSI`
                  ,data=nkla_train,family=binomial)
summary(lm.mod_nkla)

lm.mod_psec<- glm(pricechg_binary~`RSI`,data=psec_train,family=binomial)
summary(lm.mod_psec)

lm.mod_spr <- glm(pricechg_binary~`RSI`+`News.Publication.Count`,data=spr_train,family=binomial)
summary(lm.mod_spr)

lm.mod_spce <- glm(pricechg_binary~`RSI`
                   +`News Publication Count`,data=spce_train,family=binomial)
summary(lm.mod_spce)

lm.mod_vrt <- glm(pricechg_binary~RSI,data=vrt_train,family=binomial)
summary(lm.mod_vrt)

lm.mod_wsc <- glm(pricechg_binary~RSI,data=wsc_train,family=binomial)
summary(lm.mod_wsc)

#Check on train set 
threshold <- .5



be_test$pred <- predict(lm.mod_be,newdata=be_test,type='response')
be_test$pred_final <- ifelse(be_test$pred >= threshold,1,0)
#accuracy 
sum(be_test$pred_final == be_test$pricechg_binary)/nrow(be_test)
#confusion matrix
table(be_test$pricechg_binary, be_test$pred_final)



clvt_test$pred <- predict(lm.mod_clvt,newdata=clvt_test,type='response')
clvt_test$pred_final <- ifelse(clvt_test$pred >= threshold,1,0)
#accuracy 
sum(clvt_test$pred_final == clvt_test$pricechg_binary)/nrow(clvt_test)
#confusion matrix
table(clvt_test$pricechg_binary, clvt_test$pred_final)




hri_test$pred <- predict(lm.mod_hri,newdata=hri_test,type='response')
hri_test$pred_final <- ifelse(hri_test$pred >= threshold,1,0)
#accuracy
sum(hri_test$pred_final == hri_test$pricechg_binary)/nrow(hri_test)
#confusion matrix
table(hri_test$pricechg_binary, hri_test$pred_final)



itrk_test$pred <- predict(lm.mod_itrk,newdata=itrk_test,type='response')
itrk_test$pred_final <- ifelse(itrk_test$pred >= threshold,1,0)
#Accuracy
sum(itrk_test$pred_final == itrk_test$pricechg_binary)/nrow(itrk_test)
#Confusion matrix
table(itrk_test$pricechg_binary, itrk_test$pred_final)


nkla_test$pred <- predict(lm.mod_nkla,newdata=nkla_test,type='response')
nkla_test$pred_final <- ifelse(nkla_test$pred >= threshold,1,0)
#Accuracy 
sum(nkla_test$pred_final == nkla_test$pricechg_binary)/nrow(nkla_test)
#Confusion Matrix
table(nkla_test$pricechg_binary, nkla_test$pred_final)


psec_test$pred <- predict(lm.mod_psec,newdata=psec_test,type='response')
psec_test$pred_final <- ifelse(psec_test$pred >= threshold,1,0)
#Accuracy
sum(psec_test$pred_final == psec_test$pricechg_binary)/nrow(psec_test)
#Confusion Matrix
table(psec_test$pricechg_binary, psec_test$pred_final)


spce_test$pred <- predict(lm.mod_spce,newdata=spce_test,type='response')
spce_test$pred_final <- ifelse(spce_test$pred >= threshold,1,0)
#Accuracy
sum(spce_test$pred_final == spce_test$pricechg_binary)/nrow(spce_test)
#Confusion Matrix
table(spce_test$pricechg_binary, spce_test$pred_final)


spr_test$pred <- predict(lm.mod_spr,newdata=spr_test,type='response')
spr_test$pred_final <- ifelse(spr_test$pred >= threshold,1,0)
#accuracy 
sum(spr_test$pred_final == spr_test$pricechg_binary)/nrow(spr_test)
#Confusion Matrix
table(spr_test$pricechg_binary, spr_test$pred_final)



vrt_test$pred <- predict(lm.mod_vrt,newdata=vrt_test,type='response')
vrt_test$pred_final <- ifelse(vrt_test$pred >= threshold,1,0)
#Accuracy
sum(vrt_test$pred_final == vrt_test$pricechg_binary)/nrow(vrt_test)
#Confusion Matrix
table(vrt_test$pricechg_binary, vrt_test$pred_final)


wsc_test$pred <- predict(lm.mod_wsc,newdata=wsc_test,type='response')
wsc_test$pred_final <- ifelse(wsc_test$pred >= threshold,1,0)
#Accuracy 
sum(wsc_test$pred_final == wsc_test$pricechg_binary)/nrow(wsc_test)
#Confusion Matrix
table(wsc_test$pricechg_binary, wsc_test$pred_final)


#TO DO: Linear Regression - target variable % change 
head(be_train)
lm.mod_be <- lm(`Percent.Change.Price`~RSI,data=be_train)
summary(lm.mod_be)

lm.mod_clvt <- lm(pricechg_binary~ RSI,data=clvt_train)
summary(lm.mod_clvt)

lm.mod_hri <- lm(pricechg_binary~RSI,data=hri_train)
summary(lm.mod_hri)

lm.mod_itrk <- lm(pricechg_binary~`RSI`,data=itrk_train)
summary(lm.mod_itrk)

lm.mod_nkla <- lm(pricechg_binary~`RSI`
                 ,data=nkla_train)
summary(lm.mod_nkla)

lm.mod_psec<- lm(pricechg_binary~`RSI`+`News.Publication.Count`,data=psec_train)
summary(lm.mod_psec)

lm.mod_spr <- lm(pricechg_binary~`RSI`+`News.Publication.Count`,data=spr_train)
summary(lm.mod_spr)

lm.mod_spce <- lm(pricechg_binary~`RSI`
                  +`News.Publication.Count`,data=spce_train)
summary(lm.mod_spce)

lm.mod_vrt <- lm(pricechg_binary~RSI,data=vrt_train)
summary(lm.mod_vrt)

lm.mod_wsc <- lm(pricechg_binary~RSI,data=wsc_train)
summary(lm.mod_wsc)



#Creating Trees 
best <- 3
# Have to change the column names to be without spaces for it to work unfortunately 
names(be) <- make.names(names(be))
names(clvt) <- make.names(names(clvt))
names(itrk) <- make.names(names(itrk))
names(hri) <- make.names(names(hri))
names(nkla) <- make.names(names(nkla))
names(psec) <- make.names(names(psec))
names(spce) <- make.names(names(spce))
names(spr) <- make.names(names(spr))
names(vrt) <- make.names(names(vrt))
names(wsc) <- make.names(names(wsc))

#Redo train/test set
be_train  <- be[1:(nrow(be) * 0.8),]
be_test <- be[-(1:(nrow(be) * 0.8)) ,]

clvt_train  <- clvt[1:(nrow(clvt) * 0.8),]
clvt_test <- clvt[-(1:(nrow(clvt) * 0.8)) ,]

hri_train  <- hri[1:(nrow(hri) * 0.8),]
hri_test <- hri[-(1:(nrow(hri) * 0.8)) ,]

itrk_train  <- itrk[1:(nrow(itrk) * 0.8),]
itrk_test <- itrk[-(1:(nrow(itrk) * 0.8)) ,]

nkla_train  <- nkla[1:(nrow(nkla) * 0.8),]
nkla_test <- nkla[-(1:(nrow(nkla) * 0.8)) ,]

psec_train  <- psec[1:(nrow(psec) * 0.8),]
psec_test <- psec[-(1:(nrow(psec) * 0.8)) ,]

spce_train  <- spce[1:(nrow(spce) * 0.8),]
spce_test <- spce[-(1:(nrow(spce) * 0.8)) ,]

spr_train  <- spr[1:(nrow(spr) * 0.8),]
spr_test <- spr[-(1:(nrow(spr) * 0.8)) ,]


vrt_train  <- vrt[1:(nrow(vrt) * 0.8),]
vrt_test <- vrt[-(1:(nrow(vrt) * 0.8)) ,]

wsc_train  <- vrt[1:(nrow(vrt) * 0.8),]
wsc_test <- wsc[-(1:(nrow(wsc) * 0.8)) ,]

#BE
library(tree)
head(clvt_train)
tree.appt_be <- tree(pricechg_binary ~ . - Percent.Change.Price - date - rsi30 - rsi70 
                     , data = be_train)
colnames(clvt_train)
prune_be <- prune.tree(tree.appt_be,best=best)

plot(prune_be)
text(prune_be, pretty = 0)
predict(prune_be,newdata= be_test)

#confusion matrix
table(be_test$pricechg_binary, ifelse(predict(prune_be,newdata= be_test)>threshold,1,0))
#Accuracy 
sum(ifelse(predict(prune_be,newdata= be_test)>threshold,1,0) == be_test$pricechg_binary)/nrow(be_test)


#CLVT
tree.appt_clvt <- tree(pricechg_binary ~  . - Percent.Change.Price - date - rsi30 - rsi70-RSI.Percent - Volume
                       , data = clvt_train)

prune_clvt <- prune.tree(tree.appt_clvt,best=best)
plot(prune_clvt)
text(prune_clvt, pretty = 0)

#Confusion Matrix
table(clvt_test$pricechg_binary, ifelse(predict(prune_clvt,newdata= clvt_test)>threshold,1,0))
#Accuracy 
sum(ifelse(predict(prune_clvt,newdata= clvt_test)>threshold,1,0) == clvt_test$pricechg_binary)/nrow(clvt_test)

#HRI 
tree.appt_hri <- tree(pricechg_binary ~ . - Percent.Change.Price - date - rsi30 - rsi70 - Volume
                      , data = hri_train)
prune_hri <- prune.tree(tree.appt_hri,best=best)
plot(prune_hri)
text(prune_hri, pretty = 0)

#Confusion Matrix
table(hri_test$pricechg_binary, ifelse(predict(prune_hri,newdata= hri_test)>threshold,1,0))
#Accuracy 
sum(ifelse(predict(prune_hri,newdata= hri_test)>threshold,1,0) == hri_test$pricechg_binary)/nrow(hri_test)

plot(lm.mod_clvt)


#ITRK 
head(itrk_train)
tree.appt_itrk <- tree(pricechg_binary ~ . - Percent.Change.Price - date - rsi30 - rsi70 - Volume
                       , data = itrk_train)

prune_itrk <- prune.tree(tree.appt_itrk,best=best)
plot(prune_itrk)
text(prune_itrk, pretty = 0)

#Confusion Matrix
table(itrk_test$pricechg_binary, ifelse(predict(prune_itrk,newdata= itrk_test)>threshold,1,0))
#Accuracy 
sum(ifelse(predict(prune_itrk,newdata= itrk_test)>threshold,1,0) == itrk_test$pricechg_binary)/nrow(itrk_test)

#NKLA
tree.appt_nkla <- tree(pricechg_binary ~  . - Percent.Change.Price - date - rsi30 - rsi70 - Volume
                       , data = nkla_train)

prune_nkla <- prune.tree(tree.appt_nkla,best=best)
plot(prune_nkla)
text(prune_nkla, pretty = 0)

#Confusion Matrix 
table(nkla_test$pricechg_binary, ifelse(predict(prune_nkla,newdata= nkla_test)>threshold,1,0))
#Accuracy 
sum(ifelse(predict(prune_nkla,newdata= nkla_test)>threshold,1,0) == nkla_test$pricechg_binary)/nrow(nkla_test)


#PSEC
tree.appt_psec <- tree(pricechg_binary ~ . - Percent.Change.Price - date - rsi30 - rsi70 - Volume
                       , data = psec_train)

prune_psec <- prune.tree(tree.appt_psec,best=best)
plot(prune_psec)
text(prune_psec, pretty = 0)

#Confusion Matrix
table(psec_test$pricechg_binary, ifelse(predict(prune_psec,newdata= psec_test)>threshold,1,0))
#Accuracy 
sum(ifelse(predict(prune_psec,newdata= psec_test)>threshold,1,0) == psec_test$pricechg_binary)/nrow(psec_test)

#SPCE
tree.appt_spce <- tree(pricechg_binary ~ . - Percent.Change.Price - date - rsi30 - rsi70 - Volume
                       , data = spce_train)

prune_spce <- prune.tree(tree.appt_spce,best=best)
plot(prune_spce)
text(prune_spce, pretty = 0)

#Confusion Matrix 
table(spce_test$pricechg_binary, ifelse(predict(prune_spce,newdata= spce_test)>threshold,1,0))
#Accuracy 
sum(ifelse(predict(prune_spce,newdata= spce_test)>threshold,1,0) == spce_test$pricechg_binary)/nrow(spce_test)

#SPR
head(spr)
tree.appt_spr <- tree(pricechg_binary ~ . - Percent.Change.Price - date - rsi30 - rsi70 - Volume
                      , data = spr_train)

prune_spr <- prune.tree(tree.appt_spr,best=best)
plot(prune_spr)
text(prune_spr, pretty = 0)

#Confusion Matrix
table(spr_test$pricechg_binary, ifelse(predict(prune_spr,newdata= spr_test)>threshold,1,0))
#Accuracy
sum(ifelse(predict(prune_spr,newdata= spr_test)>threshold,1,0) == spr_test$pricechg_binary)/nrow(spr_test)

#VRT
tree.appt_vrt <- tree(pricechg_binary ~  . - Percent.Change.Price - date - rsi30 - rsi70 - Volume
                      , data = vrt_train)

prune_vrt <- prune.tree(tree.appt_vrt,best=best)
plot(prune_vrt)
text(prune_vrt, pretty = 0)

#Confusion Matrix
table(vrt_test$pricechg_binary, ifelse(predict(prune_vrt,newdata= vrt_test)>threshold,1,0))

#Accuracy 
sum(ifelse(predict(prune_vrt,newdata= vrt_test)>threshold,1,0) == vrt_test$pricechg_binary)/nrow(vrt_test)

#WSC
tree.appt_wsc <- tree(pricechg_binary ~ . - Percent.Change.Price - date - rsi30 - rsi70 - Volume
                      , data = wsc_train)

prune_wsc <- prune.tree(tree.appt_wsc,best=best)
plot(prune_wsc)
text(prune_wsc, pretty = 0)

#Confusion Matrix
table(wsc_test$pricechg_binary, ifelse(predict(prune_wsc,newdata= wsc_test)>threshold,1,0))
#Accuracy
sum(ifelse(predict(prune_wsc,newdata= wsc_test)>threshold,1,0) == wsc_test$pricechg_binary)/nrow(wsc_test)
