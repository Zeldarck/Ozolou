#######################################################################################
#                                                                                     #
#                    Projet: Apprentissage et aide � la d�cision                      #
#                                                                                     #
#######################################################################################



#######################################################################################
#                                                                                     #
#                        Lecture et pr�paration des donn�es                           #
#                                                                                     #
#######################################################################################

# lecture des donn�es

ozone = read.table("C:/Users/User/Documents/GitHub/Ozolou/ozone.dat",header=TRUE)
attach(ozone)

dim(ozone)
summary(ozone)

# changement du type de la variable jour

ozone[,"JOUR"]=as.factor(ozone[,"JOUR"])

# histogrammes des variables initiales

par(mfrow=c(2,4))
hist(ozone[,"O3obs"]);hist(ozone[,"MOCAGE"]);hist(ozone[,"TEMPE"]);hist(ozone[,"RMH2O"]);
hist(ozone[,"NO2"]);hist(ozone[,"NO"]);hist(ozone[,"VentMOD"]);hist(ozone[,"VentANG"]);

# transformation de variables et trac� des histogrammes apr�s transformations

ozone[,"SRMH2O"]=sqrt(ozone[,"RMH2O"])
ozone[,"LNO2"]=log(ozone[,"NO2"])
ozone[,"LNO"]=log(ozone[,"NO"])

par(mfrow=c(2,4))
hist(ozone[,"O3obs"]);hist(ozone[,"MOCAGE"]);hist(ozone[,"TEMPE"]);hist(ozone[,"SRMH2O"]);
hist(ozone[,"LNO2"]);hist(ozone[,"LNO"]);hist(ozone[,"VentMOD"]);hist(ozone[,"VentANG"]);

# suppression des variables inutiles pour la suite et cr�ation de la variable-r�ponse (variable binaire � expliquer): DepSeuil

ozone=ozone[,c(1:4,8:13)]
ozone[,"DepSeuil"]=as.factor(ozone[,"O3obs"]>150)

#######################################################################################
#                                                                                     #
#                                 D�but du travail                                    #
#                                                                                     #
#######################################################################################
set.seed(2)
train = sample(1:1041,833)
ozone.test = ozone[-train,]
ozone = ozone[train,]

#Lin�aire


#Logistique
ozone <- ozone[,!colnames(ozone)=="O3obs"]


m.logit1= glm(DepSeuil~JOUR+MOCAGE+TEMPE+as.factor(STATION)+VentMOD+VentANG+SRMH2O+LNO2+LNO,data=ozone,family=binomial(link=logit))
summary(m.logit1)
##On retire JOUR,MOCAGE,VENTANG au vue des p value
m.logit2= glm(DepSeuil~TEMPE+as.factor(STATION)+VentMOD+SRMH2O+LNO2+LNO,data=ozone,family=binomial(link=logit))
summary(m.logit2)
##On devrai peut etre retir� STATION

##comparaison des deux mod�le:
anova(m.logit1,m.logit2,test="Chisq")
##H0 rejet�, donc m.logit2 mieux que m.logit1

##On essayes sans STATION 
m.logit3= glm(DepSeuil~TEMPE+VentMOD+SRMH2O+LNO2+LNO,data=ozone,family=binomial(link=logit))
summary(m.logit3)
#On devrai enelver VentMOD selon ce mod�le
anova(m.logit2,m.logit3,test="Chisq")
##H0 conserv�, donc m.logit2 mieux que m.logit3


#On test avec une proc�dure pas � pas depuis le mod�lede base
m.step = step(m.logit1,direction="both")
#On obtiens ainsi le m�me mod�le que m.logit2

library(pROC)
test=predict(m.logit2,newdata=ozone.test,type="response")
plot.roc(ozone.test$DepSeuil,test,legacy.axes=TRUE,print.thres="best",col="blue",auc.polygon=TRUE,print.auc=TRUE)
#Il y a donc un seuil le mieux de mieux 0.213

#Mesure des levier
ozone$h = hatvalues(m.logit2)

plot(ozone$h,type="h",ylab=expression(H[ii]),ylim=c(0,.8))
#p = 10, n = 833
abline(h=30/833)
#une valeur tr�s levier =>405

plot(ozone$STDpearson[405]~ozone$DepSeuil[405],ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

plot(ozone$STDdeviance[405]~ozone$DepSeuil[405],ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")
#On voie que la valeur n'est pas ab�rante, on la garde donc

#for(i in 1:ncol(ozone$h)){
 # if (ozone$h[i]==0) levier[i]=ozone$h[i]
#}
#levier = 

#retrait de la valeur ab�rante
#ozone<-ozone[-405,]
#dim(ozone)
#m.logit2= glm(DepSeuil~TEMPE+as.factor(STATION)+VentMOD+SRMH2O+LNO2+LNO,data=ozone,family=binomial(link=logit))
#summary(m.logit2)
#ozone$h = hatvalues(m.logit2)

#plot(ozone$h,type="h",ylab=expression(H[ii]),ylim=c(0,.8))
#p = 10, n = 832
#abline(h=20/832)
#abline(h=30/832)

#test=predict(m.logit2,newdata=ozone.test,type="response")
#plot.roc(ozone.test$DepSeuil,test,legacy.axes=TRUE,print.thres="best",col="blue",auc.polygon=TRUE,print.auc=TRUE)
#Il y a donc un seuil le mieux de mieux 0.210



#Residus de Pearson
ozone$pearson = resid(m.logit2,type="pearson")
ozone$STDpearson = ozone$pearson/sqrt(1-ozone$h)

#Residus de d�viance
ozone$deviance = resid(m.logit2,type="deviance")
ozone$STDdeviance = ozone$deviance/sqrt(1-ozone$h)



###sais pas trop quoi faire de �a
plot(ozone$pearson~ozone$DepSeuil,ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

plot(ozone$STDpearson[405]~ozone$DepSeuil[405],ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

plot(ozone$deviance~ozone$DepSeuil,ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

plot(ozone$STDdeviance[405]~ozone$DepSeuil[405],ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

cook=cooks.distance(m.logit2)




#LDA

#QDA