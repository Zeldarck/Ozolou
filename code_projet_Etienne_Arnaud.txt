#######################################################################################
#                                                                                     #
#                    Projet: Apprentissage et aide à la décision                      #
#                                                                                     #
#######################################################################################



#######################################################################################
#                                                                                     #
#                        Lecture et préparation des données                           #
#                                                                                     #
#######################################################################################

# lecture des données

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

# transformation de variables et tracé des histogrammes après transformations

ozone[,"SRMH2O"]=sqrt(ozone[,"RMH2O"])
ozone[,"LNO2"]=log(ozone[,"NO2"])
ozone[,"LNO"]=log(ozone[,"NO"])

par(mfrow=c(2,4))
hist(ozone[,"O3obs"]);hist(ozone[,"MOCAGE"]);hist(ozone[,"TEMPE"]);hist(ozone[,"SRMH2O"]);
hist(ozone[,"LNO2"]);hist(ozone[,"LNO"]);hist(ozone[,"VentMOD"]);hist(ozone[,"VentANG"]);

# suppression des variables inutiles pour la suite et création de la variable-réponse (variable binaire à expliquer): DepSeuil

ozone=ozone[,c(1:4,8:13)]
ozone[,"DepSeuil"]=as.factor(ozone[,"O3obs"]>150)

#######################################################################################
#                                                                                     #
#                                 Début du travail                                    #
#                                                                                     #
#######################################################################################

#Séparation en échantillons : apprentissage et test
set.seed(2)
train = sample(1:1041,833)
ozone.test = ozone[-train,]
ozone = ozone[train,]

################################################
#brève régression linéaire
#Linéaire
summary(ozone)
head(ozone)
m.linear = lm(O3obs~JOUR+MOCAGE+TEMPE+STATION+VentMOD+VentANG+SRMH2O+LNO2+LNO,data=ozone)
summary(m.linear)
plot(residuals(m.linear)~fitted(m.linear))
plot(fitted(m.linear)~ozone$MOCAGE)
#rég linéaire possible ? R² mauvais
m.linear2 = lm(O3obs~MOCAGE+TEMPE+STATION+VentMOD+VentANG+LNO2+LNO,data=ozone)
summary(m.linear2)
#après enlever les variables =0 selon le test, on a tj un R² petit, donc modèle pas adapté

################################################
#Logistique
ozone <- ozone[,!colnames(ozone)=="O3obs"]


m.logit1= glm(DepSeuil~JOUR+MOCAGE+TEMPE+as.factor(STATION)+VentMOD+VentANG+SRMH2O+LNO2+LNO,data=ozone,family=binomial(link=logit))
summary(m.logit1)
##On retire JOUR,MOCAGE,VENTANG au vue des p value
m.logit2= glm(DepSeuil~TEMPE+as.factor(STATION)+VentMOD+SRMH2O+LNO2+LNO,data=ozone,family=binomial(link=logit))
summary(m.logit2)
##On devrai peut etre retiré STATION

##comparaison des deux modèle:
anova(m.logit1,m.logit2,test="Chisq")
##H0 rejeté, donc m.logit2 mieux que m.logit1

##On essayes sans STATION 
m.logit3= glm(DepSeuil~TEMPE+VentMOD+SRMH2O+LNO2+LNO,data=ozone,family=binomial(link=logit))
summary(m.logit3)
#On devrai enelver VentMOD selon ce modèle
anova(m.logit2,m.logit3,test="Chisq")
##H0 conservé, donc m.logit2 mieux que m.logit3


#On test avec une procédure pas à pas depuis le modèlede base
m.step = step(m.logit1,direction="both")
#On obtiens ainsi le même modèle que m.logit2

library(pROC)
test2=predict(m.logit2,newdata=ozone.test,type="response")
plot.roc(ozone.test$DepSeuil,test2,legacy.axes=TRUE,print.thres="best",col="blue",auc.polygon=TRUE,print.auc=TRUE)
#Il y a donc un seuil le mieux de mieux 0.213

#Mesure des levier
ozone$h = hatvalues(m.logit2)

plot(ozone$h,type="h",ylab=expression(H[ii]),ylim=c(0,.8))
#p = 10, n = 833
abline(h=30/833)
#plusieurs valeurs leviées

#Residus de Pearson
ozone$pearson = resid(m.logit2,type="pearson")
ozone$STDpearson = ozone$pearson/sqrt(1-ozone$h)

#Residus de déviance
ozone$deviance = resid(m.logit2,type="deviance")
ozone$STDdeviance = ozone$deviance/sqrt(1-ozone$h)


plot(ozone$STDpearson[ozone$h>30/833],ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

plot(ozone$STDdeviance[ozone$h>30/833],ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")
#Il ya à une valeur abérante levier selon pearson, nous décidons de la retirée, sont index est 354
aberantes = 354;


#retrait de la valeur abérante
m.logit4= glm(DepSeuil~TEMPE+as.factor(STATION)+VentMOD+SRMH2O+LNO2+LNO,data=ozone[-aberantes,],family=binomial(link=logit))
summary(m.logit4)
ozone.h = hatvalues(m.logit4)


#Residus de Pearson
ozone.pearson = resid(m.logit4,type="pearson")
ozone.STDpearson = ozone.pearson/sqrt(1-ozone.h)

#Residus de déviance
ozone.deviance = resid(m.logit4,type="deviance")
ozone.STDdeviance = ozone.deviance/sqrt(1-ozone.h)


plot(ozone.h,type="h",ylab=expression(H[ii]),ylim=c(0,.8))
#p = 10, n = 832
abline(h=30/832)


plot(ozone.STDpearson[ozone.h>30/832],ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

plot(ozone.STDdeviance[ozone.h>30/832],ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

#plus de valeur abérante

test4=predict(m.logit4,newdata=ozone.test,type="response")
plot.roc(ozone.test$DepSeuil,test4,legacy.axes=TRUE,print.thres="best",col="blue",auc.polygon=TRUE,print.auc=TRUE)
#Il y a donc un seuil le mieux de mieux 0.096

y.test2=as.numeric(test2>0.213)
y.test4=as.numeric(test4>0.096)

table(obs=ozone.test$DepSeuil,pred=y.test2)
table(obs=ozone.test$DepSeuil,pred=y.test4)

#S'il n'est pas grave de faire de faux positif, alorsle modèle m.logit4 est plus adaptés, sinon c'est le m.logit2

################################################
#Classificaion

################################################
#LDA sous hypothèse de normalité
library(MASS)
#Modèle lda avec toutes les variables
m.lda=lda(DepSeuil~JOUR+MOCAGE+TEMPE+as.factor(STATION)+VentMOD+VentANG+SRMH2O+LNO2+LNO,data=ozone)
m.lda
predlda=predict(object=m.lda,newdata=ozone.test)
lda.pred = predict(m.lda)
names(lda.pred)
#matrice de confusion
table(ozone.test[,"DepSeuil"],predlda$class)

var.names=c("MOCAGE","TEMPE","VentMOD","VentANG","SRMH2O","LNO2","LNO")
nb_var = length(var.names)
for(k in 1:nb_var){
  print(var.names[k])
  print(anova(aov(ozone[,var.names[k]]~DepSeuil,data=ozone)))
}
#On enlève les var : LNO,SRMH2O,VentAng,LNO2

#modèle LDA avec les varialbles JOUR, MOCAGE, TEMPE, STATION, VentMOD
m.lda2=lda(DepSeuil~JOUR+MOCAGE+TEMPE+as.factor(STATION)+VentMOD,data=ozone)
m.lda2
predlda2=predict(object=m.lda2,newdata=ozone.test)
table(ozone.test[,"DepSeuil"],predlda2$class)

#modèle LDA avec les varialbles MOCAGE, TEMPE, STATION, VentMOD
m.lda3=lda(DepSeuil~MOCAGE+TEMPE+as.factor(STATION)+VentMOD,data=ozone)
m.lda3
predlda3=predict(object=m.lda3,newdata=ozone.test)
table(ozone.test[,"DepSeuil"],predlda3$class)

#modèle LDA avec les varialbles JOUR, MOCAGE, TEMPE, VentMOD
m.lda4=lda(DepSeuil~JOUR+MOCAGE+TEMPE+VentMOD,data=ozone)
m.lda4
predlda4=predict(object=m.lda4,newdata=ozone.test)
table(ozone.test[,"DepSeuil"],predlda4$class)
#Moins bien sans station

#modèle LDA avec les varialbles MOCAGE, TEMPE, VentMOD
m.lda5=lda(DepSeuil~MOCAGE+TEMPE+VentMOD,data=ozone)
m.lda5
predlda5=predict(object=m.lda5,newdata=ozone.test)
table(ozone.test[,"DepSeuil"],predlda5$class)
#On confirme la présence de Station dans le modèle

#modèle LDA avec les varialbles JOUR, MOCAGE, TEMPE, STATION
m.lda6=lda(DepSeuil~as.factor(JOUR)+MOCAGE+TEMPE+as.factor(STATION),data=ozone)

predlda6=predict(object=m.lda6,newdata=ozone.test)
table(ozone.test[,"DepSeuil"],predlda6$class)
#Ce modèle est mieux quand on enlève VentMOD

#le modèle LDA avec les varialbles JOUR, MOCAGE, TEMPE, STATION semble le plus adapté4

################################################
#QDA
library(MASS)
m.qda=qda(DepSeuil~JOUR+MOCAGE+TEMPE+as.factor(STATION)+VentMOD+VentANG+SRMH2O+LNO2+LNO,data=ozone)
predqda=predict(object=m.qda,newdata=ozone.test)
head(predqda$class)
head(predqda$posterior)

table(ozone.test$DepSeuil,predqda$class)

var.names=c("MOCAGE","TEMPE","VentMOD","VentANG","SRMH2O","LNO2","LNO")
nb_var = length(var.names)
for(k in 1:nb_var){
  print(var.names[k])
  print(anova(aov(ozone[,var.names[k]]~DepSeuil,data=ozone)))
}
#VentANG,SRMH2o,LNO2 et LNO bof

m.qda=qda(DepSeuil~JOUR+MOCAGE+TEMPE+VentMOD,data=ozone)
predqda=predict(object=m.qda,newdata=ozone.test)
table(ozone.test$DepSeuil,predqda$class)

m.qda=qda(DepSeuil~MOCAGE+TEMPE+as.factor(STATION)+VentMOD,data=ozone)
predqda=predict(object=m.qda,newdata=ozone.test)
table(ozone.test$DepSeuil,predqda$class)

m.qda=qda(DepSeuil~MOCAGE+TEMPE+VentMOD,data=ozone)
predqda=predict(object=m.qda,newdata=ozone.test)
table(ozone.test$DepSeuil,predqda$class)

m.qda=qda(DepSeuil~JOUR+MOCAGE+TEMPE+as.factor(STATION)+VentMOD,data=ozone)
predqda=predict(object=m.qda,newdata=ozone.test)
table(ozone.test$DepSeuil,predqda$class)

#On garde JOUR et STATION car le modèle as une meilleur spécificité ET sensitivité


################################################
#Choix du modèle

#qda
table(ozone.test$DepSeuil,predqda$class)
#sensibilité
23/32
#spécificité
160/176

#lda
table(ozone.test[,"DepSeuil"],predlda6$class)
#sensibilité
16/32
#spécificité
169/176

#logit2
table(obs=ozone.test$DepSeuil,pred=y.test2)
#sensibilité
28/32
#spécificité
150/176

#logit4
table(obs=ozone.test$DepSeuil,pred=y.test4)
#sensibilité
31/32
#spécificité
130/176
  
#On choisi le m.logit2 qui  maximise au mieux la sensibilité et la spécificité
