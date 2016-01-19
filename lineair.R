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

ozone = read.table("C:/Users/Etienne/Documents/GitHub/Ozolou/ozone.dat",header=TRUE)
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

train = sample(1:1041,833)
ozone.test = ozone[-train,]
ozone = ozone[train,]
dim(ozone.train)

#Linéaire
summary(ozone)
head(ozone)
linear = lm(O3obs~as.factor(JOUR)+MOCAGE+TEMPE+STATION+VentMOD+VentANG+SRMH2O+LNO2+LNO,data=ozone)
summary(linear)
plot(residuals(linear)~fitted(linear))
plot(fitted(linear)~ozone$MOCAGE)

#rég linéaire pertinente
linear2 = lm(O3obs~MOCAGE+TEMPE+STATION+VentMOD+VentANG+LNO2+LNO,data=ozone)
summary(linear2)

#Logistique

#########################
#Classificaion
head(ozone[1:4])
head(ozone[,"DepSeuil"])
par(mar=c(0,0,0,0))
pan=function(x,y){
  xy=cbind.data.frame(x,y)
  s.class(xy,ozone$DepSeuil,include.ori=F,add.p=T,clab=1.5,col=c("red","green"),cpoi=2,csta=0.5)
}
pairs(ozone[,2:9],panel=pan)


ozone2 = ozone[,-1]
ozone2 = ozone2[,-4]
head(ozone2)

##marche pas :
var.names=c("O3obs","MOCAGE","TEMPE","VentMod","VentAng","SRMH2O","LNO2","LNO")
var.nb = length(var.names)
for(k in 1:var.nb){
  print(var.names[k])
  print(anova(aov(ozone[,k]~O3obs,data=ozone)))
}

#LDA

#QDA