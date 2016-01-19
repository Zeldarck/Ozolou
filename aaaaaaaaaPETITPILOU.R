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

ozone = read.table("H:/ozone.dat",header=TRUE)
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
ozone.train = ozone[train,]
dim(ozone.train)

#Linéaire

#Logistique

#LDA

#QDA