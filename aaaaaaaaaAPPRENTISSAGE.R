#Exos 1

challenger = read.table("http://dupuy.perso.math.cnrs.fr/enseignement/RLogistique/challenger.txt",header=FALSE)
colnames(challenger)=c("Lancer","Temp","StressT")
attach(challenger)


plot(StressT~Temp,challenger,xlim=c(50,85),xlab="Temperature",ylab="Joint damage",main="Incidence of booster joint damage")

plot(jitter(StressT,amount=0.01)~Temp,challenger,xlim=c(50,85),xlab="Temperature",ylab="Joint damage",main="Incidence of booster joint damage")

m.lineaire = lm(StressT~Temp)
summary(m.lineaire)
abline(m.lineaire)

predict(m.lineaire,newdata=data.frame(Temp=31),type='response')

#on récupère les résidus

residus = residuals(m.lineaire)
prediction = predict(m.lineaire,newdata=data.frame(Temp),type='response')

plot(residus~prediction)

#OU plot(residus~fitted(m.lineaire))


#les residus ont l'air linéairement liés, on ne respecte donc pas les hypothèse du modèle linéaire
#variance non constante

# Y = prend des valeurs entre 0 et ni, combien de joint abimé pour la température xi
# n = nombre d'observation à la temérature xi 
# p = poba de détérioration à la temp xi

# si tous les xi sont différents : bernouilli car ni =1  Bin(1,pi)=Ber(pi)



x= seq(-5,5,0.01)
plot(x,exp(x)/(1+exp(x)),type="l")
lines(x,pnorm(x),type="l",col="blue")



logitmod = glm(StressT~Temp,data=challenger,family="binomial")
summary(logitmod)
#si Pr < 0.05 => le Bi = 0, statistique de test : z value
#z value = Estimate / std Error
# pour trouver Bchapo on fait la dérivée de la vraissemblence par B0 et B1 pour trouver le max (cad quand derivée = 0)
#l'info de Fisher estimée donne avec sa diago les valeur des Std error

confint.default(logitmod)
summary(logitmod)$coeff[1]+1.96*summary(logitmod)$coeff[3]
summary(logitmod)$coeff[1]-1.96*summary(logitmod)$coeff[3]

library(faraway)
plot(StressT~Temp,challenger,xlim=c(25,85),ylim=c(0,1),xlab="Temp'",ylab="Prob' of damage")
x=seq(25,85,1)
lines(x,ilogit(15.0429-0.2322*x))

probitmod=glm(StressT~Temp,data=challenger,family=binomial(link=probit))
summary(probitmod)

lines(x,pnorm(8.774903-0.135096*x),col="red")
#car le truc ça suis une lois normale

#Deviance d'un modèle M par rapport au modèle Msaturé
#D(M) = 2*ln ( L(M)/L(Msat) )= 2  * ( lnL (Msat) - lnL (M) )
#Modèle sat : on suppose que la prévision est parfaite, donc valeur prédite = valeur mesurées.
#donc si deviance petite, s'cool

summary(logitmod)$deviance
#20.31519

p=logitmod$fitted.values

-2*(sum(StressT*log(p)+(1-StressT)*log(1-p)))

logitmod2 = glm(StressT~1,data=challenger,family="binomial")
summary(logitmod2)$deviance


anova(logitmod2,logitmod)
anova(logitmod2,logitmod,test="Chisq")

qchisq(0.95,1)
#si D(1)-D(2) > à ça, on rejette H0. De même si p value < 5%

pvalue= 1-pchisq(7.952,1)

pvalue
#Donc le modèle avec  variable est mieux

#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
#EXO3
limule = read.table("http://dupuy.perso.math.cnrs.fr/enseignement/RLogistique/limule.txt",header=FALSE)
colnames(limule)=c("color","spine","width","satell","weight")
limule$y=ifelse(limule$satell>0,1,0)
attach(limule)
limule$weight=weight/1000

set.seed(40)
n=nrow(limule)
testind=sample(1:n,50)
apprind=setdiff(1:n,testind)
limappr=limule[apprind,]
limtest=limule[testind,]
attach(limappr)


m.lineaire=lm(y~weight)
summary(m.lineaire)
predict(m.lineaire,newdata=data.frame(weight=5.2),type='response')
plot(y~weight)
abline(m.lineaire)

plot(residuals(m.lineaire)~fitted(m.lineaire))

#la valeur prédie est >1 r le modèle est cmpris entre 0 et 1.
#De plus les données ne suivent pas du tout le modèle de la courbe
#les résidus sont structurés, donc sont liés et il n'y as pas homoscédaticité

#La loi pourrait être Bernouilli

fmin=function(beta,Z,yobs){
p=Z%*%beta
-sum(log(p^yobs*(1-p)^(1-yobs)))
}
fit=optim(par=c(0.1,0.1),fn=fmin,Z=cbind(rep(1,nrow(limappr)),limappr$weight),y=limappr$y,hessian=TRUE)
fit

-0.2835495+5.2*0.3752124
#Donne 1.667, spo très bon comme >1

plot(y~weight)
abline(m.lineaire)
x= seq(-5,5,0.01)
lines(x,-0.2835495+x*0.3752124,col="red")


solve(fit$hessian)
#ecart type B1chapo
sqrt(0.03197861)
#ecart type B2chapo
sqrt(0.004199913)

#equivalent à sqrt(diag(solve(fit$hessian)))
#Ce modèle pue



plot(weight,col=y+2,pch=18)

m.logit=glm(y~weight,family=binomial(link=logit))
summary(m.logit)

logitpi=m.logit$coef[1] + m.logit$coef[2]*5.2
logitpi

predict(m.logit,newdata=data.frame(weight=5.2),type="link")
predict(m.logit,newdata=data.frame(weight=5.2),type='response')

weight.x=seq(0,to=6,by=0.05)
pi.logit=predict(m.logit,newdata=data.frame(weight=weight.x),type='response')
pi.id=fit$par[1]+fit$par[2]*weight.x
lin=predict(m.lineaire,newdata=data.frame(weight=weight.x),type='response')

plot(weight,y,xlim=c(0,6),main="P(Y=1|poids)",xlab="poids",ylab="y")
lines(weight.x,pi.logit,col="red")
lines(weight.x,pi.id,col="blue")
lines(weight.x,lin,col="green")
legend(x=0,y=.9,legend=c("obs","logistique","identite","lineaire"),lty=c(-1,1,1,1),pch=c(1,-1,-1,-1),col=c("black","red","blue","green"))


m.logit=glm(y~weight+width+as.factor(color)+as.factor(spine),family=binomial(link=logit))
summary(m.logit)	

library(faraway)
ilogit(-10.9488+5.2*0.5389+25*0.3655+0.7042+0.3853)

m.logit2=glm(y~weight+width+as.factor(spine),family=binomial(link=logit))

anova(m.logit2,m.logit,test="Chisq")
# la p value est > à 5% donc on pourrai conclure que mlogit2 est mieux que le 1 donc la couleur 
# de la carapace n'aurai pas d'influence (conservation du H0)

deltaD = (m.logit2$deviance)-(m.logit$deviance)


m.logit=glm(y~weight+width+as.factor(color)+as.factor(spine),family=binomial(link=logit))
m.ba=step(m.logit,direction="backward")

#A chaque étape on fait la règle de la ligne avec le plus petit AIC, 
#on s'arrète donc quand on arrive à la règle <none> qui correspond à ne rien changer.

anova(m.ba,test="Chisq")
#On test vis à vis du modèle nul " H0: modèle nul est mieux "

m.step=step(m.logit,direction="both")

#la même sauf qu'une fois une règle appliqué, on rajoue la règle inverse dans les possibles.

anova(m.step,test="Chisq")


library(boot)
cost=function(r,pi) mean(abs(r-pi)>0.5)
#fonction qui va définir le % de faux
m1.logit=glm(y~width,data=limappr)
cv.glm(limappr,m1.logit,cost,K=10)$delta[1]

m2.logit=glm(y~width+as.factor(color),data=limappr)
cv.glm(limappr,m2.logit,cost,K=10)$delta[1]

cv.glm(limappr,m2.logit,cost,K=nrow(limappr))$delta[1]
cv.glm(limappr,m1.logit,cost,K=nrow(limappr))$delta[1]
# % de mal placer
#m1 est mieux

p.est.test=predict(m1.logit,newdata=limtest,type="response")
y.est.test=as.numeric(p.est.test>0.5)

table(obs=limtest$y,pred=y.est.test)
#limtest est le jeu de test retiré pour faire limappr

diag(1/table(limtest$y))%*%table(obs=limtest$y,pred=y.est.test)


#sensibilité:proportion de vrais positifs => vrai positif/tout les positifs = VP/(VP+FN) = 0.77
#spécificité proportion de vrai négatif => VN/N = VN/(VN+FP) = 0.47
#taux de fausse alarme : proportion de faux positif = FP/N = 0.53

library(pROC)
plot.roc(limtest$y,p.est.test,legacy.axes=TRUE,print.thres=c(.5),clo="blue",auc.polygon=TRUE,print.auc=TRUE)
plot.roc(limtest$y,p.est.test,legacy.axes=TRUE,print.thres="best",clo="blue",auc.polygon=TRUE,print.auc=TRUE)


p.est.appr=predict(m1.logit,newdata=limappr,type="response")
plot.roc(limappr$y,p.est.appr,print.thres="best",col="red",add=TRUE)


#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
#EXO4
X  = read.table("http://dupuy.perso.math.cnrs.fr/enseignement/RLogistique/pressionarti.txt")
attach(X)
plot((y/n)~x)

#ça a l'air un peu lineraire

m.logit=glm(cbind(y,n-y)~x,data=X,family=binomial(link=logit))
summary(m.logit)
# B1 != 0
#spo le même logit car dans les entree ont as pas la colonne de 1 et de 0, mais juste leur proportion

anova(m.logit,test="Chisq")

deltaD = 5.9092
#chi2 à 6 degré de liberté : nombre de truc dans le tableau, moins le nb de parametre. 8-2
1-pchisq(deltaD,6)
# > 0.05 donc on garde H0 au risque de 5%

X.pred=data.frame(x=seq(from=100,to=200,length.out=20))
eta=predict(m.logit,newdata=X.pred,se.fit=TRUE,type="link")

#eta$fit (diapo32) => b1chapo + b2chapo * x

pchapo = exp(eta$fit)/(1+exp(eta$fit))
binf = eta$fit-qnorm(0.975)*eta$se.fit
bsup = eta$fit+qnorm(0.975)*eta$se.fit
binfp = exp(binf)/(1+exp(binf))
bsupp = exp(bsup)/(1+exp(bsup))
bproba = cbind(X.pred,binfp,bsupp,pchapo)
colnames(bproba)=c("x","lwr","upr","pchap")
attach(bproba)


plot(pchap~x,data=bproba,type="l")
lines(lwr~x,data=bproba,type="l",col="green")
lines(upr~x,data=bproba,type="l",col="blue")
points(X$x,X$y/X$n,col="red")
legend('topleft',legend=c('probabilite predite','bornes sup et inf 95','valeurs observees'),lty=c(1,1,0),pch=c(-1,-1,1),col=c('black','green','red'))

X$y.pred=round(X$n*predict(m.logit,type="response"),digits=1)
X$h=hatvalues(m.logit)
X$filsdepoire=resid(m.logit,type="pearson")
X$fdpstd=X$filsdepoire/sqrt(1-X$h)
X$deviance=resid(m.logit,type="deviance")
X$devstd=X$deviance/sqrt(1-X$h)

plot(X$h,type="h",ylab=expression(H[ii]),ylim=c(0,.8))
abline(h=0.5)	
abline(h=0.75)	
#pas de pointds leviers!!! 

par(mfrow=c(2,2))

plot(X$filsdepoire~X$x,ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

plot(X$fdpstd~X$x,ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

plot(X$deviance~X$x,ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

plot(X$devstd~X$x,ylim=c(-2.5,2.5))
abline(h=-2,col="green")	
abline(h=2,col="green")

cook=cooks.distance(m.logit)

#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
#EXO2
#diapo 42 et +

#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
#######################################################################
 # # # # # # # # # # # # # # # PILOU # # # # # # # # # # # # # # # # #
#######################################################################
###################   ################        ############     
###################   #################     ###############
       ####           ####          ####   ####        ####
       ####           ####          ####   ###        ####
       ####           ################               ####
       ####           ##############               ####
       ####           ####                        #### 
       ####           #### 			       ####
       ####           ####                      ####    
       ####           ####                    #### 
       ####           ####                   ################  
       ####           ####                  ################  
data(iris)
attach(iris)

plot(iris[1:4],main="Iris de Fisher",pch=c(21,25,24)[Species],bg = c("red","green3","blue")[Species],las=1,labels=c("Longueur Sepale","Largeur Sepale","Longueur Petale","Largeur Petale"))

library(ade4)
par(mar=c(0,0,0,0))
pan=function(x,y){
xy=cbind.data.frame(x,y)
s.class(xy,iris$Species,include.ori=F,add.p=T,clab=1.5,col=c("blue","black","red"),cpoi=2,csta=0.5)
}
pairs(iris[,1:4],panel=pan)

##Pilou

library(scatterplot3d)

par(mfrow=c(2,2))
scatterplot3d(iris[,1],iris[,2],iris[,3],color=c("blue","black","red")[Species],pch=19)
scatterplot3d(iris[,2],iris[,3],iris[,4],color=c("blue","black","red")[Species],pch=19)
scatterplot3d(iris[,3],iris[,4],iris[,1],color=c("blue","black","red")[Species],pch=19)
scatterplot3d(iris[,4],iris[,1],iris[,2],color=c("blue","black","red")[Species],pch=19)


var.names=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
nb_var = length(var.names)
for(k in 1:nb_var){
	print(var.names[k])
	print(anova(aov(iris[,k]~Species,data=iris)))
}
##petal.Length et petal.width sont plus discriminante

ISE=which(Species=="setosa")
IVI=which(Species=="virginica")
IVE=which(Species=="versicolor")
par(mfrow=c(2,2))
br=seq(min(iris[,3]),max(iris[,3]),length.out=30)
hist(iris[ISE,3],breaks=br,col="red",main=NULL,freq=FALSE,xlab="Longueur de pétale (cm)",ylab="Fréquence")
hist(iris[IVE,3],breaks=br,col="blue",add=TRUE,freq=FALSE)
hist(iris[IVI,3],breaks=br,col="green",add=TRUE,freq=FALSE)
title("Longueur de pétale")

br=seq(min(iris[,1]),max(iris[,1]),length.out=30)
hist(iris[ISE,1],breaks=br,col="red",main=NULL,freq=FALSE,xlab="Longueur de pétale (cm)",ylab="Fréquence")
hist(iris[IVE,1],breaks=br,col="blue",add=TRUE,freq=FALSE)
hist(iris[IVI,1],breaks=br,col="green",add=TRUE,freq=FALSE)
title("Longueur de pétale")

br=seq(min(iris[,2]),max(iris[,2]),length.out=30)
hist(iris[ISE,2],breaks=br,col="red",main=NULL,freq=FALSE,xlab="Longueur de pétale (cm)",ylab="Fréquence")
hist(iris[IVE,2],breaks=br,col="blue",add=TRUE,freq=FALSE)
hist(iris[IVI,2],breaks=br,col="green",add=TRUE,freq=FALSE)
title("Longueur de pétale")

br=seq(min(iris[,4]),max(iris[,4]),length.out=30)
hist(iris[ISE,4],breaks=br,col="red",main=NULL,freq=FALSE,xlab="Longueur de pétale (cm)",ylab="Fréquence")
hist(iris[IVE,4],breaks=br,col="blue",add=TRUE,freq=FALSE)
hist(iris[IVI,4],breaks=br,col="green",add=TRUE,freq=FALSE)
title("Longueur de pétale")


dev.new()
couleur=rep("blue",150)
couleur[Species=="setosa"]="red"
couleur[Species=="versicolor"]="green"
plot(iris[,3:4],col=couleur,pch=rep(c(16,18,20),each=50),xlab="Longueur de pétale",ylab="Largeeur de pétalee")

##C'est linéaire, les groupes sont bien dissociés



library(KernSmooth)
par(mfrow=c(4,2))
kde=bkde2D(iris[,3:4],bandwidth=.1)
image(kde$x1,kde$x2,kde$fhat)
contour(kde$x1,kde$x2,kde$fhat,add=TRUE)
points(iris[,3:4],pch=21,bg=c("red","green","blue")[as.numeric(Species)])
persp(kde$fhat,phi=45,expand=.5,col="purple")


kde.vers=bkde2D(iris[ISE,3:4],bandwidth=.2)
image(kde.vers$x1,kde.vers$x2,kde.vers$fhat)
contour(kde.vers$x1,kde.vers$x2,kde.vers$fhat,add=TRUE)
points(iris[ISE,3:4],pch=21,bg="red")
persp(kde.vers$fhat,phi=45,expand=.5,col="purple")

kde.vers=bkde2D(iris[IVI,3:4],bandwidth=.2)
image(kde.vers$x1,kde.vers$x2,kde.vers$fhat)
contour(kde.vers$x1,kde.vers$x2,kde.vers$fhat,add=TRUE)
points(iris[IVI,3:4],pch=21,bg="blue")
persp(kde.vers$fhat,phi=45,expand=.5,col="purple")

kde.vers=bkde2D(iris[IVE,3:4],bandwidth=.2)
image(kde.vers$x1,kde.vers$x2,kde.vers$fhat)
contour(kde.vers$x1,kde.vers$x2,kde.vers$fhat,add=TRUE)
points(iris[IVE,3:4],pch=21,bg="green")
persp(kde.vers$fhat,phi=45,expand=.5,col="purple")

library(mvtnorm)

dev.new()
kde.vers=bkde2D(iris[IVE,3:4],bandwidth=.2)
image(kde.vers$x1,kde.vers$x2,kde.vers$fhat)
points(iris[IVE,3:4],pch=21,bg="yellow")
contour(kde.vers$x1,kde.vers$x2,kde.vers$fhat,add=TRUE,lty=2,levels=seq(0.1,1,by=0.1))

t1=seq(min(iris[,3])-1,max(iris[,3])+1,by=0.01)
t2=seq(min(iris[,4])-1,max(iris[,4])+1,by=0.01)
T1=matrix(t1,nrow=length(t1),ncol=length(t2))
T2=t(matrix(t2,nrow=length(t2),ncol=length(t1)))
g.vers=dmvnorm(cbind(c(T1),c(T2)),mean=c(mean(iris[IVE,3]),mean(iris[IVE,4])),sigma=as.matrix(cov(iris[IVE,3:4])))
contour(t1,t2,matrix(g.vers,nrow=length(t1),ncol=length(t2)),add=TRUE,levels=seq(0.1,1,by=0.1))

dev.new()

kde.vers=bkde2D(iris[IVI,3:4],bandwidth=.2)
image(kde.vers$x1,kde.vers$x2,kde.vers$fhat)
points(iris[IVI,3:4],pch=21,bg="yellow")
contour(kde.vers$x1,kde.vers$x2,kde.vers$fhat,add=TRUE,lty=2,levels=seq(0.1,1,by=0.1))

t1=seq(min(iris[,3])-1,max(iris[,3])+1,by=0.01)
t2=seq(min(iris[,4])-1,max(iris[,4])+1,by=0.01)
T1=matrix(t1,nrow=length(t1),ncol=length(t2))
T2=t(matrix(t2,nrow=length(t2),ncol=length(t1)))
g.vers=dmvnorm(cbind(c(T1),c(T2)),mean=c(mean(iris[IVI,3]),mean(iris[IVI,4])),sigma=as.matrix(cov(iris[IVI,3:4])))
contour(t1,t2,matrix(g.vers,nrow=length(t1),ncol=length(t2)),add=TRUE,levels=seq(0.1,1,by=0.1))

dev.new()

kde.vers=bkde2D(iris[ISE,3:4],bandwidth=.2)
image(kde.vers$x1,kde.vers$x2,kde.vers$fhat)
points(iris[ISE,3:4],pch=21,bg="green")
contour(kde.vers$x1,kde.vers$x2,kde.vers$fhat,add=TRUE,lty=2,levels=seq(0.1,1,by=0.1))

t1=seq(min(iris[,3])-1,max(iris[,3])+1,by=0.01)
t2=seq(min(iris[,4])-1,max(iris[,4])+1,by=0.01)
T1=matrix(t1,nrow=length(t1),ncol=length(t2))
T2=t(matrix(t2,nrow=length(t2),ncol=length(t1)))
g.vers=dmvnorm(cbind(c(T1),c(T2)),mean=c(mean(iris[ISE,3]),mean(iris[ISE,4])),sigma=as.matrix(cov(iris[ISE,3:4])))
contour(t1,t2,matrix(g.vers,nrow=length(t1),ncol=length(t2)),add=TRUE,levels=seq(0.1,1,by=0.1))


#2 classification truc...

library(MASS)
iris.lda=lda(Species~Petal.Length+Petal.Width,data=iris)
iris.lda
#pior probabilities = probabilitée à priori => la chance de tomber sur un truc
#de ce groupe en piochant un individu au hasard dans notre jeu de données
#No fautes
#JE SUIS DIEU
#hashtag
#Coeffcients LDA : sur l'axe LD1 ya 99,47% des infos et 0.53%sur le deuxieme axe LD2

(iris.lda$svd^2)/sum(iris.lda$svd^2)

lda.pred=predict(iris.lda)
names(lda.pred)
lda.pred
#lda.pred contient : 
	#class : la race de chaque individu
	#postérior : la proba d'appartenance à la classe annoncée dans class
	#x : position de la projection

table(iris[,"Species"],lda.pred$class)

# Matrice de confusion
#             setosa versicolor virginica		(estimé)
#  setosa         50          0         0
#  versicolor      0         48         2
#  virginica       0          4        46
#  (réel)

#risque 4%  (6/150)
#connerie d'Etienne

library(klaR)
partimat(Species~Petal.Length+Petal.Width,data=iris,method="lda",imageplot=FALSE,name=c("Longueur de pétale","Largeur de pétale"))

library(biotools)
y=cbind(Petal.Length,Petal.Width)
boxM(y,Species)
#H0 : sigma1 = sigma2 = sigma3 (Variances egales)
#p value < 5% donc on rejete H0

iris.qda=qda(Species~Petal.Length+Petal.Width,data=iris)
iris.qda
qda.pred=predict(iris.qda)
table(iris[,"Species"],qda.pred$class)
partimat(Species~Petal.Length+Petal.Width,data=iris,method="qda",imageplot=FALSE,name=c("Longueur de pétale","Largeur de pétale"))

#On a fait les trucs su rle jeu de donnée initial sans jeu de test, donc snul

iris.lda2=lda(Species~Petal.Length+Petal.Width,data=iris,CV=T)
table(iris[,"Species"],iris.lda2$class)

iris.qda2=qda(Species~Petal.Length+Petal.Width,data=iris,CV=T)
table(iris[,"Species"],iris.qda2$class)

train=sample(1:150,75)
lda.train=lda(Species~Petal.Length+Petal.Width,subset=train)
predlda=predict(object=lda.train,newdata=iris[-train,])
head(predlda$class)
head(predlda$posterior)

table(iris[-train,][,"Species"],predlda$class)

#               setosa   versicolor   virginica
#  setosa         24           0         0
#  versicolor      0           26        1
#  virginica       0           2         22

#  Premier test : 4% de mal placés

#fatigue