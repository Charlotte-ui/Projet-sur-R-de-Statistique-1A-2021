######################################################################
# ENSC 1A - Projet R
#---------------------------------------------------------------------
######################################################################

library(car) 

# 1) Description des donn??es
#------------------------
ozone <- read.table("ozone.txt")  
summary(ozone)
var(ozone[,c(-13,-12)])

par(mfrow=c(1,2))
hist(ozone$maxO3,freq = FALSE,main="Distribution de maxO3",xlab="??g/m3")
lines(density(ozone$maxO3), col='blue',lwd=3)
hist(ozone$maxO3v,freq = FALSE,main="Distribution de maxO3v",xlab="??g/m3")
lines(density(ozone$maxO3), col='blue',lwd=3)

par(mfrow=c(1,3))
hist(ozone$T9,freq = FALSE,main="Distribution T9",xlab="Degr??es")
lines(density(ozone$T9), col='blue',lwd=3)
hist(ozone$T12,freq = FALSE,main="Distribution de T12",xlab="Degr??es")
lines(density(ozone$T12), col='blue',lwd=3)
hist(ozone$T15,freq = FALSE,main="Distribution de T15",xlab="Degr??es")
lines(density(ozone$T15), col='blue',lwd=3)

par(mfrow=c(1,3))
hist(ozone$Ne9,freq = FALSE,main="Distribution de Ne9",xlab="Octa")
lines(density(ozone$Ne9), col='blue',lwd=3)
hist(ozone$Ne12,freq = FALSE,main="Distribution de Ne12",xlab="Octa")
lines(density(ozone$Ne12), col='blue',lwd=3)
hist(ozone$Ne15,freq = FALSE,main="Distribution de Ne15",xlab="Octa")
lines(density(ozone$Ne15), col='blue',lwd=3)

par(mfrow=c(1,3))
hist(ozone$Vx9,freq = FALSE,main="Distribution de Vx9")
lines(density(ozone$Vx9), col='blue',lwd=3)
hist(ozone$Vx12,freq = FALSE,main="Distribution de Vx12")
lines(density(ozone$Vx12), col='blue',lwd=3)
hist(ozone$Vx15,freq = FALSE,main="Distribution de Vx15")
lines(density(ozone$Vx15), col='blue',lwd=3)

par(mfrow=c(1,2))
plot(ozone$vent,main="Direction du vent")
plot(ozone$pluie,main="Pr??sence de pluie")

boxplot(ozone$maxO3,main="Concentration en ozone")

par(mfrow=c(1,2))
boxplot(maxO3~vent,ozone,main="Ozone en fonction du vent",names=levels(ozone$vent),xlab="Direction du vent", ylab="Concentration en ozone (????g/m3)")
boxplot(maxO3~pluie,ozone,main="Ozone en fonction dd le pluie",names=levels(ozone$pluie),xlab="Pluie", ylab="Concentration en ozone (????g/m3)")

#------------------------
# 2) Etude de MaxO3
#------------------------
shapiro.test(ozone$maxO3)
wilcox.test(x = ozone$maxO3, mu=90.3)
ks.test(ozone$maxO3,"pnorm",mean=90.3, sd=sqrt(794.5))
#la normalité est rejetté
d <-density(ozone$maxO3)
ecdf(ozone$maxO3)
plot(ecdf(ozone$maxO3) ) 

#on calcul la probabilité de depasser le maximum légal
proba =0
i=0
for(x in d$x){
  i=i+1
  if (x>=180) proba=proba+d$y[i]
}
proba # 0.002671637

#------------------------
# 3) Test pour 2 variables (quanti et quali)
#------------------------
model1 <- aov(maxO3~vent,ozone)
summary(model1)
model2 <- aov(maxO3~pluie,ozone)
summary(model2)

kruskal.test(maxO3~vent,ozone)
wilcox.test(maxO3~pluie,ozone)

pairwise.wilcox.test(ozone$maxO3,ozone$vent,p.adjust.method = "BH")

#------------------------
# 4) Test pour 2 variables qualitatives (vent et pluie)
#------------------------

tab <- table(ozone$vent, ozone$pluie)
lprop(tab)
cprop(tab)
chisq.test(tab)
mosaicplot(tab, las = 3, shade = TRUE, main="Mosa??que")


#------------------------
# 4) Test pour de multiples variables quantitatives 
#------------------------

anova<-aov(maxO3~.,ozone[,c(-13,-12,-11)])
summary(anova)

#on fait les tests de correlation pour les variables retenus

cor.test(ozone$maxO3, ozone$T9,paired=TRUE)
cor.test(ozone$maxO3, ozone$T12,paired=TRUE)
cor.test(ozone$maxO3, ozone$Ne9,paired=TRUE)
cor.test(ozone$maxO3, ozone$Vx9,paired=TRUE)

cor.test(ozone$maxO3, ozone$T9,method = "spearman",paired=TRUE)
cor.test(ozone$maxO3, ozone$T12,method = "spearman",paired=TRUE)
cor.test(ozone$maxO3, ozone$Ne9,method = "spearman",paired=TRUE)
cor.test(ozone$maxO3, ozone$Vx9,method = "spearman",paired=TRUE)

#illustré par un nuage de point
scatterplot(maxO3~T9,ozone)
scatterplot(maxO3~T12,ozone)
scatterplot(maxO3~Ne9,ozone)
scatterplot(maxO3~Vx9,ozone)


#------------------------
# 5) Test pour ??chantillons appari??s
#------------------------ 

#normalit?? de l'??chantillon
shapiro.test(ozone$T9)
shapiro.test(ozone$T12)
shapiro.test(ozone$T15)
shapiro.test(ozone$Ne9)
shapiro.test(ozone$Ne12)
shapiro.test(ozone$Ne15)
shapiro.test(ozone$Vx9)
shapiro.test(ozone$Vx12)
shapiro.test(ozone$Vx15)

#Test (de Fisher) d'??galit?? des variances
var.test(ozone$Vx9,ozone$Vx12)
var.test(ozone$Vx9,ozone$Vx15)
var.test(ozone$Vx15,ozone$Vx12)


# Test (de Student) d'??galit?? des moyennes
heure <- factor(c(rep("T9", 112),rep("T12", 112),rep("T15", 112) ))
temperature <- c(ozone$T9,ozone$T12,ozone$T15)
tmp <- data.frame(temperature,heure)
pairwise.t.test(tmp$temperature,tmp$heure,p.adjust.method ="bonferroni",paired=TRUE)
pairwise.wilcox.test(tmp$temperature,tmp$heure,p.adjust.method ="bonferroni",paired=TRUE)

heure <- factor(c(rep("Ne9", 112),rep("Ne12", 112),rep("Ne15", 112) ))
nebulosite <- c(ozone$Ne9,ozone$Ne12,ozone$Ne15)
nbl <- data.frame(nebulosite,heure)
pairwise.t.test(nbl$nebulosite,nbl$heure,p.adjust.method ="bonferroni",paired=TRUE)
pairwise.wilcox.test(nbl$nebulosite,nbl$heure,p.adjust.method ="bonferroni",paired=TRUE)

heure <- factor(c(rep("Vx9", 112),rep("Vx12", 112),rep("Vx15", 112) ))
composante <- c(ozone$Vx9,ozone$Vx12,ozone$Vx15)
cmp <- data.frame(composante,heure)
pairwise.t.test(cmp$composante,cmp$heure,p.adjust.method ="bonferroni",paired=TRUE,var.equal =TRUE)
pairwise.wilcox.test(cmp$composante,cmp$heure,p.adjust.method ="bonferroni",paired=TRUE)

#test pour la concentration en ozone de la veille
t.test(ozone$maxO3,ozone$maxO3v,p.adjust.method ="bonferroni",paired=TRUE)
wilcox.test(ozone$maxO3,ozone$maxO3v,p.adjust.method ="bonferroni",paired=TRUE)


#------------------------
# 5) ACP
#------------------------
require(PCAmixdata)

#on retire les variables qualitative et la variables d'intérert maxO3
res<-PCAmix(ozone[,c(-1,-12,-13)])
res
plot(res,axes=c(1,2),choice="cor")

round(res$eig,digit=2)

#plot du choix des axes
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))
abline(h=1,col=2,lwd=2)

#plot des individus en fonction des variables qualitatives
plot(res,axes=c(1,3),choice="ind")
plot(res,choice="ind",quali=ozone$pluie,cex=0.5,posleg="topright",main="Scores",coloring.ind=ozone$pluie)
plot(res,choice="ind",quali=ozone$vent,cex=0.5,posleg="topright",main="Scores",coloring.ind=ozone$vent)


round(res$ind$cos2,digit=3)

res$quantiround(res$quanti$cos2,digit=3)

# projeter maxO3 a posteriori
cor(ozone[,1],res$scores) 
plot(res, choice="cor",axes=c(1,2),main="Correlation circle") # cercle des corrélations sur le plan 1-2
arrows(0,0,cor(ozone[,1],res$scores)[1],cor(ozone[,1],res$scores)[2],col=2,xlim=c(-1, 1) * 1.3,ylim=c(-1, 1) * 1.3,lwd=3)   


