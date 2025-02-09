tabac = read.table("tabac.txt", header = T, row.names = 1)
tabac

#permet de recuperer un vecteur avec toutes les valeurs de la variable Ventes
tabac$Vente

#permet de recuperer un vecteur avec toutes les valeurs de la variable Prix 
tabac$Prix 

#permet de recuperer la moyenne de la variable Vente
mean(tabac$Vente)

#idem pour Prix
mean(tabac$Prix)

#Calcul de la correlation
cor(tabac$Prix, tabac$Vente)

# trace simple du nuage de point
plot(tabac$Prix,tabac$Vente)

# trace sofistiqué du nuage de point
plot(tabac$Prix,tabac$Vente, main="Consommation de tabac en France",	col="blue",lwd=2, xlab="Prix relatif de vente (en euros)", ylab="Nombre de cigarettes vendues (en milliards)")

reg = lm(Vente~Prix, data = tabac)
#L'objet obtenu est la droite de regression linéaire

reg$coefficients
# 109.4994 est la valeur de l'ordonnée à l'origine de la droite
# −0.1822 est le coefficient directeur de la droite

abline(reg, col="red")

summary(reg)
#Observation...

#### TD1 ####

#1
reg$residuals
round(reg$residuals,3)
reg$residuals[16]

#2
reg$fitted.values

#3
SCEr = sum(reg$residuals^2)
SCEr

#4
meanY = mean(tabac$Vente)
SCEt = sum((tabac$Vente - meanY)^2)
SCEt

#5
SCEm = SCEt - SCEr
SCEm

#6
R2 = SCEm/SCEt
R2
R2 = summary(reg)$r.squared
R2

#7
varianceResiduelle = summary(reg)$sigma^2
varianceResiduelle
lengthVente = length(tabac$Vente)
varianceResiduelle = SCEr/(lengthVente  -1 -1)
varianceResiduelle

#8
testStudent = summary(reg)$coefficients[2,3]
testStudent 
# Analyse : testStudent n'appartient pas à ZA(H0)=[-2,05; 2,05]
#Conclusion : On peut affirmer H1

#9
probaCritique = summary(reg)$coefficients[2,4]
probaCritique
# Conclusion : probaCritique < 5% donc on refuse H0 et on accepte H1

#10
testFisher = (SCEm * (lengthVente -1 -1))/SCEr * 1
testFisher
summary(reg)$fstatistic
# Fisher(95%) = 4,2 et testFisher n'appartient pas à ZA(HO) = [0; 4,2]. Donc H1

#11
# creation du nouvel individu avec sa valeur de Prix
new = data.frame(288.3)
# label de la variable explicative
colnames(new) = "Prix"
# pour prediction moyenne
predict = predict(reg, new, interval = "confidence")
predict
predict[1]
# Valeur du TD : 57,03. Valeur obtenu par R : 56,97, donc les valeurs du TD sont bien vérifiées 

#### TD2 ####

mat = read.table("materiau.txt",header=T)
mat

regX1 = lm(Y~X1, data = mat)
regX2 = lm(Y~X2, data = mat)
regmul = lm(Y~X1+X2,data=mat)

#1
R2X1 = summary(regX1)$r.squared
R2X2 = summary(regX2)$r.squared
R2Mul = summary(regmul)$r.squared
R2X1
R2X2
R2Mul

#2
CMrX1 = sum(regX1$residuals^2)/(length(mat$X1) -1 -1)
CMrX2 = sum(regX2$residuals^2)/(length(mat$X2) -1 -1)
CMrMul = sum(regmul$residuals^2)/(length(mat$X1) -2 -1)
CMrX1
CMrX2
CMrMul

sigmatX1 = sqrt(CMrX1)
sigmatX2 = sqrt(CMrX2)
sigmatMul = sqrt(CMrMul)
sigmatX1
sigmatX2
sigmatMul

#3
meanY = mean(mat$Y)
SCEt = sum((mat$Y - meanY)^2)
CMmMul = (SCEt * R2Mul)/2
CMmMul

#4
# H0 : Aucune des deux variables X1 et X2 n'influence Y
# H1 : Au moins une des deux variables X1 et X2 influence Y
testFisherMul = CMmMul/CMrMul
testFisherMul
# testFisherMul n'appartient pas à ZA(H0) = [0; Fisher(0,95)] = [0; 4,26], donc H1

#5
# H0 : X1 n'influence pas Y
# H1 : X1 influence Y
CMmX1 = (SCEt * R2X1)/1
testFisherX1 = CMmX1/CMrX1
testFisherX1
# testFisherX1 n'appartient pas à ZA(H0) = [0; Fisher(0,95)] = [0; 4,96], donc H1

#6
summaryRegX1 = summary(regX1)
pcX1 = summaryRegX1$coefficients["X1", "Pr(>|t|)"]
pcX1
# pcX1(proba critique) < 0.05 donc H1 : l'ajout de X2 quand X1 est déjà présent apporte une info significative pour expliquer Y

#7
new = data.frame(X1 = 3.5, X2 = 3.9)
prediction = predict(regmul, new, interval = "confidence")
prediction
prediction[1]
# L'entreprise aura une résistance à la rupture de 30.27