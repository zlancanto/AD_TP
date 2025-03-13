# Noms & Prénoms : 
# - SOSSOU Horacio
# - MIHAN Zlanca-Nto

# Chargement des librairies cluster et FactoMineR
library(cluster)
library(FactoMineR)


# 1. Methode des k-means

# Chargement du fichier tel1984.txt
tel = read.table("tel1984.txt", row.names = 1, header = T, dec = ",")
tel

# Documentation de la focntion kmeans
help(kmeans)

# Classification en 4 classes des 7 premières variables
resclus = kmeans(tel[,1:7],4, algorithm = "Lloyd") # on ne s’occupe pas de la 8eme colonne
resclus

# Affichage de 5 composants kmeans
resclus$cluster
resclus$centers
resclus$withinss
resclus$tot.withinss
resclus$size
# Inertie intra-classe de chaque groupe : [1]   0.0000 374.1200 159.7025 264.0662
# Inertie intra-totale = 797.8887
# Nombre d’individus dans chaque groupe : [1] 1 9 4 8


# 1.1 Examen des r´esultats de cette premi`ere analyse

# - Groupement à choisir :
# On exécute la commande suivante plusieurs fois
resclus = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus$tot.withinss
# Les essais : 
resclus1 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus1$tot.withinss
resclus2 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus2$tot.withinss
resclus3 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus3$tot.withinss
resclus4 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus4$tot.withinss
resclus5 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus5$tot.withinss
resclus6 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus6$tot.withinss
resclus7 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus7$tot.withinss
resclus8 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus8$tot.withinss
resclus9 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus9$tot.withinss
resclus10 = kmeans(tel[,1:7],4, algorithm = "Lloyd")
resclus10$tot.withinss
# On garde la partition qui a l'inertie intra totale la plus petite
# Essai 1 : Inertie intra-totale = 1192.096
# Essai 2 : Inertie intra-totale = 810.7824
# Essai 3 : Inertie intra-totale = 797.8887
# Essai 4 : Inertie intra-totale = 992.7077
# Essai 5 : Inertie intra-totale = 1213.143
# Essai 6 : Inertie intra-totale = 797.8887
# Essai 7 : Inertie intra-totale = 810.7824
# Essai 8 : Inertie intra-totale = 810.7824
# Essai 9 : Inertie intra-totale = 810.7824
# Essai 10 : Inertie intra-totale = 966.6875
# Le plus petit intra est 797.8887 issu de resclus3 et resclus6.
# On travaillera avec resclus3 dans la suite de l'exercice

# - Examen des classes trouvées
resclus3$cluster
resclus3$centers
resclus3$withinss # Résultat : [1] 374.1200 264.0662 159.7025   0.0000
resclus3$size # Résultat : [1] 9 8 4 1
# Seule la classe 4 n'a un qu'un seul élément.
# La commande resclus3$cluster permet de visualiser cet élément
resclus3$cluster # Elément de la classe 4 : Ajaccio

# - Représentation dans le plan
plot(tel$igqs,tel$ezaa,col=resclus3$cluster)
# L'affichage confirme qu'Ajaccio est très éloigné de tous les éléments. Il est tès proche de l'origine du repère

# - Ajout d'un nouvel indice "Classe km"
tel = cbind(tel, as.factor(resclus3$cluster))
colnames(tel)[9] = "Classe km"
# Affichage des nouvelles données
tel

# - Description des classe par variables quantitatives
description  = catdes(tel[,c(1:7,9)], num.var = 8) # on s´electionne les variables descriptives (colonnes 1 `a 7, puis la colonne des classes , i.e. 9)
description
# Variables caractéristiques des classes :
# Classe 1 : tsi avec t = -3.020993
# Classe 2 : vr2 avec t = -4.003298
# Classe 3 : tcr avec t = -2.173697
# Classe 4 : tsi avec t = 1.965272
# Interprétation de la classe 4 : 
# seule tsi semble caractériser la classe 4. Or |t = 1.965272| < 2 donc aucune variable ne la caractérise

# — Visualisation des compositions des groupes
pca = PCA(tel[, 1:7], scale.unit = TRUE, ncp = 5, graph = TRUE)
coord_ACP = pca$ind$coord[, 1:2]  # Axes 1 et 2
plot(coord_ACP, col = resclus3$cluster, pch = 16, main = "Classes k-means sur le premier plan factoriel")
text(coord_ACP, labels = rownames(tel), pos = 3, cex = 0.8)
legend("topleft", legend = paste("Classe", 1:4), col = 1:4, pch = 16, title = "Classes k-means")
# Cohérence : Ces groupes sembles cohérents à l'exceptions de quelques villes comme
# Amiens, Orléans et Rennes qui sont très éloignés de leurs centroïdes respectifs


# 1.2 Nouvelle etude normee
resclusnorm = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm$tot.withinss

# - Selection du meilleur partionnement avec 10 essais
resclusnorm1 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm1$tot.withinss
resclusnorm2 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm2$tot.withinss
resclusnorm3 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm3$tot.withinss
resclusnorm4 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm4$tot.withinss
resclusnorm5 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm5$tot.withinss
resclusnorm6 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm6$tot.withinss
resclusnorm7 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm7$tot.withinss
resclusnorm8 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm8$tot.withinss
resclusnorm9 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm9$tot.withinss
resclusnorm10 = kmeans(scale(tel[,1:7]),4, algorithm = "Lloyd")
resclusnorm10$tot.withinss
# On garde la partition qui a l'inertie intra totale la plus petite
# Essai 1 : Inertie intra-totale = 29.45064
# Essai 2 : Inertie intra-totale = 30.22127
# Essai 3 : Inertie intra-totale = 28.15514
# Essai 4 : Inertie intra-totale = 33.41778
# Essai 5 : Inertie intra-totale = 28.03745
# Essai 6 : Inertie intra-totale = 85.87389
# Essai 7 : Inertie intra-totale = 28.09956
# Essai 8 : Inertie intra-totale = 33.31425
# Essai 9 : Inertie intra-totale = 27.77044
# Essai 10 : Inertie intra-totale = 29.86041
# La plus petite inertie intra est 27.77044 issu de resclusnorm9
# On travaillera avec resclusnorm9 dans la suite de l'exercice

# - Tailles des classes
resclusnorm9$size # Résultat : [1] 6 8 7 1

# -Variances intra-classes
resclusnorm9$withinss # Résultat : [1]  6.051702 11.574555 10.144180  0.000000

# - Observation de bonne séparation des classes
plot(tel$igqs,tel$ezaa,col=resclusnorm9$cluster)
# Les classes sont mieux séparées maintenant

# - Ajout de l'indice "Classe kmnorm"
tel = cbind(tel, as.factor(resclusnorm9$cluster))
colnames(tel)[10] = "Classe kmnorm"
tel

# - Descriptions des classes par les variales
descrip1=catdes(tel[,c(1:7,10)], num.var = 8) # on selectionne les variables descriptives (colonnes 1 `a 7, puis la colonne des classes, i.e. 10)
descrip1
# Classe 1 : tsi avec t = -3.020993
# Classe 2 : tsi avec t = 1.965272
# Classe 3 : tcr avec t = -2.173697
# Classe 4 : vr2 avec t = -4.003298
# Comparaison : Ici la classe 4 est fortement caractérisée par une variable (vr2). Ce qui était
# le contraire dans le cas précédent

# - Visualisation des groupes avec l'ACP
pca_norm = PCA(tel[, 1:7], scale.unit = TRUE, ncp = 5, graph = TRUE)
coord_ACP_norm = pca_norm$ind$coord[, 1:2]  # Axes 1 et 2
plot(coord_ACP_norm, col = resclusnorm9$cluster, pch = 16, main = "Classes k-means sur le premier plan factoriel")
text(coord_ACP_norm, labels = rownames(tel), pos = 3, cex = 0.8)
legend("topleft", legend = paste("Classe", 1:4), col = 1:4, pch = 16, title = "Classes")
# Villes qui sont ensemble quelle que soit la méthode :
tel
# Classe 1 : Besancon, Chalons, Clermont, Dijon, Nancy, Strasbourg 
# Classe 2 : Bordeaux, Caen, Limoges, Poitiers, Rouen, Toulouse
# Classe 3 : Lille, Paris
# Classe 4 : Ajaccio


# 2. CAH
rescah=agnes(scale(tel[,1:7]),method="ward")
plot(rescah,xlab="regions")

# - CAH avec le lien minimum
rescahmin=agnes(scale(tel[,1:7]),method="single")
plot(rescahmin,xlab="regions")
# Constat : La hauteur du dendrogram a dimunié (passée de 13 à 7)

# - CAH avec le lien maximum
rescahmax=agnes(scale(tel[,1:7]),method="complete")
plot(rescahmax,xlab="regions")

# - Découpages en classes
numclassward=cutree(rescah, k=4) # pour Ward
numclassmax=cutree(rescahmax,k=4) # pour lien max
numclassward
numclassmax
table(numclassward,numclassmax)
# Constat : Ces deux méthodes ont des éléments communs dans leurs différentes classes qui sont :
# Classe 1 : 1 ville
# Classe 2 : 2 villes
# Classe 3 : 6 villes
# Classe 4 : 9 villes
# On a des éléments non nuls que sur la diagonal, à l'xception du couple (2,3)

# Tracé de graphe
# numclassward :
plot(coord_ACP, col = numclassward, pch = 16, main = "Groupes CAH (Ward) sur le premier plan factoriel")
text(coord_ACP, labels = rownames(tel), pos = 3, cex = 0.8)
legend("topleft", legend = paste("Groupe", 1:4), col = 1:4, pch = 16, title = "Groupes")

# numclassmax :
plot(coord_ACP, col = numclassmax, pch = 16, main = "Groupes CAH (Lien max) sur le premier plan factoriel")
text(coord_ACP, labels = rownames(tel), pos = 3, cex = 0.8)
legend("topleft", legend = paste("Groupe", 1:4), col = 1:4, pch = 16, title = "Groupes")

# Ajout de l'indice "ClassesCAHW"
tel = cbind(tel, as.factor(numclassward))
colnames(tel)[11] = "ClassesCAHW"
tel

# En combien de classes couper l'arbre ?
rescah2=as.hclust(rescah)
barplot(rev(rescah2$height), names.arg = c(1:21))

# - Description des classes par les variables
descrip2=catdes(tel[,c(1:7,11)], num.var = 8)
descrip2
# Affichage de la meilleure classe : Il s'agit de la classe 4
composition=tel[tel[,8] == 4, ]
composition

# - Comparaison : La composotion des classes est différente en fonction de la méthode utilisée 
  
