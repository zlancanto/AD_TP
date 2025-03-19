#Binôme :
# - SOSSOU Horacio
# - MIHAN Zlanca-Nto

# Chargement de la librairie FactoMineR
library(FactoMineR)

# 1. Données

data(JO)
JO
# Question : A-t-on un tableau de contingence ?
# Réponse : Oui, car il croise deux variables qualitatives (épreuves et pays)
#   avec des effectifs (nombre de médailles) dans les cellules

# Question : Quels sont les individus? Quelles sont les variables associées à chaque individu?
# Réponse : Individus : épreuves d'athlétisme.
#           Variables associées : Pays

table(JO)
barplot(table(JO))

# Quel est le nombre de cellules contenant 0 ? : 1211 cellules
# Question : Quel est le pays qui a obtenu le plus grand nombre de medailles pour une ´epreuve particuliere et pour laquelle ?
# Réponse : Le plus grand nombre de médailles obtenus est 12. Et d'après le tableau de contingence, 
#           ce pays est : Kenya, et l'épreuve est : 3000mSteeple

rev(sort(apply(JO,2,sum)))
# Pays ayant le plus de médailes : USA(82), Kenya(35) 


# 2. AFC

resca = CA(JO)
resca
# Quelle est la valeur du test de Khi 2 ? Qu’en concluez-vous ?
# Réponse : test = 2122.231
# Conclusion : Cette valeur très élevée et p-value = 2.320981e-41 < 5% indique qu'il existe
#             une association significative entre les deux variables (épreuves d'athlétisme et pays).

round(resca$eig,2)
barplot(resca$eig[,1],names.arg = c(1:23))

# Combien d’axes sont necessaires pour representer tous les profils-lignes et colonnes sans perte d’inertie ?
# Réponse : il est nécessaire d'utiliser 23 axes. Cela est indiqué par le fait que la somme
#           des pourcentages d'inertie cumulée atteint 100% à la 23ème dimension

# Est-ce normal ?
# Réponse : Oui, c'est normal. Dans une analyse des correspondances, le nombre total d'axes nécessaires pour représenter
#           toutes les données sans perte d'inertie est égal au minimum du nombre de lignes ou de colonnes moins un. Dans ce cas, 
#           avec 24 épreuves (lignes) et 58 pays (colonnes), le nombre d'axes nécessaires est 23 (24 - 1).

# Combien d’axes allez-vous conserver pour analyser ces donnees? Pourquoi?
# Réponse : 6 car c'est à partir du 6eme axe que l'inertie cumulée vaut au moins 50% (54.67% dans notre cas)

# Quel est le pourcentage d’inertie exprimé sur le premier plan factoriel ?
# Réponse : Axe1(13.85%) + Axe2(10.53%) = 24.38%

plot(resca,axes=3:4)
round(resca$row$contrib,2)

# Quelles sont les ´epreuves qui contribuent le plus au premier axe?
# Réponse : 10000m(23.85%), 5000m(17.79%), 3000m Steeple(13.23%), 1500m(11.02%), Marathon(6.67%)

#
# Réponse : Les épreuves d'endurance 10000m(23.85%), 5000m(17.79%), 3000m Steeple(13.23%), 1500m(11.02%), Marathon(6.67%)
#           50km(0.56%) et 20km(0.41%) contribuent le plus à la formation de l'axe1

# Pour le second axe, quelles sont les ´epreuves les plus contributives?
# Réponse : Disque(25.47%), Marteau(19.57%), 50km(9.67%), 100m(9.09%), 200m(7.89%)

round(resca$col$contrib,2)

# Quels sont les pays qui contribuent le plus au premier axe ?
# Réponse : KEN(31.39%), ETH(22.07%), MAR(12.16%), USA(9.15%), GBR(2.14%)

# Quels sont les pays qui contribuent le plus au 2eme axe ?
# Réponse : USA(11.32%), LTU(10.94%), BLR(7.17%), HUN(6.91%), POL(6.31%), FIN(5.92%) EUN(5.58%), TRI(4.79%), EST(4.23%), GER(3.77%), NAM(3.64%), JAM(3.63%), MEX(3.61%), RUS(2.67%), SLO(2.10%)

# Compte-tenu des 2 resultats precedents, comment interpretez-vous le premier axe, et le second axe?
# Réponse : 
#   Axe1 : les pays ken, eth, mar, usa et bgr, remportent beaucoup les courses d'endurance 10000m, 5000m, 3000m, 1500m et marathon
#   Axe2 : les pays USA, LTU, BLR, HUN, POL, FIN, EUN, TRI, EST, GER, NAM, JAM, MEX, RUS, SLO remportent beaucoup les épreuves disques, marteau, 50km, 100m? 200m

# Quels sont les epreuves mal representes sur le plan 1-2?
cos2_epreuves = resca$row$cos2[,1:2]
mal_rep_epreuves = row.names(cos2_epreuves)[rowSums(cos2_epreuves) < 0.5]
mal_rep_epreuves
# Résultats : [1] "100m"         "110mH"        "1500m"        "200m"         "20km"         "3000mSteeple" "400m"         "400mH"       
#             [9] "4x100m"       "4x400m"       "50km"         "800m"         "Decathlon"    "Disque"       "Hauteur"      "Javelot"     
#             [17] "Longueur"     "Marathon"     "Marteau"      "Perche"       "Poids"        "Triple saut" 

# Quels sont les pays mal representes sur le plan 1-2?
cos2_pays <- resca$col$cos2[,1:2]
mal_rep_pays <- row.names(cos2_pays)[rowSums(cos2_pays) < 0.5]
mal_rep_pays
# Résultat : [1] "alg" "aus" "bah" "bar" "bdi" "bra" "brn" "can" "chn" "cub" "cze" "den" "dom" "ecu" "eri" "esp" "est" "eun" "fin" "fra" "gbr"
#            [22] "ger" "gre" "hun" "ita" "jam" "jpn" "kaz" "kor" "ksa" "lat" "ltu" "mex" "nam" "ngr" "nor" "nzl" "pan" "pol" "por" "qat" "rou"
#            [43] "rsa" "rus" "slo" "sud" "swe" "tch" "tri" "tur" "uga" "ukr" "zam"

# Refaites la meme chose pour le plan 3-4
round(resca$row$contrib,2)
# Epreuves : Axe3 : disque(24,62%), javelot(23,70%), 20km(16,68%), 50km(10,53%), marteau(9%)
#            Axe4 : javelot(38,95%), 20km(19,88%), disque(10,77%), decathlon(9,23%), marteau(6,04%), 50km(4,83%)

round(resca$col$contrib,2)
# Pays : Axe3 : rus(13,07%), ltu(12,06%), lat(6,88%), mex(6,58%), fin(5,92%), blr(5,80%), hun(4,91%), nor(4,22%), usa(3,67%)
#        Axe4 : fin(12,17%),  cze(11,57%), nor(9,14%)

# Interprétation : 
#                 Axe3 : les pays rus, ltu, lat, mex, fin, blr, hun, nor et usa remportent beaucoup les épreuves disque, javelot, 20km, 50km et marteau
#                 Axe4 : les pays fin, cze, nor remportent beaucoup les épreuvesjavelot, 20km, disque, decathlon, marteau, 50km

# Quels sont les epreuves mal representes sur le plan 3-4?
cos2_epreuves_3_4 = resca$row$cos2[,3:4]
mal_rep_epreuves_3_4 = row.names(cos2_epreuves_3_4)[rowSums(cos2_epreuves_3_4) < 0.5]
mal_rep_epreuves_3_4
# Résultat : "10000m"       "100m"         "110mH"        "1500m"        "200m"         "3000mSteeple" "400m"         "400mH"       
#            "4x100m"       "4x400m"       "5000m"        "50km"         "800m"         "Decathlon"    "Disque"       "Hauteur"     
#            "Longueur"     "Marathon"     "Marteau"      "Perche"       "Poids"        "Triple saut" 

# Quels sont les pays mal representes sur le plan 3-4?
cos2_pays_3_4 <- resca$col$cos2[,3:4]
mal_rep_pays_3_4 <- row.names(cos2_pays_3_4)[rowSums(cos2_pays_3_4) < 0.5]
mal_rep_pays_3_4
# Résultat : "alg" "bah" "bar" "bdi" "blr" "bra" "brn" "can" "chn" "cub" "cze" "den" "dom" "ecu" "eri" "esp" "est" "eth" "eun" "fra" "gbr"
#            "ger" "gre" "hun" "ita" "jam" "jpn" "kaz" "ken" "kor" "ksa" "ltu" "mar" "nam" "ngr" "nzl" "pan" "pol" "por" "qat" "rou" "rsa"
#            "slo" "sud" "swe" "tri" "tur" "uga" "ukr" "usa" "zam"


# 3. Classification
library(cluster)
resclassif=agnes(resca$col$coord[,1:4],method="ward") # classification
plot(resclassif) # tracé de l’arbre
resclassif2=as.hclust(resclassif)
barplot(rev(resclassif2$height), names.arg = c(1:24))
# Combien de classes allez-vous choisir ?
# Réponse : 5 classes. En effet il y a un gros saut à partir du 5eme bâton du diagramme des indices

# Coupure de l'arbre
numclassward=cutree(resclassif2, k=5)
plot(resca$col$coord[,1:2],col=numclassward)

# Description des classes
paysward=cbind.data.frame(t(JO)[,1:24],as.factor(numclassward))
colnames(paysward)[25]="Classward"
result_catdes=catdes(paysward, num.var = 25)
result_catdes
# Commentaires :
# Classe 1 : Pays dominants dans les courses d’endurance
# Classe 2 : Pays performants en marche athlétique et disciplines techniques
# Classe 3 : Pays spécialisés dans les sprints et sauts
# Classe 4 : Pays forts dans les épreuves de lancer
# Classe 5 : Pays exceptionnels en javelot




