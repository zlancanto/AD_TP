# MIHAN Zlanca-Nto & SOSSOU Horacio

# Chargement de la librairie nécessaire:
library(FactoMineR)

# Chargement des données
foot = read.csv2("foot_acp.csv", row.names = 1, stringsAsFactors = T)
# Les données que l'on va étudier concerne les équipes de football des 5 championnats majeurs européens:
# Angleterre, France, Espagne, Allemagne, Italie
# Pour chaque équipe de ces championnats, on a recolté des informations à propos de 15 variables (saison en cours) : 
# - Buts : nb de buts marqués dans la saison
# - Tirs : nb de tirs moyen effectués par match
# - Jaune : nb de cartons jaunes pris par l'équipe dans la saison
# - Rouge : -------------- rouges------------------------------
# - Possession : possession de balle (en pourcentage)
# - PassesReuss : % de passes réussies
# - DuelsAériens : nb moyen de duels aériens gagnés par match
# - TirsContre : nb de tirs moyen subis par match
# - Tacles : nb de tacles moyen effectués par match
# - Interception : nb d'interception moyen effectuées par match
# - Fautes : nb de fautes moyen effectuées par match
# - Hors jeu : nb de hors jeu moyen par match
# - TirsCadres : nb de tirs cadrés moyen effectués par match
# - Dribbles : nb de dribbles moyen par match
# - FautesSubies : nb de fautes moyen subies par match

#########################################################
###      Première partie: Description des données   #####
#########################################################

head(foot) # Cette commande affiche les 6 premières lignes du tableau de données
dim(foot) # Cette commande vous indique le nb de lignes (individus) et de variables (colonnes)
summary(foot) # Cette commande vous permet d'avoir des informations sur chacune des variables du tableau:
# - minimum, maximum, quartiles et moyenne pour les variables quantitatives
# - répartition des individus dans les différentes modalités pour les variables qualitatives

# Questions : 
# - Quelle est la valeur minimale de cartons jaunes pris par une équipe ?
min_carton_rouge_par_equipe = min(foot$Rouge) # valeur = 0
# - Combien d'équipes participent à la ligue française ?
nombre_equipes_france = sum(foot$Ligue == "France")
nombre_equipes_france # valeur = 20

# Diagramme en camembert de la répartition des équipes par Ligue
eff = table(foot$Ligue)
pie(eff, main = "Répartition des équipes par Ligue", labels = paste(names(eff), round(eff*100/98,1),"%"))
# Note : la commande $ permet de récupérer toutes les valeurs d'une colonne (en l'appelant par son nom, ici la colonne Ligue)

#Histogramme du nombre de fautes par match
hist(foot$Fautes, main= "Histogramme du nombre moyen de fautes par match", xlab = 'Nombre de fautes')

# Utiliser la commande summary appliquée à la colonne Fautes pour récupérer les informations
# sur cette variable
summary(foot$Fautes)

# Appliquer cette même commande summary, mais cette fois-ci uniquement aux équipes de la ligue anglaise
# Aide : les indices des lignes dans lesquelles sont les équipes anglaises sont données par:
idx_anglais = which(foot$Ligue=="Angleterre")
# et donc la commande suivante permet de récupérer uniquement les lignes des équipes anglaises
foot_anglais = foot[idx_anglais,]
summary(foot_anglais$Fautes)

# Question : Que constatez-vous pour les équipes anglaises et le nombre de fautes ?
# Réponse : Les équipes anglaises ont tendance à commettre moins de fautes que l'ensemble des équipes,
# comme le montrent les valeurs plus basses pour la plupart des statistiques descriptives (quartiles, médiane, moyenne et maximum).

# La commande boxplot permet de dessiner la boite à moustache d'une variable (en la mettant en paramètre)
# Question : Dessiner la boîte à moustaches de la variable Fautes (sur tout le tableau de données)
boxplot(foot$Fautes, main = "Boîte à moustaches des Fautes", ylab = "Nombre de Fautes")

# Boxplot permet également de dessiner la boîte à moustache par modalités d'une variable qualitative (ici Ligue):
boxplot(foot$Fautes~foot$Ligue, xlab="Ligue", ylab = "Nombre de fautes par match") # 5 boites à moustache : 1 par ligue
# Retrouvez vous la conclusion que l'on avait fait à propos des équipes anglaises et du nb de fautes ?
# Oui. En effet les équipes anglaises font moins de fautes comparé aux équipes des autres groupes

# Essayez de la même façon d'étudier la variable DuelsAeriens en fonction de la ligue, 
# ainsi que la variable Jaune en fonction de la ligue. Quelles conclusions pouvez vous faire ?
boxplot(foot$DuelsAeriens~foot$Ligue, xlab="Ligue", ylab = "Nombre de duels aériens par match")
boxplot(foot$Jaune~foot$Ligue, xlab="Ligue", ylab = "Nombre de cartons jaunes par match")
# Duels aériens : Les équipes allemandes sont en moyenne meilleures dans les duels aériens que les équipes des autres groupes
# Parcontre les équipes italiennes sont les plus mauvaises
# Cartons jaunes : Les équipes espagnols prennent beaucoup trop de cartonss jaunes
# A l'inverse, les équipes allemandes pratique un jeu beaucoup plus sain 

# La commande cor permet de calculer la corrélation entre 2 variables, par exemple:
cor(foot$Buts, foot$Possession)
# Ces deux variables sont-elles corrélées ?
# Réponse : valeur = 0.759747 donc oui, il existe une correlation importante entre le fait de posseder la balle et de marquer des buts

# On peut également obtenir la corrélation entre une variable et les autres:
cor(foot$Buts, foot[,1:15])
# Question : Quelle est la variable la plus corrélée à Buts ?
# Réponse : TirsCadres avec une valeur de corrélation égale à 0.8103011
# Regardez la corrélation d'une autre variable de votre choix à toutes les autres.
cor(foot$TirsCadres, foot[,1:15])
# Commentaire : TirsCadres est très correlé avec Tirs(0.8909303) et Buts (0.8103011). 
# Conclusion : Les qui tirent beaucoup ont beaucoup de tirs cadrés, et donc marquent beaucoup

#########################################################
###      Deuxième partie: ACP sur ces données       #####
#########################################################

foot_acp = PCA(foot, quali.sup = 16)
# La commande PCA permet de faire l'ACP. On lui indique que la 16ème colonne (variable ligue)
# est une variable qualitative qui ne doit pas être utilisée pour faire l'ACP (mais on pourra la 
# visualiser tout de même). Par défaut, toutes les variables sont centrées et réduites.

# Question : Quelle est l'inertie du nuage de point centré-réduit ?
# Réponse : 15

# Vous devriez avoir à l'écran les deux graphes de l'ACP : celui des individus et celui des variables.
# On les étudiera plus en détail tout à l'heure.

# La première chose à faire pour analyser les résultats d'une ACP est de décider
# combien d'axes vont être nécéssaires pour analyser les données.
# Pour cela, on doit regarder les valeurs propres de la matrice des corrélations. Elles 
# sont données par la commande:
foot_acp$eig
# Les noms des colonnes signifient : 
# - Valeur propre
# - pourcentage d'inertie
# - pourcentage cumulé d'inertie

# Questions:
# - Combien y a t'il de valeurs propres en tout? Est-ce normal ?
# Réponse : 15. Cela est normal et attendu, car le nombre de valeurs propres est égal au nombre de variables
# actives dans notre tableau de données

# - Que vaut la première valeur propre ?
# Réponse : 5.83543719

# - Quel est le pourcentage d'inertie expliqué par le premier axe ?
# Réponse : 38.9029146

# - Quel est le pourcentage d'inertie expliqué par le premier plan ?
# Réponse : valeur = 38.9029146 + 13.1226900 = 52.0256046

# - Combien d'axes décidez-vous de conserver pour mener cette analyse ? 
# Réponse : 4 car cela nous permettra d'expliquer environ 70.87% de la variance totale des données


# La commande 
foot_acp$var  
# fournit les coordonnéees des
# variables sur les axes factoriels (coord), leurs cosinus carrés(cos2) et leurs contributions (contrib). 
# Questions : 
# - Quelles sont les variables qui ont le plus contribué à créer l'axe 1 ? l'axe 2 ? l'axe 3 ? l'axe 4?
# Réponse : 
#   - Axe 1 : Possession(14.77%), PassesReuss(13.66%), TirsCadres(13.49%), Buts(12.51%), Tirs(12.08%)
#   - Axe 2 : Fautes(24.88%), FautesSubies(22.05%), Tacles(19.46%)
#   - Axe 3 : Jaune(25.89%), DuelsAeriens(18.64%), Interception(16.77%)
#   - Axe 4 : Rouge(74.63%) 

# - Comment pouvez-vous intérpréter le premier axe factoriel ? le deuxième ?
# Réponse : 
#   - Axe 1 : Performance offensive
#   - Axe 2 : Défense, agressivité


# La commande 
foot_acp$ind 
# permet de récupérer les mêmes informations mais au niveau des individus.
# Questions : 
# - Quelles sont les individus qui ont le plus contribué à créer l'axe 1 ? 
# Aide : sort(round(foot_acp$ind$contrib[,1], 2)) trie les contributions du premier axe dans un vecteur
# Réponse : 
sort(round(foot_acp$ind$contrib[,1], 2), decreasing = TRUE)
#   Manchester City : 8.47%
#   Paris Saint Germain : 7.66%
#   Barcelona : 5.88%
#   Napoli : 5.15%

# - Que pouvez-vous dire de ces individus ?
# Réponse : ces individus sont parmi les plus performants et dominants en termes de performance offensive.

# - Que peut-on dire de particulier sur l'équipe de Crotone ?
# Réponse : Creton contribue à 2.65% sur l'axe 1.Ce qui est moyen Donc l'équipe semble avoir des performances offensives limitées

# - Quelles sont les individus qui ont le plus contribué à créer l'axe 2 ? 
# Réponse :
sort(round(foot_acp$ind$contrib[,2], 2), decreasing = TRUE)
#   Bournemouth : 8.51
#   Schalke 04 : 5.96
#   Brighton : 5.56
#   Swansea : 5.25
#   Leicester : 4.84

# - Que pouvez vous dire de Bournemouth ? De Schalke 04 ?
# Réponse : Ces deux équipes ont un aspect très défensives et agressives

plot(foot_acp, choix = "ind", axes = c(1,2), habillage = 16)
# On a colorié les équipes en fonction de leur ligue.
# Question : où se situent principalement les équipes anglaises ? Quelles conclusions peut-on en tirer ?
# Réponse : Elles sont toutes du côté négatif de l'axe 2.
# Conclusion : Les équipes anglaises recoivent très peu défensives et agressives

# On va maintenant faire la même chose pour les axes 3 et 4 
plot(foot_acp, choix = "ind", axes = c(3,4), habillage = 16)
dev.new()
plot(foot_acp, choix = "var", axes = c(3,4))
# Question : - où se situent les équipes allemandes sur le graphe du deuxième plan ?
# Réponse : Elles se trouvent toutes du côté positif de l'axe 3

# - Quelles conclusions peut-on en tirer ?
# Réponse : Ces équipes sont douées dans les duels aériens, interception, mais sont trop sanctionnés par des cartons jaunes
#           D'autres vont jusqu'à être sanctionnées fréquemment par des cartons rouges

# Où est Saint-Etienne sur le deuxième plan ? Que peut on dire de cette équipe ?
# Réponse : Saint-Etienne se trouve du côté très négatif de l'axe 3, et du côté très positf de l'axe 4.
#           Saint-Etienne est l'équipe qui a le plus de cartons rouges, mais aussi le moins de duels aériens et d'interceptions

# Observez la position de Real Madrid et de Nantes sur ce deuxième plan. Peut-on alors dire que ces 
# deux équipes se ressemblent ?
# Réponse : Nantes et Real Madrid sont très proches. Cependant on ne peux pas affirmer qu'elles se ressemblent 
# car les 2 composantes principales de ce plan sont correlés entre elles d'où le risque que les projections 
# de ces points sur le plan soient mauvaises

