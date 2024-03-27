#acquisition des données

data=read.csv2("murderusa.csv")
txcrime09=data$tauxmeurtres
PDMabolie09=data$PDMabolie09

abolie09=txcrime09[PDMabolie09=="oui"]
nonabolie09=txcrime09[PDMabolie09=="non"]

abolie09=as.numeric(abolie09)
nonabolie09=as.numeric(nonabolie09)

#statistiques descriptives

boxplot(abolie09, nonabolie09,
        main = "Taux de criminalité",
        col = c("orange", "red"),
        names = c("abolie09", "nonabolie09"))

boxplot(txcrime09~PDMabolie09)

#anova

summary(aov(txcrime09~PDMabolie09))
anova(lm(txcrime09~PDMabolie09))

#détails

#somme des carrés due au modèle
SCM= length(abolie09)*(mean(abolie09)-mean(txcrime09))^2 + length(nonabolie09)*(mean(nonabolie09)-mean(txcrime09))^2
SCM

#somme des carrés des écarts
SCE= sum((abolie09-mean(abolie09))^2) + sum((nonabolie09-mean(nonabolie09))^2)
SCE

#variable de test
Fobs=SCM/(SCE/48)
Fobs

#p-value
1-pf(Fobs,1,48)

#la p-value est trop grande pour rejeter l'égalité des moyennes des deux groupes.
#On ne peut pas affirmer qu'il y a un lien significatif entre abolition de la peine de mort et taux de criminalité.