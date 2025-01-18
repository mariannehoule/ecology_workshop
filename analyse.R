# charger packages 
library(tidyverse)
library(ratadat)

#Graphique 
ggplot(data = complete_old, aes( x = weight, y = hindfoot_length)) +
  geom_point(color = "blue2")

#on doit d'abord sauvegarder la modification dans le fichier script ctrl s 

# on doit add la modification 
#git add analyse.R

#on commit la modification
#git commit -m "Modifier la couleur"

#on peut vérifier le status 
#git status 