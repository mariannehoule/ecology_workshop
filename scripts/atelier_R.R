library(tidyverse)
library(ratdat)

#pour avoir informations sur le jeu de données provenant de ratdat
?complete_old
#exploration de donnees
summary(complete_old)
head(complete_old)
#nous dit que cest un tibble, ce qui veut dire un data frame 
#tout dans R est un vecteur, un data frame est donc un assemblage de vecteurs (les variables) mis ensemble et qui comprennent les valeurs 
#la difference avec une matrice est que la matrice comprend tout le mm type de valeurs, le data frame comprend des nb, mots, etc
str(complete_old)

#c'est le temps d'utiliser ggplot :) 
library(ggplot2)

##################################################################################

# NUAGE DE POITNS
#écriture de base : on doit indiquer quel jeu de donnees (data=), quelles variables (mapping =) et le type de graph quon veut (geom_function=)
ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length)) + 
  geom_point(alpha = 0.2)
#dans geom on peut spécifier des arguments, ici on a mis alpha pour ajuster le niveau de transparence pour mieux voir les pts

#on recoit msg avis pcq on a des valeurs manquantes, on va les enlever manuellement 
complete_old <- filter(complete_old, !is.na(weight) & !is.na(hindfoot_length))
ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) + 
  geom_point(alpha = 0.2)
#on ajouté dans aes une couleur selon le le type de plot (type de variables, ici l'environnement des souris)

#faire un graphique avec la forme des points qui changent selon le sexe 
ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, shape = sex, color = sex)) + 
  geom_point(alpha = 0.2)
#faire un graphique avec la couleur en fonction de l'année 
ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = year)) + 
  geom_point(alpha = 0.2)
#comment choisir de meilleures couleurs avec la fonction scale 
ggplot(complete_old, mapping = aes(x = weight, y = hindfoot_length, color = plot_type)) + 
  geom_point(alpha = 0.2) + 
  scale_color_viridis_d() +
  scale_x_log10() #on peut aussi changer l'échelle pour mieux voir

#####################################################################################

# BOX PLOT 
ggplot(complete_old, mapping = aes( x = plot_type, y = hindfoot_length, color = plot_type)) +
  geom_jitter(alpha = 0.1) + #introduit un bruit aleatoire pour que les points soient répartis aléatoire a gauche/droit du boxplot
  geom_boxplot(outlier.shape = NA, fill = NA) + #on enleve les valeurs aberrantes
  scale_x_discrete(labels = label_wrap_gen(width = 10)) #on change les labels de la variable en x pour mieux voir 
#le premier argument qu'on écrit est mis en premier et les autres sont ajoutés par dessus, on peut donc décider si on veut les pts ou boxplot soient en avant plan 

# challenge faire un diagramme en violon 
ggplot(complete_old, mapping = aes( x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + #introduit un bruit aleatoire pour que les points soient répartis aléatoire a gauche/droit du boxplot
  geom_violin(fill = NA) + #on enleve les valeurs aberrantes et le filling du box
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() + #on peut changer avec des theme par defaut, ici noir et blanc
  theme(legend.position = "none")+ #on a enlevé la legende 
  labs(x = "Plot type", y = "Hindfoot length (mm)") #changer le titre des axes 


ggplot(complete_old, mapping = aes( x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + #introduit un bruit aleatoire pour que les points soient répartis aléatoire a gauche/droit du boxplot
  geom_boxplot(fill = NA, outlier.shape = NA) + #on enleve les valeurs aberrantes et le filling du box
  facet_wrap(vars(sex), ncol = 1) + #on veut séparer les mâles et femelles
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() + #on peut changer avec des theme par defaut, ici noir et blanc
  theme(legend.position = "none")+ #on a enlevé la legende 
  labs(x = "Plot type", y = "Hindfoot length (mm)") #changer le titre des axes 
#pour exporter le graphique 
plot_final <- ggplot(complete_old, mapping = aes( x = plot_type, y = hindfoot_length)) +
  geom_jitter(alpha = 0.1, aes(color = plot_type)) + #introduit un bruit aleatoire pour que les points soient répartis aléatoire a gauche/droit du boxplot
  geom_boxplot(fill = NA, outlier.shape = NA) + #on enleve les valeurs aberrantes et le filling du box
  facet_wrap(vars(sex), ncol = 1) + #on veut séparer les mâles et femelles
  scale_x_discrete(labels = label_wrap_gen(width = 10)) +
  theme_bw() + #on peut changer avec des theme par defaut, ici noir et blanc
  theme(legend.position = "none")+ #on a enlevé la legende 
  labs(x = "Plot type", y = "Hindfoot length (mm)")
ggsave(filename = "figures/plot_final.png", 
       plot = plot_final, 
       height = 6, 
       width = 8)

###############################################################################################

# TIDY VERSE 

surveys <- read_csv("data/raw/surveys_complete_77_89.csv")
view(surveys)

# select pour colonnes
select(surveys, plot_id, species_id)
select(surveys, c(3, 4)) #numéros de colonnes, pas super bonne pratique
select(surveys, -plot_id)
select(surveys, where(is.numeric)) #pour colonnes avec valeurs numériques
select(surveys, where(anyNA)) #pour colonnes avec valeurs manquantes

#filter pour lignes 
filter(surveys, year == 1988) #toutes les données de cette année
filter(surveys, species_id %in% c("RM", "DO")) #il regarde toutes les species id qui font partie de RM et DO
filter(surveys, year == 1988 & species_id %in% c("RM", "DO")) # on utilise & pour ajouter différentes fonctions

# SELECT AND FILTER
# challenge filter les données entre 1980 et 1985 ET les variables year, month, species id et plot id 
# 1ere facon : créer un objet
surveys_80_85 <- filter(surveys, year >= 1980 & year <= 1985)
select(surveys_80_85, year, month, species_id, plot_id) 
# si on doit faire bcp d'objets ca devient vrm compliqué à suivre 

#2e facon : emboiter les fonctions
select(filter(surveys, year >= 1980 & year <= 1985), year, month, species_id, plot_id)
#ligne de code pas super claire 

#3e et meilleure facon ###utiliser ctrl shift M pour les pipelines
surveys %>% 
  filter(year == 1980:1985) %>% 
  select(year, month, species_id, plot_id) #le pipe sert de rendre le code clair 

#challenge données de 1988 et prendre variables record_id, month, species_id 
surveys %>% 
  filter(year == 1988) %>% 
  select(record_id, month, species_id)

# MUTATE pour créer de nouvelles colonnes à partir de celles existantes
surveys %>% 
  mutate(weight_kg = weight / 1000,
         weight_lbs = weight_kg *2.2) %>% 
  relocate(weight_kg, .after = record_id) %>% 
  relocate(weight_lbs, .after = weight_kg)#relocaliser la nouvelle colonne au début pour quon puisse la voir
#on va enlever les valeurs manquantes 
surveys %>% 
  filter(!is.na(weight)) %>% 
  mutate(weight_kg = weight / 1000,
         weight_lbs = weight_kg *2.2) %>% 
  relocate(weight_kg, .after = record_id) %>% 
  relocate(weight_lbs, .after = weight_kg)


surveys %>% 
  mutate(date = paste(year, month, day, sep = "-")) %>% 
  relocate(date, .after = year)

#manipuler efficacement les dates 
library(lubridate)
surveys %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% #fonction ymd pour créer une vraie date
  relocate(date, .after = year)

# GROUP BY ET SUMMARIZE 
surveys %>% 
  group_by(sex) %>%  
  summarize(mean.weight = mean(weight, na.rm = TRUE), 
            count = n())
#challenge male et femelle avec date en x et nb observations en y  
surveys %>% 
  filter(!is.na(sex)) %>% 
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% #premiere etape on crée une colonne date
  group_by(sex, date) %>% #on groupe par sexe et date
  summarize(count = n()) %>% #nombre d'observations
  ggplot(aes(x = date, y = count, color= sex)) +
  geom_line() #lignes pour série temporelle
  
  
  
