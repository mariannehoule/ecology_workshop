# charger packages 
library(tidyverse)
library(ratadat)

#Graphique 
ggplot(data = complete_old, aes( x = weight, y = hindfoot_length)) +
  geom_point(color = "blue")