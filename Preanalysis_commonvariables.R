##Packages

library(ggplot2) #graphique
library(cowplot) # pour arranger plusieurs graphiques de ggplot  dans la mem fenetre 
library(lmerTest)
library(nlme)
#library()
library(AICcmodavg)
library(multcomp)
library(dplyr)

#libray de stepwise
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

library(MuMIn)# pour calcul de r2
library(emmeans)#pour la comparison multiple 

#Pour clipcopy
library("questionr") 

#pour table model
library(sjPlot)
#pour table aussi apartir de data frame 
library(formattable)

#Appel des données
decay1and2season_vf <- read.csv2("Data_decomposition.csv") #

#ce fichier provient de  du code R Amélioration consigne mod hermine 
#qui a été enreistré sous  decay1_2019and2season_2019to2020Final_Vrai_for_model_vabsup.csv

#Je vais créer ici une colonne pour regrouper en sphaignes et Mousses 
#regrouper en sphaignes et pleuro mousse
decay1and2season_vf <- mutate (decay1and2season_vf, Bryophyte_group= ifelse(Substrate=="Pleurozium",yes= "Pleurozium", no ="Sphagnum"))

#str(decay1and2season_vf)
decay1and2season_vf$Treatment <- as.factor(decay1and2season_vf$Treatment)
decay1and2season_vf$Depth_depot <- as.factor(decay1and2season_vf$Depth_depot)
decay1and2season_vf$Substrate <- as.factor(decay1and2season_vf$Substrate)
Bryophyte_group <- as.factor(decay1and2season_vf$Bryophyte_group)
decay1and2season_vf$decomposition_state <- as.factor(decay1and2season_vf$decomposition_state)
decay1and2season_vf$Gene_decomp_state <- as.factor(decay1and2season_vf$Gene_decomp_state)
decay1and2season_vf$Growth_season <- as.factor(decay1and2season_vf$Growth_season)
decay1and2season_vf$Duration_months <- as.factor(decay1and2season_vf$Duration_months)
decay1and2season_vf$Duration_months_meanp <- as.factor(decay1and2season_vf$Duration_months_meanp)
decay1and2season_vf$Place_depot <- as.factor(decay1and2season_vf$Place_depot)

#par defaut R fait un contraste par reference et predn le premier  fateur par orde alphabétique
#Donc pour traitement comme je veux Control comme temoin il faut le modifier
decay1and2season_vf$Treatment <- relevel(decay1and2season_vf$Treatment, ref = "Control")
#Au cas ou je veux chnager lcordre mais pas besoin pour les traitements, il semettenet bien dans l'ordre
#english_newBD_rate$Treatment <- factor(english_newBD_rate$Treatment,levels = c("Control", "CPRS", "Harrow"))

#Aussi pour les substrat il prend capilifolium comme refrence or on a pa sde refrence je vais faire un contraste sum
# mais pas obligatoire. Donc la comparison se fait selon la moyenne globale
contrasts(decay1and2season_vf$Substrate) <- "contr.sum"
colnames(contrasts(decay1and2season_vf$Substrate)) <- c("Pleurozium", "Fuscum")
#Mettre en ordre voulu les substrats 

decay1and2season_vf$Substrate <- factor(decay1and2season_vf$Substrate, levels = c("Pleurozium", "Capillifolium", "Fuscum"))

#Pour etat de decompstion ELEMENTAIRE 
contrasts(decay1and2season_vf$decomposition_state) <- "contr.sum"
colnames(contrasts(decay1and2season_vf$decomposition_state)) <- c("F" ,  "H"  , "M1"  ,"M2"  ,"M3")
#Mettre en ordre les niveaux level  "F" , "M1" , "M2" , "M3" ,"H" ,"MIN" 
decay1and2season_vf$decomposition_state <- factor(decay1and2season_vf$decomposition_state, levels = c("F" , "M1" , "M2" , "M3" ,"H" ,"MIN" ))

#Pour etat de decompstion GENERAL
contrasts(decay1and2season_vf$Gene_decomp_state) <- "contr.sum"
colnames(contrasts(decay1and2season_vf$Gene_decomp_state)) <- c("F"  , "H" ,  "M" ) #on laisse toujours le dernier

#Mettre en ordre les niveaux level  
decay1and2season_vf$Gene_decomp_state <- factor(decay1and2season_vf$Gene_decomp_state, levels = c("F", "M", "H", "MIN"))

#Et si je fais un contrast de ref et prenant min comme reference 
#decay1and2season_vf$decomposition_state <- relevel(decay1and2season_vf$decomposition_state, ref = "F")


#Un contrast de refrence aussi pour profondeur de depot mais cest deja fait dans R pas besoin de faire 



#


