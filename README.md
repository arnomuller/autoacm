# autoacm
Package R : Shiny facilitant l'exploration d'ACM à la française.
Version en développement


## Installation depuis Github

1) Dans R, installer le package "devtools"

```{r }
install.packages("devtools")
```

2) Installer le package "autoacm"

```{r }
library("devtools")
install_github("arnomuller/autoacm")
```

*2bis) Durant l'installation*

Le message suivant peut apparaître dans la console : 

> install_github("arnomuller/autoacm")  
Downloading GitHub repo arnomuller/autoacm@HEAD  
These packages have more recent versions available.  
It is recommended to update all of them.  
Which would you like to update?  
  
 1: All                                           
 2: CRAN packages only                            
 3: None 

Dans ce cas, rentrez 1 dans la console puis ENTER, pour installer toutes les dépendances.


3) Lancer l'application
```{r }
library(autoacm)
autoacm()
```



## Démarrage de l'application

![](https://github.com/arnomuller/autoacm/blob/main/img/autoacm_start.gif)


## Importation des données 

![](https://github.com/arnomuller/autoacm/blob/main/img/gif_import.gif)

## Séléction de la population

![](https://github.com/arnomuller/autoacm/blob/main/img/gif_selectpop2.gif)

## Exploration Uni/bi-variée

![](https://github.com/arnomuller/autoacm/blob/main/img/gif_exploracm.gif)

## Choix des variables

![](https://github.com/arnomuller/autoacm/blob/main/img/gif_varacm.gif)

## Transition vers explor()

![](https://github.com/arnomuller/autoacm/blob/main/img/gif_explor.gif)

## Exploration de l'ACM

![](https://github.com/arnomuller/autoacm/blob/main/img/gif_exploracm.gif)

## Classification

![](https://github.com/arnomuller/autoacm/blob/main/img/gif_classif.gif)
