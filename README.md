# {autoacm}
Interface Shiny pour faire des ACM.  

**Documentation en cours de rédaction.**  

Le package a été développé avec la version 4.3 de R.

## Installation du package

### Solution 1
**Installation depuis Github**

1) Dans R, installer le package "devtools"

```{r }
install.packages("devtools")
```

2) Installer le package "autoacm"

```{r }
library("devtools")
install_github("arnomuller/autoacm")
```

3) Lancer l'application
```{r }
library(autoacm)
autoacm()
```



### Solution 2
**Installation locale**

1) Télécharger le package contenu dans le fichier "autoacm_0.0.0.9000.tar"

2) Dans R, lancer les codes suivants :

```{r }
library("devtools") 
# Installer autoacm
install.packages("chemin/statdesk_0.1.0.tar.gz", repos = NULL, type = "source")
```

3) Ouvrir l'application

```{r }
library(autoacm)
autoacm()
```





