#' Fonction pour l'import de données SAS
#'
#' @param data_file une base de donnée au format .sas7bdat
#' @param catalog_file un catalogue de format au format .sas7bcat
#' @param blanc_as_NA "oui" ou "non" pour changer les cellules vides en NA
#'
#' @return Une fonction pour l'import de donnée issues du logiciel SAS
#' @export
import_sas_label <- function(data_file,
                             catalog_file,
                             blanc_as_NA = TRUE){

  ### GESTION LIBRARY ----

  # Liste des packages à charger
  packages <- c("haven","dplyr")

  # Vérifier si les packages sont déjà installés
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

  # Installer les packages manquants
  if (length(missing_packages) > 0) {
    message("Installation des packages manquants : ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }

  # Charger les packages
  lapply(packages, require, character.only = TRUE)



  ### Import des données ----

  # Je crée un data.frame
  dt <- as.data.frame(
    # Je vais appliquer une fonction à toutes les colonnes de la liste issue
    # du read_sas avec le catalog.
    lapply(as.list(read_sas(data_file,
                            catalog_file = catalog_file)),
           function(col) {
             # Pour toutes les variables qui ont un label et
             # qui ont quasiment autant de label que de valeurs possibles
             if (any(class(col) == "haven_labelled")){


               # ATTENTION : les NA sont recodés en "", il ne faut donc pas le
               # compter parmis les modalités de la variable
               # De même les NSP sont souvent 99 ou 88 etc.

               # Je choisis de faire la différence entre le nb de modalité
               # et le nombre de label < 5 pour ces cas là.
               # A réflechir

               if (length(unique(col)) - length(attributes(col)$labels) < 5){
                 # On transforme en factor
                 return(as_factor(col))


               } else {

                 # Sinon on touche pas
                 return(col)
               }
             }else {
               # Sinon on touche pas
               return(col)

             }


           }))

  # Gestion des NA, pour les laisser en "" ou en NA
  if( isTRUE(blanc_as_NA)) {
    # On transforme les facteurs en NA
    dt <- dt %>%
      mutate_if(is.factor, ~ifelse(. == "", NA, .))
    dt
  } else {
    # On touche pas.
    dt
  }
}
