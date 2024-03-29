% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fonction_import_sas_label.R
\name{import_sas_label}
\alias{import_sas_label}
\title{Fonction pour l'import de données SAS}
\usage{
import_sas_label(data_file, catalog_file, blanc_as_NA = TRUE)
}
\arguments{
\item{data_file}{une base de donnée au format .sas7bdat}

\item{catalog_file}{un catalogue de format au format .sas7bcat}

\item{blanc_as_NA}{"oui" ou "non" pour changer les cellules vides en NA}
}
\value{
Une fonction pour l'import de donnée issues du logiciel SAS
}
\description{
Fonction pour l'import de données SAS
}
