#' Application Autoacm
#'
#' @param - no param for this function
#' @return
#' @export


autoacm <- function() {


  ### GESTION LIBRARY ----

  # Liste des packages à charger
  packages <- c("shiny", "shinyWidgets", "shinythemes", "shinyBS", "shinyjs", "fresh",
                "openxlsx", "haven","labelled","FactoMineR","factoextra","GDAtools","explor",
                "ade4","fastcluster","tidyverse","sortable","tidyr","DT","ggplot2",
                "colourpicker","RColorBrewer","corrplot","ggrepel","ggiraph","ggExtra","questionr",
                "rmarkdown","knitr","shinyFiles","here","flextable","officer")

  # Vérifier si les packages sont déjà installés
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

  # Installer les packages manquants
  if (length(missing_packages) > 0) {
    message("Installation des packages manquants : ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }

  # Charger les packages
  lapply(packages, require, character.only = TRUE)


  remove(list = ls(), envir = .GlobalEnv)


  appDir <- system.file("autoacm_app", package = "autoacm")
  shiny::runApp(appDir, host = '0.0.0.0', port = 3838)


  appOne <- shinyApp(
    ui,
    server)
  route <- "appOne"


  while (TRUE) {
    app <- switch(
      route,
      appOne = runApp(appOne),
      explorACM = explor(res_mca)
    )
    if (is.null(app)) {  # not matched
      warning("Unknown route ", route)
      route <- "appOne"
      next
    }

    # Execute a new app and retrieve return code
    route <- print(app)
    message("New route ", route)
    # browser()
  }


}
