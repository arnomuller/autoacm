

### Packages utilisées ----


# Shiny :
library(shiny)         # Pour Shiny
library(shinyWidgets)  # Ajout d'options notamment SelectPicker
library(shinythemes)   # Changer le theme du Shiny
library(shinyBS)       # Ajout Bootstrap pour Shiny
library(shinyjs)       # Ajout d'option à Shiny, notamment la création d'un Hub qui appelle différent Shiny
library(fresh)         # Personnaliser l'interface CSS
#library(spsComps)


# Import de données
library(openxlsx)     # Pour importer et écrire des données dans d'autres formats (Excel)
library(haven)         # Pour importer et écrire des données dans d'autres formats (SAS)
library(labelled)      # Pour transformer les label SAS en factor


# ACM :
library(FactoMineR)    # Pour faire des ACM et créer un objet pour explor()
library(factoextra)    # Embellir les graphiques de FactoMineR (peut-être pas utilisé dans la VF)
library(GDAtools)      # Outils pour ACM : faire des ACM en ggplot2
library(explor)        # Pour manipuler les ACM dans un Shiny interactif
library(ade4)          # Autre package pour faire des ACM, on l'utilise pour faire des classifications
# Attention : si on indique pas le nombre de npc = Inf dans FactoMineR, alors les classifications seront différentes.
library(fastcluster)   # Accélère les clusters


# Manipulation de données :

library(tidyverse)     # Fonctions de manipulation de données
library(sortable)      # Pour réordonner les modalités
library(tidyr)         # NEW POUR TABLE PIVOT

# Tables
library(DT)            # Afficher des tables au format HTML

# Graphiques
library(ggplot2)       # Graphiques du tidyverse
library(colourpicker)  # Shiny : sélection manuelle des couleurs
library(RColorBrewer)  # Création de palette de couleurs
#library(plotly)        # Interactif
library(ggrepel)       # Eviter que les étiquettes des graphiques ne se superpose
library(corrplot)      # Graphique des sur/sous-représentations
library(ggiraph)
library(ggExtra)       # Permet plus d'options sur ggplot2

# Autres :
library(questionr)     # Plusieurs options utiles (pas forcément utilisé dans la VF)
# A AJOUTER :
# Sauvegarde :
library(rmarkdown)
library(knitr)
library(shinyFiles)
library(here)
library(flextable)
library(officer)

## Import SAS

#source("fonction_import_sas_label.R")



### Options ----

# Problème d'accents
options(encoding = 'UTF-8')

# Pour importer des données plus importantes (ici 80Mo)
options(shiny.maxRequestSize = 80*1024^2)

# Pour enlever les messages de Dplyr
options(dplyr.summarise.inform = FALSE)


options(scipen=999)
# Pour supprimer la notation scientifique des tables notamment.


jscode <- "shinyjs.closeWindow = function() { window.close(); }"
# Pour permettre la création d'un "hub" de Shiny permettant de lancer successivement des Shiny différents
# Dans notre cas, on supprime le Hub en faisant une passerelle directement de l'application 1 (la mienne),
# vers la fonction explor() de Julien Barnier.




shinyApp(
  ui = shinyUI(
    fluidPage(



      # Choisir le theme : exemple : https://rstudio.github.io/shinythemes/

      # On crée un thème personnalisé à partir du package "fresh"
      use_theme(create_theme(
        theme = "default",
        # Wellpanel et sidebar panel par défaut blanc + bordure grise
        bs_vars_wells(
          bg = "#FFF",
          border =  "#E2E2E2"

        ),
        # Texte des tabs
        bs_vars_global(
          link_color = "#78B27A" #texte pas sélectionné du tabs
        ),
        # Tabulation
        bs_vars_pills(
          border_radius = "100px", # radius de l'arrondi du coin (0% = carrée)
          active_link_hover_color = "#FFF",
          active_link_hover_bg = "#78B27A"
        ),
        # Police des textes
        bs_vars_font(
          size_base = "11px",
          size_h4 = "15px",
          size_h5 = "14px",

        )

      )),

      # sidebarPanel pour Import, Sous-population et sauvegarde
      sidebarPanel(
        # Largeur du panel
        width = 3,
        # Bordure en blanc
        style = "border: white",

        # Import du logo
        tags$figure(
          align = "center",
          tags$img(
            src = "chameau.png",
            width = "50%",
          )
        ),
        br(),
        wellPanel(
          style = "background: #E2E2E2",
          h4("Import des données :"),
          # IMPORT DES DONNEES
          # Création de bouton CSV ou Excel, pour l'import des données
          radioButtons("datatype","Format des données à importer : ",choices = c(".csv",".xlsx",".sas7bdat",".dta"), selected=".csv",inline=TRUE),

          conditionalPanel(condition="input.datatype=='.csv'",
                           # Comme d'une base à l'autre le séparateur du CSV peut changer, on propose un choix avec radioButtons() :
                           radioButtons("separator","Séparateur des colonnes :",choices = c(";",",",":"), selected=";",inline=TRUE)),


          # Si les données sont en SAS
          conditionalPanel(condition="input.datatype=='.sas7bdat'",
                           # On crée un bouton pour savoir si l'util possede un catalogue des labels
                           radioButtons("catalog","Avez-vous un catalogue de labels (format .sas7bcat) ?",
                                        choices = c("Oui","Non"), selected="Non",inline=TRUE)),


          fileInput('target_upload', 'Choix de la base de donnée',
                    accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      '.csv',
                      '.xlsx',
                      '.sas7bdat',
                      '.dta'),
                    buttonLabel = "Parcourir...",
                    placeholder = "Pas de base sélectionnée"),


          # Si l'util a dit avoir un catalogue de format, alors on lui permet de l'importer dans le signal catalog_upload
          conditionalPanel(condition="input.catalog=='Oui' && input.datatype=='.sas7bdat'",
                           fileInput('catalog_upload', 'Choix du catalogue de label SAS (.sas7bcat)',
                                     accept = '.sas7bcat',
                                     buttonLabel = "Parcourir...",
                                     placeholder = "Pas de catalogue sélectionné")),

          # DIMENSION DE LA BASE DE DONNEES
          textOutput("info_row"),
          textOutput("info_col"),

          # Possibilité de trier les colonnes alphabetiquement
          checkboxInput(inputId = "col_alpha", label = "Tri colonnes alphabétique", value = FALSE)

        ), # Fin Wellpanel

        br(),


        conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",

                         # CHOIX DE LA SOUS-POPULATION
                         wellPanel(
                           style = "background: #F4F4F4",
                           h4("Choix de la sous-population"),
                           uiOutput("selection_variables"),
                           # Choix des modalités d'intérêt
                           conditionalPanel(condition="output.afficher_choix_moda == 'Oui'",
                                            h5("Filtrer les modalités d'intérêt dans les variables choisies :"),
                                            # On appelle la boucle qui permet de faire la selection des modalités
                                            # Creée dans ServeurTableau.R
                                            uiOutput("selection_modalites"),
                                            fluidRow(column(12, align = "center",
                                                            actionButton("fill_modasouspop", "Bis"))),
                                            br()

                           ),
                           # Nombre d'individus
                           textOutput("info_row_filter")
                         ),

                         br(),




                         wellPanel(
                           style = "background: #A3D6A4",
                           h4("Sauvegarde"),

                           conditionalPanel(condition="output.afficher_sauvegarde_indic == 'Non'",
                                            helpText("Sauvegarder les résultats de l'ACM et/ou de la classification.")
                           ),

                           conditionalPanel(condition="output.afficher_sauvegarde_indic == 'Oui'",

                                            helpText("Appuyer sur ce bouton pour créer un rapport avec la sélection de population, les paramètres et les résultats de l'ACM et de la CAH"),

                                            br(),

                                            radioButtons('report_format', 'Document format', c('Word', 'HTML', 'PDF'),
                                                         inline = TRUE),
                                            fluidRow(column(12, align = "center",
                                                            downloadButton("report", "Rapport"))),
                                            br(),

                                            helpText("Sauvegarder les indicateurs de l'ACM :"),
                                            fluidRow(column(12, align = "center",downloadButton("save_indic", "Indicateurs ACM")))
                           )

                         ) # Fin sauvegarde
        ) # Fin conditional




      ), # Fin SidebarPanel


      # AIRE CENTRALE
      mainPanel(
        width = 9,
        # Un panneau composé de plusieurs onglets
        tabsetPanel(
          id = "windows",  type = "pills",

          # Onglet qui affiche la base de données
          tabPanel("Base de données", value = "BDD",
                   br(),

                   # Option de Zoom dans la table
                   conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                                    fluidRow(
                                      column(2, offset = 10,
                                             sliderInput("zoom_tab", label = NULL, min = 50,
                                                         max = 150, value = 80, post = "%", ticks = F)
                                      )
                                    )),
                   # Affichage de la table générale
                   fluidRow(
                     uiOutput("view_tab")
                   ),

                   conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                                    br(),
                                    br(),
                                    helpText("Télécharger les données récodées et/ou filtrées"),
                                    fluidRow(column(12, align="center", id="buttons",
                                                    downloadButton('downLoadFilter',"Télécharger"))))


          ), # Fin BDD


          # Onglet qui permet de recoder ou réordonner des variables
          tabPanel("Variables", value = "Recod_Reord",
                   br(),
                   h4("Recoder ou réordonner les modalités des variables"),
                   helpText("- Recoder une variable permet de modifier les noms de modalités et de regrouper plusieurs modalités dans une même catégorie."),
                   helpText("- Réordonner une variable permet de changer l'ordre d'affichage des modalités (pour les tables ou les graphiques)"),
                   br(),
                   # Bouton pour choisir entre recoder ou réordonner
                   radioButtons("recod_or_reord","Voulez-vous :",choices = c("Recoder","Réordonner"), selected="Recoder",inline=TRUE),
                   # Voir ServeurVariable.R

                   # Si on choisit recoder :
                   conditionalPanel(condition="input.recod_or_reord == 'Recoder'",
                                    # Choix de la variable à recoder
                                    uiOutput("choix_var_recod"),
                                    # Mise en forme du bouton GO en CSS
                                    tags$head(
                                      tags$style(
                                        HTML("
                                      #RecodeGO {
                                      background-color: #5E6FFF;
                                      color: white;
                                      border-color: #5E6FFF;
                                      }
                                      #RecodeGO:hover {
                                      background-color: darkblue;
                                      border-color: darkblue;
                                      }
                                           ")
                                      )
                                    ),
                                    # Bouton pour ouvrir l'interface de recodage
                                    fluidRow(column(4, align = "center",
                                                    actionButton("RecodeGO", "GO !", class = "btn-success"))),
                                    br(),
                                    # Affichage d'une table de la variable à recoder
                                    fluidRow(column(12, align = "center",
                                                    tableOutput("table_recod_avant"))),
                                    br(),
                                    # Si une variable est sélectionnée, affichage de l'interface de recodage.
                                    conditionalPanel(condition = "input.var_recod != ''",
                                                     uiOutput("recodage")
                                    ),
                                    # Affichage des tables de la variable recodée avant/après avec textes explicatifs
                                    conditionalPanel(condition = "input.var_recod == ''",
                                                     fluidRow(column(12, align = "center",
                                                                     textOutput("texte_table_avant"))),
                                                     fluidRow(column(12, align = "center",
                                                                     uiOutput("aff_table_avant2")))),
                                    # Affichage de la table de la nouvelle variable
                                    conditionalPanel(condition = "input.var_recod == ''",
                                                     fluidRow(column(12, align = "center",
                                                                     textOutput("texte_table_apres"))),
                                                     fluidRow(column(12, align = "center",
                                                                     uiOutput("aff_table_apres"))))),

                   # Si on choisit réordonner
                   conditionalPanel(condition="input.recod_or_reord == 'Réordonner'",
                                    # Choix de la variable à réordonner
                                    uiOutput("choix_var_reorder"),
                                    br(),
                                    # Interface pour réordonner
                                    uiOutput("reorder_ui"),
                                    # Bouton pour valider la réorganisation
                                    conditionalPanel(condition="input.var_reord != ''",
                                                     fluidRow(column(12, align = "center",
                                                                     actionButton("ReorderOK", "Valider", class = "btn-success")))),
                                    # Affichage de la table réorganisée
                                    fluidRow(column(12, align = "center",
                                                    uiOutput("aff_table_apres_reord")))
                   ) # Fin Reorder
          ), # Fin Variable


          # Onglet d'exploration des tables
          tabPanel("Tables", value = "Tables",
                   br(),
                   # h4("Tableaux croisées"),
                   helpText("Dans cet onglet vous pouvez croiser jusqu'à trois variables."),
                   # Voir ServeurTable.R

                   fluidRow(
                     # Affichage choix variables
                     column(5, wellPanel(
                       uiOutput("affichage_choix_var1"),
                       uiOutput("affichage_choix_var2"),
                       uiOutput("affichage_choix_var3"),
                       uiOutput("affichage_choix_var3_moda"))),
                     # Affichage image explicative
                     column(2, tags$figure(
                       align = "center",
                       tags$img(
                         src = "little_robot.svg",
                         width = "100%",
                       )
                     )),
                     # Affichage choix tables
                     column(5,
                            uiOutput("affichage_choix_table_type")),
                   ),
                   br(),

                   # Affichage de la table
                   fluidRow(column(12, align = "center",
                                   uiOutput("affichage_table")
                   )),
                   br(),
                   br(),
                   # Sauvegarde de la table

                   conditionalPanel(condition="output.afficher_choix_souspop == 'Oui' && input.var_table1 != ''",
                                    fluidRow(column(12, align="center", id="buttons",
                                                    downloadButton('savetable',"Télécharger la table"))))



          )# Fin Tables

          ,
          # Onglet ACM
          tabPanel("ACM", value = "ACM",
                   br(),
                   helpText("Dans cet onglet vous pouvez créer votre analyse des correspondances multiples"),
                   #h4("Analyse des Correspondances Multiples"),
                   # On affiche une phrase avec le nombre de personnes dans la population
                   uiOutput("recap_activ"),
                   br(),

                   conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                                    # On sélectionne les variables active et supplémentaires
                                    h5("Sélection des variables actives et supplémentaires"),
                                    uiOutput("acm_main_act"),
                                    uiOutput("acm_main_sup"),

                                    # On affiche 2 boutons
                                    helpText("Cliquer sur 'Valider' une fois votre choix de variables fait."),
                                    fluidRow(column(5, offset = 2,
                                                    # Valider l'ACM
                                                    actionButton("acmOK", "Valider", class = "btn-success"))
                                    )

                   ),



                   br(),
                   hr(), # Barre horizontale


                   # On affiche le bandeau de choix des représentations graphiques et tables
                   uiOutput("affichage_choix_ACM"),

                   # On affiche la représentation demandée
                   uiOutput("affichage_ACM"),

                   br(),
                   br())
          , # Fin ACM

          # Onglet CAH
          tabPanel("Classification", value = "CAH",

                   br(),
                   helpText("Dans cet onglet vous pouvez créer une classification à partir des distances de Ward."),

                   conditionalPanel(condition="output.afficher_choix_souspop == 'Oui'",
                                    fluidRow(column(6, align = "center",
                                                    actionButton("cahOK", "Lancer la classification", class = "btn-success")))),

                   br(),
                   br(),

                   conditionalPanel(condition="output.lancer_cah == 'Oui'",

                                    # On affiche le bandeau de choix de représentations
                                    uiOutput("affichage_choix_graph_cah"),
                                    # On affiche la représentation demandée
                                    uiOutput("affichage_graphique"),

                                    br(),
                                    br(),


                                    hr(),

                                    #### condipan

                                    h5("Description bivariée de la variable 'cluster' :"),
                                    # On affiche l'outil de selection de variable
                                    uiOutput("affichage_choix_table_cah"),
                                    # On affiche le tri croisé de "cluster" avec la variable voulue
                                    fluidRow(column(12, align="center", uiOutput("affichage_table_cah"))),

                                    br(),
                                    hr(),

                                    fluidRow(h5("Vous pouvez exporter les données avec la nouvelle variable de classe")),

                                    br(),

                                    fluidRow(column(12, align = "center",id="buttons",
                                                    downloadButton('downLoadCluster',"Télécharger avec variable de classes"))),

                                    br(),
                                    hr(),
                                    br()
                   )
                   ###########"
                   ,
                   conditionalPanel(condition="output.lancer_cah == 'Non'",

                                    fluidRow(
                                      column(12, align = "center",id="buttons",
                                             h5("ATTENTION : Vous avez changé la taille de l'échantillon,
                                    veuillez retourner sur l'onglet ACM pour la créer sur ce nouvel échantillon.")))

                   ),

                   conditionalPanel(condition="output.lancer_cah == 'Non_newACM'",

                                    fluidRow(
                                      column(12, align = "center",id="buttons",
                                             h5("ATTENTION : Vous avez changé l'ACM,
                                    veuillez relancer la classification.")))

                   )


          ) # Fin CAH


        ) # Fin tabsetpanel
      ) # Fin mainpanel
    ) # Fin fluidpage
  ), # Fin UI






  server = shinyServer(
    function(input, output, session) {



      # Import des données ----

      data <- reactive({
        inFile <<- input$target_upload
        if (is.null(inFile)) {
          return(NULL)
        }

        if (input$datatype == ".xlsx"){

          # Si ce qu'on importe n'est pas un xlsx, il ne se passe rien
          if (substr(inFile$datapath, nchar(inFile$datapath)-4, nchar(inFile$datapath)) == ".xlsx") {

            # On remplace les virgules par des points pour en faire des variables numeriques
            df <- data.frame(lapply(openxlsx::read.xlsx(inFile$datapath,1),
                                    function(x) {gsub(",", ".", x)})) %>%

              # On crée un identifiant pour chaque ligne
              mutate(autoacm_id = rownames(.)) %>%
              select(autoacm_id, everything())
            nomcol_data <<- colnames(df)

            return(df)
          } else {
            return(NULL)
          }

        } else if (input$datatype == ".csv"){ # Même processus pour les fichiers csv.
          if (substr(inFile$datapath, nchar(inFile$datapath)-3, nchar(inFile$datapath)) == ".csv") {

            tryCatch({

              df <- data.frame(lapply(read.csv(inFile$datapath, header = TRUE, sep = input$separator),
                                      function(x) {gsub(",", ".", x)})) %>%
                mutate(autoacm_id = rownames(.)) %>%
                select(autoacm_id, everything())


              nomcol_data <<- colnames(df)

            }, error = function(e) {
              return(NULL)
            })

            return(df)
          } else {
            return(NULL)
          }

        } else if (input$datatype == ".dta"){
          df <- as.data.frame(lapply(
            read_stata(inFile$datapath),
            as_factor)) %>%
            mutate(autoacm_id = rownames(.)) %>%
            select(autoacm_id, everything())
          nomcol_data <<- colnames(df)
          return(df)

        } else if (input$datatype == ".sas7bdat"){

          # Si pas de catalogue de labels :
          if (input$catalog == "Non"){

            if(substr(inFile$datapath, nchar(inFile$datapath)-3, nchar(inFile$datapath)) == "bdat"){
              #df <- as.data.frame(read_sas(inFile$datapath))

              df <- as.data.frame(read_sas(inFile$datapath)) %>%
                mutate(autoacm_id = rownames(.)) %>%
                select(autoacm_id, everything())

              nomcol_data <<- colnames(df)
              return(df)
            } else {
              return(NULL)
            }

          }

          # Si on a un catalogue de labels :
          if (input$catalog == "Oui"){
            # Import du fichier sas7bcat
            CataloginFile <<- input$catalog_upload
            if (is.null(CataloginFile))
              return(NULL)

            if(substr(inFile$datapath, nchar(inFile$datapath)-3, nchar(inFile$datapath)) == "bdat"){
              # Import de la base avec les labels qui deviennent des modalités

              df <- import_sas_label(data_file = inFile$datapath,
                                     catalog_file = CataloginFile$datapath,
                                     blanc_as_NA = FALSE)  %>%
                mutate(autoacm_id = rownames(.)) %>%
                select(autoacm_id, everything())


              nomcol_data <<- colnames(df)
              return(df)

            } else {
              return(NULL)
            }

          }
        }
      }) # Fin Import


      # Warning si pas le bon format de données
      observeEvent(input$target_upload, {
        if(is.null(data()) == T ){

          ### AJOUT IF POUR LE TEMPS DE METTRE LE CATALOGUE AVEC SAS
          showModal(modalDialog(
            title = "Attention",
            "Le format du fichier choisi n'est pas bon, choisir un autre fichier.",
            easyClose = TRUE,
            footer = NULL))

        }


      })


      # On crée un objet "reactiveValues" qui est une sorte d'objet reactif, qui va pouvoir
      # contenir d'autres objets comme des dataframes.
      # L'avantage c'est qu'on peut le modifier dans différents observeEvent
      # au contraire du simple reactive, qui ne peut être crée et modifié que dans un
      # unique bloc de code.

      v <- reactiveValues(data = NULL)
      # Idem pour les noms de variables
      nomreac <- reactiveValues(nomcol = NULL)


      # 1ère modification : quand on importe les données v$data prend la valeur des
      # des données.


      # L'objet contenant les données se modifie quand :
      # On importe les données
      observeEvent(input$target_upload, {
        data_init <<- data()
        v$data <<- data()
        nomreac$nomcol <<- colnames(data())
      })

      # On change le séparateur pour les csv
      observeEvent(input$separator, {
        validate(need(input$target_upload, 'Importer des données'))
        data_init <<- data()
        v$data <<- data()
        nomreac$nomcol <<- colnames(data())
      })


      # On active le catalogue
      observeEvent(input$catalog_upload, {
        validate(need(input$catalog_upload, 'Importer des données'))
        data_init <<- data()
        v$data <<- data()
        nomreac$nomcol <<- colnames(data())
      })

      # On change le type de données avec un message d'erreur si ce n'est pas le bon
      observeEvent(input$datatype, {
        validate(need(input$target_upload, 'Importer des données'))
        data_init <<- data()
        v$data <<- data()
        nomreac$nomcol <<- colnames(data())

        if(is.null(data()) == T ){

          showModal(modalDialog(
            title = "Attention",
            "Le format du fichier choisi n'est pas bon, choisir un autre fichier.",
            easyClose = TRUE,
            footer = NULL))

        }
      })


      # Changement de l'ordre des colonnes alphabetiquement
      observeEvent(input$col_alpha, {
        validate(need(input$target_upload, 'Importer des données'))


        if (input$col_alpha == TRUE) {
          #nomcol_data <<- order(colnames(data()))
          v$data <<- data() %>%
            select(order(colnames(data())))

          nomreac$nomcol <<- colnames(data() %>%
                                        select(order(colnames(data()))))

        } else{
          #nomcol_data <<- colnames(data())
          v$data <<- data()
          nomreac$nomcol <<- colnames(data())
        }


      })





      # On sauvegarde des objets réactifs qui renvoie les noms de variables.
      # Pour la base importée (= nomcol_data)
      nomcol_data_start <- reactive({
        nomcol_data
      })

      # Idem pour la base qui sera modifiée (utilisé dans les pickers)
      nomcol_data_reac <- reactive({
        colnames(v$data)
      })




      # Dimension de la table en entrée
      n_col_start <- reactive({ ncol(data()) })
      n_row_start <- reactive({ nrow(data()) })

      # Affichage dimension table
      output$info_row <- renderPrint({
        validate(need(data(), ""))
        cat("Nombre d'individus :", isolate(n_row_start()))
      })
      output$info_col <- renderPrint({
        validate(need(data(), ""))
        cat("Nombre de variables :", isolate(n_col_start()))
      })




      # SUITE DU SERVER ----

      # Pour la suite, on continue le server dans des scripts différents pour chacun des onglets.


      ################
      #### Onglet Recodage
      ################

      # C'est la première étape après l'import de données.
      # On va pouvoir modifier des variables en les recodant ou en les réordonnant.


      ###########################
      #### CREATION DE L'INTERFACE        ----
      ### POUR RECODER : ----

      # Choix de la variable à importer
      output$choix_var_recod <- renderUI({

        validate(need(input$target_upload, 'Importer des données'))

        pickerInput("var_recod", "Choix de la variable à recoder :", c("",nomreac$nomcol),
                    multiple = F,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable à recoder"))


      })

      # Table de cette variable
      output$table_recod_avant <- renderTable({
        validate(need(input$var_recod,''))

        hop <- as.data.frame(t(as.data.frame(with(v$data, addmargins(table(get(input$var_recod), useNA = "always")))))) %>%
          slice(2)
        colnames(hop) <- c(with(v$data, names(table(get(input$var_recod)))), "Non Réponse", "Total")
        hop

      })


      # Création d'un reactiveValues qui permet de contrôler le fait d'appuyer sur les boutons
      # permet l'affichage et la disparition de table
      button_state <- reactiveValues(bttn = 0, valid = FALSE)

      # Quand on clique prend valeur >= 1
      observeEvent(input$RecodeGO, {
        button_state$bttn <- button_state$bttn +1
      })
      # Quand on change de variable se reset
      observeEvent(input$var_recod, {
        button_state$bttn <- 0
      })
      # Si 0 table s'efface, si 1 table s'affiche

      # Idem pour RecodeOK
      observeEvent(input$RecodeOK, {
        button_state$valid <- button_state$valid +1
      })


      # Cadre avec le nom de la variable à recoder
      output$nom_var_recod_avant <- renderText({
        validate(need(input$var_recod,''))

        if (button_state$bttn > 0) {
          input$var_recod
        }
      })



      # Pour chaque modalité de la variable a recoder, on créer un cadre de texte
      # avec le nom de la modalité
      observeEvent(input$RecodeGO, {
        validate(need(input$var_recod,''))
        lapply((1:length(unique(with(v$data, get(input$var_recod))))), function(i) {
          outputId <- paste0("OUT", i)
          output[[outputId]] <- renderText(levels(with(v$data, as.factor(get(input$var_recod))))[i])

        })
      })


      # Ancien nom de la variable provient d'un input
      # C'est pas super utile, on pourrait se contenter de input$var_recod, mais
      # ça posait quelques problèmes plus loin
      var_recod_nom <- reactive({
        validate(need(input$var_recod,''))
        input$var_recod
      })


      # Nouveau nom que l'on retiendra pour nommer la nouvelle variable dans la base
      var_recod_nom_apres <- reactive({
        validate(need(input$var_recod,''))
        # Si l'utilisateur n'écrit pas un nom de variable,
        # on ajoute par defaut le suffixe _recode, sinon choix utilisateur
        if (input$nom_var_recod_apres == "") {
          paste0(input$var_recod, "_recode") # Pas utile
        } else {
          input$nom_var_recod_apres
        }
      })


      ## CREATION DES CASES AVEC LES MODALITES

      # Quand on choisit une variable à recoder, ça ouvre l'interface suivante :
      observeEvent(input$RecodeGO, {
        output$recodage <- renderUI({

          if (button_state$bttn > 0) {
            validate(need(input$target_upload,''))
            validate(need(input$var_recod, 'Choisir une variable'))
            # Par ligne :

            # Titre des colonnes
            wellPanel(style = "background: #FBFBFB",
                      fluidRow(
                        column(4, offset = 1,
                               h4("Variable à recoder :")),
                        column(4,offset = 2,
                               h4("Nouveau nom de la variable :"))
                      ),

                      # les noms de variables :
                      fluidRow(
                        column(4, align = "center",  offset = 1,
                               verbatimTextOutput("nom_var_recod_avant")),
                        column(2, align = "center",
                               icon("arrow-right", class = "fa-3x", lib = "font-awesome")),
                        column(4,
                               textInput("nom_var_recod_apres", NULL, placeholder = paste0(input$var_recod,"_recode"))) ### changement
                      ),
                      br(),
                      br(),

                      # Les titres pour les modalités
                      fluidRow(
                        column(4,  offset = 1,
                               h4("Modalité à recoder :")),
                        column(4,offset = 2,
                               h4(paste0("Nouvelles modalités de la variable :")))
                      ),

                      # Les cases de recodages :
                      lapply((1:length(unique(with(v$data, get(input$var_recod))))), function(i){ # Changement

                        inputId <- paste0("input_", i)
                        fluidRow(column(4,  offset = 1,
                                        verbatimTextOutput(outputId=paste0("OUT",i))),
                                 column(2, align = "center",
                                        icon("arrow-right", class = "fa-3x", lib = "font-awesome")),
                                 column(4,
                                        textInput(paste0("input_", i), NULL, width = 500, placeholder = levels(as.factor(with(v$data, get(input$var_recod))))[i]))) # Changement
                        #textInput(paste0("input_", i), NULL, width = 500, placeholder = "Même modalité")))

                      }), # FIN cases

                      # Affichage du bouton de validation
                      fluidRow(column(4, offset = 4 , align = "center",
                                      actionButton("RecodeOK", "Valider", class = "btn-success")))
            ) # FIN wellpanel

          } # FIN if
        }) # FIN UI
      }) # FIN observeEvent




      ### POUR REORDER : ----

      ## Choix de la variable à réordonner
      output$choix_var_reorder<- renderUI({

        validate(need(input$target_upload, 'Importer des données'))
        # Pour le moment, on ne peut reordonner que les variables en entrée
        # (techniquement possible avec des variables recodées, mais ça rend pas bien)
        fluidRow(pickerInput("var_reord", "Choix de la variable à réordonner :", c("",nomreac$nomcol),
                             multiple = F,
                             options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable à réordonner")))
      })


      ## Interface de reorder :
      # avec library(sortable)
      output$reorder_ui <- renderUI({

        validate(need(input$var_reord, 'Choisir une variable'))

        sortable::rank_list(
          text = "Cliquer et glisser les modalités dans l'ordre voulu",
          labels = with(v$data, levels(as.factor(get(input$var_reord)))),
          input_id = "rank_list_basic"
        )



      })

      ###########################


      ###########################
      #### RECODAGE                       ----


      # Quand on appuye sur le bouton recodage GO
      observeEvent(input$RecodeGO, {
        validate(need(input$var_recod,''))

        # On sauvegarde le nom de la variable choisi pour l'affichage de table même
        # quand var_recod est reset
        var_recod <<- input$var_recod

        # On crée une autre base et une nouvelle variable basée sur l'input
        recod_data <<- v$data  %>%
          mutate(newvar = as.character(get(var_recod)))

      }) # Fin var_recod



      # Quand on valide le recodage
      observeEvent(input$RecodeOK, {

        # On procède au recodage

        # Recodage si NA :
        if (anyNA(with(recod_data, get(var_recod))) == T) {
          # Pour chaque modalité de la variable (-1 pour les NA)
          # On donne la valeur dans la case recodage si l'utilisateur à écrit dedans,
          # sinon on garde la valeur précédente.
          for (i in c(1: (length(unique(with(recod_data, as.factor(get(var_recod)))))-1))) {
            recod_data <- recod_data %>%
              mutate(newvar = ifelse(is.na(get(var_recod)) == T, NA,
                                     ifelse(get(var_recod) != levels(with(recod_data, as.factor(get(var_recod))))[i], newvar,
                                            ifelse(input[[paste0("input_", i)]] != '' & input[[paste0("input_", i)]] != "NA" ,
                                                   input[[paste0("input_", i)]],
                                                   ifelse(input[[paste0("input_", i)]] == "NA", NA,
                                                          levels(with(recod_data, as.factor(get(var_recod))))[i])))))
          }
          # POUR LES NAs :
          recod_data <- recod_data %>%
            mutate(newvar = ifelse(is.na(get(var_recod)) == F, newvar,
                                   ifelse(input[[paste0("input_", length(unique(with(recod_data, get(var_recod)))))]] != '',
                                          input[[paste0("input_", length(unique(with(recod_data, get(var_recod)))))]], NA)))


        } else { # Si pas de NA dans la variable :

          for (i in c(1: length(unique(with(recod_data, get(var_recod)))))) {
            recod_data[ with(recod_data, get(var_recod)) ==
                          levels(with(recod_data, as.factor(get(var_recod))))[i],]$newvar <-  ifelse(input[[paste0("input_", i)]] != '' & input[[paste0("input_", i)]] != "NA" ,
                                                                                                     input[[paste0("input_", i)]],
                                                                                                     ifelse(input[[paste0("input_", i)]] == "NA", NA,
                                                                                                            levels(with(recod_data, as.factor(get(var_recod))))[i]))
          } # Fin for
        } # Fin else


        ## GESTION DES NOMS DE VARIABLES
        old_name <- "newvar"
        new_name <<- var_recod_nom_apres()

        # Si le nom existe déjà, la variable est automatiquement renommée avec le suffixe _new
        if (new_name %in% names(df) && new_name != old_name) {
          showModal(modalDialog(
            title = "ATTENTION : Nom de variable existant",
            "Ce nom de variable est déjà utilisée, la nouvelle variable a été recodée avec un suffixe numérique.",
            easyClose = TRUE,
            footer = NULL))
        }

        # Vérification si le nouveau nom est déjà dans le dataframe
        i <- 1
        while (new_name %in% names(recod_data) && new_name != old_name) {
          # Si le nom est déjà présent on ajoute un numéro à la fin du nom
          i <- i + 1
          new_name <- paste0(new_name, i)
        }
        # Renommer les variables avec le nouveau nom
        names(recod_data)[names(recod_data) == old_name] <- new_name
        # Sauvegarde de la base pour être sur
        recod_data <<- recod_data

        # Modification du fichier en entrée
        v$data <- recod_data
        nomreac$nomcol <- colnames(v$data)




        # Table de la variable selectionnee
        # Info
        output$texte_table_avant <- renderPrint({
          cat("Variable à recoder: ", var_recod)
        })
        # Table
        output$table_recod_avant2 <- renderTable({
          hop <- as.data.frame(t(as.data.frame(with(v$data, addmargins(table(get(var_recod), useNA = "always")))))) %>%
            slice(2)
          colnames(hop) <- c(with(v$data, names(table(get(var_recod)))), "Non Réponse", "Total")
          hop
        })
        # Affichage de la table avant recod
        output$aff_table_avant2 <- renderUI({
          tableOutput("table_recod_avant2")
        })



        # Table de la variable nouvelle
        # Info
        output$texte_table_apres <- renderPrint({
          cat("Nouvelle variable: ", new_name)
        })
        # Table
        output$table_recod_apres <- renderTable({
          tab <- as.data.frame(t(as.data.frame(with(v$data, addmargins(table(get(new_name), useNA = "always")))))) %>%
            slice(2)

          colnames(tab) <- c(with(v$data, names(table(get(new_name)))), "Non Réponse", "Total")
          tab
        })
        # Affichage de la table recodée
        output$aff_table_apres <- renderUI({
          tableOutput("table_recod_apres")
        })

      })








      #### REORDONNER                     ----

      # Quand on appuie sur le bouton :
      observeEvent(input$ReorderOK, {

        validate(need(input$var_reord, ''))

        # On sauvegarde le nouvelle ordre des modalités (pas forcément utile)
        new_order <<- input$rank_list_basic

        # On change l'ordre directement dans le fichier en entrée
        v$data[,input$var_reord] <- with(v$data, factor(get(input$var_reord), levels = input$rank_list_basic))


        # On affiche une table de la variable réordonnée
        output$table_reord_apres <- renderTable({
          tab_reord <- as.data.frame(t(as.data.frame(with(v$data, addmargins(table(get(input$var_reord), useNA = "always")))))) %>%
            slice(2)
          colnames(tab_reord) <- c(with(v$data, names(table(get(input$var_reord)))), "Non Réponse", "Total")
          tab_reord
        })
        output$aff_table_apres_reord <- renderUI({
          tableOutput("table_reord_apres")
        })

      }) # Fin reordonne



      ###########################






      ##################
      #### Onglet sous-population ----
      ##################



      ### Condition pour affichage des filtres ----

      # (dans UI)
      # Le filtre se fait sur la base recodée dans ServeurVariable.R
      output$afficher_choix_souspop <- reactive({
        if (is.null(v$data) == FALSE) {
          "Oui"
        }
      })
      outputOptions(output, "afficher_choix_souspop", suspendWhenHidden=FALSE)



      ### Filtre des modalités                 ----

      # Création de PickerInput dynamique, selon le nombre de variables de sous-population
      observeEvent(input$target_upload, {

        output$selection_variables <- renderUI({
          v$data
          tagList(
            fluidRow(
              column(9, selectizeInput('var_souspop',
                                       label=paste0("Sélection des variables"),
                                       # Choix parmis les noms de variables de data
                                       choices=c("",colnames(v$data)),
                                       # Plusieurs options :
                                       options = list(`actions-box` = TRUE, placeholder = 'Quelles variables pour sous-population ?'),
                                       multiple = TRUE # Si TRUE alors on peut choisir plusieurs variables.
              )),
              column(1, actionButton("fill_varsouspop", "Bis", style = 'margin-top:23px'))


            )
          )})


      })



      ### BOUTON REMPLISSAGE PRECEDENT

      previousVarSousPop <- reactiveValues(var_souspop = character(0))

      observeEvent(input$fill_varsouspop, {
        validate(need(input$target_upload, ''))
        # validate(need(input$acmOK, ''))

        updateSelectizeInput(session, "var_souspop", selected = previousVarSousPop$var_souspop)
      })

      observeEvent(input$var_souspop, {
        validate(need(input$target_upload, ''))
        previousVarSousPop$var_souspop <- input$var_souspop
      })






      # Condition affichage des boites de sélection des modalités (dans UI)
      output$afficher_choix_moda <- reactive({
        if (length(input$var_souspop) >= 1) { "Oui" }
      })
      outputOptions(output, "afficher_choix_moda", suspendWhenHidden=FALSE)



      # Je crée des boites qui renvoie une liste des modalités pour chaque variables séléctionnées,
      # l'utilisateur peut donc choisir celles qui l'intéresse
      output$selection_modalites <- renderUI({
        tagList(
          if(length(input$var_souspop) >= 1) { # si une variable sélectionné
            lapply((1:length(input$var_souspop)), function(i){
              pickerInput(inputId= input$var_souspop[i], # On donne comme signal le nom de la variable dans vect_var
                          label=paste0("Choix des filtres pour '",input$var_souspop[i], "'"),
                          # choices=levels(v$data[,which(nomcol_data == vect_var[i])]),
                          choices=levels(as.factor(as.character(v$data[,which(colnames(v$data) == input$var_souspop[i])]))),
                          options = list(`actions-box` = TRUE, `live-search` = TRUE, title = "Pas de filtre"),
                          multiple = TRUE)
            })
          }
        )}) # Fin renderUI


      ## BOUCLE FOR POUR FILL

      observeEvent(input$fill_modasouspop, {
        validate(need(input$target_upload, ''))
        validate(need(input$acmOK, ''))

        for (i in input$var_souspop) {
          updateSelectizeInput(session, i , selected = names(table(tempo[,i])))
        }
      })






      ### Création des donnees filtrees        ----


      filter_data <- reactive({
        validate(need(input$target_upload,"Importer des données"))

        data_conditionnel <- v$data

        if(length(input$var_souspop) >= 1) {
          for (i in c(1:length(input$var_souspop))) {
            if (is.null(input[[input$var_souspop[i]]]) == FALSE) {
              data_conditionnel <- data_conditionnel[data_conditionnel[[input$var_souspop[i]]] %in% input[[input$var_souspop[i]]],]

            }
          }
        }
        # S'il n'y a pas de sélection, on ne fait rien
        if (is.null(input$var_souspop)== T) { data_conditionnel <- data_conditionnel }

        filter_data2 <<- data_conditionnel

        return(data_conditionnel)
      })


      # Creation de l'objet dynamique qui contient les noms des variables
      nomcol_data2 <- reactive({ colnames(filter_data()) })

      # Nombre d'individus dans les données filtrées
      n_row_filter <- reactive({ nrow(filter_data()) })

      output$info_row_filter <- renderPrint({
        validate(need(filter_data(), ""))
        cat("Nombre d'individus après la sélection :", isolate(n_row_filter()))
      })






      ### Affichage de la table filtrees       ----


      output$table <- renderDataTable({
        validate(need(input$target_upload,""))

        tryCatch({

          ##############
          DT::datatable(filter_data(), extensions = 'Scroller', rownames = F, options = list(deferRender = F,
                                                                                             dom = 't',
                                                                                             # columnDefs = list(list(className = 'dt-center',
                                                                                             #                        targets = 5)),
                                                                                             scrollY = 500,  #Hauteur de la table en pixel
                                                                                             scroller = TRUE,
                                                                                             scrollX = T,
                                                                                             pageLength = 5))

        }, error = function(e) {

          datatable(
            data.frame(Erreur = c(paste("Error: ", e$message),
                                  "Vous pouvez changer le séparateur ou le type de fichier, dans l'import du fichier",
                                  "Ne pas changer d'onglet, sinon risque de 'crash'")),
            options = list(
              searching = FALSE,
              paging = FALSE,
              info = FALSE
            ), rownames = F
          )

        })

      })





      # Affichage table avec option du ZOOM
      output$view_tab <- renderUI({
        div(DTOutput("table"), style = paste0("font-size: ",input$zoom_tab,"%"))
      })



      ### Sauvegarde de la table               ----

      output$downLoadFilter <- downloadHandler(
        filename = function() {
          paste('Nouvelle_table_', Sys.Date(), '.csv', sep = '')
        },
        content = function(file){
          write_csv2(filter_data2,file)
        }
      )





      ###########
      #### Onglet Tables
      ###########


      ### GESTION DE L'UI            ----



      # Condition d'affichage des pickers de variables à mettre dans les tables
      output$affichage_choix_var1 <- renderUI({

        validate(need(input$target_upload, 'Importer des données'))

        fluidRow(pickerInput("var_table1", "Variable 1 :", c("",nomcol_data_reac()),
                             multiple = F,
                             options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable en ligne")))
      })

      output$affichage_choix_var2 <- renderUI({
        validate(need(input$target_upload, ''))
        fluidRow(
          conditionalPanel(condition="input.var_table1 != ''",
                           pickerInput("var_table2", "Variable 2 :", c("",nomcol_data_reac()),
                                       multiple = F,
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Variable en colonne"))))
      })

      output$affichage_choix_var3 <- renderUI({

        validate(need(input$target_upload, ''))
        fluidRow(
          conditionalPanel(condition="input.var_table2 != ''",
                           pickerInput("var_table3", "Variable 3 / modalités :", c("",nomcol_data_reac()),
                                       multiple = F,
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE, title ="Diviser les tables selon :"))))
      })

      output$affichage_choix_var3_moda <- renderUI({
        validate(need(input$target_upload, ''))
        fluidRow(
          # Choix de la moda de var3
          conditionalPanel(condition="input.var_table3 != ''",
                           selectizeInput('var_table3_moda',
                                          label=NULL,
                                          # Choix parmis les noms de variables de data
                                          choices= levels(as.factor(as.character(filter_data()[,which(colnames(filter_data()) == input$var_table3)]))) ,
                                          # Plusieurs options :
                                          options = list(`actions-box` = TRUE),
                                          multiple = FALSE,
                                          width = 450))
        )
      })



      # Condition d'affichage du choix de variable de pondération
      output$afficher_tri_ponder <- reactive({
        validate(need(input$checkbox_ponder,""))
        if (input$checkbox_ponder != FALSE) {
          "Oui"
        }
      })
      outputOptions(output, "afficher_tri_ponder", suspendWhenHidden=FALSE)


      # Bandeau du choix de la table a afficher
      output$affichage_choix_table_type <- renderUI({
        validate(need(input$target_upload, ''))
        wellPanel(
          fluidRow(
            selectInput(inputId="choix_table",
                        label="Choix du type de table : ",
                        choices= c("Effectifs univariés" = "eff_uni",
                                   "Effectif bivariés" = "eff",
                                   "% Ligne" = "pct_lign",
                                   "% Colonne" = "pct_col"))),
          fluidRow(
            # Case a cocher pour pondération
            checkboxInput(inputId = "checkbox_ponder", label = "Utiliser une pondération ?", value = FALSE, width = NULL)),
          fluidRow(
            # Choix de la variable de pondération
            conditionalPanel(condition="output.afficher_tri_ponder == 'Oui'",
                             selectizeInput('var_ponder_tri',
                                            label=NULL,
                                            # Choix parmis les noms de variables de data
                                            choices=c("",nomcol_data_reac()),
                                            # Plusieurs options :
                                            options = list(`actions-box` = TRUE, placeholder = 'Pas de pondération'),
                                            multiple = FALSE,
                                            width = 450))
          ),
          fluidRow(
            checkboxInput(inputId = "checkbox_na", label = "Afficher les valeurs manquantes ?", value = TRUE, width = NULL)
          )
        )


      })





      ### CREATION DES TABLES        ----

      table_to_save <- reactive({
        validate(need(input$target_upload, ''))
        validate(need(input$var_table1, 'Choisir une 1ère variable'))

        if (input$choix_table == "eff_uni"
        ){
          # SANS PONDERATION
          if (input$checkbox_ponder == FALSE) {
            # AVEC NA
            if (input$checkbox_na == TRUE) {

              # Sans Ponder avec NA
              as.data.frame(filter_data() %>%
                              group_by(get(input$var_table1)) %>%
                              summarise(Effectif = n()) %>%
                              rename(Variable = 1) %>%
                              mutate(Variable = as.character(Variable)) %>%
                              mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>%
                              mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3)) %>%
                              rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>%
                              mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                                     `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))

            }else{ # ELSE SANS NA

              # Sans Ponder sans NA
              as.data.frame(filter_data() %>%
                              group_by(get(input$var_table1)) %>%
                              summarize(Effectif = n()) %>%
                              rename(Variable = 1) %>%
                              mutate(Variable = as.character(Variable)) %>%
                              filter(is.na(Variable) == F) %>%
                              mutate(`Pourcent (%)` =  round(100 * Effectif / sum(Effectif),3)) %>%
                              rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>%
                              mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                                     `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))

            }

          }else{ # ELSE AVEC PONDER

            validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))

            # AVEC NA
            if (input$checkbox_na == TRUE) {


              # Avec Ponder avec NA
              as.data.frame(filter_data() %>%
                              group_by(get(input$var_table1)) %>%
                              summarise(Effectif = round(sum(as.numeric(get(input$var_ponder_tri))),3)) %>%
                              rename(Variable = 1) %>%
                              mutate(Variable = as.character(Variable)) %>%
                              mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>%
                              mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3))  %>%
                              rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>%
                              mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                                     `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))


            }else{ # ELSE SANS NA

              # Avec Ponder sans NA
              as.data.frame(filter_data() %>%
                              group_by(get(input$var_table1)) %>%
                              summarise(Effectif = round(sum(as.numeric(get(input$var_ponder_tri))),3)) %>%
                              rename(Variable = 1) %>%
                              mutate(Variable = as.character(Variable)) %>%
                              filter(is.na(Variable) == F) %>%
                              mutate(Variable = ifelse(is.na(Variable) == T, "Val.Manq.", Variable)) %>%
                              mutate(`Pourcent (%)` = round(100 * Effectif / sum(Effectif),3))  %>%
                              rows_insert(tibble(Variable = "Total", Effectif = NA, `Pourcent (%)` = NA)) %>%
                              mutate(Effectif = ifelse(is.na(Effectif), sum(Effectif, na.rm = T),Effectif),
                                     `Pourcent (%)` = ifelse(is.na(`Pourcent (%)`), sum(`Pourcent (%)`, na.rm = T),`Pourcent (%)`)))

            }
          }
        } else if(input$choix_table == "eff"
        ){
          validate(need(input$var_table2, 'Choisir une 2ème variable'))

          # Si 2 variables :
          if(input$var_table3 == ""){
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {


                # Sans Ponder avec NA

                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2))


              }else{ # ELSE SANS NA

                # Sans Ponder sans NA

                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Total"))) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2) )


              }

            }else{ # ELSE AVEC PONDER

              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))

              # AVEC NA
              if (input$checkbox_na == TRUE) {

                # Avec Ponder avec NA


                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                mutate(Effectif = round(ifelse(is.na(Effectif) == T,0, Effectif),2)) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2))


              }else{ # ELSE SANS NA

                # Avec Ponder sans NA



                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = round(ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif),2)) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Total"))) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2))




              }
            }

          } else { # Si 3 variables
            validate(need(input$var_table3,'Choisir une 3ème variable'))
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {


                # Sans Ponder avec NA AVEC 3 VARIABLES

                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2))


              }else{ # ELSE SANS NA

                # Sans Ponder sans NA AVEC 3 VARIABLES

                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Total"))) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2) )


              }

            }else{ # ELSE AVEC PONDER

              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))

              # AVEC NA
              if (input$checkbox_na == TRUE) {

                # Avec Ponder avec NA  AVEC 3 VARIABLE


                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                mutate(Effectif = round(ifelse(is.na(Effectif) == T,0, Effectif),2)) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2))


              }else{ # ELSE SANS NA

                # Avec Ponder sans NA  AVEC 3 VARIABLE


                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Effectif = NA)) %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(Variable2 == "Total", sum(Effectif, na.rm = T), Effectif)) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = round(ifelse(Variables == "Total", sum(Effectif, na.rm = T), Effectif),2)) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Total"))) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                pivot_wider(values_from = Effectif,
                                            names_from = Variable2))




              }
            }


          }
        } else if(input$choix_table == "pct_lign"
        ){
          validate(need(input$var_table2, 'Choisir une 2ème variable'))

          # Si 2 variables :
          if(input$var_table3 == ""){
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {

                # Sans Ponder avec NA
                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_l = 100*Effectif/Total) %>%
                                #select(Variables, Variable2, Pct_l) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variables) %>%
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>%
                                group_by(Variable2) %>%
                                mutate(Pct_l = round(ifelse(Variables == "Total", 100*sum(Effectif, na.rm = T)/nrow(filter_data()), Pct_l),2))%>%
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_l) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2)%>%
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))



              }else{ # ELSE SANS NA


                # Sans Ponder sans NA
                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_l = 100*Effectif/Total) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variables) %>%
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>%
                                group_by(Variable2) %>%
                                mutate(Pct_l = round(ifelse(Variables == "Total",
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter_data()[is.na(with(filter_data(),get(input$var_table1))) == F &
                                                                                   is.na(with(filter_data(),get(input$var_table2)))== F,]),
                                                            Pct_l),2))%>%

                                select(Variables, Variable2, Pct_l) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2) %>%
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))

              }

            }else{ # ELSE AVEC PONDER

              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))

              # AVEC NA
              if (input$checkbox_na == TRUE) {


                # Avec Ponder avec NA

                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_l = 100*Effectif/Total) %>%
                                #select(Variables, Variable2, Pct_l) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variables) %>%
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>%
                                group_by(Variable2) %>%
                                mutate(Pct_l = round(ifelse(Variables == "Total", 100*sum(Effectif, na.rm = T)/sum(with(filter_data(), as.numeric(get(input$var_ponder_tri)))), Pct_l),2))%>%
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_l) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2)%>%
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))



              }else{ # ELSE SANS NA

                # Avec Ponder sans NA
                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_l = 100*Effectif/Total) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variables) %>%
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>%
                                group_by(Variable2) %>%

                                mutate(Pct_l = round(ifelse(Variables == "Total",
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[is.na(with(filter_data(),get(input$var_table1))) == F &
                                                                                       is.na(with(filter_data(),get(input$var_table2)))== F,],
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_l),2))%>%


                                select(Variables, Variable2, Pct_l) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2) %>%
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))





              }
            }

          } else { # Si 3 variables

            validate(need(input$var_table3,'Choisir une 3ème variable'))

            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {


                # Sans Ponder avec NA AVEC 3 VARIABLES
                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_l = 100*Effectif/Total) %>%
                                #select(Variables, Variable2, Pct_l) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variables) %>%
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>%
                                group_by(Variable2) %>%
                                mutate(Pct_l = round(ifelse(Variables == "Total",
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter(filter_data(), get(input$var_table3) == input$var_table3_moda)),
                                                            Pct_l),2))%>%

                                select(Variables, Variable2, Pct_l) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2)%>%
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))


              }else{ # ELSE SANS NA


                # Sans Ponder sans NA AVEC 3 VARIABLE
                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_l = 100*Effectif/Total) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variables) %>%
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>%
                                group_by(Variable2) %>%
                                mutate(Pct_l = round(ifelse(Variables == "Total",
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter(filter_data(),
                                                                          get(input$var_table3) == input$var_table3_moda &
                                                                            is.na(get(input$var_table1)) == F &
                                                                            is.na(get(input$var_table2)) == F )),
                                                            Pct_l),2))%>%

                                select(Variables, Variable2, Pct_l) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2) %>%
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))

              }

            }else{ # ELSE AVEC PONDER

              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))

              # AVEC NA
              if (input$checkbox_na == TRUE) {

                # Avec Ponder avec NA AVEC 3 VARIABLES
                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_l = 100*Effectif/Total) %>%
                                #select(Variables, Variable2, Pct_l) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variables) %>%
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>%
                                group_by(Variable2) %>%
                                mutate(Pct_l = round(ifelse(Variables == "Total",
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[with(filter_data(),get(input$var_table3)) ==
                                                                                       input$var_table3_moda,],
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_l),2))%>%
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_l) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2)%>%
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))



              }else{ # ELSE SANS NA



                # Avec Ponder sans NA AVEC 3 VARIABLE
                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variables) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_l = 100*Effectif/Total) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Total", Pct_l = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variables) %>%
                                mutate(Pct_l = ifelse(Variable2 == "Total", sum(Pct_l, na.rm = T), Pct_l)) %>%
                                group_by(Variable2) %>%
                                mutate(Pct_l = round(ifelse(Variables == "Total",
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[with(filter_data(),get(input$var_table3)) ==
                                                                                       input$var_table3_moda  &
                                                                                       with(filter_data(),is.na(get(input$var_table1))) == F &
                                                                                       with(filter_data(),is.na(get(input$var_table2))) == F  ,],
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_l),2))%>%

                                select(Variables, Variable2, Pct_l) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Total"))) %>%
                                pivot_wider(values_from = Pct_l,
                                            names_from = Variable2) %>%
                                mutate(Variables = ifelse(Variables == "Total", "Marge", Variables),
                                       Total = ifelse(Variables == "Marge", 100, Total)))


              }
            }


          }
        } else if(input$choix_table == "pct_col"
        ){
          validate(need(input$var_table2, 'Choisir une 2ème variable'))

          # Si 2 variables :
          if(input$var_table3 == ""){
            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {

                # Sans Ponder avec NA

                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_c = 100*Effectif/Total) %>%
                                #select(Variables, Variable2, Pct_c) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variable2) %>%
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>%
                                group_by(Variables) %>%
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge", 100*sum(Effectif, na.rm = T)/nrow(filter_data()), Pct_c),2))%>%
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_c) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>%
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2)%>%
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))



              }else{ # ELSE SANS NA

                # Sans Ponder sans NA
                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_c = 100*Effectif/Total) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variable2) %>%
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>%
                                group_by(Variables) %>%
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge",
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter_data()[is.na(with(filter_data(),get(input$var_table1))) == F &
                                                                                   is.na(with(filter_data(),get(input$var_table2)))== F,]),
                                                            Pct_c),2))%>%

                                select(Variables, Variable2, Pct_c) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>%
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2) %>%
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))


              }

            }else{ # ELSE AVEC PONDER

              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))

              # AVEC NA
              if (input$checkbox_na == TRUE) {

                # Avec Ponder avec NA

                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_c = 100*Effectif/Total) %>%
                                #select(Variables, Variable2, Pct_c) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variable2) %>%
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>%
                                group_by(Variables) %>%
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge", 100*sum(Effectif, na.rm = T)/sum(with(filter_data(), as.numeric(get(input$var_ponder_tri)))), Pct_c),2))%>%
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_c) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>%
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2)%>%
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))


              }else{ # ELSE SANS NA

                # Avec Ponder sans NA
                as.data.frame(filter_data() %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_c = 100*Effectif/Total) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variable2) %>%
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>%
                                group_by(Variables) %>%

                                mutate(Pct_c = round(ifelse(Variable2 == "Marge",
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[is.na(with(filter_data(),get(input$var_table1))) == F &
                                                                                       is.na(with(filter_data(),get(input$var_table2)))== F,],
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_c),2))%>%


                                select(Variables, Variable2, Pct_c) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>%
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2) %>%
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))




              }
            }

          } else { # Si 3 variables

            validate(need(input$var_table3,'Choisir une 3ème variable'))

            # SANS PONDERATION
            if (input$checkbox_ponder == FALSE) {
              # AVEC NA
              if (input$checkbox_na == TRUE) {


                # Sans Ponder avec NA AVEC 3 VARIABLES

                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_c = 100*Effectif/Total) %>%
                                #select(Variables, Variable2, Pct_c) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variable2) %>%
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>%
                                group_by(Variables) %>%
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge",
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter(filter_data(), get(input$var_table3) == input$var_table3_moda)),
                                                            Pct_c),2))%>%
                                select(Variables, Variable2, Pct_c) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>%
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2)%>%
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))


              }else{ # ELSE SANS NA

                # Sans Ponder sans NA AVEC 3 VARIABLE
                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = n()) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_c = 100*Effectif/Total) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variable2) %>%
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>%
                                group_by(Variables) %>%
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge",
                                                            100*sum(Effectif, na.rm = T)/
                                                              nrow(filter(filter_data(),
                                                                          get(input$var_table3) == input$var_table3_moda &
                                                                            is.na(get(input$var_table1)) == F &
                                                                            is.na(get(input$var_table2)) == F )),
                                                            Pct_c),2))%>%

                                select(Variables, Variable2, Pct_c) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>%
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2) %>%
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))


              }

            }else{ # ELSE AVEC PONDER

              validate(need(input$var_ponder_tri, 'Choisir une variable de pondération'))

              # AVEC NA
              if (input$checkbox_na == TRUE) {

                # Avec Ponder avec NA AVEC 3 VARIABLES

                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = round(sum(as.numeric(get(input$var_ponder_tri))),5)) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                mutate(Variables = ifelse(is.na(Variables) == T, "Val.Manq.", as.character(Variables))) %>%
                                mutate(Variable2 = ifelse(is.na(Variable2) == T, "Val.Manq.", as.character(Variable2))) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_c = 100*Effectif/Total) %>%
                                #select(Variables, Variable2, Pct_c) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variable2) %>%
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>%
                                group_by(Variables) %>%
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge",
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[with(filter_data(),get(input$var_table3)) ==
                                                                                       input$var_table3_moda,],
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_c),2))%>%
                                #### ATTENTION AU DESSUS, ON DIVISE PAR nrow(filter_data), mais pour 3 variables ou sans NA, il faut enlever ces gens !!
                                ### Et pour ponder il faut la somme des pondérations
                                select(Variables, Variable2, Pct_c) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>%
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2)%>%
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))


              }else{ # ELSE SANS NA


                # Avec Ponder sans NA AVEC 3 VARIABLE
                as.data.frame(filter_data() %>%
                                filter(get(input$var_table3) == input$var_table3_moda) %>%
                                group_by(get(input$var_table1), get(input$var_table2)) %>%
                                summarise(Effectif = sum(as.numeric(get(input$var_ponder_tri)))) %>%
                                rename(Variables = 1) %>%
                                rename(Variable2 = 2) %>%
                                filter(is.na(Variables) == F) %>%
                                filter(is.na(Variable2) == F) %>%
                                mutate(Variables = as.character(Variables)) %>%
                                mutate(Variable2 = as.character(Variable2)) %>%
                                ungroup() %>%
                                complete(Variables,Variable2) %>%
                                group_by(Variable2) %>%
                                mutate(Effectif = ifelse(is.na(Effectif) == T,0, Effectif)) %>%
                                mutate(Total = sum(Effectif, na.rm = T)) %>%
                                mutate(Pct_c = 100*Effectif/Total) %>%
                                ungroup() %>%
                                rows_insert(tibble(Variables = "Total", Variable2 = "Marge", Pct_c = NA)) %>%
                                complete(Variables,Variable2)%>%
                                group_by(Variable2) %>%
                                mutate(Pct_c = ifelse(Variables == "Total", sum(Pct_c, na.rm = T), Pct_c)) %>%
                                group_by(Variables) %>%
                                mutate(Pct_c = round(ifelse(Variable2 == "Marge",
                                                            100*sum(Effectif, na.rm = T)/
                                                              sum(with(filter_data()[with(filter_data(),get(input$var_table3)) ==
                                                                                       input$var_table3_moda  &
                                                                                       with(filter_data(),is.na(get(input$var_table1))) == F &
                                                                                       with(filter_data(),is.na(get(input$var_table2))) == F  ,],
                                                                       as.numeric(get(input$var_ponder_tri)))),
                                                            Pct_c),2))%>%

                                select(Variables, Variable2, Pct_c) %>%
                                arrange(match(Variables, c(levels(as.factor(with(filter_data(),get(input$var_table1)))), "Val.Manq.", "Total")),
                                        match(Variable2, c(levels(as.factor(with(filter_data(),get(input$var_table2)))), "Val.Manq.", "Marge"))) %>%
                                pivot_wider(values_from = Pct_c,
                                            names_from = Variable2) %>%
                                mutate(Marge = ifelse(Variables == "Total", 100, Marge)))



              }
            }


          }
        }


      })



      #### Affichage de la table     -----


      # Possibilité de l'afficher en data.frame, on préfère une simple table

      output$tri_affiche <- renderTable({
        table_to_save()

      })

      observeEvent(input$var_table1, {
        output$affichage_table <- renderUI({
          tableOutput("tri_affiche")
        })
      })




      #### Warning trop de modalités ----

      # Pour la variable 1, si plus de 9 modalités
      observeEvent(input$var_table1, {
        validate(need(input$var_table1,""))
        if(dim(with(filter_data(), table(get(input$var_table1))))  >= 9 ){

          # showNotification("This is a notification.")
          showModal(modalDialog(
            title = "Nombre de modalités trop important",
            "La variable que vous avez sélectionné a trop de modalité pour être observée correctement dans une table. \n Vous pouvez la recoder dans l'onglet 'Variables'",
            easyClose = TRUE,
            footer = NULL))

        }
      })

      # Pour la variable 2, si plus de 9 modalités
      observeEvent(input$var_table2, {
        validate(need(input$var_table2,""))
        if(dim(with(filter_data(), table(get(input$var_table2))))  >= 9 ){

          showModal(modalDialog(
            title = "Nombre de modalités trop important",
            "La variable que vous avez sélectionné a trop de modalité pour être observée correctement dans une table. \n Vous pouvez la recoder dans l'onglet 'Variables'",
            easyClose = TRUE,
            footer = NULL))

        }

      })

      # Pour la variable 3, si plus de 5 modalités
      observeEvent(input$var_table3, {
        validate(need(input$var_table3,""))
        if(dim(with(filter_data(), table(get(input$var_table3))))  >= 9 ){

          showModal(modalDialog(
            title = "Nombre de modalités trop important",
            "La variable que vous avez sélectionné a trop de modalité pour être observée correctement dans une table. \n Vous pouvez la recoder dans l'onglet 'Variables'",
            easyClose = TRUE,
            footer = NULL))

        }

      })


      #### Sauvegarde de la table    ----


      output$savetable <- downloadHandler(
        filename = function() {
          paste('statdesc-', Sys.Date(), '.xlsx', sep = '')
        },
        content = function(file){
          openxlsx::write.xlsx(table_to_save(),file)
        }
      )



      #### Server ACM

      # Dans ce script, je continue la partie server et je crée les objets nécessaires à l'ACM
      # Je présente quelques outils pour explorer l'ACM, repris d'Anton Perdoncin : https://quanti.hypotheses.org/1871


      ### TEXTE INTRODUCTION ----


      n_row_acm <- reactive({
        nrow(filter_data())
      })

      n_row_acm_rapport <- reactive({
        nrow(data_acm)
      })


      output$recap_activ <- renderPrint({
        validate(need(filter_data(), ""))
        cat("La taille de la population sélectionnée est de :", isolate(n_row_acm()),"individus. \n")
      })




      ### SELECTION DES VARIABLES ACTIVES ET SUPPLEMENTAIRES ----


      # Selection des variables actives
      output$acm_main_act <- renderUI({

        validate(need(input$target_upload, 'Importer des données'))

        tagList(
          fluidRow(
            column(4, selectizeInput('var_act',
                                     label=paste0("Choix des variables actives"),
                                     # Choix parmis les noms de variables de data
                                     choices=c("",nomcol_data_reac()),
                                     # Plusieurs options :
                                     options = list(`actions-box` = TRUE, placeholder = 'Pas de variables actives'),
                                     multiple = TRUE, # Si TRUE alors on peut choisir plusieurs variables.
                                     width = 450)),
            column(1, actionButton("fill_act", "Bis", style = 'margin-top:23px')),

            #Sélection de pondération
            column(4, offset = 1,
                   selectizeInput('var_ponder',
                                  label=paste0("Choix de la variable de pondération"),
                                  # Choix parmis les noms de variables de data
                                  choices=c("",nomcol_data_reac()),
                                  # Plusieurs options :
                                  options = list(`actions-box` = TRUE, placeholder = 'Pas de pondération'),
                                  multiple = FALSE,
                                  width = 450)),
            column(1, actionButton("fill_ponder", "Bis", style = 'margin-top:23px'))
          )
        )})



      # Selection des variables supplémentaires

      # La sélection ne se fait que quand des variables ont ête prises en actives
      # On enlève les variables choisies en active des choix possibles en supplémentaire.

      observeEvent(input$var_act,{
        if(length(input$var_act) ==""){

          output$acm_main_sup <- renderUI({
            data()
            tagList(
              fluidRow(
                column(4,
                       selectizeInput('var_sup',
                                      label=paste0("Choix des variables supplémentaires"),
                                      choices=c("",nomcol_data_reac()),
                                      options = list(`actions-box` = TRUE, placeholder = 'Pas de variables supplémentaires'),
                                      multiple = TRUE,
                                      width = 450)),
                column(1, actionButton("fill_sup", "Bis", style = 'margin-top:23px'))
              ))})

        }else{
          # Si des variables ont été choisies en actives :
          output$acm_main_sup <- renderUI({
            data()
            tagList(
              fluidRow(
                column(4,
                       selectizeInput('var_sup',
                                      label=paste0("Choix des variables supplémentaires"),
                                      choices = c("",nomcol_data_reac()[!(nomcol_data_reac() %in% input$var_act)]), # on enlève les actives
                                      options = list(`actions-box` = TRUE, placeholder = 'Pas de variables supplémentaires'),
                                      multiple = TRUE,
                                      width = 450)),
                column(1, actionButton("fill_sup", "Bis", style = 'margin-top:23px'))
              ))})
        }
      })




      #### BOUTON RECUPERER LES VARIABLES SELECTIONNEES

      previousSelections <- reactiveValues(var_act = character(0), var_sup = character(0), var_ponder = character(0))


      observeEvent(input$fill_act, {
        validate(need(input$target_upload, ''))
        validate(need(input$acmOK, ''))

        updateSelectizeInput(session, "var_act", selected = previousSelections$var_act)
      })

      observeEvent(input$fill_sup, {
        validate(need(input$target_upload, ''))
        validate(need(input$acmOK, ''))

        updateSelectizeInput(session, "var_sup", selected = previousSelections$var_sup)
      })


      observeEvent(input$fill_ponder, {
        validate(need(input$target_upload, ''))
        validate(need(input$acmOK, ''))

        updateSelectizeInput(session, "var_ponder", selected = previousSelections$var_ponder)
      })



      observeEvent(input$var_act, {
        validate(need(input$target_upload, ''))
        previousSelections$var_act <- input$var_act
      })

      observeEvent(input$var_sup, {
        validate(need(input$target_upload, ''))
        previousSelections$var_sup <- input$var_sup
      })

      observeEvent(input$var_ponder, {
        validate(need(input$target_upload, ''))
        previousSelections$var_ponder <- input$var_ponder
      })





      ########### CLIQUER SUR LE BOUTON ACM ----


      observeEvent(input$acmOK, {


        showModal(modalDialog("Le chargement de l'ACM peut prendre quelques secondes", footer=NULL))
        #showNotification("Le chargement de l'ACM peut prendre quelques secondes", duration = 15, type = "warning")

        n_acmOK <<- input$acmOK[1]
        indic_cah <<- "Non"

        # if(exists("ncluster") == TRUE){
        #   rm("ncluster", "arbre", "inertie")
        # }

        ncluster <<- NULL
        arbre <<- NULL
        inertie <<- NULL
        meth_dist <<- NULL


        ### CREATION DE LA TABLE SOUS-POP

        if(length(input$var_souspop) >= 1){
          # Pour le rapport automatisé :
          # Je garde une base temporaire avec les variables sur lesquelles on a fait une selection
          tempo <<- filter_data() %>%
            select(all_of(input$var_souspop))

          # Je créer une table avec les variables et les modalités de sélection de population.
          tableau_modalite <<- data.frame("Variable" = input$var_souspop) %>%
            mutate(Modalite = NA)

          for (i in c(1:ncol(tempo))) {
            tableau_modalite[i,]$Modalite <<- paste(names(table(tempo[,i])), collapse = ", ")
          }
        } else {

          tableau_modalite <<- data.frame("Variable" = "Pas de sélection", "Modalite" = "Pas de sélection" )

        }

        ### CREATION DE L'OBJET FACTOMINER ----

        # On ne continue que si on a des variables actives
        validate(need(input$var_act,''))

        # Copie des variables utilisées (on pourra enlever à l'avenir)
        var_sup <<- input$var_sup
        var_act <<- input$var_act
        var_ponder <<- input$var_ponder

        # On ne garde que les variables choisies en actives et supplementaires



        ####################
        if("cluster" %in% nomreac$nomcol) {

          # cluster_data <- filter_data() %>%
          #   select(-cluster)

          data_init2 <<- v$data %>%
            select(-"cluster")

        } else {
          # cluster_data <<- filter_data()
          data_init2 <<-  v$data
        }
        ####################

        data_acm <<- filter_data() %>%
          select(autoacm_id,all_of(input$var_sup), all_of(input$var_act))

        # On créer un vecteur avec les poids quand il y en a :
        if(var_ponder != ""){
          ponder <<- as.numeric(as.character(as_vector(filter_data() %>% select(all_of(input$var_ponder)))))
        }



        # On fait l'ACM :

        # Si on a des variables supplementaires :
        if(is.null(var_sup) == FALSE){

          # Si on a une pondération
          if(var_ponder != ""){
            # Comme on a placé les var sup en 1er, on a juste à indiquer leurs numéros dans quali.sup
            res_mca <<- MCA(data_acm[,2:ncol(data_acm)], graph = FALSE , ncp = Inf,
                            quali.sup = c(1:length(input$var_sup)),
                            row.w = ponder)
          }
          # Sans pondération
          else{
            res_mca <<- MCA(data_acm[,2:ncol(data_acm)], graph = FALSE , ncp = Inf,
                            quali.sup = c(1:length(input$var_sup)))
          }
        }

        # Sans variables supplementaires
        else {
          # Si on a une pondération
          if(var_ponder != ""){
            res_mca <<- MCA(data_acm[,2:ncol(data_acm)], graph = FALSE, ncp = Inf,
                            row.w = ponder)
          }
          # Sans pondération
          else{
            res_mca <<- MCA(data_acm[,2:ncol(data_acm)], graph = FALSE, ncp = Inf)
          }
        }
        ############################





        ############################
        ### CREATIONS D'OBJETS POUR TABLES ET GRAPHIQUES ----

        # Pour les sauts d'inertie : ----
        variances <<- as.data.frame(round(res_mca$eig,2)) %>%
          #rownames_to_column() %>%                                  # récupérer les noms de lignes (dim 1, dim 2, etc) dans une colonne distincte
          mutate(rowname = rownames(.)) %>%
          slice(1:10) %>%                                           # conserver que les infos des 10 premiers axes
          mutate(Axes = str_replace_all(rowname, "dim", "Axe")) %>% # créer une nouvelle variable à partir de rowname, qui prend les valeurs "Axe 1, Axe 2, etc" au lieu de "dim 1, dim 2, etc."
          select(-rowname) %>%                                      # on enlève cette colonne dont on n'a plus besoin
          rename(`Valeurs propres` = eigenvalue) %>%
          rename(`% de variance` = `percentage of variance`) %>%    # on renomme les autres colonnes
          rename(`% cumulé de variance` = `cumulative percentage of variance`) %>%
          mutate(Axes = fct_relevel(Axes, paste("Axe", 1:10)))      # pour que l'ordre de 1 à 10 soit bien respecté dans les graphiques


        # Pour les différents indicateurs de l'ACM : ----
        frequences <- gather(data_acm[,2:ncol(data_acm)], variables, modalites) %>%       # étendre le jeu de données par variable et modalité
          # compter le nombre de couples "variable/modalité" unique (donc le nombre d'individus par modalité du jeu de données)
          count(variables, modalites) %>%
          group_by(variables) %>%
          mutate(pourcentage = round(100 * n / nrow(data_acm), 1)) %>% # calculer des pourcentages pour chaque groupe de variable
          ungroup() %>%
          select(variables, modalites, n, pourcentage) %>%
          rename(moda = modalites) %>%
          # On ajoute le nom de la variable à l'avant pour les cas de variables avec les mêmes noms de modalités
          mutate(modalites = ifelse(is.na(moda)== F, paste0(variables,"_",moda), paste0(variables,".",moda )))

        ###############
        # Modalités actives ----

        # Coordonnées (modalités actives)

        coordonnees <- as.data.frame(round(res_mca$var$coord, 2)) %>% # récupérer les coordonnées des modalités actives et arrondir à deux décimales (c'est bien suffisant)
          rename_all(tolower) %>%                                     # tout en minuscules
          rename_all(~ str_replace(., " ", "")) %>%                   # renommer les variables en supprimant les espaces : par exemple : "dim 1" devient "dim1"
          rename_all(~ str_c(., "coord", sep = "_")) %>%              # ajouter le suffixe _coord à chaque nom de variable. On obtient ainsi par exemple "dim1_coord"
          mutate(modalites = rownames(.))                             # récupérer les noms des modalités, stockées dans le nom des lignes de res_mca$var$coord

        # Contributions (modalités actives)
        contributions <- as.data.frame(round(res_mca$var$contrib, 2))  %>%
          rename_all(tolower) %>%
          rename_all(~ str_replace(., " ", "")) %>%
          rename_all(~ str_c(., "contrib", sep = "_")) %>%            # idem sauf qu'ici on obtient "dim1_contrib"
          mutate(modalites = rownames(.))

        # Cosinus carrés (modalités actives)
        cos2 <- as.data.frame(round(res_mca$var$cos2, 2)) %>%
          rename_all(tolower) %>%
          rename_all(~ str_replace(., " ", "")) %>%
          rename_all(~ str_c(., "cos2", sep = "_")) %>% # idem avec "cos2"
          mutate(modalites = rownames(.))

        # vtest (modalités actives)
        vtest <- as.data.frame(round(res_mca$var$v.test, 2)) %>%
          rename_all(tolower) %>%
          rename_all(~ str_replace(., " ", "")) %>%
          rename_all(~ str_c(., "vtest", sep = "_")) %>% # idem avec vtest
          mutate(modalites = rownames(.))


        # Compilation des indicateurs

        resultats_act <- frequences %>%
          right_join(coordonnees) %>%
          right_join(contributions) %>%
          right_join(cos2) %>%
          right_join(vtest) %>%                        # fusionner les jeux de données ; la clé de fusion (implicite) est la variable "modalites", qui est commune à tous.
          mutate(type = "Variable active") %>%         # ajout d'une colonne contenant la chaîne de caractères "Variable active" (pour pouvoir distinguer plus tard avec les variables supplémentaires)
          select(type, variables, modalites, n, pourcentage,
                 contains("dim1_"), contains("dim2_"),
                 contains("dim3_"), contains("dim4_")) # conserver et réorganiser les variables pertinentes axe par axe, on garde que 4 axes

        # Problème des variables avec les mêmes modalités
        meme_moda <- resultats_act %>%
          filter(is.na(variables) == T) %>%
          select(- c(variables, n, pourcentage)) %>%
          left_join(frequences, by = c("modalites"  = "moda")) %>%
          mutate(modalites = paste0(variables,"_",modalites)) %>% # On ajoute le nom de la variable devant la modalité pour éviter les noms doublons
          select( -modalites.y)

        # On enlève les modalités en doubles et on ajoute leurs corrections
        resultats_actives <<- resultats_act %>%
          filter(is.na(variables) == F) %>%
          rbind(meme_moda)


        ################
        # Modalités supplémentaires ----

        # Si on a des variables supplementaires :
        if(is.null(var_sup) == FALSE){

          # Coordonnées (modalités supplémentaires)
          coordonnees_sup <- as.data.frame(round(res_mca$quali.sup$coord, 2)) %>% # la démarche est la même que supra, mais avec le sous-objet quali.sup qui stocke les informations sur les variables qualitatives supplémentaires
            rename_all(tolower) %>%
            rename_all(~ str_replace(., " ", "")) %>%
            rename_all(~ str_c(., "coord", sep = "_")) %>%
            mutate(modalites = rownames(.))

          # Cosinus carrés (modalités supplémentaires)
          cos2_sup <- as.data.frame(round(res_mca$quali.sup$cos2, 2)) %>%
            rename_all(tolower) %>%
            rename_all(~ str_replace(., " ", "")) %>%
            rename_all(~ str_c(., "cos2", sep = "_")) %>%
            mutate(modalites = rownames(.))

          # vtest (modalités supplémentaires)
          vtest_sup <- as.data.frame(round(res_mca$quali.sup$v.test, 2)) %>%
            rename_all(tolower) %>%
            rename_all(~ str_replace(., " ", "")) %>%
            rename_all(~ str_c(., "vtest", sep = "_")) %>%
            mutate(modalites = rownames(.))

          # Assemblage du tableau des résultats (modalités actives)

          resultats_sup <- frequences %>%
            right_join(coordonnees_sup) %>%
            right_join(cos2_sup) %>%
            right_join(vtest_sup) %>%
            mutate(type = "Variable supplementaire") %>% # comme supra pour le tableau des résultats des modalités actives : on distingue ici le type de variable.
            select(type, variables, modalites, n, pourcentage,
                   contains("dim1_"), contains("dim2_"),
                   contains("dim3_"), contains("dim4_"))

          # Même problème de doublons
          meme_moda_sup <- resultats_sup %>%
            filter(is.na(variables) == T) %>%
            select(- c(variables, n, pourcentage)) %>%
            left_join(frequences, by = c("modalites"  = "moda")) %>%
            mutate(modalites = paste0(variables,"_",modalites)) %>%
            select( -modalites.y)

          resultats_suplem <<- resultats_sup %>%
            filter(is.na(variables) == F) %>%
            rbind(meme_moda_sup)

        } # Fin Var Supp


        ###############
        # TABLEAU COMPLET  ----

        # Si on a des var supplémentaires
        if(is.null(var_sup) == FALSE){
          resultats_complet <<- bind_rows(resultats_actives, resultats_suplem)
        }

        # Si on a que des variables actives
        if(is.null(var_sup) == TRUE){
          resultats_complet <<- resultats_actives
        }

        ############################




        ############################
        ### FAIRE LES GRAPHIQUES ET LES TABLES ----

        # Table de saut d'inertie
        output$variances <- renderDataTable({
          datatable(variances %>% select(Axes, everything()))
        })

        # Graphique de saut d'inertie
        output$variances_graph <- renderPlot({
          ggplot(variances, aes(x = Axes)) +     # initialiser du graphique et de l'axe horizontal
            geom_bar(aes(y = `% de variance`),   # indiquer le type de graphique (barplot) et la variable à représenter sur l'axe vertical
                     stat = "identity",
                     fill = "lightgrey") +       # parce que j'aime bien le rouge
            xlab("") +                           # on enlève le label de l'axe des x, pas très utile
            ylab("% de variance") +              # renommer proprement le label de l'axe des y
            theme_minimal()

        })


        # Table de saut d'inertie, corrigé selon le critère de Benzécri
        variances_modif <<- round(modif.rate(res_mca)$modif,2)

        output$variances_Benz <- renderDataTable({
          datatable(variances_modif)
        })


        # Graphique de saut d'inertie, corrigé selon le critère de Benzécri
        output$variances_Benz_graph <- renderPlot({
          ggplot(variances_modif, aes(x = reorder(row.names(variances_modif), -mrate))) + # initialiser du graphique et de l'axe horizontal
            geom_bar(aes(y = modif.rate(res_mca)$modif$mrate),                            # indiquer le type de graphique (barplot) et la variable à représenter sur l'axe vertical
                     stat = "identity",
                     fill = "red") +                                                      # choix couleur
            xlab("") +                                                                    # on enlève le label de l'axe des x, pas très utile
            ylab("% de variance") +                                                       # renommer proprement le label de l'axe des y
            theme_minimal()

        })


        # Graphique des variables actives

        output$plot_acm_act <- renderPlot({

          g_act <<- resultats_actives %>%
            mutate(modalites = substr(modalites, nchar(variables)+2, nchar(modalites))) %>%
            filter(get(paste0("dim",input$axe_X,"_contrib")) > input$seuil |
                     get(paste0("dim",input$axe_Y,"_contrib")) > input$seuil) %>%   # on part du tableau de résultat des modalités actives et on filtre uniquement celles dont la contribution dépasse le seuil pour l'un ou l'autre des deux axes (| est l'opérateur logique OU).

            ggplot(aes(x = get(paste0("dim",input$axe_X,"_coord")), y = get(paste0("dim",input$axe_Y,"_coord")), # initialisation du graphique
                       label = modalites,                                           # les labels des points sont les modalités
                       shape = variables,                                           # les formes des points dépendent des variables : à chaque variable son symbole
                       colour = variables,
                       size = 15)) +

            geom_point() +                                                          # tracer les points
            geom_text_repel(size = input$taille_label, segment.alpha = 0.5) +       # tracer les labels, en utilisant cette fonction du package ggrepel qui permet de s'assurer qu'il n'y a pas de chevauchement. segment.alpha : transparence du petit tiret qui relie si besoin le libellé au point
            coord_fixed() +                                                         # pour que les échelles des axes soient identiques

            geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +  # ligne horizontale y = 0
            geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +  # ligne verticale x = 0

            xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) + # label de l'axe horizontal, qui intègre automatiquement le pourcentage de variance
            ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) + # idem pour l'axe vertical

            scale_shape_manual(name="", values = 0:20) +                            # sélection des types de symboles

            guides(colour=guide_legend(title="", nrow = 2),                         # paramètres de la légende : pas de titre
                   size = "none",                                                   # Pas de legende pour la taille et la forme
                   shape = "none") +

            theme_minimal(base_size = 18) +                                         # mise en forme globale du graphique ; theme_minimal est la plus "sobre" mais d'autres sont possibles...

            theme(legend.position="bottom", legend.text = element_text(size = 20))  # pour que la légende se place sous le graphique.
          g_act



        })


        # Graphique des variables actives et supplémentaires

        output$plot_acm_complet <- renderPlot({

          resultats_complet %>%
            mutate(modalites = substr(modalites, nchar(variables)+2, nchar(modalites))) %>%
            filter(get(paste0("dim",input$axe_X,"_contrib")) > input$seuil |
                     get(paste0("dim",input$axe_Y,"_contrib")) > input$seuil |
                     is.na(get(paste0("dim",input$axe_X,"_contrib"))) == T |
                     is.na(get(paste0("dim",input$axe_Y,"_contrib"))) == T) %>%

            ggplot(aes(x = get(paste0("dim",input$axe_X,"_coord")), y = get(paste0("dim",input$axe_Y,"_coord")),
                       label = modalites,
                       shape = variables,
                       colour = type,                                               # on distingue par des couleurs différentes les variables actives et supplémentaires
                       size = 15)) +

            geom_point() +
            geom_text_repel(size = input$taille_label, segment.alpha = 0.5) +
            coord_fixed() +

            geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") +
            geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") +

            xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) +
            ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) +

            scale_shape_manual(name="", values = 0:20) +
            scale_color_manual(values = c("black", "red")) +
            # scale_color_brewer(palette = "Set1") +
            # scale_color_grey() +
            # scale_color_brewer(palette = "Accent")

            guides(shape = "none",
                   colour = guide_legend(title= "Type de variable",                 # titre de la légende distinguant actives et supplémentaires
                                         title.position = "top",
                                         nrow = 2),
                   size = "none") + # toujours pas de légende pour les tailles de point

            theme_minimal(base_size = 18) +
            theme(legend.position="bottom", legend.text = element_text(size = 20))

        })









        # Graphique interactifs des variables



        output$plot_acm_interact <- renderGirafe({

          gg_point = resultats_complet %>%
            mutate(modalites = substr(modalites, nchar(variables)+2, nchar(modalites)),
                   tooltip = c(paste0("Modalites = ", modalites,
                                      "\n Variables = ", variables,
                                      "\n Contrib1 = ", dim1_contrib,
                                      "\n Contrib2 = ", dim2_contrib,
                                      "\n Type = ", type))) %>%
            filter(get(paste0("dim",input$axe_X,"_contrib")) > input$seuil |
                     get(paste0("dim",input$axe_Y,"_contrib")) > input$seuil |
                     is.na(get(paste0("dim",input$axe_X,"_contrib"))) == T |
                     is.na(get(paste0("dim",input$axe_Y,"_contrib"))) == T) %>%
            ggplot() +
            geom_point_interactive(aes(x = get(paste0("dim",input$axe_X,"_coord")),
                                       y = get(paste0("dim",input$axe_Y,"_coord")),
                                       color = variables,
                                       shape = type,
                                       tooltip = tooltip,
                                       data_id = modalites)) +
            coord_fixed() +
            xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) + # label de l'axe horizontal, qui intègre automatiquement le pourcentage de variance
            ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) + # idem pour l'axe vertical
            theme_minimal()+
            theme(legend.position="none")
          # + theme(legend.position="bottom", legend.text = element_text(size = 12))

          girafe(ggobj = gg_point)



        })




        # Nuage des individus

        coord_indiv <<- as.data.frame(res_mca$ind$coord) # récupérer les coordonnées des individus sur les deux premiers axes.


        output$plot_acm_interact_indiv <- renderGirafe({

          texte <- vector()
          text_id <- vector()
          text_sup <- vector()
          text_sup2 <- vector()
          text_act <- vector()
          text_act2 <- vector()

          for (k in c(1:nrow(data_acm))) {

            text_id[k] <- paste0("autoacm_id : ", data_acm[k,1], "\n\n" )
            for (i in var_sup) {
              text_sup[k] <- paste0(text_sup[k], "\n ", i, " : ", data_acm[k,i] )
            }

            text_sup[k] = substr(text_sup[k], 3, nchar(text_sup[k]))
            text_sup2[k] <- paste0(c("SUPPLEMENTAIRES : "), text_sup[k])

            for (j in var_act) {
              text_act[k] <- paste0(text_act[k], "\n ", j, " : ", data_acm[k,j] )
            }

            text_act[k] = substr(text_act[k], 3, nchar(text_act[k]))
            text_act2[k] <- paste0(c("\n\n ACTIVES : "), text_act[k])

            texte[k] <- paste0(text_id[k],text_sup2[k],text_act2[k])

          }


          gg_point_ind = coord_indiv %>%
            cbind(data_acm) %>%
            mutate(tooltip = texte) %>%
            ggplot() +
            geom_point_interactive(aes(x = get(paste0("Dim ", input$axe_X)),
                                       y = get(paste0("Dim ", input$axe_Y)),
                                       color = "#E41A1C",
                                       tooltip = tooltip
            )) +
            coord_fixed() +
            xlab(paste0("Axe", input$axe_X ,"(", round(variances[input$axe_X, 2], 1), " %)")) + # label de l'axe horizontal, qui intègre automatiquement le pourcentage de variance
            ylab(paste0("Axe", input$axe_Y ,"(", round(variances[input$axe_Y, 2], 1), " %)")) + # idem pour l'axe vertical
            theme_minimal()+
            theme(legend.position="none")
          # + theme(legend.position="bottom", legend.text = element_text(size = 12))

          girafe(ggobj = gg_point_ind)


        })





        # Tables des indicateurs actifs

        output$resultat_acm_table_act <- renderDataTable({
          datatable(resultats_complet  %>%
                      filter(type == "Variable active")%>%
                      select(variables, modalites, n, pourcentage,
                             contains(paste0("dim",input$axe_X,"_")), contains(paste0("dim",input$axe_Y,"_")))
          )

        })



        # Tables des indicateurs supplémentaires

        output$resultat_acm_table_sup <- renderDataTable({
          datatable(resultats_complet %>%
                      filter(type == "Variable supplementaire") %>%
                      select(variables, modalites, n, pourcentage,
                             contains(paste0("dim",input$axe_X,"_")), contains(paste0("dim",input$axe_Y,"_")))
          )

        })


        ############################




        ############################
        ### UI : CHOIX GRAPHIQUES & PARAMETRES


        output$affichage_choix_ACM <- renderUI({

          # On crée une liste pour les différents objets qu'on va représenter
          list_acm <- list("Saut d'inertie" = "variances" ,
                           "Saut d'inertie (Benzecri)" = "variances_Benz",
                           "ACM : variables actives" = "plot_acm_act",
                           "ACM : variables actives et supplémentatires" = "plot_acm_complet",
                           "ACM : graphique interactif" = "plot_acm_interact",
                           "ACM : individus" = "plot_acm_interact_indiv",
                           "Tables des variables actives" = "resultat_acm_table_act",
                           "Tables des variables supp." = "resultat_acm_table_sup")

          # On crée un bandeau avec les choix possibles


          output$warning_explor <- renderText({"    Attention : Cliquer sur 'ACM avec explor' ferme l'application en cours et ouvre l'interface d'explor()."})
          tagList(
            h5("Exploration de l'ACM"),
            column(12,
                   wellPanel(
                     # 1ere ligne dans le bandeau
                     fluidRow(
                       # Choix de ce qu'on montre
                       column(5, selectInput(inputId="choix_MCA",
                                             label="Choix de la représentation : ",
                                             choices= list_acm,
                                             selected = list_acm[1])),
                       # Choix de l'axe X
                       column(2, offset=1, numericInput("axe_X", "Axe X :", 1, min = 1, max = 4)),
                       # Choix de l'axe Y
                       column(2, numericInput("axe_Y", "Axe Y :", 2, min = 1, max = 4)),
                       # Choix du seuil de contribution pour la représentation
                       column(2, numericInput("seuil", "Seuil contrib.:", 100 / nrow(res_mca$var$coord) , min = 0, max = 10))),

                     # 2ème ligne dans le bandeau
                     fluidRow(
                       column(7,
                              fluidRow(span(textOutput("warning_explor"), style="color:red")),
                              # fluidRow(helpText("Attention : Cliquer sur 'ACM avec explor' ferme l'application en cours et active la fonction explor().")),
                              fluidRow(column(4, offset=3, actionButton("explorACM", "ACM avec explor", class = "btn-danger")))),
                       column(2, offset = 1,
                              # Taille du graphique
                              sliderInput(inputId = "taille_graph",
                                          label = "Taille du graphique",
                                          min = 300, max = 3000, step = 100, value = 800)),
                       column(2,
                              # Taille des étiquette
                              sliderInput(inputId = "taille_label",
                                          label = "Taille des étiquettes",
                                          min = 2, max = 15, step = 1, value = 7)))

                   )))
        }) # Fin renderUI

        removeModal()


      }) # Fin Bouton ACM


      ############################




      ############################
      ### UI : REPRESENTATION GRAPHIQUES ET TABLES


      # Il faut choisir dans le bandeau le type de représentation
      observeEvent(input$choix_MCA, {

        # Pour saut d'inertie
        if (input$choix_MCA == "variances"){

          output$affichage_ACM <- renderUI({
            fluidRow(
              column(7,dataTableOutput(input$choix_MCA)),
              column(4, offset = 1,plotOutput("variances_graph"))
            )
          })
        }
        # Pour saut d'inertie Benzécri
        else if (input$choix_MCA == "variances_Benz"){
          output$affichage_ACM <- renderUI({
            fluidRow(
              column(7,dataTableOutput(input$choix_MCA)),
              column(4, offset = 1,plotOutput("variances_Benz_graph"))
            )
          })
        }
        # Pour graphique ACM sans variables supplémentaires
        else if (input$choix_MCA == "plot_acm_complet" & is.null(var_sup) == T ){
          output$affichage_ACM <- renderUI({
            fluidRow(
              column(12, plotOutput("plot_acm_act", height = input$taille_graph, width = input$taille_graph) )
            )
          })
        }
        # Pour la table des informations sur l'ACM
        else if (input$choix_MCA == "resultat_acm_table_act" ){
          output$affichage_ACM <- renderUI({
            fluidRow(
              column(12, dataTableOutput(input$choix_MCA))
            )
          })
        }


        # Pour la table des informations sur l'ACM
        else if (input$choix_MCA == "resultat_acm_table_sup" ){
          output$affichage_ACM <- renderUI({
            fluidRow(
              column(12, dataTableOutput(input$choix_MCA))
            )
          })
        }


        # Pour graphique ACM interactif
        else if (input$choix_MCA == "plot_acm_interact"){
          output$affichage_ACM <- renderUI({
            fluidRow(
              column(12, girafeOutput("plot_acm_interact", height = input$taille_graph, width = input$taille_graph))
            )
          })
        }

        else if (input$choix_MCA == "plot_acm_interact_indiv"){
          output$affichage_ACM <- renderUI({
            fluidRow(
              column(12, girafeOutput("plot_acm_interact_indiv", height = input$taille_graph, width = input$taille_graph))
            )
          })
        }


        # Pour toutes les autres graphiques
        else{
          output$affichage_ACM <- renderUI({
            plotOutput(input$choix_MCA, height = input$taille_graph, width = input$taille_graph)
          })
        }


      })





















      #### Server CAH

      # Dans ce script, je continue la partie server et je fais une classification
      # On peut choisir le nombre de classes et faire des tri à plat simple avec la variables obtenue.


      # Quand on appuye sur le bouton Valider l'ACM dans ServeurACM, on crée aussi des
      # objets qu'on réutilise dans la partie CAH.


      # Message d'erreur si on change l'ACM, pour montrer que c'est pas réactif
      output$lancer_cah <- reactive({
        validate(need(input$target_upload, ''))
        validate(need(input$acmOK, ''))
        validate(need(input$cahOK, ''))

        if(n_cahOK == input$acmOK ){
          "Oui"
        } else {
          "Non_newACM"
        }

      })
      outputOptions(output, "lancer_cah", suspendWhenHidden=FALSE)




      #### BOUTON CAH

      observeEvent(input$cahOK, {

        validate(need(input$target_upload, ''))
        validate(need(input$acmOK, ''))

        # Controle ACM = CAH
        n_cahOK <<- input$acmOK[1]
        indic_cah <<- "Oui"

        # Si on a des données dans data_acm, on calcule l'ACM
        if(is.null(data_acm) == FALSE){

          showModal(modalDialog("Classification ...", footer=NULL))

          # On transforme les données de data_acm en factor
          # Attention pour les variables numériques
          # Attention pour les individus ou modalités supplémentaires par la suite
          data_acm <- as.data.frame(lapply(data_acm,to_character))
          data_acm <- as.data.frame(lapply(data_acm,to_factor))


          ### CREER LES OBJETS POUR LA CLASSIFICATION

          # Plus facile de faire des classifications avec le package ade4, mais factominer est utile pour explor()


          # Si on a une pondération
          if(var_ponder != ""){

            # On commence à la 2ème variables (sans autoacm_id) + les varsup s'il y en a
            ade4_mca <<- dudi.acm(data_acm[c((length(var_sup)+2) : length(data_acm))], # On ne garde que les variables actives
                                  scannf = FALSE, nf = Inf, row.w = ponder)
          } else{
            ade4_mca <<- dudi.acm(data_acm[c((length(var_sup)+2) : length(data_acm))], # On ne garde que les variables actives
                                  scannf = FALSE, nf = Inf)
          }


          # OBJET CAH INITIAUX
          # Ils peuvent être modifié selon les choix utilisateurs plus loin

          # Matrice des distances
          md <- dist.dudi(ade4_mca)
          # Dendrogramme et saut d'inertie
          meth_dist <<- "ward.D2"
          arbre <<- hclust(md, method = "ward.D2")
          inertie <<- sort(arbre$height, decreasing = TRUE)
          ncluster <<- 2



          ### CREER LES UI POUR LES GRAPHIQUES ET LES TABLES -----

          ## PARAM CLUSTER :

          output$affichage_choix_graph_cah <- renderUI({

            # On crée une liste des graphiques possibles
            list_graph <- c("Dendrogramme" = "dendro", "Saut d'inertie" = "inertie", "ACM avec classes" = "ellipse")

            # On crée un bandeau avec les options graphiques possibles
            tagList(
              column(12,
                     wellPanel(
                       fluidRow(

                         # Choix du type de distance
                         column(4, selectInput("meth_dist", "Méthodes de calcul des distances :",
                                               choices= c("single", "complete", "average", "mcquitty", "ward.D",
                                                          "ward.D2", "centroid", "median"),
                                               multiple = FALSE,
                                               selected = "ward.D2")),

                         # Selection du graphique
                         column(4,selectInput(inputId="choix_graph_cah",
                                              label="Choix du graphique : ",
                                              choices= list_graph)),

                         # Choix du nombre de cluster
                         column(4, numericInput("ncluster", "Nombre de classes :", 2, min = 1, max = 20))



                       ))))
          }) # FIN UI PARAM CLUSTER


          ## EXPLORATION CLUSTER

          output$affichage_choix_table_cah <- renderUI({

            validate(need(input$target_upload, 'Importer des données'))

            # Choix du type de table
            list_graph <- c("Effectifs Classes" = "eff_uni","Effectifs bivariés" = "eff", "% Ligne" = "pct_lign", "% Colonne" = "pct_col", "Sur/Sous-représentation" = "heatmap")
            # On crée un bandeau avec les options graphiques possibles
            tagList(
              column(12,
                     wellPanel(
                       fluidRow(
                         # Choix de la variable à croiser avec la variable de classe
                         column(5,pickerInput("var_row", "Croiser la variable 'classe' avec :", c("",nomcol_data_reac()),
                                              multiple = F,
                                              options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                         # Choix du type de tables
                         column(4, selectInput(inputId="choix_table_cah",
                                               label="Choix du type de table : ",
                                               choices= list_graph))
                       ))))
          }) # FIN EXPLORATION CLUSTER





          ### CREATION DE VARIABLE CLUSTER
          data_cah <<- data_acm

          if (ncluster >= 1) {
            data_cah$cluster <<- as.factor(cutree(arbre, ncluster))
          } else{data_cah$cluster <<- 1}

          cluster_data2 <- data_cah %>%
            select(autoacm_id, cluster)

          cluster_data3 <<- data_init2 %>%
            left_join(cluster_data2, by = "autoacm_id") %>%
            mutate(cluster = ifelse(is.na(cluster) == TRUE, 0, cluster))

          v$data <- cluster_data3
          nomreac$nomcol <- colnames(v$data)





          ### GRAPHIQUES
          if(ncluster < 2){ # Si moins de deux clusters, on affiche juste les graphiques normaux sans les rectangles

            # Dendrogramme
            output$dendro <- renderPlot({
              plot(arbre, labels = FALSE, main = "Choix de la partition",
                   xlab = "", ylab = "", sub = "",
                   axes = FALSE, hang = -1)
            })
            # Saut d'inertie
            output$inertie <- renderPlot({
              plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
            })

            # Ellipse ACM
            output$ellipse <- renderPlot({
              fviz_mca_ind(res_mca, geom = "point", alpha.ind = .25, title = "")
            })

          } else { # Si on a des clusters, alors on les affiches sur les graphiques

            # Dendrogramme
            output$dendro <- renderPlot({
              plot(arbre, labels = FALSE, main = "Choix de la partition",
                   xlab = "", ylab = "", sub = "",
                   axes = FALSE, hang = -1)
              rect.hclust(arbre, ncluster, border = "red")

            })
            # Saut d'inertie
            output$inertie <- renderPlot({
              plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
              points(ncluster, inertie[ncluster], col = "red", cex = 2, lwd = 3)
            })

            # Ellipse ACM
            output$ellipse <- renderPlot({
              fviz_mca_ind(res_mca, geom = "point", alpha.ind = .25, habillage = as.factor(data_cah$cluster), addEllipses = TRUE, title = "")
            })
          }





          ### TABLES BIVARIEES ----

          # Effectifs univarié
          output$eff_uni <- renderTable({
            as.data.frame.matrix(with(cluster_data3, freq(cluster)))
          }, include.rownames = T)


          # Effectifs
          output$eff <- renderTable({
            validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'))
            as.data.frame.matrix(with(cluster_data3, addmargins(table(get(input$var_row), cluster_data3$cluster))))
          }, include.rownames = T)

          # Pourcentage Ligne
          output$pct_lign <- renderTable({
            validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'),
                     need(cluster_data3$cluster,''))

            as.data.frame.matrix(with(cluster_data3, lprop(table(get(input$var_row), cluster_data3$cluster))))
          }, include.rownames = T)

          # Pourcentage Colonne
          output$pct_col <- renderTable({
            validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'),
                     need(cluster_data3$cluster,''))

            as.data.frame.matrix(with(cluster_data3, cprop(table(get(input$var_row), cluster_data3$cluster))))
          }, include.rownames = T)

          # Sur-representation
          output$heatmap <- renderPlot({

            validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'))

            chisq <- chisq.test(with(cluster_data3, table(get(input$var_row), cluster_data3$cluster)))
            corrplot(chisq$residuals, is.cor = FALSE)

          })





          removeModal()

        }
      })









      ### BOUTON METH_DIST ----

      observeEvent(input$meth_dist, {

        validate(need(input$target_upload, ''))
        validate(need(input$acmOK, ''))
        validate(need(input$ncluster,''))

        showModal(modalDialog("Classification ...", footer=NULL))




        ### CREER LES OBJETS POUR LA CLASSIFICATION

        # Matrice des distances
        md <- dist.dudi(ade4_mca)
        # Dendrogramme et saut d'inertie
        arbre <<- hclust(md, method = input$meth_dist)
        inertie <<- sort(arbre$height, decreasing = TRUE)
        ncluster <<- input$ncluster
        meth_dist <<- input$meth_dist





        ### CREATION DE VARIABLE CLUSTER
        data_cah <<- data_acm

        if (ncluster >= 1) {
          data_cah$cluster <<- as.factor(cutree(arbre, input$ncluster))
        } else{data_cah$cluster <<- 1}

        cluster_data2 <- data_cah %>%
          select(autoacm_id, cluster)

        cluster_data3 <<- data_init2 %>%
          left_join(cluster_data2, by = "autoacm_id") %>%
          mutate(cluster = ifelse(is.na(cluster) == TRUE, 0, cluster))

        v$data <- cluster_data3
        nomreac$nomcol <- colnames(v$data)



        ### GRAPHIQUES
        if(input$ncluster < 2){ # Si moins de deux clusters, on affiche juste les graphiques normaux sans les rectangles

          # Dendrogramme
          output$dendro <- renderPlot({
            plot(arbre, labels = FALSE, main = "Choix de la partition",
                 xlab = "", ylab = "", sub = "",
                 axes = FALSE, hang = -1)
          })
          # Saut d'inertie
          output$inertie <- renderPlot({
            plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
          })

          # Ellipse ACM
          output$ellipse <- renderPlot({
            fviz_mca_ind(res_mca, geom = "point", alpha.ind = .25, title = "")
          })

        } else { # Si on a des clusters, alors on les affiches sur les graphiques

          # Dendrogramme
          output$dendro <- renderPlot({
            plot(arbre, labels = FALSE, main = "Choix de la partition",
                 xlab = "", ylab = "", sub = "",
                 axes = FALSE, hang = -1)
            rect.hclust(arbre, input$ncluster, border = "red")

          })
          # Saut d'inertie
          output$inertie <- renderPlot({
            plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
            points(input$ncluster, inertie[input$ncluster], col = "red", cex = 2, lwd = 3)
          })

          # Ellipse ACM
          output$ellipse <- renderPlot({
            fviz_mca_ind(res_mca, geom = "point", alpha.ind = .25, habillage = as.factor(data_cah$cluster), addEllipses = TRUE, title = "")
          })
        }






        ### TABLES BIVARIEES ----

        # Effectifs univarié
        output$eff_uni <- renderTable({
          as.data.frame.matrix(with(cluster_data3, freq(cluster)))
        }, include.rownames = T)


        # Effectifs
        output$eff <- renderTable({
          validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'))
          as.data.frame.matrix(with(cluster_data3, addmargins(table(get(input$var_row), cluster_data3$cluster))))
        }, include.rownames = T)

        # Pourcentage Ligne
        output$pct_lign <- renderTable({
          validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'),
                   need(cluster_data3$cluster,''))

          as.data.frame.matrix(with(cluster_data3, lprop(table(get(input$var_row), cluster_data3$cluster))))
        }, include.rownames = T)

        # Pourcentage Colonne
        output$pct_col <- renderTable({
          validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'),
                   need(cluster_data3$cluster,''))

          as.data.frame.matrix(with(cluster_data3, cprop(table(get(input$var_row), cluster_data3$cluster))))
        }, include.rownames = T)

        # Sur-representation
        output$heatmap <- renderPlot({

          validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'))

          chisq <- chisq.test(with(cluster_data3, table(get(input$var_row), cluster_data3$cluster)))
          corrplot(chisq$residuals, is.cor = FALSE)

        })




        removeModal()

      })










      ### BOUTON NCLUST ----


      observeEvent(input$ncluster ,{

        validate(need(input$ncluster,''))

        # On sauvegarde le nombre de cluster
        ncluster <<- input$ncluster




        ### CREATION DE VARIABLE CLUSTER

        data_cah <<- data_acm

        if (ncluster >= 1) {
          data_cah$cluster <<- as.factor(cutree(arbre, input$ncluster))
        } else{data_cah$cluster <<- 1}


        cluster_data2 <- data_cah %>%
          select(autoacm_id, cluster)

        cluster_data3 <<- data_init2 %>%
          left_join(cluster_data2, by = "autoacm_id") %>%
          mutate(cluster = ifelse(is.na(cluster) == TRUE, 0, cluster))

        v$data <- cluster_data3
        nomreac$nomcol <- colnames(v$data)





        ### GRAPHIQUES
        if(input$ncluster < 2){ # Si moins de deux clusters, on affiche juste les graphiques normaux sans les rectangles

          # Dendrogramme
          output$dendro <- renderPlot({
            plot(arbre, labels = FALSE, main = "Choix de la partition",
                 xlab = "", ylab = "", sub = "",
                 axes = FALSE, hang = -1)
          })
          # Saut d'inertie
          output$inertie <- renderPlot({
            plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
          })

          # Ellipse ACM
          output$ellipse <- renderPlot({
            fviz_mca_ind(res_mca, geom = "point", alpha.ind = .25, title = "")
          })

        } else { # Si on a des clusters, alors on les affiches sur les graphiques

          # Dendrogramme
          output$dendro <- renderPlot({
            plot(arbre, labels = FALSE, main = "Choix de la partition",
                 xlab = "", ylab = "", sub = "",
                 axes = FALSE, hang = -1)
            rect.hclust(arbre, input$ncluster, border = "red")

          })
          # Saut d'inertie
          output$inertie <- renderPlot({
            plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
            points(input$ncluster, inertie[input$ncluster], col = "red", cex = 2, lwd = 3)
          })

          # Ellipse ACM
          output$ellipse <- renderPlot({
            fviz_mca_ind(res_mca, geom = "point", alpha.ind = .25, habillage = as.factor(data_cah$cluster), addEllipses = TRUE, title = "")
          })
        }





        ### TABLES BIVARIEES ----

        # Effectifs univarié
        output$eff_uni <- renderTable({
          as.data.frame.matrix(with(cluster_data3, freq(cluster)))
        }, include.rownames = T)


        # Effectifs
        output$eff <- renderTable({
          validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'))
          as.data.frame.matrix(with(cluster_data3, addmargins(table(get(input$var_row), cluster_data3$cluster))))
        }, include.rownames = T)

        # Pourcentage Ligne
        output$pct_lign <- renderTable({
          validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'),
                   need(cluster_data3$cluster,''))

          as.data.frame.matrix(with(cluster_data3, lprop(table(get(input$var_row), cluster_data3$cluster))))
        }, include.rownames = T)

        # Pourcentage Colonne
        output$pct_col <- renderTable({
          validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'),
                   need(cluster_data3$cluster,''))

          as.data.frame.matrix(with(cluster_data3, cprop(table(get(input$var_row), cluster_data3$cluster))))
        }, include.rownames = T)

        # Sur-representation
        output$heatmap <- renderPlot({

          validate(need(input$var_row,'Choisir une variable à croiser avec la variable de classes'))

          chisq <- chisq.test(with(cluster_data3, table(get(input$var_row), cluster_data3$cluster)))
          corrplot(chisq$residuals, is.cor = FALSE)

        })


      })




      #### UI


      # Affichage des graphiques dans l'UI :
      output$affichage_graphique <- renderUI({

        validate(need(input$target_upload, 'Importer des données'))
        validate(need(input$acmOK, 'Importer des données'))

        # UTILE CA ?
        # Potentiellement c'est ce qui fait que les graphiques sont crées même en dehors de cahOK
        validate(need(input$ncluster,''))
        ###

        fluidRow(
          column(8, offset = 1,
                 plotOutput(input$choix_graph_cah, height = 600)
          ),
          column(3, align = "center",
                 br(),
                 tableOutput("rappel_cah"),
                 actionButton("rappel_souspop", "Sous-population")
          )

        )
      })



      observeEvent(input$acmOK ,{

        output$rappel_cah <- renderTable({

          if(is.null(var_sup)){

            if(var_ponder == ""){
              data.frame(
                RAPPEL = c("","VARIABLE ACTIVE :",var_act,"","VARIABLE SUPPLEMENTAIRE :","Pas de var. supp.", "", "PONDERATION :", "Pas de pondération")
              )
            } else {
              data.frame(
                RAPPEL = c("","VARIABLE ACTIVE :",var_act,"","VARIABLE SUPPLEMENTAIRE :","Pas de var. supp.", "", "PONDERATION :", var_ponder)
              )
            }

          } else {

            if(var_ponder == ""){
              data.frame(
                RAPPEL = c("","VARIABLE ACTIVE :",var_act,"","VARIABLE SUPPLEMENTAIRE :",var_sup, "", "PONDERATION :", "Pas de pondération")
              )
            } else {
              data.frame(
                RAPPEL = c("","VARIABLE ACTIVE :",var_act,"","VARIABLE SUPPLEMENTAIRE :",var_sup, "", "PONDERATION :", var_ponder)
              )
            }

          }
        })

      })



      # Ouvrir la fenêtre pop-up quand le bouton est cliqué
      observeEvent(input$rappel_souspop, {
        showModal(
          modalDialog(
            title = "Rappel de la sous-population utilisée :",
            renderTable({
              tableau_modalite
            }),
            footer = modalButton("Fermer")
          )
        )
      })




      # Affichage des tables dans l'UI :
      observeEvent(input$choix_table_cah, {
        validate(need(input$ncluster,'Choisir un nombre de classes'))


        if (input$choix_table_cah == "heatmap"){
          output$affichage_table_cah <- renderUI({
            plotOutput(input$choix_table_cah)
          })
        }
        else {
          output$affichage_table_cah <- renderUI({
            tableOutput(input$choix_table_cah)
          })
        }
      })




      ### TELECHARGER LES DONNEES AVEC LA VARIABLE CLUSTER

      output$downLoadCluster <- downloadHandler(
        filename = function() {
          paste0('Filtered_data_cluster-', Sys.Date(),".csv")
        },
        content = function(file) {
          showModal(modalDialog("Téléchargement des données avec clusters...", footer=NULL))
          on.exit(removeModal())
          write_csv2(cluster_data3,file)
        }
      )















      # PASSERELLE VERS EXPLOR() ----


      # Appuyer sur le bouton explorACM, stoppe l'ACM et renvoie le signal explorACM
      # Ce signal sera reçu dans la fonction VirageACM (script fonction.R), pour ouvrir explor()

      observeEvent(input$explorACM, {
        js$closeWindow(); stopApp("explorACM")
      })




      # SAUVEGARDE ----


      # https://shiny.rstudio.com/articles/generating-reports.html
      # https://shiny.rstudio.com/gallery/download-knitr-reports.html
      # https://resources.symbolix.com.au/2020/10/28/downloadable-reports-shiny/

      # Création d'un rapport automatique
      output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        # Nom du rapport en sortie
        filename =     function() {
          paste0('report-',Sys.Date(),".", switch(
            input$report_format, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },
        # Contenu du rapport
        content = function(file) {

          showModal(modalDialog("Création du rapport automatisé...", footer=NULL))
          on.exit(removeModal())
          # Copy the template report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy(paste0(system.file("rmd", package = "autoacm"),"/report.Rmd"), tempReport, overwrite = TRUE)

          # Set up parameters to pass to Rmd document
          params <- list( name_data = inFile$name,
                          n_data = isolate(n_row_start()),
                          n_souspop = nrow(data_acm),
                          axe_X= input$axe_X,
                          axe_Y= input$axe_Y,
                          seuil= input$seuil,
                          taille_label= input$taille_label,
                          meth_dist = meth_dist,
                          ncluster = ncluster,
                          indic_cah = indic_cah,
                          is_html = input$report_format)

          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          out <- rmarkdown::render(tempReport,
                                   output_format = switch(
                                     input$report_format,
                                     PDF = pdf_document(), HTML = html_document(), Word = word_document()),
                                   output_file = file,
                                   params = params,
                                   envir = new.env(parent = globalenv())
          )
          file.rename(out, file)
        }
      )



      # Reactif pour afficher la possibilité de sauvegarder les indicateur de l'ACM
      output$afficher_sauvegarde_indic <- reactive({
        if (input$acmOK >= 1) {
          "Oui"
        } else {
          "Non"
        }

      })
      outputOptions(output, "afficher_sauvegarde_indic", suspendWhenHidden=FALSE)


      # Bouton de téléchargement de la base des indicateurs
      output$save_indic <- downloadHandler(
        filename = function() {
          paste('autoacm-indicACM-', Sys.Date(), '.csv', sep = '')
        },
        content = function(file){
          write_csv(resultats_complet,file)
        }
      )



    }) # Fin ShinyServeur


) # Fin ShinyApp















