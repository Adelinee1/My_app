
library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(readxl)
library(bslib)
library(thematic)


# Charger les données
depenses <- read_excel("~/master 1 IA/R_shine/Activite_shiny _oncadvive/depenses-remboursees.xlsx")
Total_depenses <- read_excel("~/master 1 IA/R_shine/Activite_shiny _oncadvive/Total_cancer.xlsx")


# Identifier les colonnes numériques du excel depense-rembousees
variable_num <- which(sapply(depenses, is.numeric))

# Exclure 'Annee' des colonnes numériques pour le selectInput du excel depense-rembousees
numeric_data <- depenses[, variable_num]
numeric_columns_no_annee <- names(numeric_data)[names(numeric_data) != "Annee"]

# Identifier les colonnes catégorielles du excel depense-rembousees
factor_columns <- which(sapply(depenses, is.factor) | sapply(depenses, is.character))
factor_data <- depenses[, factor_columns]

# Identifier les colonnes numériques du excel Total_depenses
variable_num_T <- which(sapply(Total_depenses, is.numeric))

# Exclure 'Annee' des colonnes numériques pour le selectInput du excel Total_depenses
numeric_data_T <- Total_depenses[, variable_num_T]
numeric_columns_no_annee_T <- names(numeric_data_T)[names(numeric_data_T) != "Annee"]

# Identifier les colonnes catégorielles du excel Total_depenses
factor_columns_T <- which(sapply(Total_depenses, is.factor) | sapply(Total_depenses, is.character))
factor_data_T <- Total_depenses[, factor_columns_T]

#### Application

ui <- fluidPage(
  
theme = bs_theme(preset = "minty"),
  
  ## Titre de l'application
  titlePanel("OncAdvice"),
  tagList(
  # CSS for navbar elements
  tags$style(
    HTML('.navbar.navbar-default.navbar-static-top {background: #90c0ae !important;}')
  )),
  navbarPage("Analyse des dépenses de remboursement des traitements du cancer et des effectifs",
             tabPanel("Graphique Global",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("AXEY", "Choix de données :", choices = numeric_columns_no_annee_T, selected = "Total_des_depenses"),
                          selectInput("an", "Select années :", choices = unique(Total_depenses$Annee), selected = unique(Total_depenses$Annee)[1], multiple = TRUE),
                          sliderInput("alphaSlider2", "Transparency", min = 0, max = 1, value = 0.5),
                          sliderInput("sizeSlider2", "Marker Size", min = 1, max = 10, value = 5),
                          checkboxInput("showTable2", "Show Data Table", value = TRUE)
                        ),
                        mainPanel(
                          plotlyOutput("scatterPlot2"),
                          DTOutput("dataTable2")
                        )
                      )
             ),
             
             tabPanel("Graphique détaillé",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("VARY", "Choix de données :", choices = numeric_columns_no_annee, selected = "Total_des_depenses"),
                          selectInput("COLOR", "Select Cancer Type(s) :", choices = unique(depenses$Type_de_cancer), selected = unique(depenses$Type_de_cancer)[1], multiple = TRUE),
                          sliderInput("alphaSlider", "Transparency", min = 0, max = 1, value = 0.5),
                          sliderInput("sizeSlider", "Marker Size", min = 1, max = 10, value = 5),
                          checkboxInput("showTable", "Show Data Table", value = TRUE)
                        ),
                        mainPanel(
                          plotlyOutput("scatterPlot"),
                          DTOutput("dataTable")
                        )
                      )
             ),
             
             tabPanel("Présentation d'OncAdvice",
                      sidebarLayout(
                        sidebarPanel(
                          h3("Ce qu'il faut savoir sur OncAdvice"),
                          radioButtons(
                            inputId = "checkquestion",
                            label = h3("Choix de question"),
                            choices = list(
                              "Qu'est-ce que OncAdvice ?" = 1,
                              "Depuis quand cette association existe ?" = 2,
                              "Combien de personnes ont intégré l'association ?" = 3
                            ),
                            selected = 1,
                          ),
                          actionButton(
                            inputId = "bouton",
                            label = "Pour nous rejoindre",
                          ),
                    
                          
                        ),
                        mainPanel(
                          h1("Réponse :"),
                          uiOutput("dynamicContent"),
                          br(),
                          uiOutput("message_rejoindre"),
                        )
                      )
             ),
  )
)

### Serveur (`server`)

server <- function(input, output) {
  output$scatterPlot2 <- renderPlotly({
    # Initialiser le graphique Plotly pour le graph global
    p2 <- plot_ly()
    
    # Filtrer les données selon les années sélectionnées
    filtered_data_T <- Total_depenses[Total_depenses$Annee %in% input$an, ]
    
    # Ajouter une trace pour relier les points
    p2 <- add_trace(p2, data = filtered_data_T, x = ~Annee, y = as.formula(paste("~", input$AXEY)),
                    type = 'scatter', mode = 'lines+markers',
                    name = 'Global',
                    line = list(shape = "linear"),
                    marker = list(size = input$sizeSlider2, opacity = input$alphaSlider2))
    
    # Configurer la mise en page du graph global
    p2 <- layout(p2, title = "Tendances Globales des effectifs et des dépenses par année",
                 xaxis = list(title = "Année",
                              tickmode = "linear",
                              tickvals = unique(Total_depenses$Annee),
                              ticktext = unique(Total_depenses$Annee)),
                 yaxis = list(title = input$AXEY,
                              tickformat = ",d"))  # Changer le format des ticks pour arrondir les valeurs
    
    return(p2)
  })
  
  output$dataTable2 <- renderDT({
    if(input$showTable2) {
      datatable(Total_depenses[Total_depenses$Annee %in% input$an, ], options = list(pageLength = 5))
    }
  })
  
  output$scatterPlot <- renderPlotly({
    # Initialiser le graphique Plotly pour le graph détaillé
    p <- plot_ly()
    
    # Filtrer les données selon les types de cancer sélectionnés
    filtered_data <- depenses[depenses$Type_de_cancer %in% input$COLOR, ]
    
    # Boucler sur chaque type de cancer sélectionné pour ajouter une trace de ligne
    for (type in unique(filtered_data$Type_de_cancer)) {
      subset_data <- filtered_data[filtered_data$Type_de_cancer == type, ]
      p <- add_trace(p, data = subset_data, x = ~Annee, y = as.formula(paste("~", input$VARY)),
                     type = 'scatter', mode = 'lines+markers',
                     name = type,
                     line = list(shape = "spline"),
                     marker = list(size = input$sizeSlider, opacity = input$alphaSlider))
    }
    
    # Configurer la mise en page du graph détaillé
    p <- layout(p, title = "Tendances des effectifs et des dépenses par type de cancer",
                xaxis = list(title = "Année",
                             tickmode = "linear",
                             tickvals = unique(depenses$Annee),
                             ticktext = unique(depenses$Annee)),
                yaxis = list(title = input$VARY,
                             tickformat = ",d"))  # Changer le format des ticks pour arrondir les valeurs
    
    return(p)
  })
  


output$dataTable <- renderDT({
  if(input$showTable) {
    datatable(depenses[depenses$Type_de_cancer %in% input$COLOR, ], options = list(pageLength = 5))
  }
})
  ### Réponse au question en fonction de la case cocher
  output$dynamicContent <- renderUI({
    switch(as.character(input$checkquestion),
           "1" = list(
             img(src="Logo_Onc.png", height=100),  # Insérer logo
             br(),
             br(),
             br(),
             h4("Qu'est-ce que OncAdvice ?"),
             br(),
             p(" OncAdvice est une association dédiée à l'aide et au soutien des personnes atteintes de cancer.
                Son ambition principale est de développer une application pour aider les personnes atteintes de cancer à mieux se nourrir au quotidien afin qu'elles vivent au mieux leur parcours de soins et qu'elles évitent la dénutrition.")
             
             
           ),
           "2" = list(
             h4("Depuis quand cette association existe ?"),
             br(),
             p("L'association existe depuis un an. Elle est toujours en développement. Cependant, une version bêta de l'application devrait apparaître d'ici quelques mois afin de recueillir les premiers avis."),
             tags$div(
             img(src="an.png", height=300),
             style="text-align: center;"
             )
           ),
           "3" = list(
             h4("Combien de personnes ont intégré l'association ?"),
             br(),
             p("L'association a été créée par cinq étudiantes venant de la même formation. Depuis sa création, OncAdvice a accueilli 12 bénévoles passionnés venant de multiples domaines."),
             
             tags$div(
             img(src="benev.png", height=390),
             style="text-align: center;"
             )
           )
    )
  }) 
  
  # Réagir lorsque le bouton est cliqué
  observeEvent(input$bouton, {
    output$message_rejoindre <- renderUI({
      tags$p(
        "Pour nous rejoindre c'est simple !",
        tags$a(
          tags$span("Rejoignez notre page LinkedIn", style = "font-size: 20px; font-weight: bold;"),
          href = "https://www.linkedin.com/company/oncadvice/", 
          target = "_blank"
        )
      )
    })
  })
}

shinyApp(ui, server)