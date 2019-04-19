library(shiny)
library(xml2)

# ui -------------------------------------------------------------
fluidPage(
   # App title
   titlePanel("Latticles App"),
   helpText("Importe artigos, classifique pelo Qualis-CAPES e calcule metricas como Eq. A1 e quantitativo A1+A2+B1, tudo de forma automatica a partir de arquivos XML do curriculo Lattes."),
   sidebarLayout(
      # Menu here
      sidebarPanel(
         fileInput("XML", label = "Selecionar arquivo (XML)"),
         selectizeInput("area", "Area de avaliacao:",
              choices = c("Administracao" = "ADM",
                          "Astronomia, Fisica" = "FIS", 
                          "Biodiversidade" = "BIOD", 
                          "Biotecnologia" = "BIOT", 
                          "Ciencias Agrarias I" = "CAI", 
                          "Ciencias de Alimentos" = "ALI",
                          "Ciencias Ambientais" = "AMB",
                          "Ciencias Biologicas I" = "BIOI",
                          "Ciencia da Computacao" = "COM",
                          "Engenharias I" = "ENG1", 
                          "Engenharias II" = "ENG2", 
                          "Engenharias III" = "ENG3", 
                          "Engenharias IV" = "ENG4", 
                          "Ensino" = "ENS",
                          "Interdisciplinar" = "INT",
                          "Linguistica e Literatura" = "LET",
                          "Matematica" = "MAT",
                          "Quimica" = "QUI",
                          "Medicina Veterinaria" = "VET",
                          "Zootecnia, Recursos Pesqueiros" = "ZOO"),
              multiple = TRUE, options = list(maxItems = 1), select = NULL),
         sliderInput("range", "Intervalo de tempo:",
              min = 2000, max = 2021, value = c(2014, 2019), step = 1, sep = ""),
         submitButton("Submit"),
         br(),
         tags$hr(),
         h6("Powered by:",
         tags$img(src = "agrometrics.jpg", heigth = 110, width = 110),
         tags$img(src = "ppgpp.png", heigth = 90, width = 90),
         tags$img(src = "logoIF.png", heigth = 90, width = 90),
         helpText("Developed by: Silva, A. R. (anderson.silva@ifgoiano.edu.br)"))
      ),
      # Ouput here
      mainPanel(
         tabsetPanel(
            tabPanel("Resumo",
               h6("Identificacao"), 
                  verbatimTextOutput("nome"),
               h6("Metricas CAPES"),
                  verbatimTextOutput("resumo")),
            tabPanel("Graficos", h6("Evolucao da producao cientifica"), 
               plotOutput("graph1"),
               br(),
               h6("Proporcao por estrato"),
               plotOutput("graph2")),
            tabPanel("Artigos", br(),
               downloadButton("downloadData", "Download"),
               br(), tableOutput("planilha"))
         )
      )
   )
)
