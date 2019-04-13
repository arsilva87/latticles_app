library(shiny)
library(xml2)

# -------------------------------------
artigosLattes <- function(XML, area)
{
   if(is.null(XML)) return()
   if(is.null(area)) return()

   nome <- attr(as_list(xml_children(XML)[[1]]), "NOME-COMPLETO")
   atuali <- attr(as_list(XML)$"CURRICULO-VITAE", "DATA-ATUALIZACAO")
   datat <- strsplit(atuali, "")[[1]]
   datat. <- paste0(datat[1], datat[2], "-", datat[3], datat[4], "-", 
      datat[5], datat[6], datat[7], datat[8])

   prod <- xml_children(XML)[[2]]
   artpub <- as_list(xml_children(prod)[[2]])

   if(area == "CAI") {
      qualis <- read.csv("qualisCAI.csv", colClasses = "character")
   } else if(area == "ALI") {
      qualis <- read.csv("qualisALI.csv", colClasses = "character")
   } else if(area == "AMB") {
      qualis <- read.csv("qualisAMB.csv", colClasses = "character")
   } else if(area == "BIOI") {
      qualis <- read.csv("qualisBIOI.csv", colClasses = "character")
   } else if(area == "COM") {
      qualis <- read.csv("qualisCOM.csv", colClasses = "character")
   } else if(area == "ENS") {
      qualis <- read.csv("qualisENS.csv", colClasses = "character")
   } else if(area == "INT") {
      qualis <- read.csv("qualisINT.csv", colClasses = "character")
   } else if(area == "LET") {
      qualis <- read.csv("qualisLET.csv", colClasses = "character")
   } else if(area == "MAT") {
      qualis <- read.csv("qualisMAT.csv", colClasses = "character")
   } else if(area == "QUI") {
      qualis <- read.csv("qualisQUI.csv", colClasses = "character")
   } else if(area == "VET") {
      qualis <- read.csv("qualisVET.csv", colClasses = "character")
   } else if(area == "ZOO") {
      qualis <- read.csv("qualisZOO.csv", colClasses = "character")
   }
   qualis$ISSN. <- sapply(strsplit(qualis$ISSN, "-"), paste, collapse = "")

   # loop para data frame de artigos publicados
   n <- length(artpub)
   artigodat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Periodico = NA, ISSN = NA,
	Qualis = NA, Vol = NA, pIni = NA, pFin = NA)
   i = 1
   repeat {
      art <- artpub[[i]]
      artigodat[i, ] <- 
         c(strsplit(attr(art[["AUTORES"]], "NOME-PARA-CITACAO"), ";")[[1]][1],
         attr(art[["DADOS-BASICOS-DO-ARTIGO"]], "ANO-DO-ARTIGO"),
         attr(art[["DADOS-BASICOS-DO-ARTIGO"]], "TITULO-DO-ARTIGO"),
         attr(art[["DETALHAMENTO-DO-ARTIGO"]], "TITULO-DO-PERIODICO-OU-REVISTA"),
         attr(art[["DETALHAMENTO-DO-ARTIGO"]], "ISSN"),
         qualis[grep(attr(art[["DETALHAMENTO-DO-ARTIGO"]], "ISSN"), qualis$ISSN.)[1], "Estrato"],
         attr(art[["DETALHAMENTO-DO-ARTIGO"]], "VOLUME"),
         attr(art[["DETALHAMENTO-DO-ARTIGO"]], "PAGINA-INICIAL"),
         attr(art[["DETALHAMENTO-DO-ARTIGO"]], "PAGINA-FINAL"))
      i = i + 1
      if(i > n) break()
   }
   artigodat$Qualis[is.na(artigodat$Qualis)] = "C"
   artigodat$Qualis <- factor(artigodat$Qualis, 
      levels = c("A1", "A2", "B1", "B2", "B3", "B4", "B5", "C"))

   # output
   metrics <- sapply(split(artigodat$Qualis, artigodat$Ano), table)
   metrics. <- cbind(metrics, Total = rowSums(metrics))
   w <- c(1, 0.85, 0.7, 0.55, 0.4, 0.25, 0.1, 0)
   Eq.A1 <- colSums(metrics.*w)
   A1A2B1 = colSums(metrics.[1:3, ])
   resumo <- rbind(metrics., Total = colSums(metrics.), Eq.A1, A1A2B1)
   out <- list(nome = nome, atual = datat., metricas = resumo, planilha = artigodat)
   return(out)
}

# -------------------------------------------------------------
ui <- fluidPage(
   # App title
   titlePanel("Latticles App"),
   helpText("Importe artigos, classifique pelo Qualis-CAPES e calcule metricas como Eq. A1 e quantitativo A1.A2.B1, tudo de forma automatica a partir de arquivos XML do curriculo Lattes."),
   sidebarLayout(
      # Menu here
      sidebarPanel(
         fileInput("XML", label = "Select file (XML)"),
         selectizeInput("area", "Area de avaliacao:",
              choices = c("Ciencias Agrarias I" = "CAI", 
                          "Ciencias de Alimentos" = "ALI",
                          "Ciencias Ambientais" = "AMB",
                          "Ciencias Biologicas I" = "BIOI",
                          "Ciencia da Computacao" = "COM",
                          "Ensino" = "ENS",
                          "Interdisciplinar" = "INT",
                          "Linguistica e Literatura" = "LET",
                          "Matematica" = "MAT",
                          "Quimica" = "QUI",
                          "Medicina Veterinaria" = "VET",
                          "Zootecnia e Recursos Pesqueiros" = "ZOO"),
              multiple = TRUE, options = list(maxItems = 1), select = NULL),
         submitButton("Submit"),
         br(),
         tags$hr(),
         h6("Powered by:",
         tags$img(src = "agrometrics.jpg", heigth = 110, width = 110),
         tags$img(src = "ppgpp.png", heigth = 90, width = 90),
         tags$img(src = "logoIF.png", heigth = 90, width = 90),
         helpText("Developed by: Anderson R. da Silva"))
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

# ---------------------------------------------------
server <- function(input, output) {
   XML <-reactive({
      f1 <- input$XML
      if(is.null(f1)) return() else read_xml(f1$datapath)
   })
   area <- reactive({
      a <- input$area
      if(is.null(a)) return() else a 
   })
   output$nome <- renderText({ 
      paste0("Nome completo: ", artigosLattes(XML(), area())$nome, "\n",
         "Ultima atualizacao do Lattes: ", artigosLattes(XML(), area())$atual)
   })
   output$resumo <- renderPrint({ 
         artigosLattes(XML(), area())$metricas 
   })
   output$graph1 <- renderPlot({
      metrics <- artigosLattes(XML(), area())$metricas
      par(cex = 0.8, mar = c(4.5, 4.5, 1, 1), family = "serif", las = 1)
      barplot(rbind(Total = metrics["Total", -ncol(metrics)], 
         A1A2B1 = metrics["A1A2B1", -ncol(metrics)], 
         Eq.A1 = metrics["Eq.A1", -ncol(metrics)]),
         ylab = "Artigos publicados",
         col = c("orange", "cyan", "lightgreen"),
         beside = TRUE, legend.text = TRUE, 
         args.legend = list(x = "topleft", cex = 0.9))
   })
   output$graph2 <- renderPlot({
      metrics <- artigosLattes(XML(), area())$metricas
      nc <- ceiling(ncol(metrics)/3)
      par(mfrow = c(nc, 3), cex = 0.8, mar = c(1, 1, 1, 1), family = "serif", las = 1)
      for(i in 1:(ncol(metrics)-1)) {
         pie(metrics[1:8, i], main = colnames(metrics)[i],
            col = terrain.colors(nrow(metrics)-3)) 
      }
   })
   output$planilha <- renderTable({ 
         artigosLattes(XML(), area())$planilha
   })
   output$downloadData <- downloadHandler(
         filename = function() {
            paste("artigos", ".csv", sep = "")
         },
         content = function(file) {
            write.csv(artigosLattes(XML(), area())$planilha, file, row.names = FALSE)
   })
}

# Run the app
shinyApp(ui = ui, server = server)
