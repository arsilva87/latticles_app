library(shiny)
library(xml2)
library(wordcloud)

artigosLattes <- function(XML, area, anos)
{
   if(is.null(XML)) return()
   if(is.null(area)) return()
   if(is.null(anos)) return()

   nome <- attr(as_list(xml_children(XML)[[1]]), "NOME-COMPLETO")
   atuali <- attr(as_list(XML)$"CURRICULO-VITAE", "DATA-ATUALIZACAO")
   datat <- strsplit(atuali, "")[[1]]
   datat. <- paste0(datat[1], datat[2], "-", datat[3], datat[4], "-", 
      datat[5], datat[6], datat[7], datat[8])

   # tabelas qualis ----------------------------------------------------
   if(area == "ADM") {
      qualis <- read.csv(unz("qualis.zip", "qualisADM.csv"), colClasses = "character")
   } else if(area == "ANT") {
      qualis <- read.csv(unz("qualis.zip", "qualisANT.csv"), colClasses = "character")
   } else if(area == "AUD") {
      qualis <- read.csv(unz("qualis.zip", "qualisAUD.csv"), colClasses = "character")
   } else if(area == "ART") {
      qualis <- read.csv(unz("qualis.zip", "qualisART.csv"), colClasses = "character")
   } else if(area == "FIS") {
      qualis <- read.csv(unz("qualis.zip", "qualisFIS.csv"), colClasses = "character")
   } else if(area == "BIOD") {
      qualis <- read.csv(unz("qualis.zip", "qualisBIOD.csv"), colClasses = "character")
   } else if(area == "BIOT") {
      qualis <- read.csv(unz("qualis.zip", "qualisBIOT.csv"), colClasses = "character")
   } else if(area == "COM") {
      qualis <- read.csv(unz("qualis.zip", "qualisCOM.csv"), colClasses = "character")
   } else if(area == "ALI") {
      qualis <- read.csv(unz("qualis.zip", "qualisALI.csv"), colClasses = "character")
   } else if(area == "CPRI") {
      qualis <- read.csv(unz("qualis.zip", "qualisCPRI.csv"), colClasses = "character")
   } else if(area == "CAI") {
      qualis <- read.csv(unz("qualis.zip", "qualisCAI.csv"), colClasses = "character")
   } else if(area == "AMB") {
      qualis <- read.csv(unz("qualis.zip", "qualisAMB.csv"), colClasses = "character")
   } else if(area == "BIO1") {
      qualis <- read.csv(unz("qualis.zip", "qualisBIO1.csv"), colClasses = "character")
   } else if(area == "BIO2") {
      qualis <- read.csv(unz("qualis.zip", "qualisBIO2.csv"), colClasses = "character")
   } else if(area == "BIO3") {
      qualis <- read.csv(unz("qualis.zip", "qualisBIO3.csv"), colClasses = "character")
   } else if(area == "CRT") {
      qualis <- read.csv(unz("qualis.zip", "qualisCRT.csv"), colClasses = "character")
   } else if(area == "COMI") {
      qualis <- read.csv(unz("qualis.zip", "qualisCOMI.csv"), colClasses = "character")
   } else if(area == "DIR") {
      qualis <- read.csv(unz("qualis.zip", "qualisDIR.csv"), colClasses = "character")
   } else if(area == "ECO") {
      qualis <- read.csv(unz("qualis.zip", "qualisECO.csv"), colClasses = "character")
   } else if(area == "EDU") {
      qualis <- read.csv(unz("qualis.zip", "qualisEDU.csv"), colClasses = "character")
   } else if(area == "EDUF") {
      qualis <- read.csv(unz("qualis.zip", "qualisEDUF.csv"), colClasses = "character")
   } else if(area == "ENF") {
      qualis <- read.csv(unz("qualis.zip", "qualisENF.csv"), colClasses = "character")
   } else if(area == "ENG1") {
      qualis <- read.csv(unz("qualis.zip", "qualisENG1.csv"), colClasses = "character")
   } else if(area == "ENG2") {
      qualis <- read.csv(unz("qualis.zip", "qualisENG2.csv"), colClasses = "character")
   } else if(area == "ENG3") {
      qualis <- read.csv(unz("qualis.zip", "qualisENG3.csv"), colClasses = "character")
   } else if(area == "ENG4") {
      qualis <- read.csv(unz("qualis.zip", "qualisENG4.csv"), colClasses = "character")
   } else if(area == "ENS") {
      qualis <- read.csv(unz("qualis.zip", "qualisENS.csv"), colClasses = "character")
   } else if(area == "FAR") {
      qualis <- read.csv(unz("qualis.zip", "qualisFAR.csv"), colClasses = "character")
   } else if(area == "FIL") {
      qualis <- read.csv(unz("qualis.zip", "qualisFIL.csv"), colClasses = "character")
   } else if(area == "GEOC") {
      qualis <- read.csv(unz("qualis.zip", "qualisGEOC.csv"), colClasses = "character")
   } else if(area == "GEOG") {
      qualis <- read.csv(unz("qualis.zip", "qualisGEOG.csv"), colClasses = "character")
   } else if(area == "HIS") {
      qualis <- read.csv(unz("qualis.zip", "qualisHIS.csv"), colClasses = "character")
   } else if(area == "INT") {
      qualis <- read.csv(unz("qualis.zip", "qualisINT.csv"), colClasses = "character")
   } else if(area == "LET") {
      qualis <- read.csv(unz("qualis.zip", "qualisLET.csv"), colClasses = "character")
   } else if(area == "MAT") {
      qualis <- read.csv(unz("qualis.zip", "qualisMAT.csv"), colClasses = "character")
   } else if(area == "MTR") {
      qualis <- read.csv(unz("qualis.zip", "qualisMTR.csv"), colClasses = "character")
   } else if(area == "MED1") {
      qualis <- read.csv(unz("qualis.zip", "qualisMED1.csv"), colClasses = "character")
   } else if(area == "MED2") {
      qualis <- read.csv(unz("qualis.zip", "qualisMED2.csv"), colClasses = "character")
   } else if(area == "MED3") {
      qualis <- read.csv(unz("qualis.zip", "qualisMED3.csv"), colClasses = "character")
   } else if(area == "VET") {
      qualis <- read.csv(unz("qualis.zip", "qualisVET.csv"), colClasses = "character")
   } else if(area == "NUT") {
      qualis <- read.csv(unz("qualis.zip", "qualisNUT.csv"), colClasses = "character")
   } else if(area == "ODO") {
      qualis <- read.csv(unz("qualis.zip", "qualisODO.csv"), colClasses = "character")
   } else if(area == "PLA") {
      qualis <- read.csv(unz("qualis.zip", "qualisPLA.csv"), colClasses = "character")
   } else if(area == "PSI") {
      qualis <- read.csv(unz("qualis.zip", "qualisPSI.csv"), colClasses = "character")
   } else if(area == "QUI") {
      qualis <- read.csv(unz("qualis.zip", "qualisQUI.csv"), colClasses = "character")
   } else if(area == "SAU") {
      qualis <- read.csv(unz("qualis.zip", "qualisSAU.csv"), colClasses = "character")
   } else if(area == "SER") {
      qualis <- read.csv(unz("qualis.zip", "qualisSER.csv"), colClasses = "character")
   } else if(area == "SOC") {
      qualis <- read.csv(unz("qualis.zip", "qualisSOC.csv"), colClasses = "character")
   } else if(area == "ZOO") {
      qualis <- read.csv(unz("qualis.zip", "qualisZOO.csv"), colClasses = "character")
   }
   qualis$ISSN. <- sapply(strsplit(qualis$ISSN, "-"), paste, collapse = "")

   # artigos publicados ------------------------------------------------------
   prod <- xml_children(XML)[[grep("ARTIGOS-PUBLICADOS", xml_children(XML))]]
   artpub <- as_list(xml_children(prod))[[grep("ARTIGOS-PUBLICADOS", xml_children(prod))]]

   # loop
   if(!is.null(artpub)) {
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
   } else artigodat = NULL

   # artigos aceitos ---------------------------------------------------------
   artace <- as_list(xml_children(prod))[[grep("ARTIGOS-ACEITOS-PARA-PUBLICACAO", xml_children(prod))]]

   # loop
   if(!is.null(artace)) {
   n2 <- length(artace)
   artigodat2 <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Periodico = NA, ISSN = NA,
      Qualis = NA, Vol = NA, pIni = NA, pFin = NA)
   i = 1
   repeat {
      art <- artace[[i]]
      artigodat2[i, ] <- 
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
      if(i > n2) break()
   }
   artigodat2$Qualis[is.na(artigodat2$Qualis)] = "C"
   artigodat2$Qualis <- factor(artigodat2$Qualis, 
      levels = c("A1", "A2", "B1", "B2", "B3", "B4", "B5", "C"))
   } else artigodat2 = NULL

   # trabalhos em eventos ----------------------------------------------------
   trab <- as_list(xml_children(prod))[[grep("TRABALHOS-EM-EVENTOS", xml_children(prod))]]

   # loop
   if(!is.null(trab)) {
   n3 <- length(trab)
   trabdat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Natureza = NA, Evento = NA, 
      Local = NA, ISBN = NA, Vol = NA, pIni = NA, pFin = NA)
   i = 1
   repeat {
      a <- trab[[i]]
      trabdat[i, ] <- 
         c(strsplit(attr(a[["AUTORES"]], "NOME-PARA-CITACAO"), ";")[[1]][1],
         attr(a[["DETALHAMENTO-DO-TRABALHO"]], "ANO-DE-REALIZACAO"),
         attr(a[["DADOS-BASICOS-DO-TRABALHO"]], "TITULO-DO-TRABALHO"),
         attr(a[["DADOS-BASICOS-DO-TRABALHO"]], "NATUREZA"),
         attr(a[["DETALHAMENTO-DO-TRABALHO"]], "NOME-DO-EVENTO"),
         attr(a[["DETALHAMENTO-DO-TRABALHO"]], "CIDADE-DO-EVENTO"),
         attr(a[["DETALHAMENTO-DO-TRABALHO"]], "ISBN"),
         attr(a[["DETALHAMENTO-DO-TRABALHO"]], "VOLUME"),
         attr(a[["DETALHAMENTO-DO-TRABALHO"]], "PAGINA-INICIAL"),
         attr(a[["DETALHAMENTO-DO-TRABALHO"]], "PAGINA-FINAL"))
      i = i + 1
      if(i > n3) break()
   }
   trabdat$Ano <- factor(trabdat$Ano, levels = seq(anos[1], anos[2]))
   outtrab <- table(list(Natureza = trabdat$Natureza, trabdat$Ano))
   } else outtrab = NULL
   
   # livros ------------------------------------------------------------------
   licap <- as_list(xml_children(prod))[[grep("LIVROS-E-CAPITULOS", xml_children(prod))]]
   livro <- licap[["LIVROS-PUBLICADOS-OU-ORGANIZADOS"]]

   # loop
   if(!is.null(livro)) {
   n4 <- length(livro)
   livrodat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Ed = NA,
      Local = NA, Editora = NA, ISBN = NA, Pags = NA)
   i = 1
   repeat {
      a <- livro[[i]]
      livrodat[i, ] <- 
         c(strsplit(attr(a[["AUTORES"]], "NOME-PARA-CITACAO"), ";")[[1]][1],
         attr(a[["DADOS-BASICOS-DO-LIVRO"]], "ANO"),
         attr(a[["DADOS-BASICOS-DO-LIVRO"]], "TITULO-DO-LIVRO"),
         attr(a[["DETALHAMENTO-DO-LIVRO"]], "NUMERO-DA-EDICAO-REVISAO"),
         attr(a[["DETALHAMENTO-DO-LIVRO"]], "CIDADE-DA-EDITORA"),
         attr(a[["DETALHAMENTO-DO-LIVRO"]], "NOME-DA-EDITORA"),
         attr(a[["DETALHAMENTO-DO-LIVRO"]], "ISBN"),
         attr(a[["DETALHAMENTO-DO-LIVRO"]], "NUMERO-DE-PAGINAS"))
      i = i + 1
      if(i > n4) break()
   }
   livrodat$Ano <- factor(livrodat$Ano, levels = seq(anos[1], anos[2]))
   outlivro <- table(livrodat$Ano)
   } else outlivro = NULL

   # capitulos ---------------------------------------------------------------
   cap <- licap[["CAPITULOS-DE-LIVROS-PUBLICADOS"]]

   # loop
   if(!is.null(cap)) {
   n4. <- length(cap)
   capdat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Livro = NA, Ed = NA,
      Local = NA, Editora = NA, ISBN = NA, pIni = NA, pFin = NA)
   i = 1
   repeat {
      a <- cap[[i]]
      capdat[i, ] <- 
         c(strsplit(attr(a[["AUTORES"]], "NOME-PARA-CITACAO"), ";")[[1]][1],
         attr(a[["DADOS-BASICOS-DO-CAPITULO"]], "ANO"),
         attr(a[["DADOS-BASICOS-DO-CAPITULO"]], "TITULO-DO-CAPITULO-DO-LIVRO"),
         attr(a[["DETALHAMENTO-DO-CAPITULO"]], "TITULO-DO-LIVRO"),
         attr(a[["DETALHAMENTO-DO-CAPITULO"]], "NUMERO-DA-EDICAO-REVISAO"),
         attr(a[["DETALHAMENTO-DO-CAPITULO"]], "CIDADE-DA-EDITORA"),
         attr(a[["DETALHAMENTO-DO-CAPITULO"]], "NOME-DA-EDITORA"),
         attr(a[["DETALHAMENTO-DO-CAPITULO"]], "ISBN"),
         attr(a[["DETALHAMENTO-DO-CAPITULO"]], "PAGINA-INICIAL"),
         attr(a[["DETALHAMENTO-DO-CAPITULO"]], "PAGINA-FINAL"))
      i = i + 1
      if(i > n4.) break()
   }
   capdat$Ano <- factor(capdat$Ano, levels = seq(anos[1], anos[2]))
   outcap <- table(capdat$Ano)
   } else outcap = NULL

   # orientacoes -------------------------------------------------------------
   orien <- as_list(xml_children(XML))[[grep("ORIENTACOES-CONCLUIDAS", xml_children(XML))]][["ORIENTACOES-CONCLUIDAS"]]

   # loop
   if(!is.null(orien)) {
   MS = DS = Outra = factor(levels = seq(anos[1], anos[2]))
   for(i in grep("MESTRADO", orien)) {
      MS[i] <- attr(orien[[i]][["DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO"]], "ANO")
   }
   for(i in grep("DOUTORADO", orien)) {
      DS[i] <- attr(orien[[i]][["DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO"]], "ANO")
   }
   for(i in grep("OUTRAS", orien)) {
      Outra[i] <- attr(orien[[i]][["DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS"]], "ANO")
   }
   outorien <- t(sapply(list(Mestrado = MS, Doutorado = DS, Outra = Outra), table))
   } else outorien = NULL

   # output ------------------------------------------------------------------
   artigodat <- subset(artigodat, Ano >= anos[1] & Ano <= anos[2])
   artigodat2 <- subset(artigodat2, Ano >= anos[1] & Ano <= anos[2])
   metrics <- sapply(split(artigodat$Qualis, artigodat$Ano), table)
   metrics. <- cbind(metrics, Total = rowSums(metrics))
   w <- c(1, 0.85, 0.7, 0.55, 0.4, 0.25, 0.1, 0)
   Eq.A1 <- colSums(metrics.*w)
   A1A2B1 = colSums(metrics.[1:3, ])
   resumo <- rbind(metrics., Total = colSums(metrics.), Eq.A1, A1A2B1)
   out <- list(nome = nome, atual = datat., metricas = resumo, 
      planilha = artigodat, aceitos = artigodat2,
      trabalhos = summargins(outtrab), 
      livros = summargins(outlivro), capitulos = summargins(outcap),
      orientacoes = summargins(outorien))
   return(out)
}

# aux function
summargins <- function(table) {
   if(length(dim(table)) > 1) {
      table. <- rbind(table, Total = colSums(table))
      cbind(table., Total = rowSums(table.))
   } else c(table, Total = sum(table))
}

# -------------------------------------------------------------
ui <- fluidPage(
   # App title
   titlePanel("Latticles App"),
   helpText("Importe artigos, classifique pelo Qualis-CAPES (2013-2016) e calcule metricas como Eq. A1 e quantitativo A1_A2_B1, tudo de forma automatica a partir de arquivos XML do curriculo Lattes."),
   sidebarLayout(
      # Menu here
      sidebarPanel(
         fileInput("XML", label = "Selecionar arquivo (XML)"),
         selectizeInput("area", "Area de avaliacao:",
              choices = c("Administracao" = "ADM",
                          "Antropologia, Arqueologia" = "ANT", 
                          "Arquitetura, Urbanismo e Design" = "AUD", 
                          "Artes" = "ART", 
                          "Astronomia, Fisica" = "FIS", 
                          "Biodiversidade" = "BIOD", 
                          "Biotecnologia" = "BIOT", 
                          "Ciencia da Commputacao" = "COM", 
                          "Ciencias de Alimentos" = "ALI",
                          "Ciencia Politica e Relacoes Internacionais" = "CPRI",
                          "Ciencias Agrarias I" = "CAI", 
                          "Ciencias Ambientais" = "AMB",
                          "Ciencias Biologicas I" = "BIO1",
                          "Ciencias Biologicas II" = "BIO2",
                          "Ciencias Biologicas III" = "BIO3",
                          "Ciencias da Religiao e Teologia" = "CRT",
                          "Comunicacao e Informacao" = "COMI",
                          "Direito" = "DIR",
                          "Economia" = "ECO",
                          "Educacao" = "EDU",
                          "Educacao Fisica" = "EDUF",
                          "Enfermagem" = "ENF",
                          "Engenharias I" = "ENG1", 
                          "Engenharias II" = "ENG2", 
                          "Engenharias III" = "ENG3", 
                          "Engenharias IV" = "ENG4", 
                          "Ensino" = "ENS",
                          "Farmacia" = "FAR",
                          "Filosofia" = "FIL",
                          "Geociencias" = "GEOC",
                          "Geografia" = "GEOG",
                          "Historia" = "HIS",
                          "Interdisciplinar" = "INT",
                          "Linguistica e Literatura" = "LET",
                          "Matematica" = "MAT",
                          "Materiais" = "MTR",
                          "Medicina I" = "MED1",
                          "Medicina II" = "MED2",
                          "Medicina III" = "MED3",
                          "Medicina Veterinaria" = "VET",
                          "Nutricao" = "NUT",
                          "Odontologia" = "ODO",
                          "Planejamento Urbano e Regional, Demografia" = "PLA",
                          "Psicologia" = "PSI",
                          "Quimica" = "QUI",
                          "Saude Coletiva" = "SAU",
                          "Servico Social" = "SER",
                          "Sociologia" = "SOC",
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
               plotOutput("graph2"),
               h6("Wordcloud"),
               plotOutput("graph3")),
            tabPanel("Artigos publicados", br(),
               downloadButton("downloadData", "Exportar"),
               br(), tableOutput("planilha")),
            tabPanel("Artigos aceitos para publicacao", br(),
               br(), tableOutput("aceitos")),
            tabPanel("Outras informacoes de producao",
               h6("Livros publicados"), 
                  verbatimTextOutput("livros"),
               h6("Capitulos de livros"), 
                  verbatimTextOutput("capitulos"),
               h6("Trabalhos em eventos"), 
                  verbatimTextOutput("trabalhos"),
               h6("Orientacoes concluidas"),
                  verbatimTextOutput("orientacoes"))
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
   anos <- reactive({
      ano <- input$range
      if(is.null(ano)) return() else as.integer(ano)
   })
   output$nome <- renderText({ 
      paste0("Nome completo: ", artigosLattes(XML(), area(), anos())$nome, "\n",
         "Ultima atualizacao do Lattes: ", artigosLattes(XML(), area(), anos())$atual)
   })
   output$resumo <- renderPrint({ 
         artigosLattes(XML(), area(), anos())$metricas 
   })
   output$graph1 <- renderPlot({
      metrics <- artigosLattes(XML(), area(), anos())$metricas
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
      metrics <- artigosLattes(XML(), area(), anos())$metricas
      nc <- ceiling(ncol(metrics)/3)
      par(mfrow = c(nc, 3), cex = 0.8, mar = c(1, 1, 1, 1), family = "serif", las = 1)
      for(i in 1:(ncol(metrics)-1)) {
         pie(metrics[1:8, i], main = colnames(metrics)[i],
            col = terrain.colors(nrow(metrics)-3)) 
      }
   })
   output$graph3 <- renderPlot({
      art <- artigosLattes(XML(), area(), anos())$planilha
      palavras <- unlist(strsplit(art$Titulo, " "))
      o <- which(sapply(strsplit(palavras, ""), length) > 3)
     par(mar = c(1, 1, 1, 1), family = "serif")
      wordcloud(palavras[o], col = terrain.colors(length(palavras)))
   })
   output$planilha <- renderTable({ 
         artigosLattes(XML(), area(), anos())$planilha
   })
   output$downloadData <- downloadHandler(
         filename = function() {
            paste("artigos", ".csv", sep = "")
         },
         content = function(file) {
            write.csv(artigosLattes(XML(), area(), anos())$planilha, file, row.names = FALSE)
   })
   output$aceitos <- renderTable({ 
         artigosLattes(XML(), area(), anos())$aceitos
   })
   output$livros <- renderPrint({ 
         artigosLattes(XML(), area(), anos())$livros 
   })
   output$capitulos <- renderPrint({ 
         artigosLattes(XML(), area(), anos())$capitulos
   })
   output$trabalhos <- renderPrint({ 
         artigosLattes(XML(), area(), anos())$trabalhos 
   })
   output$orientacoes <- renderPrint({ 
         artigosLattes(XML(), area(), anos())$orientacoes
   })
}

# Run the app
shinyApp(ui = ui, server = server)

