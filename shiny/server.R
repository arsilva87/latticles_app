library(shiny)
library(xml2)

# -------------------------------------
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

   prod <- xml_children(XML)[[2]]
   artpub <- as_list(xml_children(prod)[[2]])

   if(area == "ADM") {
      qualis <- read.csv("qualisADM.csv", colClasses = "character")
   } else if(area == "FIS") {
      qualis <- read.csv("qualisFIS.csv", colClasses = "character")
   } else if(area == "BIOD") {
      qualis <- read.csv("qualisBIOD.csv", colClasses = "character")
   } else if(area == "BIOT") {
      qualis <- read.csv("qualisBIOT.csv", colClasses = "character")
   } else if(area == "CAI") {
      qualis <- read.csv("qualisCAI.csv", colClasses = "character")
   } else if(area == "ALI") {
      qualis <- read.csv("qualisALI.csv", colClasses = "character")
   } else if(area == "AMB") {
      qualis <- read.csv("qualisAMB.csv", colClasses = "character")
   } else if(area == "BIOI") {
      qualis <- read.csv("qualisBIOI.csv", colClasses = "character")
   } else if(area == "COM") {
      qualis <- read.csv("qualisCOM.csv", colClasses = "character")
   } else if(area == "ENG1") {
      qualis <- read.csv("qualisENG1.csv", colClasses = "character")
   } else if(area == "ENG2") {
      qualis <- read.csv("qualisENG2.csv", colClasses = "character")
   } else if(area == "ENG3") {
      qualis <- read.csv("qualisENG3.csv", colClasses = "character")
   } else if(area == "ENG4") {
      qualis <- read.csv("qualisENG4.csv", colClasses = "character")
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
   artigodat <- subset(artigodat, Ano >= anos[1] & Ano <= anos[2])
   metrics <- sapply(split(artigodat$Qualis, artigodat$Ano), table)
   metrics. <- cbind(metrics, Total = rowSums(metrics))
   w <- c(1, 0.85, 0.7, 0.55, 0.4, 0.25, 0.1, 0)
   Eq.A1 <- colSums(metrics.*w)
   A1A2B1 = colSums(metrics.[1:3, ])
   resumo <- rbind(metrics., Total = colSums(metrics.), Eq.A1, A1A2B1)
   out <- list(nome = nome, atual = datat., metricas = resumo, planilha = artigodat)
   return(out)
}

# server ---------------------------------------------------
shinyServer(function(input, output, session){
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
         args.legend = list(x = "topright", cex = 0.9))
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
   onSessionEnded = function(callback) {
      "Registers the given callback to be invoked when the session is closed
      (i.e. the connection to the client has been severed). The return value
      is a function which unregisters the callback. If multiple callbacks are
      registered, the order in which they are invoked is not guaranteed."
      return(.closedCallbacks$register(callback))
   }
   session$onSessionEnded(function() {
        stopApp()
   })
})

