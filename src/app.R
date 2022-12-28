# install.packages("shiny")
library(shiny)
library(readr)

gastos_prestaciones <- read.csv2("../data/gastos_prestaciones.csv", sep=";", fileEncoding = "UTF-8")

# Define UI ----
ui <- fluidPage(
  titlePanel("Despeses de les prestacions del 2013 al 2022"),
  
  sidebarLayout(
    sidebarPanel("Panel de filtres:",
                 selectInput("ccaa", "Comunitat autònoma", 
                             choices = c("Totes", "Andalucía", "Aragón", "Asturias","Illes Balears", "Canarias", "Cantabria",                  
                                        "Castilla y León","Castilla - La Mancha","Cataluña", "Comunitat Valenciana", "Extremadura",
                                        "Galicia", "Madrid", "Murcia", "Navarra",
                                        "País Vasco","La Rioja","Ceuta", "Melilla"), selected = "Totes"),
                 selectInput("any","Anys", choices = c("Tots","2013","2014","2015","2016","2017","2018","2019",
                                                          "2020","2021","2022"), selected="Tots"),
                 selectInput("gasto","Tipus de gastos", choices =
                               list("Totals" = 1, "Prestació contributiva" = 2,
                                    "Subsidi d'atur" = 3,"Renta activa"=4, "Subsidi agrari"=5), selected = 1)
                 ),
    
                
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Overview", h2("Introducció"),
                           p("Es realitza la pràctica PAC3 sobre la creació d'una visualització interactiva de l'assignatura de Visualització de dades."),
                           p("Per aquesta pràctica s'ha seleccionat les dades sobre les despeses de les prestacions que esta publicat a la web del govern espanyol que estan desagregades per provincia i el tipus de prestació."),
                           h3("Origen dades"),
                           p("L'origen de les dades estan publicades a la web de datos.gob.es i es publica mensualment les despeses de cada provincia."),
                           p("Pel que fa a la llicencia no es veu clarament a la web quina es pero al revisar al 'aviso legal' fa referencia que hi ha una llicencia pública i que s'has de fer referencia de la font on s'ha tret."),
                           p("La pàgina web es troba:"),a("https://datos.gob.es/es/catalogo/ea0021425-gasto-en-prestaciones"),
                           h3("Resumen del dataset"),
                           p("Es la informació de les columnes i quin tipus de valors hi ha emmagatzemats a cada variable"),
                           verbatimTextOutput("summary"),
                           h3("Mostra dataset"),
                           p("Es mostra els 5 primers observacions del dataset"),
                           tableOutput('table')
                           ),
                  tabPanel("Gràfics", h2("Gràfics"),
                           p("En aquest apartat es mostra la informació de les despeses a traves de grafics, on amb el menu de l'esquerra es pot filtrar i tenir les dades que es necesiten:"),
                           h3("Gràfica de barres"),
                           plotOutput("distPlot"),
                           plotOutput("proviPlot"),
                           h3("Gràfica de densitat"),
                           p("Es calcula la densitat de les despeses sobre 1/10000"),
                           plotOutput("histPlot")
                  ),
                  tabPanel("Taula",sliderInput("slider", "Despesa total:",
                                                min = 0, max = 10000000, value = 10000000), 
                           dataTableOutput("tablePres")
                  )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$summary <- renderPrint({
    summary(gastos_prestaciones)
  })
  
  output$table <- renderTable(head(gastos_prestaciones))
  
  
  output$distPlot <- renderPlot({
    if(input$ccaa == "Totes"){
      temp <- gastos_prestaciones
    }else{
      temp <- gastos_prestaciones[gastos_prestaciones$Comunidad.Autónoma == input$ccaa,]
    }
    
    if(input$any == "Tots"){
      temp <- temp
    }else{
      temp <- temp[temp$any == input$any,]
    }
    
    if(input$gasto == 1){
      t <- aggregate(temp$Gasto.Total.Prestación, by=list(any=temp$any), FUN=sum)
      text <- "Totals"
    }else if(input$gasto == 2){
      t <- aggregate(temp$Gasto.Prestación.Contributiva, by=list(any=temp$any), FUN=sum)
      text <- "Prestació contributiva"
    }else if(input$gasto == 3){
      t <- aggregate(temp$Gasto.Subsidio.Desempleo, by=list(any=temp$any), FUN=sum)
      text <- "Subsidi d'atur"
    }else if(input$gasto == 4){
      t <- aggregate(temp$Gasto.Renta.Activa.Inserción, by=list(any=temp$any), FUN=sum)
      text <- "Renta activa"
    }else if(input$gasto == 5){
      t <- aggregate(temp$Gasto.Subsidio.Eventuales.Agrarios, by=list(any=temp$any), FUN=sum)
      text <- "Subsidi agrari"
    }

    bp <- barplot(t$x, names.arg = t$any, main=paste0("Tipus de gasto: ",text))
    text(bp, t$x/2,labels=round(t$x,digits=2))
  })
  
  
  output$proviPlot <- renderPlot({
    if(input$ccaa == "Totes"){
      temp <- gastos_prestaciones
    }else{
      temp <- gastos_prestaciones[gastos_prestaciones$Comunidad.Autónoma == input$ccaa,]
    }
    
    if(input$any == "Tots"){
      temp <- temp
    }else{
      temp <- temp[temp$any == input$any,]
    }
    
    if(input$gasto == 1){
      t <- aggregate(temp$Gasto.Total.Prestación, by=list(provincia=temp$Provincia), FUN=sum)
      text <- "Totals"
    }else if(input$gasto == 2){
      t <- aggregate(temp$Gasto.Prestación.Contributiva, by=list(provincia=temp$Provincia), FUN=sum)
      text <- "Prestació contributiva"
    }else if(input$gasto == 3){
      t <- aggregate(temp$Gasto.Subsidio.Desempleo, by=list(provincia=temp$Provincia), FUN=sum)
      text <- "Subsidi d'atur"
    }else if(input$gasto == 4){
      t <- aggregate(temp$Gasto.Renta.Activa.Inserción, by=list(provincia=temp$Provincia), FUN=sum)
      text <- "Renta activa"
    }else if(input$gasto == 5){
      t <- aggregate(temp$Gasto.Subsidio.Eventuales.Agrarios, by=list(provincia=temp$Provincia), FUN=sum)
      text <- "Subsidi agrari"
    }
    
    bp <- barplot(t$x, names.arg = t$provincia, main=paste0("Gràfica per provincia"))
    text(bp, t$x/2,labels=round(t$x,digits=2))
  })
  
  
  output$histPlot <- renderPlot({
    if(input$ccaa == "Totes"){
      temp <- gastos_prestaciones
    }else{
      temp <- gastos_prestaciones[gastos_prestaciones$Comunidad.Autónoma == input$ccaa,]
    }
    
    if(input$any == "Tots"){
      temp <- temp
    }else{
      temp <- temp[temp$any == input$any,]
    }
    if(input$gasto == 1){
      d <- density(temp$Gasto.Total.Prestación/10000) # returns the density data
      text <- "Totals"
    }else if(input$gasto == 2){
      gastos_prestaciones
      d <- density(temp$Gasto.Prestación.Contributiva/10000) # returns the density data
      text <- "Prestació contributiva"
    }else if(input$gasto == 3){
      d <- density(temp$Gasto.Subsidio.Desempleo/10000) # returns the density data
      text <- "Subsidi d'atur"
    }else if(input$gasto == 4){
      d <- density(temp$Gasto.Renta.Activa.Inserción/10000) # returns the density data
      text <- "Renta activa"
    }else if(input$gasto == 5){
      d <- density(temp$Gasto.Subsidio.Eventuales.Agrarios/10000) # returns the density data
      text <- "Subsidi agrari"
    }
    
    plot(d,main=paste0("Tipus de gasto: ",text))
  })
  
  output$tablePres <- renderDataTable({
    if(input$ccaa == "Totes"){
      temp <- gastos_prestaciones
    }else{
      temp <- gastos_prestaciones[gastos_prestaciones$Comunidad.Autónoma == input$ccaa,]
    }
    
    if(input$any == "Tots"){
      temp <- temp
    }else{
      temp <- temp[temp$any == input$any,]
    }
    if(input$gasto == 1 && input$slider != 10000000){
      temp <- temp[temp$Gasto.Total.Prestación <= input$slider,]
    }else if(input$gasto == 2 && input$slider != 10000000){
      temp <- temp[temp$Gasto.Prestación.Contributiva <= input$slider,]
    }else if(input$gasto == 3 && input$slider != 10000000){
      temp <- temp[temp$Gasto.Subsidio.Desempleo <= input$slider,]
    }else if(input$gasto == 4 && input$slider != 10000000){
      temp <- temp[temp$Gasto.Renta.Activa.Inserción <= input$slider,]
    }else if(input$gasto == 5 && input$slider != 10000000){
      temp <- temp[temp$Gasto.Subsidio.Eventuales.Agrarios <= input$slider,]
    }
    temp})
}

# Run the app ----
shinyApp(ui = ui, server = server)
