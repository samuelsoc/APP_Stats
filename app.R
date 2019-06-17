
library(shiny)
library(jsonlite)
library(dplyr)
library(httr)
library(highcharter)
library(shinythemes)
library(viridisLite)

ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("Portales de Datos Abiertos en Chile"),
  
  
  # Lista de portales
  sidebarLayout(
    sidebarPanel(helpText("."),
      selectInput("Portales", "Seleccionar un Portal:", 
                  choices = c("Junaeb Abierta","Peñalolen", 
                              "Mineduc", "Puente Alto", "MTT",
                              "CNE", "Mineria Abierta", "División Gobierno Digital")),
      
      
      # Copy the line below to make a text input box
      helpText("Por defecto la selección trae los hits de los últimos 365 días."),
      helpText("Argumentos: 
               &days=, &hours=, &minutes=, &from=dd/mm/yyyy, &to=dd/mm/yyyy, &limit= "),
      textInput("text1", label = "Consulta", value = ""),
      
      submitButton("Aplicar"),
      hr(),
      #Descargar tablas especificas
      # 1 boton para cada tabla: Vistas, Visualizaciones y Colecciones
      ("Seleccione la tabla que desea descargar:"),
      hr(),
      downloadButton('downVistas', 'Hits Vistas') ,
      hr(),
      downloadButton('downViz', 'Hits Visualizaciones') ,
      hr(),
      downloadButton('downColec', 'Hits Colecciones') 
      ),
    
    
    
    mainPanel(
      hr(),
      helpText("String consulta completa"),
      verbatimTextOutput("value3"),
      
      hr(),
      helpText("Dirección web del portal consultado"),
      verbatimTextOutput("urls"),
      
      tabsetPanel(type = "tabs",
                  tabPanel("Vistas", tableOutput("tabla1"), highchartOutput("plot1", height = "500px")),
                  tabPanel("Visualizaciones", tableOutput("tabla2"),highchartOutput("plot2", height = "500px")),
                  tabPanel("Colecciones", tableOutput("tabla3"), highchartOutput("plot3", height = "500px"))
      )
      
      
    )
   ))

###  SERVER ####


server <- function(input, output) {
  
  #api estatadistica portales
  
  c1 = "http://cne.cloudapi.junar.com/api/v2/stats/?auth_key=50ec79952723047e1458e769a02a970a93eccdc5" #cne 
  c2 = "http://api.recursos.penalolen.cl/api/v2/stats/?auth_key=cf62f2ce9b3a44243fc30544845053aeadb34189" #peñalolen
  c3 = "http://junaebabierta.cloudapi.junar.com/api/v2/stats/?auth_key=687eb54a7c3a489a81bdbd6608e64d256a9fad3e" #junaeb
  c4 = "http://mineria.cloudapi.junar.com/api/v2/stats/?auth_key=992333be6c6de9405a04df84ef5d27a5af080c9d" #mineria
  c5 = "http://mpuentealto.cloudapi.junar.com/api/v2/stats/?auth_key=89463a6e47b704634ea8aa575343ae11be3326f6" #puente alto
  c6 = "http://api.datos.mineduc.cl/api/v2/stats/?auth_key=f22a0a86919812187e3167e380e6ca84333c1457" #mineduc
  c7 = "https://api.datos.observatoriologistico.cl/api/v2/stats/?auth_key=0ce0637660dae7c6cc528df48307289698126c52" #mtt
  c8 = "https://api.beta.datos.gob.cl/api/v2/stats/?auth_key=b05d1083a486aaea8363d6f9c7b2407d662a45f1"
  
  
  datasetInput <- reactive({
    switch(input$Portales,
           "CNE" = c1,
           "Peñalolen" = c2,
           "Junaeb Abierta" = c3,
           "Mineria Abierta" = c4,
           "Puente Alto" = c5,
           "Mineduc" = c6,
           "MTT" = c7)
  })
  
  # URL portales 
  
  p1 <- "http://junaebabierta.opendata.junar.com"
  p2 <- "http://energiaabierta.cl"
  p3 <- "https://datos.penalolen.cl"
  p4 <- "https://www.mineriaabierta.cl"
  p5 <- "https://datos.mpuentealto.cl"
  p6 <- "http://datos.mineduc.cl"
  p7 <- "https://datos.observatoriologistico.cl"
  
  urlportales <- reactive({
    switch(input$Portales,
           "CNE" = p1,
           "Peñalolen" = p3,
           "Junaeb Abierta" = p1,
           "Mineria Abierta" = p4,
           "Puente Alto" = p5,
           "Mineduc" = p6,
           "Observatorio Logístico" = p7)
    
  })
  
  
  
#string url api se iran agregando los parametros
  output$value1 <- renderPrint({ input$text1 })
  
  output$value3 <- renderText(paste(datasetInput(),input$text1, sep = ""))
  
  output$urls <- renderText(urlportales())
  
  
  #get Vistas  
  getQuery <- reactive({
    get <- fromJSON(paste0(datasetInput(),input$text1), flatten = TRUE)
    tab <- get$datastream$stats$resources
    tab <- tab %>% 
      mutate ( URL = paste0(urlportales(),landingPage),collapse = NULL) %>% 
      select('Cantidad Visitas' = count,
             'Nombre Vista' =title,
             'Categoria' = category,
              URL
      )
    
  })
  
  output$tabla1 = renderTable(
    getQuery()
  )
  
  #get VIZ
  
  getQueryV <- reactive({
    getV <- fromJSON(paste0(datasetInput(),input$text1), flatten = TRUE)
    tabV <- getV$visualizations$stats$resources
    tabV <- tabV %>% 
      mutate ( URL = paste0(urlportales(),landingPage),collapse = NULL) %>%
      select('Cantidad Visitas' = count,
             'Nombre Visualización' =title,
             'Categoria' = category,
             URL)
    
  })
  
  output$tabla2 = renderTable(
    getQueryV()
  )
  
  
  #get colecciones
  getQueryC <- reactive({
    getC <- fromJSON(paste0(datasetInput(),input$text1), flatten = TRUE)
    tabC <- getC$dashboards$stats$resources
    tabC <- tabC %>% 
      select('Cantidad Visitas' = count,
             'Nombre Colección' =title,
             'Categoria' = category,
             URL)
    
  })
  
  output$tabla3 = renderTable(
    getQueryC()
  )
  
  
  #### descarga archivos
  output$downVistas <- downloadHandler(
    filename = function() {
      paste("HitVistas-", Sys.Date(),input$Portales, ".csv", sep="")
    },
    content = function(file) {
      write.csv(getQuery(), file, row.names = FALSE)
    }
  )
  
  output$downViz <- downloadHandler(
    filename = function() {
      paste("HitViz-", Sys.Date(),input$Portales, ".csv", sep="")
    },
    content = function(file) {
      write.csv(getQueryV(), file, row.names = FALSE)
    }
  )
  
  output$downColec <- downloadHandler(
    filename = function() {
      paste("HitColec-", Sys.Date(),input$Portales, ".csv", sep="")
    },
    content = function(file) {
      write.csv(getQueryC(), file, row.names = FALSE)
    }
  )
  
  
  #plot

  
  output$plot1 <- renderHighchart({
    
    p1 <- hchart(getQuery(), "bar", hcaes(x = `Nombre Vista`, y = `Cantidad Visitas`, color = `Cantidad Visitas`)) %>% 
      hc_exporting(
        enabled = TRUE) %>% 
      hc_title(text = "Top 10 Vistas más consultadas",style = list(color = "#CCC")) %>% 
      hc_xAxis(labels = list(style = list(color = "#CCC"))) %>% 
      hc_yAxis(labels = list(style = list(color = "#CCC"))) 
    return(p1)
    
  })
  
  output$plot2 <- renderHighchart({
    
    p2 <- hchart(getQueryV(), "bar", hcaes(x = `Nombre Visualización`, y = `Cantidad Visitas`, color = `Cantidad Visitas`)) %>% 
      hc_exporting(
        enabled = TRUE) %>% 
      hc_title(text = "Top 10 Visualizaciones más consultadas",style = list(color = "#CCC"))%>% 
      hc_xAxis(labels = list(style = list(color = "#CCC"))) %>% 
      hc_yAxis(labels = list(style = list(color = "#CCC")))  
    
    return(p2)
    
  })
  
  output$plot3 <- renderHighchart({
    
    p3 <- hchart(getQueryC(), "bar", hcaes(x = `Nombre Colección`, y = `Cantidad Visitas`, color = `Cantidad Visitas`)) %>% 
      hc_exporting(
        enabled = TRUE) %>% 
      hc_title(text = "Top 10 Colecciones más consultadas",style = list(color = "#CCC")) %>% 
    hc_xAxis(labels = list(style = list(color = "#CCC"))) %>% 
    hc_yAxis(labels = list(style = list(color = "#CCC")))  
    
    
    return(p3)
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

