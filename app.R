
library(shiny)
library(jsonlite)
library(dplyr)
library(httr)
library(ggplot2)
 
ui <- fluidPage(
  titlePanel("Ranking de Hits a recursos"),
  
  
  # Lista de portales
  sidebarLayout(
    sidebarPanel(
      selectInput("Portales", "Seleccionar un Portal:", 
                  choices = c("Junaeb Abierta","Peñalolen", 
                              "Mineduc", "Puente Alto", "MTT",
                              "CNE", "Mineria Abierta", "Pilar","La Plata","Enacom","La Nación","Arsat", 
                              "San Lorenzo", "Vicente Lopez", "Bahía Blanca", "Tigre", "Villa María",
                              "Lujan","Olavarria","Pergamino","San Isidro", "PBA", "OEFA","MEF","Agrorural",
                              "Miraflores","MSI","Senace","Palo Alto","San Jose","Mesaaz","Cupertino",
                              "Anaheim","RTA","Arlington","SAC County","Jackson", "IAFA", "MOPT", "Escazu",
                              "MSJ","Presidencia CR", "munipalmares","Infralatam")),
      
      
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
      
      tabsetPanel(type = "tabs",
                  tabPanel("Vistas", tableOutput("tabla1")),
                  tabPanel("Visualizaciones", tableOutput("tabla2")),
                  tabPanel("Colecciones", tableOutput("tabla3"))
      )
      
      
    ))
   )

###  SERVER ####


server <- function(input, output) {
  
  
  c1 = "http://cne.cloudapi.junar.com/api/v2/stats/?auth_key=50ec79952723047e1458e769a02a970a93eccdc5" #cne 
  c2 = "http://api.recursos.penalolen.cl/api/v2/stats/?auth_key=cf62f2ce9b3a44243fc30544845053aeadb34189" #peñalolen
  c3 = "http://junaebabierta.cloudapi.junar.com/api/v2/stats/?auth_key=687eb54a7c3a489a81bdbd6608e64d256a9fad3e" #junaeb
  c4 = "http://mineria.cloudapi.junar.com/api/v2/stats/?auth_key=992333be6c6de9405a04df84ef5d27a5af080c9d" #mineria
  c5 = "http://mpuentealto.cloudapi.junar.com/api/v2/stats/?auth_key=89463a6e47b704634ea8aa575343ae11be3326f6" #puente alto
  c6 = "http://api.datos.mineduc.cl/api/v2/stats/?auth_key=f22a0a86919812187e3167e380e6ca84333c1457" #mineduc
  c7 = "https://api.datos.observatoriologistico.cl/api/v2/stats/?auth_key=0ce0637660dae7c6cc528df48307289698126c52" #mtt
  #Argentina
  c8 = "http://pilar.cloudapi.junar.com/api/v2/stats/?auth_key=ac0d7fc2804b535637ae44aa5f0bc57c674d3353"#pilar
  c9 = "http://api.datos.laplata.gov.ar/api/v2/stats/?auth_key=a1ebd0b5eb581230098dc99ed06636760810b8c8"#laplata
  c10 =("http://api.datosabiertos.enacom.gob.ar/api/v2/stats/?auth_key=47e5cd82e8ad35adbb246007be3d205e35e48a2a")#enacom
  c11 =("http://lanacion.cloudapi.junar.com/api/v2/stats/?auth_key=2b4f0bb7cf85c208d4dc0f79bd93c66b975b8924")#lanacion
  c12 = ("http://api.datos.arsat.com.ar/api/v2/stats/?auth_key=f48e691e4683448d27e49f1583cdab4536c85fa6")#arsat
  c13 = ("http://api.datos.sanlorenzo.com.ar/api/v2/stats/?auth_key=5b1eaad3d7c3db5bba09894e7e0bd2317de5e7f4")#sanlorenzo
  c14 = ("http://vicentelopez.cloudapi.junar.com/api/v2/stats/?auth_key=9dbc569dd1a9f805fb1602fe2a119491fcfdea54")#vicentelopez
  c15 = ("http://api.datos.bahiablanca.gob.ar/api/v2/stats/?auth_key=930d8c6a9c5ae53ed91cefe448aba15d249bbc2d")#bahiablanca
  c16 = ("http://tigre.cloudapi.junar.com/api/v2/stats/?auth_key=a849ca80c3f92d7c25e76c143d93c0584ea70fa1")#tigre
  c17 = ("http://api.datos.villamaria.gob.ar/api/v2/stats/?auth_key=197933b9a86ab542d8190cac854a59eb16257cb0")#villamaria
  c18 = ("http://api.datos.lujan.gov.ar/api/v2/stats/?auth_key=3e64fdf5eb4d6baeefd10ecb4be9f5c0d9177dd9")#lujan
  c19 = ("http://api.datosabiertos.olavarria.gov.ar/api/v2/stats/?auth_key=38bea1731d869385cae6124415d2d8a279ae6b0b")#olavarria
  c20 = ("http://pergamino.cloudapi.junar.com/api/v2/stats/?auth_key=8ace8bf7e80d2eee383701a17ce1d86a7810be97")#pergamino
  c021 = ("http://api.sanisidro.cloudapi.junar.com/api/v2/stats/?auth_key=cc70c8b090d172d32667db0a84080dec7d26894e")#sanisidro
  c022 = ("http://api.datos.gba.gob.ar/api/v2/stats/?auth_key=aa29e9c649b727894cc36b5082d49c42e26092b2")#gba
  ##Peru
  c21 = ("http://oefa.cloudapi.junar.com/api/v2/stats/?auth_key=ffd5ad5c4b0675e12ccacb58d03fd4c13cf334a4")
  c22 = ("http://api.datosabiertos.mef.gob.pe/api/v2/stats/?auth_key=c3fb0e2430db0c86549c0c6836407f31fee1228e")
  c23 = ("http://api.datos.agrorural.gob.pe/api/v2/stats/?auth_key=ea9ab1569ae563edcb86ffd01b0c721b4b113f1f")
  c24 = ("http://miraflores.cloudapi.junar.com/api/v2/stats/?auth_key=9655388a2fdaa944358e6fa76bb469db09a8e270")
  c25 = ("http://api.datosabiertos.msi.gob.pe/api/v2/stats/?auth_key=d7c8b1df88eb5ac16c021e97e46fe40679ff71ca")
  #c26 = ("http://api.datosabiertos.mef.gob.pe/api/v2/stats/?auth_key=371fe013bc344033609c0a5e7dc4036bef0a3fe0")
  #c27 = ("http://oefa.cloudapi.junar.com/api/v2/stats/?auth_key=ea5b5439c5821fe45b9c357a731e25e5a171e461")
  c28 = ("http://senace.cloudapi.junar.com/api/v2/stats/?auth_key=ad6cd39a204e567492b8b8e36ef713fe78f3d6c6")
  #USA
  c29 = ("http://paloalto.cloudapi.junar.com/api/v2/stats/?auth_key=95b919eef0d6d80a19fd54dd67f431e070737579")
  c30 = ("https://api.data.sanjoseca.gov/api/v2/stats/?auth_key=16c9c574f6552e5431f5d11bf576f0f1bd8bf10f")
  c31 = ("http://api.open.mesaaz.gov/api/v2/stats/?auth_key=2db989e127d370149b9dbcf3b5ccc642cc2b309b")
  c32 = ("http://cupertino.cloudapi.junar.com/api/v2/stats/?auth_key=1663e3c7e228d4b114097d7aa853cada3f882c70")
  c33 = ("http://anaheim.cloudapi.junar.com/api/v2/stats/?auth_key=66c00f278bc9f2305ac4072796ad7b93162c80c2")
  c34 = ("http://api.data.rtachicago.org/api/v2/stats/?auth_key=3ffdded7ae0ff2f0dc66bb2a634397304cbca669")
  c35 = ("https://api.data.arlingtonva.us/api/v2/stats/?auth_key=94865ce516530b957f028dc61221b6fb83c6dce9")
  c36 = ("http://saccounty.cloudapi.junar.com/api/v2/stats/?auth_key=3bbe818514f91f8200953a4914182ae86d16f3c0")
  c37 = ("http://jacksonmi.cloudapi.junar.com/api/v2/stats/?auth_key=e79f8a1d42b5423246ff6033744b862b013b51f4")
  #CostaRica
  c38 = ("http://api.datosabiertos.iafa.go.cr/api/v2/stats/?auth_key=429bb5a3b1ea816483c90385cd5c31f70d59a5ea")
  c39 = ("http://mopt.cloudapi.junar.com/api/v2/stats/?auth_key=c8510cd6f3a76509ebe0c7ace210cae3b4cd0c7b")
  c40 = ("http://escazu.cloudapi.junar.com/api/v2/stats/?auth_key=a6c9c2a20207a8d4973481a8a3f9474930f5dc62")
  c41 = ("http://api.datosabiertos.msj.go.cr/api/v2/stats/?auth_key=291226a554075bd8f6ac59cd59bf8517f5d70203")
  c42 = ("http://api.datosabiertos.presidencia.go.cr/api/v2/stats/?auth_key=4829b2af3b9e160a3d24a0d595cfb14c05997667")
  c43 = ("http://api.datos.munipalmares.go.cr/api/v2/stats/?auth_key=54dfb7928021698d55bcef06a7f8b2357f2ae3f3")
  #Otro
  c44 = ("http://api.es.infralatam.info/api/v2/stats/?auth_key=7e6e99a2c61b5210ad947d4aa4e7087d4954340f")
  
  
  datasetInput <- reactive({
    switch(input$Portales,
           "CNE" = c1,
           "Peñalolen" = c2,
           "Junaeb Abierta" = c3,
           "Mineria Abierta" = c4,
           "Puente Alto" = c5,
           "Mineduc" = c6,
           "MTT" = c7,
           "Pilar" = c8,
           "La Plata" = c9,
           "Enacom" = c10,
           "La Nación" = c11,
           "Arsat" = c12,
           "San Lorenzo" = c13,
           "Vicente Lopez" = c14,
           "Bahía Blanca" = c15,
           "Tigre" = c16,
           "Villa María" = c17,
           "Lujan" = c18,
           "Olavarria" = c19,
           "Pergamino" = c20,
           "San Isidro" =  c021,
           "PBA" = c022,
           "OEFA" = c21,
           "MEF" = c22,
           "Agrorural" = c23,
           "Miraflores" = C24,
           "MSI" = c25,
           "Senace" = c28,
           "Palo Alto" = c29,
           "San Jose" = c30,
           "Mesaaz" = c31,
           "Cupertino" = c32,
           "Anaheim" = c33,
           "RTA" = c34,
           "Arlington" = c35,
           "SAC County" = c36,
           "Jackson" = c37,
           "IAFA" = c38,
           "MOPT" = c39,
           "Escazu" = c40,
           "MSJ" = c41,
           "Presidencia CR" = c42,
           "Muni Palmares" = c43,
           "Infralatam" = c44)
  })
  
  
  
  
  
#string url api se iran agregando los parametros
  output$value1 <- renderPrint({ input$text1 })
  
  output$value3 <- renderText(paste(datasetInput(),input$text1, sep = ""))
  
  
  #get Vistas  
  getQuery <- reactive({
    get <- fromJSON(paste0(datasetInput(),input$text1), flatten = TRUE)
    tab <- get$datastream$stats$resources
    tab <- tab %>% 
      select('Cantidad Visitas' = count,
             'Nombre Vista' =title,
             'Descripción' = description,
             #'URL Vista' = URL,
             'Categoria' = category,
             'GUID' = guid)
    
  })
  
  output$tabla1 = renderTable(
    getQuery()
  )
  
  #get VIZ
  
  getQueryV <- reactive({
    getV <- fromJSON(paste0(datasetInput(),input$text1), flatten = TRUE)
    tabV <- getV$visualizations$stats$resources
    tabV <- tabV %>% 
      select('Cantidad Visitas' = count,
             'Nombre Visualización' =title,
             'Descripción' = description,
             #'URL Vista' = URL,
             'Categoria' = category,
             'GUID' = guid)
    
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
             'Descripción' = description,
             #'URL Vista' = URL,
             'Categoria' = category,
             'GUID' = guid)
    
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
  
 
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

