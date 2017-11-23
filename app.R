
library(shiny)
library(jsonlite)
library(dplyr)
library(httr)


ui <- fluidPage(
   
   
   titlePanel("Ranking de Hits Portales "),
   
   helpText("Los resultados corresponden a los hits acumulados en los últimos 30 días"),
   # Lista de portales
   sidebarLayout(
      sidebarPanel(
        selectInput("Portales", "Seleccionar un Portal:", 
                    choices = c(" ","Peñalolen", 
                                "Junaeb Abierta","Mineduc", "Puente Alto", "MTT",
                                "CNE", "Mineria Abierta", "Pilar","La Plata","Enacom","La Nación","Arsat", 
                                "San Lorenzo", "Vicente Lopez", "Bahía Blanca", "Tigre", "Villa María",
                                "Lujan","Olavarria","Pergamino","San Isidro", "PBA", "OEFA","MEF","Agrorural",
                                "Miraflores","MSI","Senace","Palo Alto","San Jose","Mesaaz","Cupertino",
                                "Anaheim","RTA","Arlington","SAC County","Jackson", "IAFA", "MOPT", "Escazu",
                                "MSJ","Presidencia CR", "munipalmares","Infralatam")),
        br(),
        
        helpText("Seleccione el formato de descarga:"),
        radioButtons("filetype", "Tipo Archivo:",
                     choices = c("csv", "tsv")),
       
       
        hr(),
        
        downloadButton('downloadData', 'Descargar')
        
      ),
      
      
      mainPanel(
         tableOutput("tabla1"),
         hr(),
         plotOutput("Grafico1")
      
      )
   )
)

###  SERVER ####


server <- function(input, output) {
  
  
 #Chile
  c1 = ("http://cne.cloudapi.junar.com/api/v2/stats/?auth_key=50ec79952723047e1458e769a02a970a93eccdc5") #cne 
  c2 = ("http://api.recursos.penalolen.cl/api/v2/stats/?auth_key=cf62f2ce9b3a44243fc30544845053aeadb34189") #peñalolen
  c3 = ("http://junaebabierta.cloudapi.junar.com/api/v2/stats/?auth_key=687eb54a7c3a489a81bdbd6608e64d256a9fad3e") #junaeb
  c4 = ("http://mineria.cloudapi.junar.com/api/v2/stats/?auth_key=992333be6c6de9405a04df84ef5d27a5af080c9d") #mineria
  c5 = ("http://mpuentealto.cloudapi.junar.com/api/v2/stats/?auth_key=89463a6e47b704634ea8aa575343ae11be3326f6") #puente alto
  c6 = ("http://api.datos.mineduc.cl/api/v2/stats/?auth_key=f22a0a86919812187e3167e380e6ca84333c1457") #mineduc
  c7 = ("https://api.datos.observatoriologistico.cl/api/v2/stats/?auth_key=0ce0637660dae7c6cc528df48307289698126c52") #mtt
  #Argentina
  c8 = ("http://pilar.cloudapi.junar.com/api/v2/stats/?auth_key=ac0d7fc2804b535637ae44aa5f0bc57c674d3353")#pilar
  c9 = ("http://api.datos.laplata.gov.ar/api/v2/stats/?auth_key=a1ebd0b5eb581230098dc99ed06636760810b8c8")#laplata
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

  #parametros
  dd <-"&days=30"

  #gets Vistas
  
  C1<- fromJSON(paste0(c1,dd), flatten = TRUE)
  C1<- C1$datastreams$stats$resources
  C1 <- C1 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
          # 'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  
  C2<- fromJSON(paste0(c2,dd), flatte = TRUE)
  C2<- C2$datastream$stats$resources
  C2 <- C2 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  
  C3<- fromJSON(paste0(c3,dd), flatte = TRUE)
  C3<- C3$datastream$stats$resources
  C3 <- C3 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  
  C4<-fromJSON(paste0(c4,dd), flatten = TRUE)
  C4<-C4$datastream$stats$resources
  C4 <- C4 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)

  C5<-fromJSON(paste0(c5,dd), flatten = TRUE)
  C5<-C5$datastream$stats$resources
  C5 <- C5 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid) 

  C6<-fromJSON(paste0(c6,dd), flatten = TRUE)
  C6<-C6$datastream$stats$resources
  C6 <- C6 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)  

  C7<-fromJSON(paste0(c7,dd), flatten = TRUE)
  C7<-C7$datastream$stats$resources
  C7 <- C7 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)      

  C8<-fromJSON(paste0(c8,dd), flatten = TRUE)
  C8<-C8$datastream$stats$resources
  C8 <- C8 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)

  C9<-fromJSON(paste0(c9,dd), flatten = TRUE)
  C9<-C9$datastream$stats$resources
  C9 <- C9 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)      
  C10<-fromJSON(paste0(c10,dd), flatten = TRUE)
  C10<-C10$datastream$stats$resources
  C10 <- C10 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid) 
  C11<-fromJSON(paste0(c11,dd), flatten = TRUE)
  C11<-C11$datastream$stats$resources
  C11 <- C11 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)      
  C12<-fromJSON(paste0(c12,dd), flatten = TRUE)
  C12<-C12$datastream$stats$resources
  C12 <- C12 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  C13<-fromJSON(paste0(c13,dd), flatten = TRUE)
  C13<-C13$datastream$stats$resources
  C13 <- C13 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)   
  C14<-fromJSON(paste0(c14,dd), flatten = TRUE)
  C14<-C14$datastream$stats$resources
  C14 <- C14 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)    
  C15<-fromJSON(paste0(c15,dd), flatten = TRUE)
  C15<-C15$datastream$stats$resources
  C15 <- C15 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)  
  C16<-fromJSON(paste0(c16,dd), flatten = TRUE)
  C16<-C16$datastream$stats$resources
  C16 <- C16 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid) 
  C17<-fromJSON(paste0(c17,dd), flatten = TRUE)
  C17<-C17$datastream$stats$resources
  C17 <- C17 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)  
  C18<-fromJSON(paste0(c18,dd), flatten = TRUE)
  C18<-C18$datastream$stats$resources
  C18 <- C18 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  C19<-fromJSON(paste0(c19,dd), flatten = TRUE)
  C19<-C19$datastream$stats$resources
  C19 <- C19 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid) 
  C20<-fromJSON(paste0(c20,dd), flatten = TRUE)
  C20<-C20$datastream$stats$resources
  C20 <- C20 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  C021<-fromJSON(paste0(c021,dd), flatten = TRUE)
  C021<-C021$datastream$stats$resources
  C021 <- C021 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)  
  C022<-fromJSON(paste0(c022,dd), flatten = TRUE)
  C022<-C022$datastream$stats$resources
  C022 <- C022 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)                  
  C21<-fromJSON(paste0(c21,dd), flatten = TRUE)
  C21<-C21$datastream$stats$resources
  C21 <- C21 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)                                                                                                    
  C22<-fromJSON(paste0(c22,dd), flatten = TRUE)
  C22<-C22$datastream$stats$resources
  C22 <- C22 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)  
  C23<-fromJSON(paste0(c23,dd), flatten = TRUE)
  C23<-C23$datastream$stats$resources
  C23 <- C23 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)     
  C24<-fromJSON(paste0(c24,dd), flatten = TRUE)
  C24<-C24$datastream$stats$resources
  C24 <- C24 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)      
  C25<-fromJSON(paste0(c25,dd), flatten = TRUE)
  C25<-C25$datastream$stats$resources
  C25 <- C25 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  #C26<-fromJSON(paste0(c26,dd), flatten = TRUE)
  #C26<-C26$datastream$stats$resources
  #C26 <- C26 %>% 
  #  select('Cantidad Visitas' = count,
   #        'Nombre Vista' =title,
    #       'Descripción' = description,
     #      #'URL Vista' = URL,
      #     'Categoria' = category,
       #    'GUID' = guid)
  #C27<-fromJSON(paste0(c27,dd), flatten = TRUE)
  #C27<-C27$datastream$stats$resources
  #C27 <- C27 %>% 
   # select('Cantidad Visitas' = count,
    #      'Descripción' = description,
     #      #'URL Vista' = URL,
      #     'Categoria' = category,
       #    'GUID' = guid)
  C28<-fromJSON(paste0(c28,dd), flatten = TRUE)
  C28<-C28$datastream$stats$resources
  C28 <- C28 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
 C29<-fromJSON(paste0(c29,dd), flatten = TRUE)
  C29<-C29$datastream$stats$resources
  C29 <- C29 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)   

  C30<-fromJSON(paste0(c30,dd), flatten = TRUE)
  C30<-C30$datastream$stats$resources
  C30 <- C30 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)                                                        
  C31<-fromJSON(paste0(c31,dd), flatten = TRUE)
  C31<-C31$datastream$stats$resources
  C31 <- C31 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  C32<-fromJSON(paste0(c32,dd), flatten = TRUE)
  C32<-C32$datastream$stats$resources
  C32 <- C32 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  C33<-fromJSON(paste0(c33,dd), flatten = TRUE)
  C33<-C33$datastream$stats$resources
  C33 <- C33 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  C34<-fromJSON(paste0(c34,dd), flatten = TRUE)
  C34<-C34$datastream$stats$resources
  C34 <- C34 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)                                                        
  C35<-fromJSON(paste0(c35,dd), flatten = TRUE)
  C35<-C35$datastream$stats$resources
  C35 <- C35 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid) 
  C36<-fromJSON(paste0(c36,dd), flatten = TRUE)
  C36<-C36$datastream$stats$resources
  C36 <- C36 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid) 
  C37<-fromJSON(paste0(c37,dd), flatten = TRUE)
  C37<-C37$datastream$stats$resources
  C37 <- C37 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid) 
  C38<-fromJSON(paste0(c38,dd), flatten = TRUE)
  C38<-C38$datastream$stats$resources
  C38 <- C38 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)                                                        
  C39<-fromJSON(paste0(c39,dd), flatten = TRUE)
  C39<-C39$datastream$stats$resources
  C39 <- C39 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  C40<-fromJSON(paste0(c40,dd), flatten = TRUE)
  C40<-C40$datastream$stats$resources
  C40 <- C40 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)   
  C41<-fromJSON(paste0(c41,dd), flatten = TRUE)
  C41<-C41$datastream$stats$resources
  C41 <- C41 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)  
  C42<-fromJSON(paste0(c42,dd), flatten = TRUE)
  C42<-C42$datastream$stats$resources
  C42 <- C42 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)    
  C43<-fromJSON(paste0(c43,dd), flatten = TRUE)
  C43<-C43$datastream$stats$resources
  C43 <- C43 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)
  C44<-fromJSON(paste0(c44,dd), flatten = TRUE)
  C44<-C44$datastream$stats$resources
  C44 <- C44 %>% 
    select('Cantidad Visitas' = count,
           'Nombre Vista' =title,
           'Descripción' = description,
           #'URL Vista' = URL,
           'Categoria' = category,
           'GUID' = guid)                                                                              
                                                                 
                                                                











  ###########################################
  
  #cómo puedo hacer que al seleccionar una opcion de la lista, haga el get correspondiente?
  # ejemplo, SI selecciono "CNE", que se ejecute "C1"
  # es importante que se ejecute el get para hacer una consulta nueva a la API
  
  
  
 #condicionar una Query a una seleccion de la Lista 
  
  datasetInput <- reactive({
    switch(input$Portales,
           "CNE" = C1,
           "Peñalolen" = C2,
           "Junaeb Abierta" = C3,
           "Mineria Abierta" = C4,
           "Puente Alto" = C5,
           "Mineduc" = C6,
           "MTT" = C7,
           "Pilar" = C8,
           "La Plata" = C9,
           "Enacom" = C10,
           "La Nación" = C11,
           "Arsat" = C12,
           "San Lorenzo" = C13,
           "Vicente Lopez" = C14,
           "Bahía Blanca" = C15,
           "Tigre" = C16,
           "Villa María" = C17,
           "Lujan" = C18,
           "Olavarria" = C19,
           "Pergamino" = C20,
           "San Isidro" =  C021,
           "PBA" = C022,
           "OEFA" = C21,
           "MEF" = C22,
           "Agrorural" = C23,
           "Miraflores" = C24,
           "MSI" = C25,
           "Senace" = C28,
           "Palo Alto" = C29,
           "San Jose" =C30,
           "Mesaaz" = C31,
           "Cupertino" = C32,
           "Anaheim" = C33,
           "RTA" = C34,
           "Arlington" = C35,
           "SAC County" = C36,
           "Jackson" = C37,
           "IAFA" = C38,
           "MOPT" = C39,
           "Escazu" = C40,
           "MSJ" = C41,
           "Presidencia CR" = C42,
           "Muni Palmares" = C43,
           "Infralatam" = C44

           )
  })
  
  
 #resultados de la query 
  
   
  #tabla con resultados de la query 
  output$tabla1 = renderTable(
    datasetInput()
    
  )

  
  
  #### descarga archivos
  output$downloadData <- downloadHandler(
    
       filename = function() {
      paste(input$Portales, input$filetype, sep = ".")
    },
    
   
      content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      write.table(datasetInput(), file, sep = sep,
                  row.names = FALSE)
    }
  )
  
  
  ###  Grafico 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

