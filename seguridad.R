library(shiny)
library(dplyr)
library(ggplot2)
library(questionr)
library(sjPlot)
library(labelled)
library(shinydashboard)
library(haven)
library(scales)


personas <- read_dta("Características generales de las personas.dta")
viviendas <- read_dta("Datos de la vivienda.dta")

#Principales ciudades del país:
#Bogotá (11001), Medellín (05001), Cali (76001), Barranquilla (08001), Bucaramanga (68001)
#edit(names(datos))


viviendas$DEPMUNI <- as.character(viviendas$DEPMUNI)
viviendas$DEPMUNI[viviendas$DEPMUNI == "11001"] <- "BOGOTA"
viviendas$DEPMUNI[viviendas$DEPMUNI == "05001"] <- "MEDELLIN"
viviendas$DEPMUNI[viviendas$DEPMUNI == "76001"] <- "CALI"
viviendas$DEPMUNI[viviendas$DEPMUNI == "08001"] <- "BARRANQUILLA"
viviendas$DEPMUNI[viviendas$DEPMUNI == "68001"] <- "BUCARAMANGA"


vivienda <- viviendas %>% filter(DEPMUNI %in% c("BOGOTA", "MEDELLIN", "CALI", "BARRANQUILLA", "BUCARAMANGA"))

datos1 <- inner_join(vivienda, personas, by = "DIRECTORIO")


var_vivienda <- c("P5747", "P5752", "P5752S1", "P5090")

var_personas <- c("P220", "P6210", "P1366", "P1402", 
                  "P1403", "P1365", "P1363") #No incluir Edad P5785

# desarrollo de la aplicación
ui <- dashboardPage(
  dashboardHeader(title = "Seguridad Ciudadana"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vivienda", tabName = "vivienda", icon = icon("users", lib = "font-awesome")),
      menuItem("Personas", tabName = "personas", icon = icon("user-cog", lib = "font-awesome")),
      menuItem("Seguridad", tabName = "seguridad", icon = icon("user-lock", lib = "font-awesome")) #user-lock
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "vivienda",
              titlePanel("Datos de vivienda"),
              textOutput("text1"),
              selectInput(inputId = "var1",
                          label = "Seleccione la variable de acuerdo a la característica de la vivienda:",
                          choices = var_vivienda),
                
              tabsetPanel(
                tabPanel("Tabla de Frecuencia", fluidRow(box(htmlOutput("tabla1")), box(textOutput("text2")))),
                tabPanel("Gráfico de Barras", fluidRow(box(plotOutput("tabla2", width = 500)), box(textOutput("text3"))))
              )
              ),
      
      tabItem(tabName = "personas",
              titlePanel("Características de las personas de las cinco ciudades principales del país"),
              textOutput("text4"),
              selectInput(inputId = "var2",
                          label = "Seleccione la variable de acuerdo a la consulta sobre las características de las personas:",
                          choices = var_personas),
              tabsetPanel(
                tabPanel("Tabla de Frecuencia", fluidRow(box(htmlOutput("tabla3"), width = 8, height = 510), box(textOutput("text5"), width = 8))),
                tabPanel("Gráfico de Barras", fluidRow(box(plotOutput("tabla4", width = 500)), box(textOutput("text6"))))
              )
              )#,
      
      #tabItem(tabName = "seguridad", textOutput("text3")#,
        #      )
            )
  )
)



server <- function(input, output) {
  output$text1 <- renderText({
     "Dentro de los datos de vivienda contiene información sobre las condiciones físicas de la vivienda. Las preguntas encuestas
    fueron: Tipo de vivienda (P5747), Servicio de Energía Eléctrica (P5752), Estrato (P5752S1), Propiedad de la vivienda (P5090).
    "
  })  
  
  output$tabla1 <- renderUI({
    custom_table1 <- sjt.xtab(var.row = datos1[["DEPMUNI"]],
                             var.col = datos1[[input$var1]],
                             weight.by = datos1$FEX_C.x,
                             var.labels = c("Ciudad Principal",
                                            var_label(datos1[[input$var1]])),
                             show.row.prc = T, show.summary = F,
                             show.legend = F,
                             encoding = "UTF-8")
    HTML(custom_table1$knitr)
  })

  
  output$text2 <- renderText({
    "Hola"
  })
  
  output$tabla2 <- renderPlot ({ggplot(data = datos1, aes(x = datos1[[input$var1]], fill  = factor(datos1[["DEPMUNI"]]), weights = datos1$FEX_C.x)) + 
      geom_bar() + labs(x = var_label(datos1[[input$var1]]),
                        y = "Población",
                         fill = "Ciudades principales", caption = "DANE") + scale_y_continuous(labels = comma) 
  })
  
  
  output$text3 <- renderText({
    "Hola"
  })
  
  output$text4 <- renderText({
    "Las Características generales de las personas contiene información sobre edad, sexo, estado civil, y para los miembros de 15 años y más, el nivel educativo y la actividad principal. Este apartado contiene las siguientes variables:
    Sexo (P220), Nivel educativo (P6210), Estado Civil (P1366), Tiempo viviendo en la ciudad (P1402), Tiempo viviendo en el barrio (P1403), Actividad Principal (P1365), Jornada de la actividad principal (P1363).
    "
  })  
  
  output$tabla3 <- renderUI({
    custom_table2 <- sjt.xtab(var.row = datos1[["DEPMUNI"]],
                              var.col = datos1[[input$var2]],
                              weight.by = datos1$FEX_C.x,
                              var.labels = c("Ciudad Principal",
                                             var_label(datos1[[input$var2]])),
                              show.row.prc = T, show.summary = F,
                              show.legend = F,
                              encoding = "UTF-8")
    HTML(custom_table2$knitr)
  })
  
  output$tabla4 <- renderPlot ({ggplot(data = datos1, aes(x = datos1[[input$var2]], fill  = factor(datos1[["DEPMUNI"]]), weights = datos1$FEX_C.x)) + 
      geom_bar() + labs(x = var_label(datos1[[input$var2]]),
                        y = "Población",
                        fill = "Ciudades principales", caption = "DANE") + scale_y_continuous(labels = comma) 
  })
  
  
  output$text5 <- renderText({
    "Hola"
  })
  
  output$text6 <- renderText({
    "Hola"
  })
}  

shinyApp(ui = ui, server = server)

