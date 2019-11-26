library(shiny)
library(dplyr)
library(ggplot2)
library(questionr)
library(sjPlot)
library(labelled)
library(shinydashboard)
library(haven)


personas <- read_dta("Características generales de las personas.dta", enconding = "UTF-8")
viviendas <- read_dta("Datos de la vivienda.dta", enconding = "UTF-8")

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


var_personas <- c("P220", "P5785", "P5501", "P6210", "P6210S1", "P1366", "P1402", 
                  "P1403", "P1365", "P1364", "P1363")

var_vivienda <- c("P5747", "P5752", "P5752S1", "P5090")

ui <- dashboardPage(
  dashboardHeader(title = "Seguridad Ciudadana"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Personas y Vivienda", tabName = "personas", icon = icon("users", lib = "font-awesome")),
      menuItem("Seguridad", tabName = "seguridad", icon = icon("user-lock", lib = "font-awesome")) #user-lock
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "personas",
                selectInput(inputId = "variables",
                            label = "Seleccione la variable de interés:",
                            choices = var_vivienda),
                            htmlOutput("tabla", height = 200)
      ),
      tabItem(tabName = "seguridad", textOutput("text"))
    )
  )
)


server <- function(input, output) {
  output$tabla <- renderUI({
    custom_table <- sjt.xtab(var.row = datos1[["DEPMUNI"]],
                             var.col = datos1[[input$variables]],
                             weight.by = datos1$FEX_C.x,
                             var.labels = c("Ciudad Principal",
                                            var_label(datos1[[input$variables]])),
                             show.row.prc = T, show.summary = F,
                             show.legend = F,
                             encoding = "UTF-8")
    HTML(custom_table$knitr)
  })
  
  output$text <- renderText({
    "Hola"
  })
}  

  shinyApp(ui, server)


