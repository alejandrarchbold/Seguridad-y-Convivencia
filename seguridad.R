  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(questionr)
  library(sjPlot)
  library(labelled)
  library(shinydashboard)
  library(haven)
  library(scales)
  library(quantreg)
  
  
  personas <- read_dta("Características generales de las personas.dta")
  viviendas <- read_dta("Datos de la vivienda.dta")
  seguridad_hogar <- read_dta("Percepción de seguridad en el hogar.dta")
  hurto_residencias <- read_dta("Hurto a residencias.dta")
  hurto_personas <- read_dta("Hurto a personas.dta")
  inst <- read_dta("Percepción del aporte a la seguridad.dta")
  
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
  
  datos2 <- inner_join(vivienda, seguridad_hogar, by = "DIRECTORIO")
  
  datos3 <- inner_join(vivienda, hurto_residencias, by = "DIRECTORIO")
  
  datos4 <- inner_join(vivienda, hurto_personas, by = "DIRECTORIO")
  
  datos5 <- inner_join(vivienda, inst, by = "DIRECTORIO")
  
  var_vivienda <- c("P5747", "P5752", "P5752S1", "P5090")
  
  var_personas <- c("P220", "P6210", "P1366", "P1402", 
                    "P1403", "P1365", "P1363") #No incluir Edad P5785
  
  var_seguridad_hogar <- c("P1398S2", "P1398S3", "P1398S4", "P1398S5", "P1398S6", 
                           "P1398S7", "P1398S8", "P1398S9", "P1398S10", "P1398S11", "P1590S1", 
                           "P1590S2", "P1590S3", "P1590S4", "P1590S5", "P520S1", "P520S2", 
                           "P520S3", "P520S4", "P520S5", "P520S6", "P520S7")
  
  var_hurto_residencias <- c("P1379", "P1390S1", 
                             "P1390S2", "P1390S3", "P1389", "P1385", "P1380S1", 
                             "P1380S2", "P1380S3", "P1380S5", "P1380S6", "P1367S1", "P1367S4", 
                             "P1367S2", "P1367S3", "P554")
  
  var_hurto_personas <- c("P1229", "P1338", "P1339S3", "P1339S5", "P1339S1", "P1339S2", "P580S1", "P580S2", "P580S3")
  
  var_inst <- c("P1181S1", 
                "P1181S2", "P1182S3", "P1771S1", "P1771S2")
  # desarrollo de la aplicación
  ui <- dashboardPage(
    dashboardHeader(title = "Seguridad Ciudadana"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Vivienda", tabName = "vivienda", icon = icon("users", lib = "font-awesome")),
        menuItem("Personas", tabName = "personas", icon = icon("user-cog", lib = "font-awesome")),
        menuItem("Percepción de Seguridad Hogar", tabName = "seguridad", icon = icon("user-lock", lib = "font-awesome")),
        menuItem("Hurto Residencias", tabName = "hresidencias", icon = icon("user-secret", lib = "font-awesome")),
        menuItem("Hurto Personas", tabName = "hpersonas", icon = icon("user-secret", lib = "font-awesome")),
        menuItem("Instituciones", tabName = "instituciones", icon = icon("check-double", lib = "font-awesome"))
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
                  tabPanel("Gráfico de Barras", fluidRow(box(plotOutput("tabla4", width = 500)), box(textOutput("text6")))),
                  tabPanel("Edad y Nivel Educativo", plotOutput("tabla5"))
                )
                ),
        
        tabItem(tabName = "seguridad",
                titlePanel("Percepción de seguridad en el hogar"),
                textOutput("text7"),
                selectInput(inputId = "var3",
                            label = "Seleccione la variable de acuerdo a los problemas que se pueda presentar en el barrio y en el hogar:",
                            choices = var_seguridad_hogar),
                tabsetPanel(
                  tabPanel("Tabla de Frencuencia", fluidRow(box(htmlOutput("tabla6")), box(textOutput("text8")))),
                  tabPanel("Gráfico de Barras", fluidRow(box(plotOutput("tabla7", width = 500)), box(textOutput("text9")))),
                  tabPanel("Tipo de vivienda", fluidRow(box(plotOutput("tabla8", width = 500))))
                )
              ),
        tabItem(tabName = "hresidencias",
                titlePanel("Hurto a residencias"),
                textOutput("text10"),
                selectInput(inputId = "var4",
                            label = "Seleccione la variable de acuerdo a los problemas de hurto de residencias:",
                            choices = var_hurto_residencias),
                tabsetPanel(
                  tabPanel("Tabla de Frecuencia", fluidRow(box(htmlOutput("tabla9")), box(textOutput("text11")))),
                  tabPanel("Gráfico de Barras", fluidRow(box(plotOutput("tabla10", width = 500)), box(textOutput("text12"))))
                )
              ),
        tabItem(tabName = "hpersonas",
                titlePanel("Hurto a personas"),
                textOutput("text13"),
                selectInput(inputId = "var5",
                            label = "Seleccione la variable de acuerdo a los problemas de hurto a personas:",
                            choices = var_hurto_personas),
                tabsetPanel(
                  tabPanel("Tabla de Frecuencia", fluidRow(box(htmlOutput("tabla11")), box(textOutput("text14")))),
                  tabPanel("Gráfico de Barras", fluidRow(box(plotOutput("tabla12", width = 500)), box(textOutput("text15"))))
                )
              ),
        tabItem(tabName = "instituciones",
                titlePanel("Percepción de seguridad sobre las instituciones"),
                textOutput("text16"),
                selectInput(inputId = "var6",
                            label = "Seleccione la variable de acuerdo aa la institución a analizar:",
                            choices = var_inst),
                tabsetPanel(
                  tabPanel("Tabla de Frecuencia", fluidRow(box(htmlOutput("tabla13")), box(textOutput("text17")))),
                  tabPanel("Gráfico de Barras", fluidRow(box(plotOutput("tabla14", width = 500)), box(textOutput("text18"))))
                )
              )
        
        
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
      "La ubicación e importancia de las ciudades permiten observar que en Bogotá y Medellín 2/3 de la población vive en apartamentos, y debido al número de habitantes en estas, esa tendencia se mantiene a nivel nacional.
Es más que evidente que el 99% de los hogares tienen servicio de energía eléctrica en sus hogares, sin importar estrato y condición social.
      Cerca del 70% de los hogares que tienen servicio permanente de energía eléctrica, pertenecen a estratos 1, 2 y 3.
      Sorprende que cerca del 80% de las familias encuestadas viven en hogar propio, en comparación con estadísticas cercanas al 50% con las demás ciudades.
      Esto está relacionado con que más de 50% de los barranquilleros pertenecen a Estrato 1.
      "
    })
    
    output$tabla2 <- renderPlot ({ggplot(data = datos1, aes(x = datos1[[input$var1]], fill  = factor(datos1[["DEPMUNI"]]), weights = datos1$FEX_C.x)) + 
        geom_bar() + labs(x = var_label(datos1[[input$var1]]),
                          y = "Población",
                           fill = "Ciudades principales", caption = "DANE") + scale_y_continuous(labels = comma) 
    })
    
    
    output$text3 <- renderText({
      "La ubicación e importancia de las ciudades permiten observar que en Bogotá y Medellín 2/3 de la población vive en apartamentos, y debido al número de habitantes en estas, esa tendencia se mantiene a nivel nacional.
Es más que evidente que el 99% de los hogares tienen servicio de energía eléctrica en sus hogares, sin importar estrato y condición social.
      Cerca del 70% de los hogares que tienen servicio permanente de energía eléctrica, pertenecen a estratos 1, 2 y 3.
      Sorprende que cerca del 80% de las familias encuestadas viven en hogar propio, en comparación con estadísticas cercanas al 50% con las demás ciudades.
      Esto está relacionado con que más de 50% de los barranquilleros pertenecen a Estrato 1.
      "
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
      "Al igual que las estadísticas a nivel mundial, con una ligera diferencia, hay más mujeres que hombres con una varianza no mayor al 3%, en todas las ciudades.
Bogotá puede ser denominada como la "más educada", pues el 40% de la población ha estudiado en una institución de educación superior. Las demás ciudades no se quedan atrás, pero es preocupante que el 60% o más cumplió máximo con el diploma de bachiller.
      Es mayor el número de personas solteras que de casadas, y la tendencia a no casarse y vivir en pareja por más de 2 años está en aumento, siendo en Bogotá inclusive más que las personas casadas.
      Los cambios de ciudad no son frecuentes en Colombia, razón por la que aproximadamente 1 de cada 10 personas cambian de ciudad en los últimos 10 años.
      Los ciudadanos no cambian de barrio de manera frecuente, pero la tendencia a que ocurra es ocasional, donde el 40% de los habitantes han cambiado de barrio en los últimos 10 años, pero en la misma ciudad.
      En todas las ciudades hay más personas trabajando que desempleadas o realizando otra actividad. Sin embargo, entre el 15% y 23% (Dependiendo de la ciudad) se dedican a oficio en el hogar.
      Generalmente, los ciudadanos trabajan en el día (80%-90%), y algunos completan ambas jornadas (10%-20%), y menos del 2% se dedican a jornadas nocturnas exclusivamente.
      "
    })
    
    output$text6 <- renderText({
      "Al igual que las estadísticas a nivel mundial, con una ligera diferencia, hay más mujeres que hombres con una varianza no mayor al 3%, en todas las ciudades.
Bogotá puede ser denominada como la "más educada", pues el 40% de la población ha estudiado en una institución de educación superior. Las demás ciudades no se quedan atrás, pero es preocupante que el 60% o más cumplió máximo con el diploma de bachiller.
      Es mayor el número de personas solteras que de casadas, y la tendencia a no casarse y vivir en pareja por más de 2 años está en aumento, siendo en Bogotá inclusive más que las personas casadas.
      Los cambios de ciudad no son frecuentes en Colombia, razón por la que aproximadamente 1 de cada 10 personas cambian de ciudad en los últimos 10 años.
      Los ciudadanos no cambian de barrio de manera frecuente, pero la tendencia a que ocurra es ocasional, donde el 40% de los habitantes han cambiado de barrio en los últimos 10 años, pero en la misma ciudad.
      En todas las ciudades hay más personas trabajando que desempleadas o realizando otra actividad. Sin embargo, entre el 15% y 23% (Dependiendo de la ciudad) se dedican a oficio en el hogar.
      Generalmente, los ciudadanos trabajan en el día (80%-90%), y algunos completan ambas jornadas (10%-20%), y menos del 2% se dedican a jornadas nocturnas exclusivamente.
      "
    })
    
    output$tabla5 <- renderPlot({ sjp.grpfrq(datos1$P5785, datos1$P6210, type = "box", title = "Relación entre la edad y el nivel educativo")
      
    })
    
    output$text7 <- renderText({
      "Contiene información sobre los principales problemas de seguridad que afectan a los barrios y las medidas de seguridad que toma el hogar para sentirse más seguro."
    })
    
    output$tabla6 <- renderUI({
      custom_table3 <- sjt.xtab(var.row = datos2[["DEPMUNI"]],
                                var.col = datos2[[input$var3]],
                                weight.by = datos2$FEX_C.x,
                                var.labels = c("Ciudad Principal",
                                               var_label(datos2[[input$var3]])),
                                show.row.prc = T, show.summary = F,
                                show.legend = F,
                                encoding = "UTF-8")
      HTML(custom_table3$knitr)
    })
    
    output$tabla7 <- renderPlot ({ggplot(data = datos2, aes(x = datos2[[input$var3]], fill  = factor(datos2[["DEPMUNI"]]), weights = datos2$FEX_C.x)) + 
        geom_bar() + labs(x = var_label(datos2[[input$var3]]),
                          y = "Población",
                          fill = "Ciudades principales", caption = "DANE") + scale_y_continuous(labels = comma) 
    })
    
    output$tabla8 <- renderPlot ({ggplot(data = datos2, aes(x = datos2$P5747, fill  = factor(datos2[[input$var3]]), weights = datos2$FEX_C.x)) + 
        geom_bar() + labs(x = "Tipo de Vivienda (1. Casa 2. Apto 3. Cuarto 4. Otra)",
                          y = "Población",
                          fill = "Problemas de inseguridad: (1) Sí, (2) No", caption = "DANE", title = var_label(datos2[[input$var3]])) + scale_y_continuous(labels = comma) 
    })
    
    output$text8 <- renderText({"Barranquilla es la ciudad donde ha presentado mayor hurto en resistencias. El hurto a comercio no se presenta con tanta frecuencia en las cinco ciudades. Bogotá es la ciudad donde ha presentado mayores casos de robos de vehículos. Por otro lado, Barranquilla es la ciudad que presenta mayores casos de homicidios así también como extorsiones. Una de las percepciones de inseguridad en el barrio y la ciudad que afecta la capital de Antioquia es la distribución de drogas. En Barranquilla, también, presenta casos de riñas y vandalismo en sus barrios. En Bogotá, una de las percepciones de inseguridad que influye en el barrio es por consumo de drogas, expendedores de drogas, petardos o granadas. En Barranquilla afectado por combos o pandillas. Bucaramanga es la ciudad donde mayor a presentado inseguridad en sus barrios por barras bravas. Por presencia de guerrilla, en las cinco ciudades del país no presenta este tipo de percepción de inseguridad de acuerdo con su barrio, sin embargo, Barranquilla es la que mayor presenta estos casos. Asimismo, las Bracim en Medellín. En las ciudades las menores medidas preventivas de seguridad son las cámaras, seguridad informal, las rejas y cambios en las viviendas, sin embargo, Barranquilla presenta más cambios de vivienda que el resto de las ciudades. Las mayores medidas preventivas son instaurar vigilancia privada, adquirir armas de fuego, adquirir algún seguro contra robo.
Por lo general, los crímenes ocurren en los tipos de vivienda casa o apartamento.
      "
      
    })
    
    output$text9 <- renderText({ "Barranquilla es la ciudad donde ha presentado mayor hurto en resistencias. El hurto a comercio no se presenta con tanta frecuencia en las cinco ciudades. Bogotá es la ciudad donde ha presentado mayores casos de robos de vehículos. Por otro lado, Barranquilla es la ciudad que presenta mayores casos de homicidios así también como extorsiones. Una de las percepciones de inseguridad en el barrio y la ciudad que afecta la capital de Antioquia es la distribución de drogas. En Barranquilla, también, presenta casos de riñas y vandalismo en sus barrios. En Bogotá, una de las percepciones de inseguridad que influye en el barrio es por consumo de drogas, expendedores de drogas, petardos o granadas. En Barranquilla afectado por combos o pandillas. Bucaramanga es la ciudad donde mayor a presentado inseguridad en sus barrios por barras bravas. Por presencia de guerrilla, en las cinco ciudades del país no presenta este tipo de percepción de inseguridad de acuerdo con su barrio, sin embargo, Barranquilla es la que mayor presenta estos casos. Asimismo, las Bracim en Medellín. En las ciudades las menores medidas preventivas de seguridad son las cámaras, seguridad informal, las rejas y cambios en las viviendas, sin embargo, Barranquilla presenta más cambios de vivienda que el resto de las ciudades. Las mayores medidas preventivas son instaurar vigilancia privada, adquirir armas de fuego, adquirir algún seguro contra robo.
Por lo general, los crímenes ocurren en los tipos de vivienda casa o apartamento.
      "
      
    })
    
    output$text10 <- renderText({
      "Contiene información sobre las circunstancias en las que ocurren los hurtos a residencia, las medidas de seguridad preventivas y posteriores que toman los hogares cuando han sufrido de hurto a las residencias, y el nivel de denuncia al respecto"
    })
    
    output$tabla9 <- renderUI({
      custom_table9 <- sjt.xtab(var.row = datos3[["DEPMUNI"]],
                                var.col = datos3[[input$var4]],
                                weight.by = datos3$FEX_C.x,
                                var.labels = c("Ciudad Principal",
                                               var_label(datos3[[input$var4]])),
                                show.row.prc = T, show.summary = F,
                                show.legend = F,
                                encoding = "UTF-8")
      HTML(custom_table9$knitr)
    })
    
    output$tabla10 <- renderPlot ({ggplot(data = datos3, aes(x = datos3[[input$var4]], fill  = factor(datos3[["DEPMUNI"]]), weights = datos3$FEX_C.x)) + 
        geom_bar() + labs(x = var_label(datos3[[input$var4]]),
                          y = "Población",
                          fill = "Ciudades principales", caption = "DANE") + scale_y_continuous(labels = comma) 
    })
    
    
    output$text11 <- renderText({"En los hurtos de residencias, la mayoría de los casos es por manipulación de la violencia, cerradura. Los hurtos han sido la mayor parte de las veces por uso personal. En Cali, los hurtos la mayoría han sido muebles. Pocas veces ha sido dinero. En muchos casos los hurtos no ocurren cuando las personas están en las residencias. En Barranquilla, el arma de fuego es lo usual al momento de robar en las residencias. En Medellín, pocos casos con fuerza física. También, en Barranquilla utilizan sustancias como la escopolamina para robar. Bogotá y Barranquilla son las ciudades con residencias con vigilancia privada. En Barranquilla, las residencias cuentan muchas veces con vigilancia informal y cámaras de vigilancia. En Bucaramanga, métodos como las cerraduras. Bogotá y Medellín son las ciudades donde más toman alguna medida de seguridad luego de algún hurto."
      
    })
    
    
    output$text12 <- renderText({"En los hurtos de residencias, la mayoría de los casos es por manipulación de la violencia, cerradura. Los hurtos han sido la mayor parte de las veces por uso personal. En Cali, los hurtos la mayoría han sido muebles. Pocas veces ha sido dinero. En muchos casos los hurtos no ocurren cuando las personas están en las residencias. En Barranquilla, el arma de fuego es lo usual al momento de robar en las residencias. En Medellín, pocos casos con fuerza física. También, en Barranquilla utilizan sustancias como la escopolamina para robar. Bogotá y Barranquilla son las ciudades con residencias con vigilancia privada. En Barranquilla, las residencias cuentan muchas veces con vigilancia informal y cámaras de vigilancia. En Bucaramanga, métodos como las cerraduras. Bogotá y Medellín son las ciudades donde más toman alguna medida de seguridad luego de algún hurto."
      
    })

    output$text13 <- renderText({
      "Contiene información sobre las circunstancias en las que ocurren los hurtos a personas, las variables que identifican las distintas situaciones que pueden darse durante el delito, y las consecuencias que se generan del hurto. Las preguntas se dirigen a la identificación de las variables que conduzcan a su prevención."
    })
    
    output$tabla11 <- renderUI({
      custom_table11 <- sjt.xtab(var.row = datos4[["DEPMUNI"]],
                                var.col = datos4[[input$var5]],
                                weight.by = datos4$FEX_C.x,
                                var.labels = c("Ciudad Principal",
                                               var_label(datos4[[input$var5]])),
                                show.row.prc = T, show.summary = F,
                                show.legend = F,
                                encoding = "UTF-8")
      HTML(custom_table11$knitr)
    })
    
    output$tabla12 <- renderPlot ({ggplot(data = datos4, aes(x = datos4[[input$var5]], fill  = factor(datos4[["DEPMUNI"]]), weights = datos4$FEX_C.x)) + 
        geom_bar() + labs(x = var_label(datos4[[input$var5]]),
                          y = "Población",
                          fill = "Ciudades principales", caption = "DANE") + scale_y_continuous(labels = comma) 
    })
    
    
    output$text14 <- renderText({"En las personas en las ciudades principales los hurtos han sido diversificados por engaño, raponazo y atraco. Por lo general, estos hechos ocurren en una calle o avenida. Los teléfonos celulares y los aparatos electrónicos presentan pocos casos, pero en las ciudades de Bogotá y Bucaramanga es donde más se presenta casos. El dinero, los artículos de uso personal es lo que más roban en las ciudades. En el robo de estos artículos, en Bogotá, utilizan armas de fuego en muchos casos. Lo que menos se presenta, armas blancas. En la mayoría de las ciudades, también usan palos, piedras."
      
      
    })
    
    
    output$text15 <- renderText({"En las personas en las ciudades principales los hurtos han sido diversificados por engaño, raponazo y atraco. Por lo general, estos hechos ocurren en una calle o avenida. Los teléfonos celulares y los aparatos electrónicos presentan pocos casos, pero en las ciudades de Bogotá y Bucaramanga es donde más se presenta casos. El dinero, los artículos de uso personal es lo que más roban en las ciudades. En el robo de estos artículos, en Bogotá, utilizan armas de fuego en muchos casos. Lo que menos se presenta, armas blancas. En la mayoría de las ciudades, también usan palos, piedras."
      
    })
    
    output$text16 <- renderText({
     "Contiene información sobre la percepción de las personas acerca del aporte que tienen diferentes actores a su protección y seguridad, así como a la seguridad de la ciudad.
  "
    })
    
    output$tabla13 <- renderUI({
      custom_table13 <- sjt.xtab(var.row = datos5[["DEPMUNI"]],
                                 var.col = datos5[[input$var6]],
                                 weight.by = datos5$FEX_C.x,
                                 var.labels = c("Ciudad Principal",
                                                var_label(datos5[[input$var6]])),
                                 show.row.prc = T, show.summary = F,
                                 show.legend = F,
                                 encoding = "UTF-8")
      HTML(custom_table13$knitr)
    })
    
    output$tabla14 <- renderPlot ({ggplot(data = datos5, aes(x = datos5[[input$var6]], fill  = factor(datos5[["DEPMUNI"]]), weights = datos5$FEX_C.x)) + 
        geom_bar() + labs(x = var_label(datos5[[input$var6]]),
                          y = "Población",
                          fill = "Ciudades principales", caption = "DANE") + scale_y_continuous(labels = comma) 
    })
    
    
    output$text17 <- renderText({"En las instituciones, la mayoría de las ciudades se siente seguro con los policías, en especial, Bogotá. Con las fuerzas militares, Medellín, siente más confianza en la seguridad. En las ciudades, las personas confían algo, poco o nada en la Alcaldía, Fiscalía y Jueces."
      
    })
    
    
    output$text18 <- renderText({"En las instituciones, la mayoría de las ciudades se siente seguro con los policías, en especial, Bogotá. Con las fuerzas militares, Medellín, siente más confianza en la seguridad. En las ciudades, las personas confían algo, poco o nada en la Alcaldía, Fiscalía y Jueces."
      
    })
  }  
  
  shinyApp(ui = ui, server = server)
  