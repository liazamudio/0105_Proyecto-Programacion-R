## app.R ##

## Dashboard para el data set 'mtcars'

library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)

########## Parte corresponsdiente al UI (ui.R)
ui <- 
  fluidPage(
    dashboardPage(
      dashboardHeader(title = "Análisis de FIBRAS"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Precios en el mercado", tabName = "data_table", icon = icon("table")),
          menuItem("Comparativo a 5 años", tabName = "to5year", icon = icon("area-chart")),
          menuItem("Comparativo a 2 años", tabName = "to2year", icon = icon("area-chart")),
          menuItem("Comparativo 5 vs 2 años", tabName = "com52y", icon = icon("area-chart")),
          menuItem("Medidas de tendencia de las FIBRAS", tabName = "medtend_table", icon = icon("table")),
          # menuItem("Comparativo", tabName = "graph_fibras", icon = icon("area-chart")),
          menuItem("Resumen de las FIBRAS", tabName = "sumary_tables", icon = icon("table"))
          # menuItem("Histograma", tabName = "Dashboard", icon = icon("dashboard")),
          # menuItem("Dispersión", tabName = "graph", icon = icon("area-chart")),
          # menuItem("Imágen", tabName = "img", icon = icon("file"))
        )
      ),
      
      dashboardBody(
        tabItems(
          # Tabla general de Precios en el mercado
          tabItem(tabName = "data_table",
                  fluidRow(        
                    titlePanel(h3("Precios de las FIBRAS en el mercado")),
                    dataTableOutput("data_table"),
                  )
          ),

          # Comparativo a 5 años
          tabItem(tabName = "to5year", 
                  fluidRow(
                    titlePanel(h3("Comparación de precios de FIBRAS de los últimos 5 años")),
                    # dataTableOutput("res5y_table")
                  ),
                  fluidRow(box(plotOutput("tn_por_var_5a_plot", height = 600, width = 400))
                  ),
                  fluidRow(box(plotOutput("ren_prom_men_5a_plot", height = 600, width = 400))
                  ),
                  fluidRow(box(plotOutput("t_rie_fib_5as_plot", height = 600, width = 400))
                  ) 
          ),
          
          # Comparativo a 2 años
          tabItem(tabName = "to2year", 
                  fluidRow(
                    titlePanel(h3("Comparación de precios de FIBRAS de los últimos 2 años")),
                    # dataTableOutput("res5y_table"),
                  )
          ),
          
          # Comparativo de desempeños de 5 y 2 años
          tabItem(tabName = "com52y", 
                  fluidRow(
                    titlePanel(h3("Comparativo de desempeños de 5 y 2 años")),
                    # dataTableOutput("res5y_table"),
                  )
          ),
          
          # medidas de tendencia de las FIBRAS
          tabItem(tabName = "medtend_table", 
                  fluidRow(
                    titlePanel(h3("Medidas de tendencia de las FIBRAS de los últimos 5 años")),
                    dataTableOutput("medtend_table"),
                  )
          ),
          
          # Evaluación del precio de una FIBRA vs el tiempo u otra FIBRA
          # tabItem(tabName = "graph_fibras", 
          #         fluidRow(
          #          titlePanel(h3("Gráficos de dispersión")),
          #          selectInput("a", "Selecciona el valor de x",
          #                      choices = names(fibras_ult_5a)),
          #          selectInput("y", "Seleccione el valor de y",
          #                      choices = names(fibras_ult_5a)),
          #          box(plotOutput("graph_fibras_plot", height = 300, width = 460) )
          #        )
          #),
          
          
          
          
          
          # Resumen de las FIBRAS
          tabItem(tabName = "sumary_tables", 
                  fluidRow(
                    titlePanel(h3("Resumen de las FIBRAS de los últimos 5 años")), verbatimTextOutput("sumary_tables"))     
          ),
          
          
          
          
          
          
          
          
          # Histograma
          tabItem(tabName = "Dashboard",
                  fluidRow(
                    titlePanel("Histograma de las variables del data set mtcars"), 
                    selectInput("x", "Seleccione el valor de X",
                                choices = names(mtcars)),
                    
                    selectInput("zz", "Selecciona la variable del grid", 
                                
                                choices = c("cyl", "vs", "gear", "carb")),
                    box(plotOutput("plot1", height = 250)),
                    
                    box(
                      title = "Controls",
                      sliderInput("bins", "Number of observations:", 1, 30, 15)
                    )
                  )
          ),
          
          # Dispersión
          tabItem(tabName = "graph", 
                  fluidRow(
                    titlePanel(h3("Gráficos de dispersión")),
                    selectInput("a", "Selecciona el valor de x",
                                choices = names(mtcars)),
                    selectInput("y", "Seleccione el valor de y",
                                choices = names(mtcars)),
                    selectInput("z", "Selecciona la variable del grid", 
                                choices = c("cyl", "vs", "gear", "carb")),
                    box(plotOutput("output_plot", height = 300, width = 460) )
                    
                  )
          ),
          
          tabItem(tabName = "img",
                  fluidRow(
                    titlePanel(h3("Imágen de calor para la correlación de las variables")),
                    img( src = "cor_mtcars.png", 
                         height = 350, width = 350)
                  )
          )
          
        )
      )
    )
)


########## Parte correspondinete al server

server <- function(input, output) {                                             # Definición de la lógica del servidor
  
  ########## Importación de librerías ##########
  library(ggplot2)
  library(dplyr)
  library(corrplot)
  library(reshape2)
  library(quantmod)

  ########## Importación de datos ##########
  setwd("d:/Alex/OneDrive/devs/github/portfolio/proyectos/15-prog-and-stat-r/proyecto-prog-r_v0.4/dataset/")
  fibras2014a2024 <- read.csv("fibras2014-2024.csv")                            # Leemos el dataset que vamos a utilizar
  ult_registro <- nrow(fibras2014a2024)                                         # Determinamos los limites del nuevo dataframe.
  nvo_prim_reg <- ult_registro-60                                               # 60 es el numero total de meses considerado, 12 repartidos en 5 años.
  fibras_ult_5a <- fibras2014a2024[nvo_prim_reg:ult_registro, ]                 # Creación del nuevo dataframe con los limites establecidos
  fibras_ult_5a                                                                 # Dataframe general con fechas
  fibras_mayor_5a_sf <- fibras_ult_5a %>% select(-Fecha)                        # Eliminamos la columna de fecha
  fibras_mayor_5a_sf                                                          # dataframe general sin fechas
  
  ########## Análisis de 5 años ##########
  
  ##### Rendimiento total
  var_5a <- sapply(fibras_mayor_5a_sf, function(col) col[length(col)] - col[1]) # Variación de precios entre el primer y el ultimo registro
  por_var_5a <- round((var_5a / fibras_mayor_5a_sf[1,] * 100), digits = 2)      # Determinamos el porcentaje de variación
  t_por_var_5a <- as.data.frame(t(por_var_5a))                                  # Trasponemos el dataframe generado
  t_por_var_5a$ticker <- row.names(t_por_var_5a)                                # Copiamos el índice existente (nombre del ticker) a una columna adicional
  row_index <- row(t_por_var_5a)                                                # Generamos un indice con el numero de renglón
  tn_por_var_5a <- cbind(row_index, t_por_var_5a)                               # Combinamos el dataframe de los numeros de indice y el de los porcentajes
  rownames(tn_por_var_5a) <- tn_por_var_5a$"1"                                  # Agisnamos los valores de la columna "1" como indice
  tn_por_var_5a <- subset(tn_por_var_5a, select = -c(1, 2))                     # Eliminamos las columnas innecesarias
  tn_por_var_5a <- tn_por_var_5a[, c("ticker", setdiff(names(tn_por_var_5a), "ticker"))]   # Movemos la columna de ticker a la primera posición
  names(tn_por_var_5a)[names(tn_por_var_5a) == "61"] <- "cinco"                 # Renombramos la columna de los resultados
  # tn_por_var_5a                                                               # Tabla de rendimientos totales en 5 años
  
  ##### Rendimiento promedio mensual
  ren_prom_men_5a <- tn_por_var_5a
  ren_prom_men_5a$cinco <- round((((1 + (ren_prom_men_5a$cinco/100))^(1/60) - 1)*100), digits = 4)   # Calculamos el rendimiento promedio mensual
  # ren_prom_men_5a                                                             # Tabla de rendimiento mensual promedio
  
  ##### Riesgo promedio mensual
  rie_fib_5a <- round((apply(fibras_mayor_5a_sf, 2, sd)), digits = 4)           # Obtenemos la desviación estandar(riesgo) de cada columna (FIBRA)
  rie_fib_5a = as.data.frame(rie_fib_5a)                                        # Lo convertimos a un dataframe
  t_rie_fib_5as = rie_fib_5a                                                    # Dejar como comentario cuando no se requiera
  t_rie_fib_5as$ticker <- row.names(t_rie_fib_5as)                              # Copiamos el índice existente (nombre del ticker) a una columna adicional
  row_index_trie5a <- row(t_rie_fib_5as)                                        # Generamos un indice con el numero de renglón
  t_rie_fib_5as <- cbind(row_index_trie5a, t_rie_fib_5as)                       # Combinamos el dataframe de los numeros de indice y el de los porcentajes
  rownames(t_rie_fib_5as) <- t_rie_fib_5as$"1"                                  # Asignamos los valores de la columna "1" como indice
  t_rie_fib_5as <- subset(t_rie_fib_5as, select = -c(1, 2))                     # Eliminamos las columnas innecesarias
  t_rie_fib_5as <- t_rie_fib_5as[, c("ticker", setdiff(names(t_rie_fib_5as), "ticker"))]    # Movemos la columna de ticker a la primera posición
  colnames(t_rie_fib_5as) <- c('ticker', 'cinco')                               # Renombramos las columnas
  # t_rie_fib_5as                                                               # Tabla del riesgo mensual promedio
  
  ##### Unión de los dataframes
  analyt_5a <- merge(ren_prom_men_5a, t_rie_fib_5as, by ='ticker')              # Unión de dataframes
  colnames(analyt_5a) <- c('Ticker', 'Rendimiento', 'Riesgo')                   # Renombramos las columnas
  # analyt_5a                                                                   # tabla resumen de 5 años
 
  ########## Análisis de 2 años ##########
  
  # Pendiente
  
  ########## Comparación entre 5 y 2 años ##########
  
  # Pendiente
  
  ########## Medidas de tendencia ##########
  
  # Costo promedio
  cost_prom_5a <- round((colMeans(fibras_mayor_5a_sf)), digits = 4)    
  # Mediana en el costo
  mediana_cost_5a <- sapply(fibras_mayor_5a_sf, function(col) if(is.numeric(col)) median(col, na.rm = TRUE) else NA)  
  
  # La moda en los costos de los tickers - Trasponer después
  # Función para calcular la moda
  moda <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  moda_cost_5a <- sapply(fibras_mayor_5a_sf, moda)                              # Aplicamos la función de la moda
  mtc_fibras <- rbind(cost_prom_5a, mediana_cost_5a, moda_cost_5a)              # Concatenamos las tres medidas
  t_mtc_fibras <- as.data.frame(t(mtc_fibras))                                  # Trasponemos el dataframe generado
  t_mtc_fibras$ticker <- row.names(t_mtc_fibras)                                # Copiamos el índice existente (nombre del ticker) a una columna adicional
  row_index_tmtc <- row(t_mtc_fibras)                                           # Generamos un indice con el numero de renglón
  t_mtc_fibras <- cbind(row_index_tmtc, t_mtc_fibras)                           # Combinamos el dataframe de los numeros de indice y el de los porcentajes
  rownames(t_mtc_fibras) <- t_mtc_fibras$"1"                                    # Agisnamos los valores de la columna "1" como indice
  t_mtc_fibras <- subset(t_mtc_fibras, select = -c(1, 2, 3, 4))                 # Eliminamos las columnas innecesarias
  t_mtc_fibras <- t_mtc_fibras[, c("ticker", setdiff(names(t_mtc_fibras), "ticker"))]   # Movemos la columna de ticker a la primera posición
  colnames(t_mtc_fibras) <- c('Ticker', 'Costo promedio', 'Mediana', 'Moda')    # Renombramos las columnas
  # t_mtc_fibras                                                                # tabla de las medidas de tendencia de las FIBRAS en 5 años
  

  
  
  
  
  
   
  # Gráficos PRECIOS EN EL MERCADO
    output$data_table <- renderDataTable( {fibras_ult_5a}, options = list(aLengthMenu = c(5,25,50), iDisplayLength = 5))

  # Graficos COMPARATIVOS A 5 AÑOS
    
    # Resumen de 5 años
    output$res5y_table <- renderDataTable( {analyt_5a}, options = list(aLengthMenu = c(5,25,50), iDisplayLength = 5))
    
    # Gráfico de rendimientos totales 
    output$tn_por_var_5a_plot <- renderPlot({
      ggplot(tn_por_var_5a, aes(x = cinco, y = ticker)) +                               
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(title = "Rendimiento acumulados de las FIBRAS en 5 años",
             x = "Rendimiento acumulado",
             y = "Ticker") +
        theme_minimal()
    })  
    # Gráfico de rendimientos mensuales promedio 
    output$ren_prom_men_5a_plot <- renderPlot({
      ggplot(ren_prom_men_5a, aes(x = cinco, y = ticker)) +                               
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(title = "Rendimiento promedio mensual de las FIBRAS en 5 años",
             x = "Rendimiento promedio mensual",
             y = "Ticker") +
        theme_minimal()
    })
    # Gráfico de riesgos mensuales promedio 
    output$t_rie_fib_5as_plot <- renderPlot({
      ggplot(t_rie_fib_5as, aes(x = cinco, y = ticker)) +                               
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        labs(title = "Riesgo promedio mensual de las FIBRAS en 5 años",
             x = "Riesgo promedio mensual",
             y = "Ticker") +
        theme_minimal()
    })

  # Gráficos PRECIOS EN EL MERCADO
    
    # Tabla de medidas de tendencia
    output$medtend_table <- renderDataTable( {t_mtc_fibras}, options = list(aLengthMenu = c(5,25,50), iDisplayLength = 5))
    
  # Graficos de las tablas de resumen
    output$sumary_tables <- renderPrint( {summary(fibras_mayor_5a_sf)})

  # Gráficos comparativo FIBRAS
  #  output$"graph_fibras_plot" <- renderPlot({ 
  #    ggplot(fibras_ult_5a, aes(x =  fibras_ult_5a[,input$a] , y = fibras_ult_5a[,input$y])) +                          # Parametroa evaluar
  #      geom_line() +                                       # Línea de gráfico
  #      geom_point()                                        # Tipo de geometría,
  #      theme_linedraw() +                                      # Temas (inteta cambiarlo)
  #        facet_wrap("period")                              # Lo divide por el núm de cilindros    
  #      ylab(input$y) +
  #      xlab(input$x) +
  #      theme_minimal()
  #  })
         
    
    
    
    # fibras_mayor_5a
    # graph_fibras
    
    
       
    
  #Gráfico de Histograma
  output$plot1 <- renderPlot({
    
    x <- mtcars[,input$x]
    bin <- seq(min(x), max(x), length.out = input$bins + 1)
    
    ggplot(mtcars, aes(x, fill = mtcars[,input$zz])) + 
      geom_histogram( breaks = bin) +
      labs( xlim = c(0, max(x))) + 
      theme_light() + 
      xlab(input$x) + ylab("Frecuencia") + 
      facet_grid(input$zz)
    
    
  })
  
  # Gráficas de dispersión
  output$output_plot <- renderPlot({ 
    
    ggplot(mtcars, aes(x =  mtcars[,input$a] , y = mtcars[,input$y], 
                       colour = mtcars[,input$z] )) + 
      geom_point() +
      ylab(input$y) +
      xlab(input$x) + 
      theme_linedraw() + 
      facet_grid(input$z)  #selección del grid
    
  })   
  

  
}


shinyApp(ui, server)