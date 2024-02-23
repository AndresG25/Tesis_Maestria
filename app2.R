#
# This is a Shiny web application for the analysis of wind system turbines
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(dashboardthemes)
library(RMySQL)
library(DBI)


  
  
  # URL del archivo CSV
  url_csv <- 'https://raw.githubusercontent.com/AndresG25/Tesis_Maestria/main/data1.csv'
  
  server <- function(input, output, session) {
    
    # Función reactiva para leer los datos del CSV
    readData <- reactive({
      invalidateLater(10000, session)  # Actualiza los datos cada 10 segundos
      datos <- read_csv(url_csv, show_col_types = FALSE)
      
      # Asegurar que las columnas están en el formato correcto
      datos <- datos %>%
        mutate(
          Fecha = as.POSIXct(Fecha, format = "%Y-%m-%d %H:%M:%S"),
          Temperatura = as.numeric(Temperatura),
          Vviento = as.numeric(Vviento),
          Presion = as.numeric(Presion) / 6895,  # Convertir Presión a PSI
          RPM = as.numeric(RPM),
          Altitud = as.numeric(Altitud)
        )
      datos
    })
    
    # Ejemplo de adaptación para una gráfica de temperatura
    output$temperatureGraph <- renderPlotly({
      datos <- readData()
      datos_agregados <- datos %>%
        filter(Fecha >= Sys.time() - minutes(30)) %>%
        mutate(minute = floor_date(Fecha, "minute")) %>%
        group_by(minute) %>%
        summarise(AvgTemp = mean(Temperatura, na.rm = TRUE)) %>%
        arrange(minute)
      
      plot_ly(datos_agregados, x = ~minute, y = ~AvgTemp, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = 'Temperatura promedio cada 1 minuto (Últimos 30 Minutos)',
               xaxis = list(title = 'Tiempo'),
               yaxis = list(title = 'Temperatura (°C)'))
    })
    
    # Adaptar este enfoque para las demás gráficas y visualizaciones necesarias
  }
  
  ui <- dashboardPage(
    dashboardHeader(title = "Sistema de Datos - Generador Eólico Axial"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Estadísticas Generales", tabName = "Estadisticas", icon = icon("chart-line")),
        menuItem("Temperatura", tabName = "Temp", icon = icon("temperature-high"))
        # Incluir otros menuItem según sea necesario
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Estadisticas", h2("Estadísticas Generales del Proceso")),
        tabItem(tabName = "Temp", h2("Temperatura"),
                fluidRow(plotlyOutput("temperatureGraph"))
                # Incluir otras gráficas y visualizaciones según sea necesario
        )
      )
    )
  )

# Run the application 
shinyApp(ui = ui, server = server)
