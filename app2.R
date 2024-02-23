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
library(readr)
library(httr)
library(jsonlite)
library(base64enc)
  

server <- function(input, output, session) {
  # Define la URL de la API de GitHub para tu archivo CSV
  url_api <- "https://api.github.com/repos/AndresG25/Tesis_Maestria/contents/data1.csv"
  
  # Función para leer y procesar datos del CSV a través de la API de GitHub
  processData <- reactive({
    invalidateLater(20000, session)  # Refrescar cada 20 segundos
    
    # Hacer la solicitud a la API de GitHub
    response <- GET(url_api)
    
    # Verificar que la solicitud fue exitosa
    if (status_code(response) == 200) {
      # Extraer la URL de descarga del contenido del archivo
      content_data <- fromJSON(rawToChar(response$content))
      url_csv_raw <- content_data$download_url
      
      # Leer el CSV directamente en R
      datos <- read_csv(url_csv_raw, show_col_types = FALSE) %>%
        mutate(Fecha = as.POSIXct(Fecha, format = "%Y-%m-%d %H:%M:%S"),
               PresionPSI = Presion / 6895,  # Convertir presión a PSI
               Temperatura = as.numeric(Temperatura),
               Vviento = as.numeric(Vviento),
               RPM = as.numeric(RPM),
               Altitud = as.numeric(Altitud))
    } else {
      stop("Error al acceder a los datos a través de la API de GitHub")
    }
    
    return(datos)
  })
  
  # Las salidas se adaptan para utilizar los datos procesados de la misma manera que antes
  output$fecha_hora <- renderText({
    datos <- processData()
    if (nrow(datos) > 0) {
      max_fecha <- max(datos$Fecha, na.rm = TRUE)
      paste("Última Actualización:", format(max_fecha, "%Y-%m-%d %H:%M:%S"))
    } else {
      "Datos no disponibles"
    }
  })
  
  # Continúa con las adaptaciones para ValueBoxes y las gráficas como antes
  # ...
}
  
  ui <- dashboardPage(
    dashboardHeader(title = "Sistema de Datos - Generador Eólico Axial",
                    titleWidth = 450),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Estadísticas Generales", tabName = "Estadisticas", icon = icon("chart-line")),
        menuItem("Voltaje L1", tabName = "VL1", icon = icon("car-battery")),
        menuItem("Voltaje L2", tabName = "VL2", icon = icon("car-battery")),
        menuItem("Voltaje L3", tabName = "VL3", icon = icon("car-battery")),
        menuItem("Corriente L1", tabName = "IL1", icon = icon("bolt")),
        menuItem("Corriente L2", tabName = "IL2", icon = icon("bolt")),
        menuItem("Corriente L3", tabName = "IL3", icon = icon("bolt")),
        menuItem("Velocidad del Viento (m/s)", tabName = "Viento", icon = icon("wind")),
        menuItem("RPM Anemometro", tabName = "RPMa", icon = icon("compass")),
        menuItem("Temperatura", tabName = "Temp", icon = icon("temperature-high")),
        menuItem("Presión Atmosferica", tabName = "Pres", icon = icon("dashboard")),
        menuItem("Altitud", tabName = "Alt", icon = icon("cloud")),
        menuItem("Github", tabName = "Github", icon = icon("github")),
        menuItem("Información", tabName = "Info", icon = icon("info"))
      )
    ),
    dashboardBody(
      
      #shinyjs::useShinyjs(),
      shinyDashboardThemes(
        theme = "blue_gradient"),
      # Boxes need to be put in a row (or column)
      tabItems(
        tabItem(tabName = "Estadisticas",
                h2("Estadísticas Generales del Proceso"),
                verbatimTextOutput("fecha_hora"),
                fluidRow(
                  valueBox(15*2, "Voltaje Fase 1", icon = icon("car-battery"), color = "yellow"),
                  valueBox(15*2, "Voltaje Fase 2", icon = icon("car-battery"), color = "yellow"),
                  valueBox(15*2, "Voltaje Fase 3", icon = icon("car-battery"), color = "yellow"),
                  valueBox(10, "Corriente Fase 1", icon = icon("bolt"), color = "red"),
                  valueBox(10, "Corriente Fase 2", icon = icon("bolt"), color = "red"),
                  valueBox(10, "Corriente Fase 3", icon = icon("bolt"), color = "red"),
                  valueBoxOutput("RPMane", width = 4),
                  valueBoxOutput("Vviento", width = 4),
                  valueBox(15*2, "RPM Rotor", icon = icon("compass"), color = "green"),
                  valueBoxOutput("Temp", width =4),
                  valueBoxOutput("Pres", width = 4),
                  valueBoxOutput("Altitud", width = 4))
        ),
        
        tabItem(tabName = "VL1",
                h2("Estadísticas Generales del Proceso")),
        
        tabItem(tabName = "VL2",
                h2("Estadísticas Generales del Proceso")),
        
        tabItem(tabName = "VL3",
                h2("Estadísticas Generales del Proceso")),
        
        tabItem(tabName = "IL1",
                h2("Estadísticas Generales del Proceso")),
        
        tabItem(tabName = "IL2",
                h2("Estadísticas Generales del Proceso")), 
        
        tabItem(tabName = "IL3",
                h2("Estadísticas Generales del Proceso")),
        
        tabItem(tabName = "Viento",
                h2("Estadísticas Generales del Proceso"),
                fluidRow(
                  plotlyOutput("windSpeedGraph1Minute"),
                  plotlyOutput("windSpeedGraphHourly"), 
                  plotlyOutput("windSpeedGraphDaily")
                )),
        
        
        tabItem(tabName = "RPMa",
                h2("Estadísticas Generales del Proceso")),
        
        
        tabItem(tabName = "Temp",
                h2("Estadísticas Generales del Proceso"),
                fluidRow(
                  plotlyOutput("temperatureGraph"),
                  plotlyOutput("temperatureGraphHourly"),
                  plotlyOutput("temperatureGraphDaily"), 
                  plotlyOutput("temperatureGraphMonthly")
                )), 
        
        tabItem(tabName = "Pres",
                h2("Estadísticas Generales del Proceso"), 
                fluidRow(
                  box(plotlyOutput("presion"), height = 250)
                )), 
        
        tabItem(tabName = "Alt",
                h2("Estadísticas Generales del Proceso")), 
        
        tabItem(tabName = "Github",
                h2("Estadísticas Generales del Proceso")), 
        
        tabItem(tabName = "Info",
                h2("Estadísticas Generales del Proceso"))
        
      )
      
      
    )
  )
  
  # Run the application 
  shinyApp(ui = ui, server = server)