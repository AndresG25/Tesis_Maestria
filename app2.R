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

server <- function(input, output, session) {
  
  
  url_csv <- 'https://raw.githubusercontent.com/AndresG25/Tesis_Maestria/main/data1.csv'
  autoInvalidate <- reactiveTimer(10000)
  
  
  # Actualiza el output de fecha y hora usando los datos leídos
  output$fecha_hora <- renderText({
    data <- latestData()
    if (is.na(data$Fecha[1])) {
      return("Última Actualización: No disponible")
    } else {
      formatted_fecha_hora <- format(max(data$Fecha), "%Y-%m-%d %H:%M:%S")
      paste("Última Actualización:", formatted_fecha_hora)
    }
  })
  
  readData <- reactive({
    autoInvalidate()
    datos <- read_csv(url_csv)
    datos$Fecha <- as.POSIXct(datos$Fecha, format = "%Y-%m-%d %H:%M:%S")
    datos
  })
  
  # Ejemplo de adaptación para una gráfica: Temperatura promedio cada 1 minuto (Últimos 30 Minutos)
  output$temperatureGraph <- renderPlotly({
    datos <- readData()
    datos_agregados <- datos %>%
      filter(Fecha >= Sys.time() - minutes(30)) %>%
      mutate(minute = floor_date(Fecha, "minute")) %>%
      group_by(minute) %>%
      summarise(AvgTemp = mean(Temperatura, na.rm = TRUE)) %>%
      arrange(desc(minute))
    
    plot_ly(datos_agregados, x = ~minute, y = ~AvgTemp, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = 'Temperatura promedio cada 1 minuto (Últimos 30 Minutos)',
             xaxis = list(title = 'Tiempo'),
             yaxis = list(title = 'Temperatura (°C)'))
  })

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
              h2("Estadísticas Generales del Proceso")),

      
      tabItem(tabName = "RPMa",
              h2("Estadísticas Generales del Proceso")),

      
      tabItem(tabName = "Temp",
              h2("Estadísticas Generales del Proceso"),
              fluidRow(
                plotlyOutput("temperatureGraph")
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
