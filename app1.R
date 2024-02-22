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

server <- function(input, output) {
  
  autoInvalidate <- reactiveTimer(10000)
    
    #url <- 'https://raw.githubusercontent.com/AndresG25/Tesis_Maestria/main/data1.csv'

  latestData <- reactive({
    autoInvalidate() # Actualiza cada 10 segundos
    tryCatch({
      # Assuming `db` is your database connection object
      db <- dbConnect(MySQL(), user = "root", password = "", dbname = "estacion", host = "127.0.0.1")
      latest_row <- dbGetQuery(db, "SELECT * FROM dataestacion ORDER BY Fecha DESC LIMIT 1")
      dbDisconnect(db)
      latest_row <- latest_row %>% mutate(Presion = as.numeric(Presion), PresionPSI = Presion/6895) %>% mutate(PresionPSI = as.character(PresionPSI))
      latest_row
    }, error = function(e) {
      # Handle error, maybe return NA or a default value
      return(data.frame(Fecha = NA, RPM = NA, Vviento = NA, Temperatura = NA, Presion = NA, Altitud = NA))
    })
  })
    
  output$fecha_hora <- renderText({
    data <- latestData()
    if (is.na(data$Fecha[1])) {
      return("Última Actualización: No disponible")
    } else {
      formatted_fecha_hora <- format(as.POSIXct(data$Fecha, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d %H:%M:%S")
      paste("Última Actualización:", formatted_fecha_hora)
    }
  })
  
  output$RPMane <- renderValueBox({
    data <- latestData()
    if (is.na(data$RPM[1])) {
      valueBox(value = "No disponible", subtitle = "RPM Anemometro", color = "green", icon = icon("compass"))
    } else {
      valueBox(value = data$RPM, subtitle = "RPM Anemometro", color = "green", icon = icon("compass"))
    }
  })
  
  output$Vviento <- renderValueBox({
    data <- latestData()
    if (is.na(data$Vviento[1])) {
      valueBox(value = "No disponible", subtitle = "Vel. Viento m/s", color = "green", icon = icon("wind"))
    } else {
      valueBox(value = data$Vviento, subtitle = "Vel. Viento m/s", color = "green", icon = icon("wind"))
    }
  })
    
  output$Temp <- renderValueBox({
    data <- latestData()
    if (is.na(data$Temperatura[1])) {
      valueBox(value = "No disponible", subtitle = "Temperatura °C", icon = icon("temperature-high"), color = "orange")
    } else {
      valueBox(value = data$Temperatura, subtitle = "Temperatura °C", icon = icon("temperature-high"), color = "orange")
    }
  })

  output$Pres <- renderValueBox({
    data <- latestData()
    if (is.na(data$PresionPSI[1])) {
      valueBox(value = "No disponible", subtitle = "Presión Atmos. PSI", icon = icon("dashboard"), color = "orange")
    } else {
      valueBox(value = data$PresionPSI, subtitle = "Presión Atmos. PSI", icon = icon("dashboard"), color = "orange")
    }
  }) 
  
  output$Altitud <- renderValueBox({
    data <- latestData()
    if (is.na(data$Altitud[1])) {
      valueBox(value = "No disponible", subtitle = "Altitud m.s.n.m", color = "orange", icon = icon("cloud"))
    } else {
      valueBox(value = data$Altitud, subtitle = "Altitud m.s.n.m", color = "orange", icon = icon("cloud"))
    }
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
    
    shinyjs::useShinyjs(),
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
                box(plotlyOutput("temperatura"), height = 250)
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
