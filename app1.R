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
  
  
  db <- dbConnect(MySQL(), user = "root", password = "", dbname = "estacion", host = "127.0.0.1")
  autoInvalidateprincipal <- reactiveTimer(3000)
    #url <- 'https://raw.githubusercontent.com/AndresG25/Tesis_Maestria/main/data1.csv'

  latestData <- reactive({
    autoInvalidateprincipal() # Actualiza cada 10 segundos
    tryCatch({
      # Assuming `db` is your database connection object
      latest_row <- dbGetQuery(db, "SELECT * FROM dataestacion ORDER BY Fecha DESC LIMIT 1")
      latest_row <- latest_row %>% mutate(Presion = as.numeric(Presion), PresionPSI = Presion/6895) 
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
    if (is.na(data$Presion[1])) {
      valueBox(value = "No disponible", subtitle = "Presión Atmos. Pa", icon = icon("dashboard"), color = "orange")
    } else {
      valueBox(value = round(data$Presion), subtitle = "Presión Atmos. Pa", icon = icon("dashboard"), color = "orange")
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
  
  output$temperatureGraph <- renderPlotly({
    # Invalidate this reactive expression every 60 seconds to refresh the data
    invalidateLater(60000, session)
    
    # SQL query to fetch and aggregate temperature data
    query <- "
      SELECT
          DATE_FORMAT(Fecha, '%Y-%m-%d %H:%i:00') AS TimeGroup,
          AVG(Temperatura) AS AvgTemp
      FROM
          dataestacion
      WHERE
          Fecha >= NOW() - INTERVAL 30 MINUTE
      GROUP BY
          TimeGroup
      ORDER BY
          TimeGroup DESC;
    "
    
    # Execute the query
    data <- dbGetQuery(db, query)
    
    # Convert TimeGroup to POSIXct for plotting
    data$TimeGroup <- as.POSIXct(data$TimeGroup, format = "%Y-%m-%d %H:%M:%S")
    
    # Plot the data using Plotly
    plot_ly(data, x = ~TimeGroup, y = ~AvgTemp, type = 'scatter', mode = 'lines+markers', marker = list(color = 'red')) %>%
      layout(title = 'Temperatura promedio cada 1 minuto (Últimos 30 Minutos)',
             xaxis = list(title = 'Tiempo'),
             yaxis = list(title = 'Temperatura (°C)'))
  })
  
  output$temperatureGraphHourly <- renderPlotly({
    invalidateLater(3600000, session) # Refresh data every hour
    
    query <- "
      SELECT
          DATE_FORMAT(Fecha, '%Y-%m-%d %H:00:00') AS TimeGroup,
          AVG(Temperatura) AS AvgTemp
      FROM
          dataestacion
      WHERE
          Fecha >= NOW() - INTERVAL 24 HOUR
      GROUP BY
          TimeGroup
      ORDER BY
          TimeGroup DESC;
    "
    
    data <- dbGetQuery(db, query)
    data$TimeGroup <- as.POSIXct(data$TimeGroup, format = "%Y-%m-%d %H:%M:%S")
    
    plot_ly(data, x = ~TimeGroup, y = ~AvgTemp, type = 'scatter', mode = 'lines+markers', marker = list(color = 'blue')) %>%
      layout(title = 'Temperatura promedio cada hora (Últimas 24 horas)',
             xaxis = list(title = 'Tiempo'),
             yaxis = list(title = 'Temperatura (°C)'))
  })
  
  output$temperatureGraphDaily <- renderPlotly({
    invalidateLater(86400000, session) # Refresh data every day
    
    query <- "
      SELECT
          DATE(Fecha) AS DateGroup,
          AVG(Temperatura) AS AvgTemp
      FROM
          dataestacion
      WHERE
          Fecha >= NOW() - INTERVAL 30 DAY
      GROUP BY
          DateGroup
      ORDER BY
          DateGroup DESC;
    "
    
    data <- dbGetQuery(db, query)
    data$DateGroup <- as.Date(data$DateGroup)
    
    plot_ly(data, x = ~DateGroup, y = ~AvgTemp, type = 'scatter', mode = 'lines+markers', marker = list(color = 'green')) %>%
      layout(title = 'Temperatura promedio diaria (Últimos 30 días)',
             xaxis = list(title = 'Fecha'),
             yaxis = list(title = 'Temperatura (°C)'))
  })
  
  output$temperatureGraphMonthly <- renderPlotly({
    invalidateLater(86400000, session) # Refresh data every day (or choose a different refresh rate as needed)
    
    query <- "
      SELECT
          DATE_FORMAT(Fecha, '%Y-%m') AS MonthGroup,
          AVG(Temperatura) AS AvgTemp
      FROM
          dataestacion
      WHERE
          Fecha >= NOW() - INTERVAL 1 YEAR
      GROUP BY
          MonthGroup
      ORDER BY
          MonthGroup DESC;
    "
    
    data <- dbGetQuery(db, query)
    data$MonthGroup <- as.Date(paste0(data$MonthGroup, "-01"))
    
    plot_ly(data, x = ~MonthGroup, y = ~AvgTemp, type = 'scatter', mode = 'lines+markers', marker = list(color = 'orange')) %>%
      layout(title = 'Temperatura promedio mensual (año completo)',
             xaxis = list(title = 'Mes'),
             yaxis = list(title = 'Temperatura (°C)'))
  })
  
  #### Gráficas velocidad del viento
  
  output$windSpeedGraph1Minute <- renderPlotly({
    invalidateLater(60000, session) # Refresh data every minute
    
    query <- "
      SELECT
          DATE_FORMAT(Fecha, '%Y-%m-%d %H:%i:00') AS TimeGroup,
          AVG(Vviento) AS AvgWindSpeed
      FROM
          dataestacion
      WHERE
          Fecha >= NOW() - INTERVAL 30 MINUTE
      GROUP BY
          TimeGroup
      ORDER BY
          TimeGroup DESC;
    "
    
    data <- dbGetQuery(db, query)
    data$TimeGroup <- as.POSIXct(data$TimeGroup, format = "%Y-%m-%d %H:%M:%S")
    
    plot_ly(data, x = ~TimeGroup, y = ~AvgWindSpeed, type = 'scatter', mode = 'lines+markers', marker = list(color = 'red')) %>%
      layout(title = 'Velocidad del viento promedio cada minuto (Últimos 30 minutos)',
             xaxis = list(title = 'Tiempo'),
             yaxis = list(title = 'Velocidad del viento (m/s)'))
  })
  
  output$windSpeedGraphHourly <- renderPlotly({
    invalidateLater(3600000, session) # Refresh data every hour
    
    query <- "
      SELECT
          DATE_FORMAT(Fecha, '%Y-%m-%d %H:00:00') AS TimeGroup,
          AVG(Vviento) AS AvgWindSpeed
      FROM
          dataestacion
      WHERE
          Fecha >= NOW() - INTERVAL 24 HOUR
      GROUP BY
          TimeGroup
      ORDER BY
          TimeGroup DESC;
    "
    
    data <- dbGetQuery(db, query)
    data$TimeGroup <- as.POSIXct(data$TimeGroup, format = "%Y-%m-%d %H:%M:%S")
    
    plot_ly(data, x = ~TimeGroup, y = ~AvgWindSpeed, type = 'scatter', mode = 'lines+markers', marker = list(color = 'blue')) %>%
      layout(title = 'Velocidad del viento promedio cada hora (Últimas 24 horas)',
             xaxis = list(title = 'Hora'),
             yaxis = list(title = 'Velocidad del viento (m/s)'))
  })
  
  output$windSpeedGraphDaily <- renderPlotly({
    invalidateLater(86400000, session) # Refresh data every day
    
    query <- "
      SELECT
          DATE(Fecha) AS DateGroup,
          AVG(Vviento) AS AvgWindSpeed
      FROM
          dataestacion
      WHERE
          Fecha >= NOW() - INTERVAL 30 DAY
      GROUP BY
          DateGroup
      ORDER BY
          DateGroup DESC;
    "
    
    data <- dbGetQuery(db, query)
    data$DateGroup <- as.Date(data$DateGroup)
    
    plot_ly(data, x = ~DateGroup, y = ~AvgWindSpeed, type = 'scatter', mode = 'lines+markers', marker = list(color = 'green')) %>%
      layout(title = 'Velocidad del viento promedio diaria (Últimos 30 días)',
             xaxis = list(title = 'Fecha'),
             yaxis = list(title = 'Velocidad del viento (m/s)'))
  })
  
  output$windSpeedGraphMonthly <- renderPlotly({
    invalidateLater(86400000, session) # Consider adjusting this based on your needs
    
    query <- "
      SELECT
          DATE_FORMAT(Fecha, '%Y-%m') AS MonthGroup,
          AVG(Vviento) AS AvgWindSpeed
      FROM
          dataestacion
      WHERE
          Fecha >= NOW() - INTERVAL 1 YEAR
      GROUP BY
          MonthGroup
      ORDER BY
          MonthGroup DESC;
    "
    
    data <- dbGetQuery(db, query)
    data$MonthGroup <- as.Date(paste0(data$MonthGroup, "-01"))
    
    plot_ly(data, x = ~MonthGroup, y = ~AvgWindSpeed, type = 'scatter', mode = 'lines+markers', marker = list(color = 'orange')) %>%
      layout(title = 'Velocidad del viento promedio mensual (año completo)',
             xaxis = list(title = 'Mes'),
             yaxis = list(title = 'Velocidad del viento (m/s)'))
  })
  
  # Remember to close the database connection when the app closes
  onSessionEnded(function() {
    dbDisconnect(db)
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
      theme = "flat_red"),
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
