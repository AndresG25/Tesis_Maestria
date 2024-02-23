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

  
  
  
  
  server <- function(input, output, session) {
    # Define la URL del archivo CSV
    url_csv <- 'https://raw.githubusercontent.com/AndresG25/Tesis_Maestria/main/data1.csv'
    
    # Función para leer y procesar datos del CSV
    processData <- reactive({
      invalidateLater(10000, session)  # Refrescar cada 10 segundos
      datos <- read_csv(url_csv, show_col_types = FALSE) %>%
        mutate(Fecha = as.POSIXct(Fecha, format = "%Y-%m-%d %H:%M:%S"),
               PresionPSI = Presion / 6895,  # Convertir presión a PSI
               Temperatura = as.numeric(Temperatura),
               Vviento = as.numeric(Vviento),
               RPM = as.numeric(RPM),
               Altitud = as.numeric(Altitud))
      return(datos)
    })
    
    # Adaptar las salidas para utilizar los datos procesados
    output$fecha_hora <- renderText({
      datos <- processData()
      if (nrow(datos) > 0) {
        max_fecha <- max(datos$Fecha, na.rm = TRUE)
        paste("Última Actualización:", format(max_fecha, "%Y-%m-%d %H:%M:%S"))
      } else {
        "Datos no disponibles"
      }
    })
    
    # Adaptación para ValueBoxes
    output$RPMane <- renderValueBox({
      datos <- processData()
      ultimo_RPM <- tail(datos$RPM, 1)
      valueBox(value = ultimo_RPM, subtitle = "RPM Anemometro", color = "green", icon = icon("compass"))
    })
    
    output$Vviento <- renderValueBox({
      datos <- processData()
      ultimo_Vviento <- tail(datos$Vviento, 1)
      valueBox(value = ultimo_Vviento, subtitle = "Vel. Viento m/s", color = "green", icon = icon("wind"))
    })
    
    output$Temp <- renderValueBox({
      datos <- processData()
      ultima_Temperatura <- tail(datos$Temperatura, 1)
      valueBox(value = ultima_Temperatura, subtitle = "Temperatura °C", color = "orange", icon = icon("temperature-high"))
    })
    
    output$Pres <- renderValueBox({
      datos <- processData()
      ultima_PresionPSI <- tail(datos$PresionPSI, 1)
      valueBox(value = round(ultima_PresionPSI, 2), subtitle = "Presión Atmos. PSI", color = "orange", icon = icon("dashboard"))
    })
    
    output$Altitud <- renderValueBox({
      datos <- processData()
      ultima_Altitud <- tail(datos$Altitud, 1)
      valueBox(value = ultima_Altitud, subtitle = "Altitud m.s.n.m", color = "orange", icon = icon("cloud"))
    })
    
    # Adaptación para las gráficas (ejemplo con temperatura)
    # Repetir un enfoque similar para adaptar las demás gráficas como se requiera
    output$temperatureGraph <- renderPlotly({
      datos <- processData()
      datos_agregados <- datos %>%
        filter(Fecha >= Sys.time() - hours(24)) %>%
        mutate(Hour = floor_date(Fecha, "hour")) %>%
        group_by(Hour) %>%
        summarize(AvgTemp = mean(Temperatura, na.rm = TRUE)) %>%
        ungroup()
      
      plot_ly(datos_agregados, x = ~Hour, y = ~AvgTemp, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = 'Temperatura promedio cada hora (Últimas 24 Horas)',
               xaxis = list(title = 'Hora'),
               yaxis = list(title = 'Temperatura (°C)'))
    })
  }