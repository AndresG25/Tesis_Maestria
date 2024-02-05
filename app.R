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
library(lubridate)

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
server <- function(input, output) {
  
  autoInvalidate <- reactiveTimer(1000)
    
    shinyjs::runjs(
        "function reload_page() {
  window.location.reload();
  setTimeout(reload_page, 55000);
}
setTimeout(reload_page, 55000);
")
    url <- 'https://raw.githubusercontent.com/AndresG25/Tesis_Maestria/main/data1.csv'

    query <- read.csv(url)

    #database<- dbConnect(MySQL(), user="root", host="127.0.0.1", password="", dbname="estacion")
  
    #query<- dbGetQuery(database,statement ="SELECT * FROM dataestacion")
  
    query1 <- query %>% mutate(Fecha1 = parse_date_time(Fecha, "ymd HMS"))

    df <- as.data.frame(query1)

    output$RPMane <- renderValueBox({
      
      isolate({
        
        autoInvalidate()
        pos <- df[nrow(df),]
    
        pos <- pos %>% mutate(RPM = as.numeric(RPM)) %>% select(-Fecha, -Temperatura, -Presion, -Altitud, -Vviento, -Fecha1)
    
        valueBox(value = pos$RPM, subtitle = "RPM Anemometro", color = "green", icon = icon("compass"))
    
        })
    })

    output$Vviento<- renderValueBox({
      
      isolate({
        
        autoInvalidate()
        pos <- df[nrow(df),]
        
        pos <- pos %>% mutate(Vviento = as.numeric(Vviento)) %>% select(-Fecha, -Temperatura, -Presion, -Altitud, -RPM, -Fecha1)
        
        valueBox(value = pos$Vviento, subtitle = "Vel. Viento m/s", icon = icon("wind"), color = "green")
        
    })
  })
    
    output$Temp<- renderValueBox({
        pos <- df[nrow(df),]
        
        pos <- pos %>% mutate(Temperatura = as.numeric(Temperatura)) %>% select(-Fecha, -Vviento, -Presion, -Altitud, -RPM, -Fecha1)
        
        valueBox(value = pos$Temperatura, subtitle = "Temperatura °C", icon = icon("temperature-high"), color = "orange")
        
    })
    
    output$Pres<- renderValueBox({
        pos <- df[nrow(df),]
        
        pos <- pos %>% mutate(Presion = as.numeric(Presion)) %>% select(-Fecha, -Vviento, -Temperatura, -Altitud, -RPM, -Fecha1)
        
        valueBox(value = pos$Presion, subtitle = "Presión Atmosférica Pa", icon = icon("dashboard"), color = "orange")
        
    })
    
    
    
    output$Altitud <- renderValueBox({
        pos <- df[nrow(df),]
        
        pos <- pos %>% mutate(Altitud = as.numeric(Altitud)) %>% select(-Fecha, -Temperatura, -Presion, -RPM, -Vviento, -Fecha1)
        
        valueBox(value = pos$Altitud, subtitle = "Altitud m.s.n.m", color = "orange", icon = icon("cloud"))
        
    })     
    
    output$temperatura <- renderPlotly({
        
        
        df$fecha <- date(df$Fecha1)
        
        df$tiempo <- format(df$Fecha1, format = "%H:%M:%S")
        
        df$hora <- hour(df$Fecha1)
        
        df1 <- df %>% group_by(hora) %>% summarise(Temp = mean(as.integer(Temperatura)))
        
        
        g <- ggplot(df1, aes(x=hora, y=Temp)) + geom_line(color = "red") + geom_point(color = "red")+
            labs(title = "Temperatura promedio diaria") + labs(x = "Periodo", y = "Temperatura")
        
        fig <- ggplotly(g)
        
        
        
    })
    
    output$presion <- renderPlotly({
        
        df$fecha <- date(df$Fecha1)
        
        df$tiempo <- format(df$Fecha1, format = "%H:%M:%S")
        
        df$hora <- hour(df$Fecha1)
        
        df2 <- df %>% group_by(hora) %>% summarize( Pres = mean(as.integer(Presion)))
        
        
        g1 <- ggplot(df2, aes(x=hora, y= Pres)) + geom_line(color = "blue") + geom_point(color = "blue")+
            labs(title = "Presión atmosferica promedio diaria") + labs(x = "Periodo", y = "Presión Atmosferica")
        
        fig1 <- ggplotly(g1)
        
        
        
    })

#    on.exit({
#      dbDisconnect(database)
#    })

}

# Run the application 
shinyApp(ui = ui, server = server)
