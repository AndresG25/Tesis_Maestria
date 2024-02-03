library(RMySQL)
library(tidyverse)
library(lubridate)

#Este código actualiza la base de datos que será subida a Github para la lectura de la shinyapp.

while (TRUE) {

database<- dbConnect(MySQL(), user="root", host="127.0.0.1", password="", dbname="estacion")

query<- dbGetQuery(database,statement ="SELECT * FROM dataestacion")

query1 <- query %>% mutate(Fecha = parse_date_time(Fecha, "ymd HMS"))

write.csv(query1, file="C:/Users/USUARIO/Desktop/Tesis_Maestria/data1.csv")

dbDisconnect(database)

#Sys.sleep(60)  # Pausa el script durante 60 segundos
}