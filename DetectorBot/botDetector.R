library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(tidytext)
library(tm)
library(colorspace)
library(sqldf)
library(rtweet)
library(curl)
library(gsubfn)
library(proto)
library(RSQLite)

token <- create_token ( 
  app<-"Servidor 45", 
  consumer_key<-"zims51dZYe7BpC1AnVl5NTdFm", 
  consumer_secret<-"xvcIpOB2g1b7xaB9K7E0QVR4OGUHM2PzoVbOmEXbw9jMXfLj5q",
  access_token<-"987408666319118336-slXOkbUWQCEMxHRiahPJcqlbK4zjk5Z", 
  access_secret<-"LdOJGJHyuqfiqX4H2k9iCbb9bycb9aYxdjtfMMy4egKkA"
)

carpeta = "/Users/alfonsoopazom/Desktop/Observatorio/DetectorBot"
carpeta_base = paste(carpeta,"Bases",sep="/")
cuentas = dir(carpeta_base)
cuentas = as.data.frame(cuentas)

if(dir.exists(paste(carpeta, "Resultados", sep = "/")))
{}else
{dir.create(paste(carpeta, "Resultados", sep = "/"))}

nombre = substr(toString(cuentas$cuentas),1,(str_length(cuentas$cuentas)-4))
archivo_temporal = paste(carpeta_base,toString(cuentas$cuentas),sep="/")
CarpetaResultados = paste(carpeta,"Resultados",sep = "/")
consulta = read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")

#Cantidad de filas de la base
total_filas <- sqldf("SELECT count(DISTINCT(screen_name)) total_filas FROM consulta")

#Cuenta del usuario
cuenta<- sqldf("SELECT DISTINCT(screen_name) Cuentas, screen_name numeros, name FROM consulta")
cuenta$numeros<-gsub('[a-z]|[A-Z]|_|-','',cuenta$numeros)
cuenta$numeros<-gsub('[a-z]|[A-Z]|_|-','',cuenta$numeros)

#---- NUMEROS EN EL NOMBRE DE CUENTA ----#
cuenta <- sqldf("SELECT Cuentas, numeros, length(Cuentas) Cantidad, length(numeros) Cantidad_num, round(((cast(length(numeros) as float))/(cast(length(Cuentas) as float)))*100,2) Porcentaje
                 FROM cuenta
                 WHERE Cantidad_num >0 AND Porcentaje > 50.00
                 ORDER BY Cantidad_num
                 DESC")
contarNum <-sqldf("SELECT count(Cuentas) Cantidad_n FROM cuenta")
#---- RETWEET ----#
retweet <- sqldf("SELECT DISTINCT(screen_name) cuenta, text, is_retweet, retweet_screen_name
                FROM consulta WHERE is_retweet='1'
                GROUP BY cuenta")
contarRetweet <- sqldf("SELECT count(cuenta) Cantidad_Re FROM retweet")

#---- DESCRIPCION ----#
descripcion <- sqldf("SELECT DISTINCT(screen_name) cuenta, description Descripcion
                     FROM consulta WHERE description =''")
contarDesc <- sqldf("SELECT count(cuenta) Cantidad_Des FROM descripcion")

#---- LOCALIZACION ----#
localizacion <- sqldf("SELECT DISTINCT(screen_name) cuenta, location Localizacion
                     FROM consulta WHERE location =''")
contarLoc <- sqldf("SELECT count(cuenta) Cantidad_Loca FROM localizacion")

porcentaje_num <- round((contarNum$Cantidad_n/total_filas$total_filas)*100,2)
porcentaje_Ret <- round((contarRetweet$Cantidad_Re/total_filas$total_filas)*100,2)
porcentaje_loca <-round((contarLoc$Cantidad_Loca/total_filas$total_filas)*100,2)
porcentaje_des <-round((contarDesc$Cantidad_Des/total_filas$total_filas)*100,2)
Total = round((porcentaje_num+porcentaje_Ret+porcentaje_loca+porcentaje_des+porcentaje_des)/100,2)

tabla <- matrix(c("Numeros en Usuario","Retweet","Localizacion","Descripcion",porcentaje_num,porcentaje_Ret,porcentaje_loca,porcentaje_des),ncol =2)
colnames(tabla) = c("Cantidad","Porcentaje")
tabla = as.data.frame(tabla)
write.csv(tabla, file = paste(CarpetaResultados,'PorcentajeBotRT.csv',sep = "/"),row.names=FALSE)


#---- NO RETWEET ----#
#NoRetweet <- sqldf("SELECT screen_name Cuenta, text
#FROM consulta WHERE is_retweet='0'")



