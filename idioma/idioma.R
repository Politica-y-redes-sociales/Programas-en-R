#Separando por Lenguajes.
library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(tidytext)
library(tm)
library(sqldf)

carpeta = "C:/Users/Administrador/Desktop/ProgramasR/Idioma"         #Ubicacion programa
carpeta_bases = paste(carpeta, "Bases", sep="/")               #Ubicacion de las bases
nombres = dir(carpeta_bases)                                   #Lista de archivos que estan en bases
nombres = as.data.frame(nombres)                               #Crea una tabla con los nombres de las bases 


if(dir.exists(paste(carpeta,"Resultados",sep = "/")))          #Comprueba si existe la carpeta resultados
{}else{dir.create(paste(carpeta,"Resultados",sep = "/"))}      #Si la carpeta Resultados no existe, la crea

x = 1
numerillo = nrow(nombres)

while(x <= numerillo)
{
  #Nombre archivo
  print('hola')
  nombre = substr(toString(nombres$nombres[x]),1,(str_length(nombres$nombres[x])-4)) 
  nombre_carpeta = paste(carpeta,"Resultados",sep = "/")
  archivo_temporal = paste(carpeta_bases,toString(nombres$nombres[x]),sep="/")
  aux <- read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "utf-7")
  aux <- as.data.frame(aux)
  #ES
  consulta_es = "SELECT COUNT(aux.status_id) Total, aux.lang Idioma, (SELECT(COUNT(aux.lang)*100.0/COUNT(a.status_id)) FROM aux a) Porcentaje FROM aux WHERE lang = 'es'"
  busqueda_es = sqldf(consulta_es)
  #EN
  consulta_en = "SELECT COUNT(aux.status_id) Total, aux.lang Idioma, (SELECT(COUNT(aux.lang)*100.0/COUNT(a.status_id)) FROM aux a) Porcentaje FROM aux WHERE lang = 'en'"
  busqueda_en = sqldf(consulta_en)  
  #Otros
  consulta_count_otros_paise = "SELECT COUNT(aux.status_id) total, aux.lang Idioma, (SELECT(COUNT(aux.lang)*100.0/COUNT(a.status_id)) FROM aux a) Porcentaje FROM aux WHERE lang <> 'es' AND lang <> 'en'"
  busqueda_otros = sqldf(consulta_count_otros_paise)
  #------------------------------------------
  #Union de tablas de idioma
  consulta_union = "SELECT Total, Idioma, Porcentaje FROM busqueda_en UNION SELECT Total, Idioma, Porcentaje FROM busqueda_es UNION SELECT Total, Idioma, Porcentaje FROM busqueda_otros ORDER BY Porcentaje"
  busqueda_union = sqldf(consulta_union)
  #Cambiando valor de dato en una consulta
  consulta_union_final = "SELECT Total, CASE Idioma WHEN 'it' THEN 'otros' ELSE b.Idioma END Idioma, Porcentaje FROM busqueda_union b ORDER BY Porcentaje"
  busqueda_final = sqldf(consulta_union_final)
  x = x+1
}

write.csv(busqueda_final, file = paste(nombre_carpeta,"idiomaFINAL.csv",sep = "/"),row.names=FALSE)
