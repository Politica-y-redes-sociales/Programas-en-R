library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(tidytext)
library(tm)
library(sqldf)

carpeta = "C:/Users/Administrador/Desktop/ProgramasR/CortarFecha" #Ubicacion programa
carpeta_bases = paste(carpeta, "Bases", sep="/")               #Ubicacion de las bases
nombres = dir(carpeta_bases)                                   #Lista de archivos que estan en bases
nombres = as.data.frame(nombres)                               #Crea una tabla con los nombres de las bases 


if(dir.exists(paste(carpeta,"Resultados",sep = "/")))          #Comprueba si existe la carpeta resultados
{}else{dir.create(paste(carpeta,"Resultados",sep = "/"))}      #Si la carpeta Resultados no existe, la crea

x=1
j=1
cont=1
numerillo = nrow(nombres)

while(x <= numerillo)
{
  
  nombre = substr(toString(nombres$nombres[x]),1,(str_length(nombres$nombres[x])-4))
  nombre_carpeta = paste(carpeta,"Resultados",sep = "/")
  archivo_temporal = paste(carpeta_bases,toString(nombres$nombres[x]),sep="/")
  print(nombre)
  lista<-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  lista<-as.data.frame(lista)
  
  palabra <- read.csv(paste(carpeta,"fechas.csv", sep = "/"), header = TRUE, sep = ";")
  
  consultasql2 = "select status_id,created_at,user_id,screen_name,text,source,display_text_width,reply_to_status_id,reply_to_user_id,reply_to_screen_name,is_quote,is_retweet,favorite_count,retweet_count,hashtags,symbols,urls_url,urls_expanded_url,media_url,media_expanded_url,media_type,ext_media_url,ext_media_expanded_url,ext_media_type,mentions_user_id,mentions_screen_name,lang,quoted_status_id,quoted_text,quoted_created_at,quoted_source,quoted_favorite_count,quoted_retweet_count,quoted_user_id,quoted_screen_name,quoted_name,quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,quoted_verified,retweet_status_id,retweet_text,retweet_created_at,retweet_source,retweet_favorite_count,retweet_user_id,retweet_screen_name,retweet_name,retweet_followers_count,retweet_friends_count,retweet_statuses_count,retweet_location,retweet_description,retweet_verified,place_url,place_name,place_full_name,place_type,country,country_code,geo_coords,coords_coords,bbox_coords from lista where created_at >='"
  fecha2= paste(consultasql2,palabra[1,1],sep = "")
  fecha2= paste(fecha2,"",sep = "")
  fecha2= paste(fecha2,"00:00:00' ",sep = " ")
  busqueda = sqldf(fecha2) #Transforma lista a sql para hacer consultas 
  
  consultasql1 = "select status_id,created_at,user_id,screen_name,text,source,display_text_width,reply_to_status_id,reply_to_user_id,reply_to_screen_name,is_quote,is_retweet,favorite_count,retweet_count,hashtags,symbols,urls_url,urls_expanded_url,media_url,media_expanded_url,media_type,ext_media_url,ext_media_expanded_url,ext_media_type,mentions_user_id,mentions_screen_name,lang,quoted_status_id,quoted_text,quoted_created_at,quoted_source,quoted_favorite_count,quoted_retweet_count,quoted_user_id,quoted_screen_name,quoted_name,quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,quoted_verified,retweet_status_id,retweet_text,retweet_created_at,retweet_source,retweet_favorite_count,retweet_user_id,retweet_screen_name,retweet_name,retweet_followers_count,retweet_friends_count,retweet_statuses_count,retweet_location,retweet_description,retweet_verified,place_url,place_name,place_full_name,place_type,country,country_code,geo_coords,coords_coords,bbox_coords from busqueda where created_at <='"
  fecha1= paste(consultasql1,palabra[1,2],sep = "")
  fecha1= paste(fecha1,"",sep = "")
  fecha1= paste(fecha1,"23:59:59' ",sep = " ")
  fecha1= paste(fecha1,"order by created_at desc",sep = " ")
  print(fecha1)
  lista <- sqldf(fecha1) #Transforma lista a sql para hacer consultas
  x = x+1
}

nombre_final=paste(nombre,".csv",sep = "")
write.csv(lista, file = paste(nombre_carpeta,nombre_final,sep = "/"),row.names=FALSE)