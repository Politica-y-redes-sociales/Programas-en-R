library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(tidytext)
library(tm)
library(sqldf)

carpeta = "C:/Users/Administrador/Desktop/Programas/BuscarPalabra" #Ubicacion programa
carpeta_bases = paste(carpeta, "Bases", sep="/")               #Ubicacion de las bases
nombres = dir(carpeta_bases)                                   #Lista de archivos que estan en bases
nombres = as.data.frame(nombres)                               #Crea una tabla con los nombres de las bases 


if(dir.exists(paste(carpeta,"Resultados",sep = "/")))          #Comprueba si existe la carpeta resultados
{}else{dir.create(paste(carpeta,"Resultados",sep = "/"))}      #Si la carpeta Resultados no existe, la crea

#for(i in 1:length(nombres)+1)
x=1
j=1
cont=1
numerillo = nrow(nombres)

while(x <= numerillo)
{
  
  nombre = substr(toString(nombres$nombres[x]),1,(str_length(nombres$nombres[x])-4))
  nombre_carpeta = paste(carpeta,"Resultados",sep = "/")
  #palabra = read.csv("D:/Users/interbarometro/Desktop/ProgramasR/BuscarPalabra/buscar.csv")
  archivo_temporal = paste(carpeta_bases,toString(nombres$nombres[x]),sep="/")
  print(nombre)
  lista<-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  lista<-as.data.frame(lista)
  
  palabra <- read.csv(paste(carpeta,"palabra.csv", sep = "/"), header = FALSE)
  #print(palabra[1,2])
  #prueba <- paste(palabra[,1],"Esto es de prueba")
  #print(prueba)
  
  for(i in 1:length(palabra[,1]))# se recorre las palabras a buscar
  {
    consultasql = "select DISTINCT(status_id),created_at,user_id,screen_name,text,source,display_text_width,reply_to_status_id,reply_to_user_id,reply_to_screen_name,is_quote,is_retweet,favorite_count,retweet_count,hashtags,symbols,urls_url,urls_expanded_url,media_url,media_expanded_url,media_type,ext_media_url,ext_media_expanded_url,ext_media_type,mentions_user_id,mentions_screen_name,lang,quoted_status_id,quoted_text,quoted_created_at,quoted_source,quoted_favorite_count,quoted_retweet_count,quoted_user_id,quoted_screen_name,quoted_name,quoted_followers_count,quoted_friends_count,quoted_statuses_count,quoted_location,quoted_description,quoted_verified,retweet_status_id,retweet_text,retweet_created_at,retweet_source,retweet_favorite_count,retweet_user_id,retweet_screen_name,retweet_name,retweet_followers_count,retweet_friends_count,retweet_statuses_count,retweet_location,retweet_description,retweet_verified,place_url,place_name,place_full_name,place_type,country,country_code,geo_coords,coords_coords,bbox_coords from lista where text LIKE '%"
    consulta= paste(consultasql,palabra[cont,1],"%'")
    print(consulta)
    busqueda = sqldf(consulta) #Transforma lista a sql para hacer consultas 
    busqueda = as.data.frame(busqueda)
    #print(palabra[cont,1])
    cont=cont+1
    
    if(j == 1)
    {
      tablafinal = busqueda
    }
    if(j > 1) 
    {
      tablafinal = rbind(tablafinal,busqueda)
    }
    j = j+1 
    
  }
  cont=1
  
  x = x+1
}

write.csv(tablafinal, file = paste(nombre_carpeta,"BusquedaFinal.csv",sep = "/"),row.names=FALSE)

