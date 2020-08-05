library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(tidytext)
library(rtweet)
library(tm)
library(curl)
library(httr)
library(httpuv)



token <- create_token ( 
  app<-"Observatorio 45", 
  consumer_key<-"jzkYB23Z1Vgca7ipBbzbL8gp0", 
  consumer_secret<-"FN1nKjdkPy4wf01IX0P9haHKDn1reQZHFVhbN9js4GBtIbZWzI",
  access_token<-"987408666319118336-n6tyEw6FndUgQyhH0VSAX0R4lxv4qh6", 
  access_secret<-"OhCUXm2XYpSTijzQDVQDE8hUrZgbd37MqMAl2eJykNwE2")


datos<- read.csv("C:/Users/Administrador/Desktop/DescargasR/queries45.csv",
                 header = TRUE,sep =";",encoding = "UTF-8")

#FECHAS DE INICIO
INICIO=as.Date("2020-07-27")
#FECHA DE FIN
FIN=as.Date("2020-07-29")
nummes=as.numeric(format(INICIO,"%m"))
mes=format(INICIO,"%B") 
mes=paste(nummes,mes,sep = "_")
resultado=as.String("resultados")
print(resultado)
i=100
for(i in 1:as.numeric(length(datos[,1])))
  {
  
  Busqueda<-toString(datos$Query1[i])
  tweets <- search_tweets( Busqueda, # Nacho decia: 'piñera' en vez de busqueda
                           n =10000000,  # y el n estaba en 1000 no en 1 millon
                           since=as.Date(INICIO), 
                           until=as.Date(FIN), 
                           retryonratelimit = TRUE,
                           token = token)
  tweets<-as.data.frame(tweets)
  largo<-length(tweets[,1])
  
  if(largo>0)
  {  
    for(j in 1:length(tweets[1,]))
    {tweets[,j]<-as.character(tweets[,j])}
    
    #En este directorio llegan las descargas
    #carpeta<-paste("C:/Users/Administrador/Desktop/DescargasR/Descargas",toString(datos$Carpeta[i]),sep="/")
    
    
    carpeta<-paste("C:/Users/Administrador/Desktop/servidor45Descargastweet/2020",mes,resultado,toString(datos$Carpeta[i]),sep="/")
    
    if(file.exists(carpeta))
    {}else{dir.create(carpeta)}
    
    archivo<-paste(carpeta,".csv",sep=paste0("/",toString(datos$Nombre_Archivo[i])))
    if(file.exists(archivo))
    {
      lista <-read.csv(archivo, header = TRUE, sep = ",")
      tweets<-rbind(tweets,lista)
    }
    print(c("query", i))
    write.csv(tweets, file = archivo,row.names=FALSE)
  }
}

