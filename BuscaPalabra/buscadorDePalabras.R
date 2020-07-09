#update.packages()
#updateR()
#install.packages("RCurl")

library(RCurl)
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

httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))

create_token ( 
  app = "Servidor 1", 
  consumer_key = "goE5sHS78ipkzCuMShM05x10A",
  consumer_secret = "6tUcSkNCEjWmc5mIrx05YxfkKTMfwlctf0LwgXZNrtgSUrp6lO", 
  access_token = "987408666319118336-QmTPYPgiFB0NcHL1y6kekk9O9maTTl0", 
  access_secret = "Jz8gkGlptmFuvaeBvm2az4i8YefnZBnwPchNeyadiWBG6")

carpeta <- "C:/Users/Administrador/Desktop/DescargasR/DescargasAutomaticas"
query <- "C:/Users/Administrador/Desktop/DescargasR/DescargasAutomaticas/queries186.csv"
datos<- read.csv(query,header = TRUE,sep =";",encoding = "UTF-8")

if(dir.exists(paste(carpeta,"Resultados",sep = "/"))){
  
}else
{
  dir.create(paste(carpeta,"Resultados",sep = "/"))
}


for(i in 1 :as.numeric(length(datos[,1])))
{


  
  Busqueda<-toString(datos$Query[i])
  tweets <- search_tweets( Busqueda, 
                           n=10000000,
                           since=as.Date("2020-05-01"),
                           until=as.Date("2020-05-05"), 
                           retryonratelimit = TRUE)
  tweets<-as.data.frame(tweets)
  largo<-length(tweets[1,])
  
  if(largo>0)
  {  
    for(j in 1:length(tweets[1,]))
    {
      tweets[,j]<-as.character(tweets[,j])
      #print(tweets[,j]<-as.character(tweets[,j]))
    }
    
    carpeta_descarga<-paste(carpeta,toString(datos$Carpeta[i]),sep="/")
    
    if(file.exists(carpeta_descarga))
    {}else
    {dir.create(carpeta_descarga)}
    
    archivo<-paste(carpeta_descarga,".csv",sep=paste0("/",toString(datos$Nombre_Archivo[i])))
    
    if(file.exists(archivo))
    {
      
      lista <-read.csv(archivo, header = TRUE, sep = ",")
      tweets<-rbind(tweets,lista)
    }
    
    
      write.csv(tweets, file =archivo,row.names=FALSE)
    
  }
  print(c("query", i))
}
print(Busqueda)

