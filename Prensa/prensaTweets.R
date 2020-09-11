library(sqldf)
library(readr)
library(tidyverse)
library(datos)

carpeta="C:/Users/Cristobal/Desktop/practica";
carpeta_base = "C:/Users/Cristobal/Desktop/practica/Bases";
nombresBases = dir(carpeta_base);
nombresBases = as.data.frame(nombresBases)

prensa=paste(carpeta,'queriesprensa.csv',sep = '/')
prensa=read.csv(prensa,sep = ';', encoding = "UTF-8")
cuentasPrensa=prensa[c('Nombre','Carpeta')]

carpetaResultado=paste(carpeta,"resultados",sep="/")

if(!dir.exists(carpetaResultado)){
  dir.create(carpetaResultado);
}

for (i in nombresBases[,1]) 
{
  
  file=paste(carpeta_base,i,sep = '/');
  file=read.csv(file);
  file <- as.data.frame(file);
  cuentasPrensa <- as.data.frame(cuentasPrensa)
  view(cuentasPrensa)
    query <- paste('SELECT screen_name,text FROM cuentasPrensa 
                   INNER JOIN file ON cuentasPrensa.nombre = file.screen_name WHERE cuentasPrensa.Carpeta = "Prensa" ')
    tweets <- sqldf(query)
    tweets <- as.data.frame(tweets);
    view(tweets)
    write.csv(tweets,file = paste(carpetaResultado,i,sep='/'), fileEncoding = "UTF-8")
}