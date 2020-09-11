library(sqldf)
library(readr)
library(tidyverse)
library(datos)

carpeta="C:/Users/Cristobal/Desktop/practica";
carpeta_base = "C:/Users/Cristobal/Desktop/practica/Bases";
nombresBases = dir(carpeta_base);
nombresBases = as.data.frame(nombresBases)

prensa=paste(carpeta,'queriesprensa.csv',sep = '/')
prensa=read.csv(prensa,sep = ';')
cuentasPrensa=prensa[c('Nombre','Carpeta')]



carpetaResultado=paste(carpeta,"resultados",sep="/")

if(!dir.exists(carpetaResultado)){
  dir.create(carpetaResultado);
}

for (i in nombresBases[,1]) {
  
  
  file=paste(carpeta_base,i,sep = '/');
  file=read.csv(file);
  file <- as.data.frame(file);
  cuentasPrensa <- as.data.frame(cuentasPrensa)
  view(cuentasPrensa)
  
    query <- paste('select screen_name as usuario,text as tweet FROM cuentasPrensa INNER JOIN file ON cuentasPrensa.nombre = file.screen_name where cuentasPrensa.Carpeta = "Prensa" ')
    tweets <- sqldf(query)
    tweets <- as.data.frame(tweets);
    query1 <- paste('SELECT screen_name as usuario, count(text) as cantidad FROM cuentasPrensa join file on cuentasPrensa.nombre = file.screen_name where cuentasPrensa.Carpeta = "Prensa" group by screen_name')
    contador <- sqldf(query1)
    print(contador)
    view(tweets)
    write.csv(contador,file = paste(carpetaResultado,i,sep='/cantidad '))
    write.csv(tweets,file = paste(carpetaResultado,i,sep='/'))
}
