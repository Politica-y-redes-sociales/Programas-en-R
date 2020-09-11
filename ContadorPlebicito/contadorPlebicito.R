
library(sqldf)
library(readr)
library(tidyverse)
library(datos)

carpeta="/Users/kevinjuanjacquecastillo/Desktop/Develoment/programasR/Programas-en-R/ContadorPlebicito";
carpeta_base = paste(carpeta,"Bases",sep="/");
nombresBases = dir(carpeta_base);
nombresBases = as.data.frame(nombresBases)

prensa=paste(carpeta,'queriesprensa.csv',sep = '/')
prensa=read.csv(prensa,sep = ';')
cuentasPrensa=prensa['Query1']



carpetaResultado=paste(carpeta,"resultados",sep="/")

if(!dir.exists(carpetaResultado)){
  dir.create(carpetaResultado);
}

x=1
for (i in nombresBases[,1]) {
  
  
  
  
  file=paste(carpeta_base,i,sep = '/')
  file=read.csv(file)
  
  resultado=data.frame(
      "cuenta"=NULL,
      "cantidad"=NULL
  )
  
  for (x in 1:length(cuentasPrensa[,1])) {
    filtro=filter(file,str_detect(text,cuentasPrensa[x,1]))
    if(length(filtro[,1])>0){
      length(filtro[,1])
      resultado=rbind(resultado,data.frame(cuentasPrensa[x,1],length(filtro[,1])))
    }
  }
  
  write.csv(resultado,paste(carpetaResultado,i,sep='/'))
  
  
}