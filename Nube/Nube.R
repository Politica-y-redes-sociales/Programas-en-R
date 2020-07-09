library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(ggplot2)
library(tidytext)
library(tm)
library(colorspace)
Sys.getlocale()
#Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
pat="(RT|via)(((?:\\b\\W*|)@\\w+)+)|,|:|(https|http)://t.co/[A-Za-z\\d]+|&amp;|http\\w*|@\\w+|(\\w+\\.|)\\w+\\.\\w+(/\\w+)*"
carpeta<-"C:/Users/Administrador/Desktop/ProgramasR/Nube"
carpeta_Bases<-paste(carpeta,"Bases",sep="/")
nombres<-dir(carpeta_Bases)
nombres<-as.data.frame(nombres)

if(dir.exists(paste(carpeta,"Resultados",sep = "/")))
{}else{dir.create(paste(carpeta,"Resultados",sep = "/"))}

for(i in 1:length(nombres[,1]))
{
  archivo_temporal<-paste(carpeta_Bases,toString(nombres$nombres[i]),sep="/")
  nombre<-substr(toString(nombres$nombres[i]),1,(str_length(nombres$nombres[i])-4))
  nombre_carpeta<-paste(carpeta,"Resultados",sep = "/")
  nombre_carpeta<-paste(nombre_carpeta,nombre,sep = "/")
  
  if(dir.exists(nombre_carpeta))
  {}else{dir.create(nombre_carpeta)}
  
  temporal<-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-7")
  temporal<-select(temporal,created_at,screen_name,text)
  tempora_nube<-temporal
 
  
  #-----------------------------------Nube-----------------------------------
  
  conectores<-read.csv(paste(carpeta,"conectores.csv",sep = "/"), header = FALSE)
  
  tempora_nube<-mutate(tempora_nube,text = str_replace_all(text,pat, ""))
  lis<-unnest_tokens(tempora_nube,word, text, token="ngrams",n=1 )
  nube<-count(lis,word,sort=TRUE)
  
  nube<-as.data.frame(nube)
  conectores<-as.data.frame(conectores)
  largo<-length(nube[,1])
  nube2<-nube
  i<-1
  j<-1
  while(i<=largo)
  {
    while(j<=length(conectores[,1]))
    {
      if(toString(nube[i,1])==toString(conectores[j,1]))
      {
        nube<-nube[-i,]
        i<-i-1
        largo<-largo-1
        j<-largo
      }else{
        j<-j+1}
    }
    j<-1
    i<-i+1
  }
  
  write.csv(nube, file = paste(nombre_carpeta,"nube.csv",sep = "/"),row.names=FALSE)
  

  
}




