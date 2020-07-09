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


carpeta<-"C:/Users/Administrador/Desktop/ProgramasR/GrafosSoles"
carpeta_Bases<-paste(carpeta,"Bases",sep="/")
nombres<-dir(carpeta_Bases)
nombres<-as.data.frame(nombres)

recuento=data.frame("Id"=character(),"Label"=character(),"Category"=character())
puntos=data.frame("Id"=character(),"Label"=character(),"Category"=character())

if(dir.exists(paste(carpeta,"Resultados",sep = "/")))
{}else{dir.create(paste(carpeta,"Resultados",sep = "/"))}

for(i in 1:length(nombres[,1]))
{
  
  mencion=toString(nombres$nombres[i])
  archivo_temporal<-paste(carpeta_Bases,mencion,sep="/")
  nombre<-substr(toString(nombres$nombres[i]),1,(str_length(nombres$nombres[i])-4))
  consulta <-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  
  consulta$text=nombre
  
  consulta=sqldf("select screen_name as Source ,text as Target, retweet_screen_name as Rt from consulta")
  recuento=rbind(recuento,consulta)
  
}


#Aristas(Source,Target,Weight)
aristas=sqldf("select Source ,Target,count(*) as Weight from recuento group by Source,Target")
#Nodos (Id,Label,Category)


write.csv(aristas, row.names = FALSE, file = paste(carpeta,"Resultados/aristas.csv",sep = "/"))

nodos=sqldf("select Target as Id , Target as Label from recuento group by Target")
recuento=sqldf("select Source as Id , Source as Label from recuento group by Source")
nodos=rbind(nodos,recuento)
nodos=sqldf("select Id,Label from nodos group by Id,Label ")

for (i in nodos$Id)
{
  frase=sqldf(paste("select Target from aristas where Source='","'",sep=i))
  palabra=""
  if(length(frase[,1])==0)
  {
    palabra=paste(" ",i)
  }
  for(j in frase$Target)
  {
    palabra=paste(palabra,j,sep=" ")
  }
  tabla=as.data.frame(t(as.data.frame(c(as.character(i),as.character(i),as.character(palabra)))))
  names(tabla)[1]<-paste("Id")
  names(tabla)[2]<-paste("Label")
  names(tabla)[3]<-paste("Category")
  rownames(tabla) <- c()
  puntos=rbind(puntos,tabla)
  
}

write.csv(puntos, row.names = FALSE, file = paste(carpeta,"Resultados/nodos.csv",sep = "/"))