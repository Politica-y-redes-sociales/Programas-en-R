library(readr)
library(sqldf)
library(plyr)  


carpeta<-"C:/Users/alfon/Desktop/R/UnionBases"

carpeta_Bases_Uno<-paste(carpeta,"Base1",sep="/")
nombres_Uno<-dir(carpeta_Bases_Uno)
nombres_Uno<-as.data.frame(nombres_Uno)

carpeta_Bases_Dos<-paste(carpeta,"Base2",sep="/")
nombres_Dos<-dir(carpeta_Bases_Dos)
nombres_Dos<-as.data.frame(nombres_Dos)

if(dir.exists(paste(carpeta,"Resultados",sep = "/")))
{}else{dir.create(paste(carpeta,"Resultados",sep = "/"))}
i=1
for(i in 1:length(nombres_Uno[,1]))
{
  
  mencion_Uno=toString(nombres_Uno[i,1])
  esta_Uno=sqldf(paste0("SELECT * FROM nombres_dos where nombres_Dos=", paste("'","'",sep=mencion_Uno)))
  esta_Uno=length(esta_Uno[,1])
  
  archivo_temporal_Uno<-paste(carpeta_Bases_Uno,mencion_Uno,sep="/")
  consulta_Uno <-read.csv(archivo_temporal_Uno,header = TRUE,sep = ",",encoding = "UTF-7")

  if(esta_Uno>0)
  {
    archivo_temporal_Dos<-paste(carpeta_Bases_Dos,mencion_Uno,sep="/")
    consulta_Dos<-read.csv(archivo_temporal_Dos,header = TRUE,sep = ",",encoding = "UTF-7")
    consulta_Uno<-rbind.fill(consulta_Uno,consulta_Dos)
    write.csv(consulta_Uno, row.names = FALSE, file = paste0(carpeta,"/Resultados/",mencion_Uno))
    
  }else{
    write.csv(consulta_Uno, row.names = FALSE, file = paste0(carpeta,"/Resultados/",mencion_Uno))
  }
  
}
i=1
for(i in 1:length(nombres_Dos[,1]))
{
  
  mencion_Dos=toString(nombres_Dos[i,1])
  esta_Dos=sqldf(paste0("SELECT * FROM nombres_Uno where nombres_Uno=", paste("'","'",sep=mencion_Dos)))
  esta_Dos=length(esta_Dos[,1])

  if(esta_Dos<1){
    
    archivo_temporal_Dos<-paste(carpeta_Bases_Dos,mencion_Uno,sep="/")
    consulta_Dos<-read.csv(archivo_temporal_Dos,header = TRUE,sep = ",",encoding = "UTF-7")

    write.csv(consulta_Dos, row.names = FALSE, file = paste0(carpeta,"/Resultados/",mencion_Dos))
    
  }
  
}


