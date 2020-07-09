library(readr)
library(sqldf)


carpeta<-"C>\Users\Administrador\Desktop\ProgramasR\Diccionario"
carpeta_Bases<-paste(carpeta,"Bases",sep="/")
nombres<-dir(carpeta_Bases)
nombres<-as.data.frame(nombres)

diccionario=read.csv("C:\practica\Diccionario.csv",header = TRUE,sep = ";",encoding = "UTF-8")

if(dir.exists(paste(carpeta,"Resultados",sep = "/")))
{}else{dir.create(paste(carpeta,"Resultados",sep = "/"))}
i=1
for(i in 1:length(nombres[,1]))
{
  
  mencion=toString(nombres$nombres[i])
  archivo_temporal<-paste(carpeta_Bases,mencion,sep="/")
  consulta <-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  consulta=sqldf("select DISTINCT(status_id),* from consulta left join diccionario on(screen_name=twitter)")
  consulta$status_id..2<-NULL
  consulta$twitter<-NULL
  consulta$text=gsub("<f1>","ñ",consulta$text)
  
  consulta$text=gsub("<e1>","á",consulta$text)
  consulta$text=gsub("<c1>","Á",consulta$text)
  
  consulta$text=gsub("<e9>","é",consulta$text)
  
  consulta$text=gsub("<ed>","í",consulta$text)
  
  consulta$text=gsub("<f3>","ó",consulta$text)
  consulta$text=gsub("<d3>","Ó",consulta$text)
  
  consulta$text=gsub("<fa>","ú",consulta$text)
  

  write.csv(consulta, row.names = FALSE, file = paste(carpeta,mencion,sep = "/Resultados/"))
  
}




 