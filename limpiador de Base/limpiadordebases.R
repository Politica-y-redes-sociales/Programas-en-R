library(dplyr)


carpeta<-"C:/Users/Administrador/Desktop/Programas/Limpiador de Bases"

carpeta_Bases<-paste(carpeta,"Bases",sep="/")
nombres<-dir(carpeta_Bases)


nombres<-as.data.frame(nombres)



for(i in 1:length(nombres[,1])){
  archivo_temporal<-paste(carpeta_Bases,toString(nombres$nombres[i]),sep="/")
  temporal<-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-7")
  
  data<-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-7")
  
  BaseLimpia <- data[!duplicated(data$status_id),]
  
  archivo_final<-"C:/Users/Administrador/Desktop/Programas/Limpiador de Bases/resultado"
  
  
  write.csv(BaseLimpia,file = paste(archivo_final,nombres$nombres[i],sep = "/"),row.names = FALSE)
}

