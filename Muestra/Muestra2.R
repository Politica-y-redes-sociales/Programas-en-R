library(dplyr)
library(sqldf)

carpeta<-"/Users/alfonsoopazo/Desktop/Observatorio/Muestra"
carpeta_Bases<-paste(carpeta,"Bases",sep="/")
nombres<-dir(carpeta_Bases)
nombres<-as.data.frame(nombres)


if(dir.exists(paste(carpeta, "Resultados", sep = "/")))
{}else{
  dir.create(paste(carpeta, "Resultados", sep = "/"))    
}

for(i in 1:length(nombres[,1])){
  
  archivo_temporal<-paste(carpeta_Bases,toString(nombres$nombres[i]),sep="/")
  consulta <- read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  respuesta <- sqldf('select "screen_name","user_id","status_id","text", "retweet_text","is_retweet"  from 	consulta ')

  x <-sample(1:length(consulta[,1]),1000,replace = FALSE)
  x <- as.data.frame(x)
 
 
  if(length(respuesta[,1])>=1000)
  {
      respuesta<-respuesta[x[,1],]
      archivo_final<-paste(carpeta,"Resultados",sep = "/")
      write.csv(respuesta,file = paste(archivo_final,nombres$nombres[i],sep = "/"),row.names = FALSE)
    
  }else{
    respuesta <- respuesta[1:length(consulta[,1]),]
    archivo_final<-paste(carpeta,"Resultados",sep = "/")
    write.csv(respuesta,file = paste(archivo_final,nombres$nombres[i],sep = "/"),row.names = FALSE)
  }
}
