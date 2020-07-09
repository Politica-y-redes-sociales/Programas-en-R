library(readr)
library(dplyr)#manejo de ficheros
library(rtweet)
library(curl)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)

create_token ( 
  app = "Servidor 4", 
  consumer_key = "ZjttWCOfpdwJIN0F5D3dlJnhl", 
  consumer_secret = "oezJcsjHKxQ4LqmHrrK1CoRwRPAeaaUEMJnLfyaGtYaf9nv4Ct", 
  access_token = "3089952580-pAyIkkGaoRL7X1TjJIXAnPvUTuAXDXAmC5Q5y6x", 
  access_secret = "Fd2fV0MsM3p39iZZVPBCT1NYlXB1AWvCI3yfweJzyghT3" 
)


datos<- read.csv("C:/Users/Administrador/Desktop/Manzanapp/Cuentas/Cuentas.csv",header = TRUE,sep =";",encoding = "UTF-8")
i=1
for(i in 1256 :as.numeric(length(datos[,1]))) {
  Busqueda<-toString(datos$Query1[i])
  

  tweets <- get_timeline(Busqueda
                         , n = 100000)
  
  ##--------------------------------------##
  
  tweets<-as.data.frame(tweets)
  #tweets$text <- sapply(tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  #tweets$retweet_text <- sapply(tweets$retweet_text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  largo<-length(tweets[,1])
  if(largo>0)
  {  
    for(j in 1:length(tweets[1,]))
    {tweets[,j]<-as.character(tweets[,j])}
    carpeta<-paste("C:/Users/Administrador/Desktop/Manzanapp/Descargas",toString(datos$Carpeta[i]),sep="/")
    if(file.exists(carpeta))
    {}
    else
    {dir.create(carpeta)}
    fecha_desde<- sqldf("select substr(min(created_at), 1,10) from tweets ")
    fecha_hasta<- sqldf("select substr(max(created_at), 1,10) from tweets ")
    fecha_desde<- paste("(",fecha_desde,sep = "")
    fecha_hasta<- paste(fecha_hasta,")",sep = "")
    nombre_fecha<- paste(fecha_desde,fecha_hasta,sep ="&" )
    nombre_final<- paste(nombre_fecha,".csv",sep = "")
    archivo<-paste(carpeta, nombre_final, sep=paste0("/",toString(datos$Nombre_Archivo[i])))
    
    if(file.exists(archivo))
    {
      print(c("query", i))
      lista <-read.csv(archivo, header = TRUE, sep = ",")
      tweets<-rbind(tweets,lista)
    }
    
    write.csv(tweets, file =archivo,row.names=FALSE)
  }
}
