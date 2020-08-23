library(readr)
library(dplyr)
library(rtweet)
library(curl)
library(sqldf)

Consumer_Key<-"yvNgbzI4T0DXl1cBtxXEdZOSf"
Consumer_Secret<-"NE8yuGAnVJtB9uxM8aOjkFVUDUle1KlCQvmAaYQNCmuiKerYYa"
Access_Token<-"3089713545-txuAs2ZwYOjkZPN9XGLU6iFItOREHJjNcfodS3x"
Access_Secret<-"5TXmHWG0YwcQaq2g78QD9DKWGszn728QXZAnEpqhxPaDF"

datos<- read.csv("/Users/alfonsoopazom/Downloads/Followers/Cuentas/Libro1.csv", header = TRUE, sep =";",encoding = "UTF-8")
i=1
length(datos[,1])
for(i in 1:as.numeric(length(datos[,1]))) {
  Busqueda<-toString(datos$Cuenta[i])
  tweets1 <- get_followers(Busqueda, n = 10000, retryonratelimit = TRUE)
  tweets <- lookup_users(tweets1$user_id)
  tweets<-as.data.frame(tweets)
  largo<-length(tweets[,1])
  
  if(largo>0)
  {  
    #Se parsean todo los datos de las columnas a Caracteres
    for(j in 1:length(tweets[1,]))
    {
      tweets[,j]<-as.character(tweets[,j])
    }
    carpeta<-paste("/Users/alfonsoopazom/Downloads/Followers","Resultados", sep="/")
    
    if(file.exists(carpeta))
    {}else
    {dir.create(carpeta)}
    
    total_seguidores<-sqldf("SELECT count(screen_name) 'Numero de seguidores' FROM tweets")
    cuentas_protegidas<-sqldf("SELECT screen_name Cuenta, protected Protegida FROM tweets WHERE protected!='FALSE'")
    n_protegidas<-sqldf("SELECT count(protected) 'Porcentaje Cuentas Protegidas' FROM tweets WHERE protected!='FALSE'")
    porcentaje<-round((n_protegidas/total_seguidores)*100,2)
    write.csv(porcentaje, file=paste(carpeta,paste0("Cuenta_",datos$Cuenta[i],".csv"), sep = "/") ,row.names=FALSE)
  }
}
