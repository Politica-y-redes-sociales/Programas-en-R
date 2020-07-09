
carpeta<-"C:/Users/Administrador/Desktop/Programas/UnirListas"

carpeta_Bases<-paste(carpeta,"Bases",sep="/")
nombres<-dir(carpeta_Bases)


nombres<-as.data.frame(nombres)

for(i in 1:length(nombres[,1]))
{
  archivo_temporal<-paste(carpeta_Bases,toString(nombres$nombres[i]),sep="/")
  temporal<-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-7")

  if(i==1)
  {
    
    final<-temporal
  } 
  else{
    final<-rbind(final,temporal)
  }
}



archivo_final<-paste(carpeta,".csv",sep="/final")
write.csv(final,file = archivo_final,row.names = FALSE)

  