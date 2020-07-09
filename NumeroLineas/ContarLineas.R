library(stringr)

carpeta<-"C:/Users/Administrador/Desktop/Programas/NumeroLineas"
carpeta_Bases<-paste(carpeta,"Bases",sep="/")
carpeta_Bases<-paste(carpeta,"Bases",sep="/")
if(dir.exists(carpeta_Bases))
{}else{
  dir.create(carpeta_Bases)
}


nombres<-dir(carpeta_Bases)
nombres<-as.data.frame(nombres)

lista1<-as.data.frame(c("Archivo","Numero_lineas"))
lista1<-t(lista1)
lista<-as.data.frame(lista1)
archivo<-paste(carpeta,"Lineas.csv",sep = "/")
write.table(lista,file = archivo,row.names = FALSE,col.names = FALSE,sep = ",")
lista<-read.csv(archivo,header = TRUE,sep=",")
for(i in 1:length(nombres[,1]))
{
  archivo_temporal<-paste(carpeta_Bases,toString(nombres$nombres[i]),sep="/")
  temporal<-read.csv(archivo_temporal,header = TRUE,sep = ",")
  nombre<-substr(toString(nombres$nombres[i]),1,(str_length(nombres$nombres[i])-4))
  linea<-c(nombre,toString(length(temporal[,1])))
  linea<-t(linea)
  linea<-as.data.frame(linea)
  lista<-rbind(lista,linea)
}
lista1<-as.character(lista1)
colnames(lista)[1]<-"Archivo"
colnames(lista)[2]<-"Numero_lineas"

write.table(lista,file = archivo,row.names = FALSE,col.names = TRUE,sep = ",")

