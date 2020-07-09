

library(stringr)

carpeta<-"C:/Users/Administrador/Desktop/Programas/DividirBases"
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
  if(dir.exists(nombre_carpeta)){
  }else{dir.create(nombre_carpeta)}
  
  lista<-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-7")
  lista<-as.data.frame(lista)
  largo<-length(lista[,1])
  largo2<-as.integer(largo/2)
  
  base1<-lista[1:largo2,]
  
  base2<-lista[(largo2+1):largo,]
  write.csv(base1,file = paste(paste(nombre_carpeta,substr(toString(nombres$nombres[i]),1,(str_length(nombres$nombres[i])-4)),sep="/"),"csv",sep = "1."), row.names = FALSE)
  write.csv(base2,file = paste(paste(nombre_carpeta,substr(toString(nombres$nombres[i]),1,(str_length(nombres$nombres[i])-4)),sep="/"),"csv",sep = "2."), row.names = FALSE)
  
}


