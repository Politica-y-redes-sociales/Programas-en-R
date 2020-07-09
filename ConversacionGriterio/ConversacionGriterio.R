#Librerias
library(stringr)
library(sqldf)

carpeta = "C:/Users/interbarometro/Desktop/ProgramasR/ConversacionGriterio"
carpeta_bases = paste(carpeta, "Bases", sep="/")
archivo_cuentas = paste(carpeta_bases, "cuenta.csv", sep = "/")

nombres = dir(carpeta_bases)
nombres = as.data.frame(nombres)


if(dir.exists(paste(carpeta,"Resultados",sep = "/")))
{}else{dir.create(paste(carpeta,"Resultados",sep = "/"))}

cuentas  <- read.csv(archivo_cuentas, header = TRUE, sep = ",", encoding = "utf-8")
busqueda <- cuentas$Cuenta

  Conversacion = paste0("SELECT * FROM aux WHERE text LIKE '",busqueda,
                        "%' OR text LIKE '%",busqueda,"%'")

  Griterio = paste0("SELECT * FROM aux WHERE text NOT LIKE '",busqueda,
                    "%' AND text NOT LIKE '%",busqueda,"%'")
x = 1
ciclo = nrow(nombres)



while(x <= ciclo)
{
  nombre = substr(toString(nombres$nombres[x]),1,(str_length(nombres$nombres[x])-4)) 
  nombre_carpeta = paste(carpeta,"Resultados",sep = "/")
  nombresillo<-substr(toString(nombres$nombres[x]),1,(str_length(nombres$nombres[x])-4))
  nombre_carpeta <- paste(nombre_carpeta,nombresillo,sep="/")
  
  dir.create(nombre_carpeta)
  archivo_temporal = paste(carpeta_bases,toString(nombres$nombres[x]),sep="/")
  
  aux <- read.csv(archivo_temporal,header = TRUE,sep = ",", encoding = "utf-8")
  aux <- as.data.frame(aux)
  
      consulta_conversacion <- sqldf(Conversacion)
      consulta_griterio <- sqldf(Griterio)
      
      GriterioF = "SELECT * FROM consulta_griterio WHERE text LIKE '%bolsonaro%' OR text LIKE '%bolsonaro'"
      consulta_griteriof <- sqldf(GriterioF)
      
      # --- Total de Filas --- #
        filas <- sqldf("SELECT COUNT(*) Filas FROM aux")      
        totalFilas <- filas$Filas
      # --- -------------- --- #
      # --- Filas Griterio y Conversacion --- #
        filasC <- sqldf("SELECT COUNT(*) Filas FROM consulta_conversacion")
        filasG <- sqldf("SELECT COUNT(*) Filas FROM consulta_griteriof")
        totalGriterio <- filasG$Filas
        totalConversacion <- filasC$Filas
        
      # --- ----------------------------- --- #
      # --- Porcentaje Griterio y Conversacion --- #
        tabla <- matrix(c(trunc((totalGriterio/totalFilas)*100*10^2)/10^2,
                          trunc((totalConversacion/totalFilas)*100*10^2)/10^2,
                          "% Griterio","% Conversacion"),ncol = 2)
        colnames(tabla) <- c("Porcentaje","Nombre")
        tabla <- as.data.frame(tabla)
      # --- ---------------------------------- --- #
        
        write.csv(consulta_conversacion, file = paste(nombre_carpeta,"conversacion.csv",sep = "/"),row.names=FALSE)
        write.csv(consulta_griterio, file = paste(nombre_carpeta,"griterio.csv",sep = "/"),row.names=FALSE)
        write.csv(consulta_griteriof, file = paste(nombre_carpeta,"griteriofinal.csv",sep = "/"),row.names=FALSE)
        write.csv(tabla, file = paste(nombre_carpeta,"porcentajeGC.csv",sep = "/"),row.names=FALSE)
        
        
  x = x+1
}
