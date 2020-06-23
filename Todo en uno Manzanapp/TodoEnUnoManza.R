#install.packages("gsubfn")
#install.packages("usethis")
#Actalizacion de paquetes 
#update.packages()
#install_github("DFJL/SamplingUtil")

library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(tidytext)
library(tm)
library(colorspace)
library(plotly)
library(sqldf)
library(gsubfn)
library(devtools)
library(SamplingUtil)

carpeta <- "/Users/alfonsoopazo/Desktop/Observatorio/Manzanapp"
carpeta_base <- paste(carpeta,"Bases",sep="/")
nombres <- dir(carpeta_base)
nombres <- as.data.frame(nombres)

pat ="(RT|via)(((?:\\b\\W*|)@\\w+)+)|,|:|(https|http)://t.co/[A-Za-z\\d]+|&amp;|http\\w*|@\\w+|(\\w+\\.|)\\w+\\.\\w+(/\\w+)*"
patref ="@[A-Za-z0-9]*[^\\s:_.<]"
patHashtag="#[A-Za-z0-9áéíóú]*"

#Api Key para los graficos de plotly
Sys.setenv("plotly_username"="observatorio2019")
Sys.setenv("plotly_api_key"="BObHnm3fSEoIg5SCk3a2")

if(dir.exists(paste(carpeta, "Resultados", sep = "/")))
  ubicacion_resultados <-"/Users/alfonsoopazo/Desktop/Observatorio/Manzanapp/Resultados"
{}else
{
  dir.create(paste(carpeta, "Resultados", sep = "/"))
  #ubicacion carpeta resultados
  ubicacion_resultados <-"/Users/alfonsoopazo/Desktop/Observatorio/Manzanapp/Resultados"
  
  #If para crear los directorios dentro de la carpeta Resultados
  if((dir.exists(paste( ubicacion_resultados, "Evolucion", sep = "/")))
     &(dir.exists(paste( ubicacion_resultados, "Comunidad", sep = "/")))
     &(dir.exists(paste( ubicacion_resultados, "EfectosyExitos", sep = "/")))
     &(dir.exists(paste( ubicacion_resultados, "CaracteristicasTecnicas", sep = "/")))
     &(dir.exists(paste( ubicacion_resultados, "DeterminantesSemanticos", sep = "/")))
     &(dir.exists(paste( ubicacion_resultados, "ResultadosGenerales", sep = "/"))))
  {}else
  {
    dir.create(paste( ubicacion_resultados, "Evolucion", sep = "/"))
    dir.create(paste( ubicacion_resultados, "Comunidad", sep = "/"))
    dir.create(paste( ubicacion_resultados, "EfectosyExitos", sep = "/"))
    dir.create(paste( ubicacion_resultados, "CaracteristicasTecnicas", sep = "/"))
    dir.create(paste( ubicacion_resultados, "DeterminantesSemanticos", sep = "/"))
    dir.create(paste( ubicacion_resultados, "ResultadosGenerales", sep = "/"))
    
  
    #if de las carpetas EVOLUCION Y SENTIDO
    if((dir.exists(paste(ubicacion_resultados,"Evolucion","Histograma", sep = "/")))
       & (dir.exists(paste(ubicacion_resultados,"Evolucion", "Grafico Torta", sep = "/"))))
    {}else
    {
      dir.create(paste(ubicacion_resultados,"Evolucion","Histograma", sep = "/"))
      dir.create(paste(ubicacion_resultados,"Evolucion","Grafico Torta", sep = "/"))
    }
    
    # if de las carpetas DETERMINANTES SEMANTICOS
    if((dir.exists(paste(ubicacion_resultados, "DeterminantesSemanticos", "Nube", sep = "/")))
       & (dir.exists(paste(ubicacion_resultados, "DeterminantesSemanticos", "Bigrama", sep = "/"))))
    {}else
    {
      dir.create(paste( ubicacion_resultados, "DeterminantesSemanticos", "Nube", sep = "/"))
      dir.create(paste( ubicacion_resultados, "DeterminantesSemanticos", "Bigrama", sep = "/"))
    }
    # if de las carpetas COMUNIDADES 
    if((dir.exists(paste( ubicacion_resultados, "Comunidad","Referentes", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "Comunidad", "Influenciadores", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "Comunidad", "Movilizadores", sep = "/"))))
    {}else
    {
      dir.create(paste( ubicacion_resultados, "Comunidad", "Referentes", sep = "/"))
      dir.create(paste( ubicacion_resultados, "Comunidad", "Influenciadores", sep = "/"))
      dir.create(paste( ubicacion_resultados, "Comunidad", "Movilizadores", sep = "/"))
    }
    
    # if de las carpetas EfectosyExitos 
    if((dir.exists(paste( ubicacion_resultados, "EfectosyExitos", "Aceptacion", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "EfectosyExitos","Viralizacion", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "EfectosyExitos","Muestra", sep = "/"))))
    {}else
    {
      dir.create(paste( ubicacion_resultados, "EfectosyExitos", "Aceptacion", sep = "/"))
      dir.create(paste( ubicacion_resultados, "EfectosyExitos","Viralizacion", sep = "/"))
      dir.create(paste( ubicacion_resultados, "EfectosyExitos","Muestra", sep = "/"))
    }
    
    # if de las carpetas CARACTERISTICAS TECNICAS 
    if((dir.exists(paste( ubicacion_resultados, "CaracteristicasTecnicas","Caracteres", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "CaracteristicasTecnicas","Multimedia", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "CaracteristicasTecnicas","Dispositivos", sep = "/"))))
    {}else
    {
      dir.create(paste( ubicacion_resultados, "CaracteristicasTecnicas", "Caracteres", sep = "/"))
      dir.create(paste( ubicacion_resultados, "CaracteristicasTecnicas", "Multimedia", sep = "/"))
      dir.create(paste( ubicacion_resultados, "CaracteristicasTecnicas", "Dispositivos", sep = "/"))
     
      
      # Crear las subcarpetas de Caracteristicas tecnicas 
    }
  }
}

i = 1
numArchivos = nrow(nombres)

while(i <= numArchivos)
{
  nombre = substr(toString(nombres$nombres[i]),1,(str_length(nombres$nombres[i])-4))
  nombre_carpeta = paste(carpeta,"Resultados",sep = "/")
  archivo_temporal = paste(carpeta_base,toString(nombres$nombres[i]),sep="/")
  nombreResultado = nombres$nombres[i]
  consulta <-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  
  
  #--- Arreglo de los tildes ---#
  consulta$text=gsub("<f1>","?",consulta$text)#ñ
  consulta$text=gsub("<e1>","á",consulta$text)#a
  consulta$text=gsub("<c1>","Á",consulta$text)#A
  consulta$text=gsub("<e9>","é",consulta$text)#e
  consulta$text=gsub("<c9>","É",consulta$text)#E
  consulta$text=gsub("<ed>","í",consulta$text)#i
  consulta$text=gsub("<cd>","Í",consulta$text)#I
  consulta$text=gsub("<f3>","ó",consulta$text)#o
  consulta$text=gsub("<d3>","Ó",consulta$text)#O
  consulta$text=gsub("<fa>","ú",consulta$text)#u
  consulta$text=gsub("<da>","Ú",consulta$text)#U
  consulta$text=gsub("<40>","?",consulta$text)#@
  
  
  #Archivos 
  archivolink = paste("porcentajelink",nombreResultado,".csv")
  archivoRanking = paste("ranking",nombreResultado,".csv")
  archivoCaracteres = paste("caracteres",nombreResultado,".csv")
  archivoAceptacion = paste("aceptacion",nombreResultado,".csv")
  archivoAceptacionTweet = paste("aceptacionTweet",nombreResultado,".csv")
  archivoViralizacion = paste("viralizacion",nombreResultado,".csv")
  archivoViralizacionTweet = paste("viralizacionTweet",nombreResultado,".csv")
  archivoReferentes = paste("referentes",nombreResultado,".csv")
  archivoInfluenciadores = paste("influenciadores",nombreResultado,".csv")
  archivoGeoreferencia = paste("georeferencia",nombreResultado,".csv")
  archivoRankingGeo = paste("ranking_georeferencia",nombreResultado,".csv")
  archivoFotos = paste("cantidad_fotos", nombreResultado,".csv")
  archivoUnionDispositivos = paste("consulta_union",nombreResultado,".csv")
  archivoMovilizadores = paste("datos_movilizadores", nombreResultado, ".csv")
  archivoMuestra = paste("muestra",nombreResultado,".csv")

  
  aux <- read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  aux <- as.data.frame(aux)
  
  try(temporal <- sqldf("SELECT created_at,screen_name,text FROM aux"), silent = TRUE)
  tempora_nube <- temporal
  
  
  # --- Evolucion y Sentido --- #
  # --- Histograma --- #
  histograma <- sqldf('SELECT substr(created_at,1,10) FECHA,
                      COUNT(substr(created_at,1,10)) CANTIDAD
                      FROM aux 
                      GROUP BY substr(created_at,1,10) 
                      ORDER BY substr(created_at,1,10) 
                      DESC')
  write.csv(histograma,file = paste(ubicacion_resultados,"Evolucion","Histograma","histogramax1dia.csv",sep = "/"),row.names=FALSE)
  write.csv(histograma,file = paste( ubicacion_resultados, "ResultadosGenerales","histogramax1diaA.csv",sep = "/"),row.names=FALSE)
  
  # --- Reproduccion, Produccion, Interaccion --- #
  consulta_total_datos = "SELECT COUNT(user_id) totalDatos FROM aux"
  total_datos = sqldf(consulta_total_datos)
  # ===================================================== #
  #Para encontrar retweet cuya columna tiene valores boolean, si es TRUE se pone = 1 y si es FALSE se pone = 0.
  consulta_RT = "SELECT * FROM aux WHERE is_retweet = 1"
  busqueda_RT = sqldf(consulta_RT)
  
  consulta_RTAI = "SELECT * FROM busqueda_RT WHERE text NOT LIKE '@%' AND text NOT LIKE 'RT%'"
  busqueda_RTAI = sqldf(consulta_RTAI)
  
  consulta_RTNAI = "SELECT * FROM busqueda_RT WHERE text NOT LIKE '@%' AND text NOT LIKE 'RT%' AND text NOT LIKE '%@%'"
  busqueda_RTNAI = sqldf(consulta_RTNAI)
  
  consulta_total_RT = "SELECT COUNT(user_id) Retweets FROM busqueda_RT"
  total_RT = sqldf(consulta_total_RT)
  
  consulta_total_RTAI = "SELECT COUNT(user_id) RetweetsAI FROM busqueda_RTAI"
  total_RTAI = sqldf(consulta_total_RTAI)
  
  consulta_total_RTNAI = "SELECT COUNT(user_id) RetweetsNAI FROM busqueda_RTNAI"
  total_RTNAI = sqldf(consulta_total_RTNAI)
  # ===================================================== #
  consulta_AA = "SELECT * FROM aux WHERE text LIKE '@%'"
  busqueda_AA = sqldf(consulta_AA)
  
  consulta_NART = "SELECT * FROM aux WHERE text NOT LIKE '@%' AND text NOT LIKE 'RT%'"
  busqueda_NART = sqldf(consulta_NART)
  
  consulta_total_AA = "SELECT COUNT(user_id) Arroba FROM busqueda_AA"
  total_AA = sqldf(consulta_total_AA)
  
  consulta_total_NART = "SELECT COUNT(user_id) noArrRet FROM busqueda_NART"
  total_NART = sqldf(consulta_total_NART)
  # ===================================================== #
  
  # Grafico Torta #
  consulta_total = "SELECT * FROM total_AA, total_NART, total_RT, total_RTAI, total_RTNAI, total_datos"
  total = sqldf(consulta_total)
  
  #Valores para el grafico de Torta.
  AA = total$Arroba
  NART = total$noArrRet
  RT = total$Retweets
  RTAI = total$RetweetsAI
  RTNAI = total$RetweetsNAI
  N = total$totalDatos
  
  #Porcentaje para el grafico.
  RIP = matrix(c(trunc((AA/N)*100*10^2)/10^2,trunc((NART/N)*100*10^2)/10^2,trunc((RT/N)*100*10^2)/10^2,"Interaccion","Produccion","Reproduccion"),ncol = 2)
  colnames(RIP) = c("Porcentaje","Tipo")
  RIP = as.data.frame(RIP)
  
  data <- RIP[,c('Porcentaje', 'Tipo')]
  
  p <- plot_ly(data, labels = ~Tipo, values = ~Porcentaje, type = 'pie') %>%
    layout(title = 'Produccion, Interaccion y Reproduccion',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p
  
  try(plotly_IMAGE(p, format = "png", out_file = paste(ubicacion_resultados,"Evolucion","Grafico Torta","ReproduccionInteraccionProduccion.png", sep = "/"),silent = TRUE))
  
  # --- Determinantes Semanticos --- #
  # --- Nube --- # Nueva nube
  conectores<-read.csv(paste(carpeta,"conectores.csv",sep = "/"), header = FALSE)
  try(tempora_nube<-sqldf("SELECT text FROM consulta"), silent = TRUE)
  tempora_nube<-mutate(tempora_nube,text = str_replace_all(text,pat, ""))
  lis<-unnest_tokens(tempora_nube,word, text, token="ngrams",n=1 )
  nube<-count(lis,word,sort=TRUE)
  nube<-as.data.frame(nube)
  conectores<-as.data.frame(conectores)
  consulta_conectores <- paste0(paste("SELECT * FROM nube WHERE word!=",conectores$V1[1],sep = "'" ),"'")
  
  for(j in 2:length(conectores[,1]))
  {
    consulta_conectores <- paste0(paste(consulta_conectores,conectores$V1[j],sep = " and word !='"),"'")
  }
  try(nube <- sqldf(consulta_conectores),outFile = getOption("try.outFile", default = stderr()),silent = TRUE)
  
  write.csv(nube, file = paste(ubicacion_resultados,"DeterminantesSemanticos","Nube","nube.csv",sep = "/"),row.names=FALSE)
  write.csv(nube, file = paste(ubicacion_resultados, "ResultadosGenerales","nubeA.csv",sep = "/"),row.names=FALSE)
  
  # --- Bigrama --- #
    lol=aux %>%
    mutate(text = str_replace_all(text,pat, "")) %>%
    unnest_tokens(word, text, token="ngrams",n=2 ) %>%
    count(word, sort=TRUE) %>% 
    separate(word, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>% 
    filter(!word2 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>%
    filter(!str_detect(word1,"\\d+")) %>% 
    filter(!str_detect(word2,"\\d+")) %>% 
    unite(bigrama, word1, word2, sep = " ") %>%
    mutate(bigrama=reorder(bigrama,n)) %>% 
    top_n(13,n) %>%
    ungroup()
  
  # --- Guardar Bigrama --- #
  
    data_bigrama=aux %>%
    mutate(text = str_replace_all(text,pat, "")) %>%
    unnest_tokens(word, text, token="ngrams",n=2 ) %>%
    count(word, sort=TRUE) %>% 
    separate(word, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>% 
    filter(!word2 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>%
    filter(!str_detect(word1,"\\d+")) %>% 
    filter(!str_detect(word2,"\\d+")) %>% 
    unite(bigrama, word1, word2, sep = " ") %>%
    mutate(bigrama=reorder(bigrama,n)) %>% 
    ungroup()
  
  write.csv(data_bigrama, file = paste(ubicacion_resultados, "DeterminantesSemanticos", "Bigrama","data_bigrama.csv",sep = "/"))
  write.csv(data_bigrama, file = paste( ubicacion_resultados, "ResultadosGenerales","data_bigramaA.csv",sep = "/"))
  
  
  # --- Comunidades --- #
  # --- Referentes --- #
  referentes = "SELECT Distinct(mentions_screen_name) as Menciones FROM aux"
  referentes = sqldf(referentes)
  
  write.csv(referentes, file = paste( ubicacion_resultados, "Comunidad", "Referentes",archivoReferentes,sep = "/"),row.names=FALSE)
  write.csv(referentes, file = paste( ubicacion_resultados, "ResultadosGenerales",archivoReferentes,sep = "/"),row.names=FALSE)
  
  # --- Influenciadores --- #
  influenciadores <- "SELECT retweet_screen_name USUARIO,count(retweet_screen_name) CANTIDAD FROM aux 
                     WHERE is_retweet 
                     GROUP BY retweet_screen_name 
                     ORDER BY count(retweet_screen_name) DESC"
  
  influenciadores <- sqldf(influenciadores)
  
  write.csv(influenciadores,file = paste( ubicacion_resultados, "Comunidad", "Influenciadores",archivoInfluenciadores,sep = "/"),row.names = FALSE)
  write.csv(influenciadores,file =  paste( ubicacion_resultados, "ResultadosGenerales",archivoInfluenciadores,sep = "/"),row.names = FALSE)
  
  # --- Movilizadores 2.0 ---#
  #Cantidad que usan Hashtag
  cantidad_hashtag <- "SELECT count(text) Cantidad_Hastags FROM aux WHERE text LIKE '%#%'"
  cantidad_hashtag <- sqldf(cantidad_hashtag)
  #Cantidad que NO usaN Hashtag
  no_hashtag <- "SELECT count(text) NO_Hastags FROM aux WHERE text NOT LIKE '%#%'"
  no_hashtag <- sqldf(no_hashtag)
  
  #Porcentaje de los que usan Hashtag
  H <- cantidad_hashtag$Cantidad_Hastags
  TOTAL <- total_datos$totalDatos
  
  calculo <- (round((H/TOTAL),3)*100)
  calculo <- as.data.frame(calculo)
  colnames(calculo) <- c('Procentajes')
  
  total_hashtag <- "SELECT * FROM cantidad_hashtag,no_hashtag,calculo"
  total_hashtag <- sqldf(total_hashtag)
  write.csv(total_hashtag, file = paste(ubicacion_resultados,"Comunidad","Movilizadores", archivoMovilizadores,sep = "/"), row.names = FALSE)
  
  #Ranking Hashtag
  ranking_hashtag  <- "SELECT DISTINCT(text) FROM aux WHERE text LIKE '%#%'"
  ranking_hashtag <- sqldf(ranking_hashtag)
  
  # --- Contenidos multimedia --- #
  consulta_ranking <- "SELECT count(urls_url) as N_URL, urls_url URL 
                        FROM aux
                        GROUP BY urls_url
                        ORDER BY N_URL DESC
                        LIMIT 10"
  
  columnas_link <-"SELECT count(distinct(urls_url)) as Porcentaje FROM aux"
  
  total_filas <-"SELECT count(user_id)  FROM aux"
  
  cantidad_fotos <-"SELECT COUNT(media_url) as '% de fotos' FROM aux"
  
  try(ranking <- sqldf(consulta_ranking), silent = TRUE)
  links <- sqldf(columnas_link)
  try(total_filas <- sqldf(total_filas), silent = TRUE)
  porcentaje_links <- round((links/total_filas)*100,3)
  try(cantidad_fotos <- sqldf(cantidad_fotos), silent = TRUE)
  fotos <- round((cantidad_fotos/total_filas)*100,3)
  
  write.csv(ranking,file = paste( ubicacion_resultados, "CaracteristicasTecnicas", "Multimedia",archivoRanking,sep = "/"),row.names = FALSE)
  write.csv(porcentaje_links, file=paste( ubicacion_resultados, "CaracteristicasTecnicas", "Multimedia",archivolink,sep="/"),row.names = FALSE)
  write.csv(fotos, file=paste( ubicacion_resultados, "CaracteristicasTecnicas", "Multimedia",archivoFotos,sep="/"),row.names = FALSE)
  write.csv(ranking,file =  paste( ubicacion_resultados, "ResultadosGenerales",archivoRanking,sep = "/"),row.names = FALSE)
  write.csv(porcentaje_links, file= paste( ubicacion_resultados, "ResultadosGenerales",archivolink,sep="/"),row.names = FALSE)
  write.csv(fotos, file= paste( ubicacion_resultados, "ResultadosGenerales",archivoFotos,sep="/"),row.names = FALSE)
  
  # --- Efectos y exitos --- #
  # --- EfectosyExitos - Muestra --- #
  for(i in 1:length(nombres[,1])){
    
    muestra <- sqldf('SELECT "screen_name","user_id","status_id","text", "retweet_text","is_retweet"  FROM	consulta ')
    x <-sample(1:length(consulta[,1]),1000,replace = FALSE)
    x <- as.data.frame(x)
    
    if(length(muestra[,1])>=1000)
    {
      muestra<-muestra[x[,1],]
      archivo_final<-paste(ubicacion_resultados,"EfectosyExitos","Muestra", sep = "/")
      write.csv(muestra,file = paste(archivo_final,nombres$nombres[i],sep = "/"),row.names = FALSE)
      
    }else{
      muestra <- muestra[1:length(muestra[,1]),]
      archivo_final<-paste(ubicacion_resultados,"EfectosyExitos","Muestra", sep = "/")
      write.csv(muestra,file = paste(archivo_final,nombres$nombres[i],sep = "/"),row.names = FALSE)
    }
  }
  
  # --- Aceptacion --- #
  aceptacion <- "SELECT user_id as ID_usuario,
                MAX(favorite_count) as Max_favoritos,
                MIN(favorite_count) as Min_favoritos,
                AVG(favorite_count) as Promedio FROM aux"
                
  aceptacionTweet <- "SELECT  user_id as ID_usuario,
                      MAX(favorite_count) as Max_favoritos,
                      text as Tweet FROM aux"
                      
  try(aceptacion <- sqldf(aceptacion),silent = TRUE)
  try(aceptacionTweet <- sqldf(aceptacionTweet), silent = TRUE)
  
  write.csv(aceptacion, file = paste( ubicacion_resultados, "EfectosyExitos", "Aceptacion",archivoAceptacion,sep="/" ),row.names = FALSE)
  write.csv(aceptacionTweet, file = paste( ubicacion_resultados, "EfectosyExitos", "Aceptacion",archivoAceptacionTweet,sep="/" ),row.names = FALSE)
  write.csv(aceptacion, file =  paste( ubicacion_resultados, "ResultadosGenerales",archivoAceptacion,sep="/" ),row.names = FALSE)
  write.csv(aceptacionTweet, file =  paste( ubicacion_resultados, "ResultadosGenerales",archivoAceptacionTweet,sep="/" ),row.names = FALSE)
  
  # --- Viralizacion --- #
  viralizacionTweet <- "SELECT user_id as ID_usuario,
                        max(retweet_count) as Max_retweet,
                        text as Tweet
                        FROM aux"
                        
  viralizacion <- "SELECT user_id as ID_usuario,
                  max(retweet_count) as Max_retweet,
                  min(retweet_count) as Min_retweet,
                  avg(retweet_count) as Promedio
                  FROM aux"
                  
  try(viralizacion <- sqldf(viralizacion), silent = TRUE)
  try(viralizacionTweet <- sqldf(viralizacionTweet), silent = TRUE)
  
  write.csv(viralizacion, file = paste( ubicacion_resultados, "EfectosyExitos","Viralizacion",archivoViralizacion,sep="/" ),row.names = FALSE)
  write.csv(viralizacionTweet, file = paste( ubicacion_resultados, "EfectosyExitos","Viralizacion",archivoViralizacionTweet,sep="/" ),row.names = FALSE)
  write.csv(viralizacion, file =  paste( ubicacion_resultados, "ResultadosGenerales",archivoViralizacion,sep="/" ),row.names = FALSE)
  write.csv(viralizacionTweet, file =  paste( ubicacion_resultados, "ResultadosGenerales",archivoViralizacionTweet,sep="/" ),row.names = FALSE)
  
  # --- Caracteristicas Tecnicas --- #
  # ---  + - x Caracteres  --- #
  cantidadCaracteres <- "SELECT max(display_text_width) as Max_caracteres,
                          min(display_text_width) as Min_caracteres, 
                          avg(display_text_width) as Promedio FROM aux"
  cantidadCaracteres <- sqldf(cantidadCaracteres)
  
  write.csv(cantidadCaracteres, file = paste( ubicacion_resultados, "CaracteristicasTecnicas", "Caracteres",archivoCaracteres,sep="/"),row.names = FALSE)
  write.csv(cantidadCaracteres, file = paste( ubicacion_resultados, "ResultadosGenerales",archivoCaracteres,sep="/"),row.names = FALSE)
  
  # --- Contenidos multimedia --- #
  
  # ---  EfectosyExitos por dispositivo --- #
  consulta_dispositivos <- "SELECT COUNT(source) Total FROM aux"
  dispositivos <- sqldf(consulta_dispositivos)
  
  dispositivos_android <- "SELECT COUNT(source) Androids FROM aux WHERE source LIKE '%Android'"
  androids <- sqldf(dispositivos_android)
  
  consulta_iphone <- "SELECT COUNT(source) Iphone FROM aux WHERE source LIKE '%iPhone'"
  iphone <- sqldf(consulta_iphone)
  
  consulta_web <- "SELECT COUNT(source) Web FROM aux WHERE source LIKE '%Web%'"
  web <- sqldf(consulta_web)
  
  consulta_otros <- "SELECT COUNT(source) Otros FROM aux WHERE source NOT LIKE '%Android' AND source NOT LIKE '%iPhone' AND source NOT LIKE '%Web%'"
  otros <- sqldf(consulta_otros)
  
  consulta_union = "SELECT * FROM androids, iphone, web, otros, dispositivos"
  consulta_union = sqldf(consulta_union)
  todos = consulta_union
  
  write.csv(consulta_union, file = paste( ubicacion_resultados, "CaracteristicasTecnicas","Dispositivos",archivoUnionDispositivos,sep = "/"),row.names = FALSE)
  write.csv(consulta_union, file =paste( ubicacion_resultados, "ResultadosGenerales",archivoUnionDispositivos,sep = "/"),row.names = FALSE)
  
  A = todos$Androids
  I = todos$Iphone
  W = todos$Web
  O = todos$Otros
  TT = todos$Total
  
  porcentaje = matrix(c(trunc((A/TT)*100*10^2)/10^2,trunc((I/TT)*100*10^2)/10^2,trunc((W/TT)*100*10^2)/10^2,trunc((O/TT)*100*10^2)/10^2,'Android','Iphone','Web','Otros'),ncol = 2)
  colnames(porcentaje) = c('Porcentaje','Dispositivos')
  porcentaje = as.data.frame(porcentaje)
  
  data <- porcentaje[,c('Dispositivos', 'Porcentaje')]
  #
  p <- plot_ly(data, labels = ~Dispositivos, values = ~Porcentaje, type = 'pie') %>%
    layout(title = 'Porcentaje de Dispositivos',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  
  
  try(plotly_IMAGE(p, format = "png", out_file = paste(ubicacion_resultados,"CaracteristicasTecnicas","Dispositivos","PorcentajeDispositivos.png", sep = "/"), silent = TRUE))
  

  
  i = i + 1
  
}





