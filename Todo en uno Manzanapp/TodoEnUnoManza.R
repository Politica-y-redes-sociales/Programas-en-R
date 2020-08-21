#install.packages("sqldf")
#remove.packages("sqldf")
library(readr)
library(dplyr)
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

carpeta <- "/Users/alfonsoopazom/Desktop/Observatorio/ProgramasenR/Todo en uno Manzanapp"
carpeta_base <- paste(carpeta,"Bases",sep="/")
nombres <- dir(carpeta_base)
nombres <- as.data.frame(nombres)

pat ="(RT|via)(((?:\\b\\W*|)@\\w+)+)|,|:|(https|http)://t.co/[A-Za-z\\d]+|&amp;|http\\w*|@\\w+|(\\w+\\.|)\\w+\\.\\w+(/\\w+)*"
patref ="@[A-Za-z0-9]*[^\\s:_.<]"
patHashtag="#[A-Za-z0-9áéíóú]*"
patref2 ='c("[A-Za-z0-9áéíóú]*")'
pat3 = "[A-Za-z0-9_]+"

#Api Key para los graficos de plotly
Sys.setenv("plotly_username"="observatorio2019")
Sys.setenv("plotly_api_key"="BObHnm3fSEoIg5SCk3a2")

if(dir.exists(paste(carpeta, "Resultados", sep = "/")))
  ubicacion_resultados <-"/Users/alfonsoopazom/Desktop/Observatorio/ProgramasenR/Todo en uno Manzanapp/Resultados"
{}else
{
  dir.create(paste(carpeta, "Resultados", sep = "/"))
  #ubicacion carpeta resultados
  ubicacion_resultados <-"/Users/alfonsoopazom/Desktop/Observatorio/ProgramasenR/Todo en uno Manzanapp/Resultados"
  
  #If para crear los directorios dentro de la carpeta Resultados
  if((dir.exists(paste( ubicacion_resultados, "Evolucion", sep = "/")))
     &(dir.exists(paste( ubicacion_resultados, "Comunidad", sep = "/")))
     &(dir.exists(paste( ubicacion_resultados, "EfectosyExitos", sep = "/")))
     &(dir.exists(paste( ubicacion_resultados, "CaracteristicasTecnicas", sep = "/")))
     &(dir.exists(paste( ubicacion_resultados, "DeterminantesSemanticos", sep = "/"))))
  {}else
  {
    dir.create(paste( ubicacion_resultados, "Evolucion", sep = "/"))
    dir.create(paste( ubicacion_resultados, "Comunidad", sep = "/"))
    dir.create(paste( ubicacion_resultados, "EfectosyExitos", sep = "/"))
    dir.create(paste( ubicacion_resultados, "CaracteristicasTecnicas", sep = "/"))
    dir.create(paste( ubicacion_resultados, "DeterminantesSemanticos", sep = "/"))
  
    #if de las carpetas EVOLUCION Y SENTIDO
    if((dir.exists(paste(ubicacion_resultados,"Evolucion","Histograma", sep = "/")))
       & (dir.exists(paste(ubicacion_resultados,"Evolucion", "Grafico Torta", sep = "/"))))
    {}else
    {
      dir.create(paste(ubicacion_resultados,"Evolucion","Histograma", sep = "/"))
      dir.create(paste(ubicacion_resultados,"Evolucion","Grafico Torta", sep = "/"))
    }
    if((dir.exists(paste(ubicacion_resultados, "DeterminantesSemanticos", "Nube", sep = "/")))
       & (dir.exists(paste(ubicacion_resultados, "DeterminantesSemanticos", "Bigrama", sep = "/"))))
    {}else
    {
      dir.create(paste( ubicacion_resultados, "DeterminantesSemanticos", "Nube", sep = "/"))
      dir.create(paste( ubicacion_resultados, "DeterminantesSemanticos", "Bigrama", sep = "/"))
    }
    if((dir.exists(paste( ubicacion_resultados, "Comunidad","Referentes", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "Comunidad", "Influenciadores", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "Comunidad", "Movilizadores", sep = "/"))))
    {}else
    {
      dir.create(paste( ubicacion_resultados, "Comunidad", "Referentes", sep = "/"))
      dir.create(paste( ubicacion_resultados, "Comunidad", "Influenciadores", sep = "/"))
      dir.create(paste( ubicacion_resultados, "Comunidad", "Movilizadores", sep = "/"))
    }
    if((dir.exists(paste( ubicacion_resultados, "EfectosyExitos", "Aceptacion", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "EfectosyExitos","Viralizacion", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "EfectosyExitos","Muestra", sep = "/"))))
    {}else
    {
      dir.create(paste( ubicacion_resultados, "EfectosyExitos", "Aceptacion", sep = "/"))
      dir.create(paste( ubicacion_resultados, "EfectosyExitos","Viralizacion", sep = "/"))
      dir.create(paste( ubicacion_resultados, "EfectosyExitos","Muestra", sep = "/"))
    }
    if((dir.exists(paste( ubicacion_resultados, "CaracteristicasTecnicas","Caracteres", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "CaracteristicasTecnicas","Multimedia", sep = "/")))
       & (dir.exists(paste( ubicacion_resultados, "CaracteristicasTecnicas","Dispositivos", sep = "/"))))
    {}else
    {
      dir.create(paste( ubicacion_resultados, "CaracteristicasTecnicas", "Caracteres", sep = "/"))
      dir.create(paste( ubicacion_resultados, "CaracteristicasTecnicas", "Multimedia", sep = "/"))
      dir.create(paste( ubicacion_resultados, "CaracteristicasTecnicas", "Dispositivos", sep = "/"))
    }
  }
}
i = 1
numArchivos = nrow(nombres)

while(i <= numArchivos)
{
  nombre <- substr(toString(nombres$nombres[i]),1,(str_length(nombres$nombres[i])-4))
  nombre_carpeta <- paste(carpeta,"Resultados",sep = "/")
  archivo_temporal <- paste(carpeta_base,toString(nombres$nombres[i]),sep="/")
  nombreResultado <- nombres$nombres[i]
  aux <- read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  aux <- as.data.frame(aux)
  
  #--- Arreglo de los tildes columna Hashtags ---#
  aux$hashtags=gsub("\xf1",'ñ',aux$hashtags)#ñ
  aux$hashtags=gsub("\xe1","a",aux$hashtags)#a
  aux$hashtags=gsub("\xc1",'Á',aux$hashtags)#A
  aux$hashtags=gsub("\xe9","é",aux$hashtags)#e
  aux$hashtags=gsub("\xc9","É",aux$hashtags)#E
  aux$hashtags=gsub("\xed","í",aux$hashtags)#i
  aux$hashtags=gsub("\xcd","Í",aux$hashtags)#I
  aux$hashtags=gsub("\xf3",'ó',aux$hashtags)#o
  aux$hashtags=gsub("\xd3","Ó",aux$hashtags)#O
  aux$hashtags=gsub("\xda","Ú",aux$hashtags)#U
  aux$hashtags=gsub("\xfa","ú",aux$hashtags)#u
  aux$hashtags=gsub("\x40","@",aux$hashtags)#@
  #aux$hashtags=gsub("c([^0-9A-Za-z///' ])","",aux$hashtags)#@
  
  #--- Arreglo de los tildes columna Text ---#
  aux$text=gsub("<f1>",'ñ',aux$text)#ñ
  aux$text=gsub("<e1>","a",aux$text)#a
  aux$text=gsub("<c1>",'A',aux$text)#A
  aux$text=gsub("<e9>","e",aux$text)#e
  aux$text=gsub("<c9>","E",aux$text)#E
  aux$text=gsub("<ed>","i",aux$text)#i
  aux$text=gsub("<cd>","I",aux$text)#I
  aux$text=gsub("<f3>",'o',aux$text)#o
  aux$text=gsub("<d3>","O",aux$text)#O
  aux$text=gsub("<da>","U",aux$text)#U
  aux$text=gsub("<fa>","u",aux$text)#u
  aux$text=gsub("<40>","@",aux$text)#@
  
  #--- Eliminacion de tildes ---#
  #aux$text=gsub("á","a",aux$text)
  #aux$text=gsub("é","e",aux$text)
  #aux$text=gsub("í","i",aux$text)
  #aux$text=gsub("ó","o",aux$text)
  #aux$text=gsub("ú","u",aux$text)
  # --- Eliminacion de Emojis ---#
  aux$text <-gsub("[^\x30-\x7f]"," ",aux$text)
  
  # ---- Total de filas de la base de datos ----#
  total_filas <- sqldf("SELECT COUNT(user_id) totalDatos FROM aux")
  
  # --- EVOLUCION SENTIDO Y ESTILO --- #
  # --- HISTOGRAMA --- #
  histograma <- sqldf('SELECT substr(created_at,1,10) FECHA,
                      COUNT(substr(created_at,1,10)) CANTIDAD
                      FROM aux 
                      GROUP BY substr(created_at,1,10) 
                      ORDER BY substr(created_at,1,10) 
                      DESC')
  write.csv(histograma,file = paste(ubicacion_resultados,"Evolucion","Histograma","histogramax1dia.csv",sep = "/"),row.names=FALSE)
 
  # --- REPRODUCCION, PRODUCCION, INTERACCION --- #
  busqueda_RT <- sqldf("SELECT * FROM aux WHERE is_retweet = 1")
  
  busqueda_RTAI <- sqldf("SELECT * FROM busqueda_RT WHERE text NOT LIKE '@%' AND text NOT LIKE 'RT%'")
  
  busqueda_RTNAI <- sqldf("SELECT * FROM busqueda_RT WHERE text NOT LIKE '@%' AND text NOT LIKE 'RT%' AND text NOT LIKE '%@%'")
  
  total_RT <- sqldf("SELECT COUNT(user_id) Retweets FROM busqueda_RT")

  total_RTAI <- sqldf("SELECT COUNT(user_id) RetweetsAI FROM busqueda_RTAI")
  
  total_RTNAI <- sqldf("SELECT COUNT(user_id) RetweetsNAI FROM busqueda_RTNAI")
  # ===================================================== #
  busqueda_AA <- sqldf("SELECT * FROM aux WHERE text LIKE '@%'")

  busqueda_NART <- sqldf("SELECT * FROM aux WHERE text NOT LIKE '@%' AND text NOT LIKE 'RT%'")

  total_AA <- sqldf("SELECT COUNT(user_id) Arroba FROM busqueda_AA")
  
  total_NART = sqldf( "SELECT COUNT(user_id) noArrRet FROM busqueda_NART")
  # ===================================================== #
  
  # Grafico Torta #
  total = sqldf( "SELECT * FROM total_AA, total_NART, total_RT, total_RTAI, total_RTNAI, total_datos")
  
  #Valores para el grafico de Torta.
  AA = total$Arroba
  NART = total$noArrRet
  RT = total$Retweets
  RTAI = total$RetweetsAI
  RTNAI = total$RetweetsNAI
  N = total$totalDatos
  
  RIP = matrix(c(trunc((AA/N)*100*10^2)/10^2,trunc((NART/N)*100*10^2)/10^2,trunc((RT/N)*100*10^2)/10^2,"Interaccion","Produccion","Reproduccion"),ncol = 2)
  colnames(RIP) = c("Porcentaje","Tipo")
  RIP = as.data.frame(RIP)
  
  data <- RIP[,c('Porcentaje', 'Tipo')]
  
  p <- plot_ly(data, labels = ~Tipo, values = ~Porcentaje, type = 'pie') %>%
    layout(title = 'Produccion, Interaccion y Reproduccion',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  try(plotly_IMAGE(p, format = "png", out_file = paste(ubicacion_resultados,"Evolucion","Grafico Torta","ReproduccionInteraccionProduccion.png", sep = "/"),silent = TRUE))
  
  # --- DETERMINANTES SEMANTICOS --- #
  # --- NUBE ---#
  conectores<-read.csv(paste(carpeta,"conectores.csv",sep = "/"), header = FALSE)
  try(tempora_nube<-sqldf("SELECT text FROM aux"), silent = TRUE)
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
  
  # --- BIGRAMA --- #
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
  
  # --- COMUNIDADES --- #
  # --- REFERENTES --- #
  referentes <- unlist(regmatches(aux$mentions_screen_name,gregexpr(pat3,aux$mentions_screen_name)))
  referentes <- as.data.frame(referentes)
  referentes <- sqldf("SELECT referentes Cuenta, count(referentes) Menciones 
                      FROM referentes
                      GROUP BY Cuenta
                      ORDER BY Menciones
                      DESC LIMIT 50")
  
  write.csv(referentes, file = paste( ubicacion_resultados,"Comunidad", "Referentes","referetes.csv",sep = "/"),row.names=FALSE)
 
  # --- INFLUENCIADORES --- #
  influenciadores <- sqldf("SELECT retweet_screen_name 'Usuario_Retweeado',count(retweet_screen_name) Cantidad_Retweet 
                      FROM aux 
                      WHERE is_retweet 
                      GROUP BY retweet_screen_name 
                      ORDER BY COUNT(retweet_screen_name) DESC
                      LIMIT 20")
  write.csv(influenciadores,file = paste( ubicacion_resultados, "Comunidad", "Influenciadores","Influenciadores.csv",sep = "/"),row.names = FALSE)
  
  # ---MOVILIZADORES 2.0 ---#
 hashtags <- str_to_lower(unlist(regmatches(aux$hashtags,gregexpr(pat3,aux$hashtags))))
 hashtags <- as.data.frame(hashtags)
  
  ranking_hashtags <- sqldf("SELECT hashtags Hashtag, count(hashtags) Cantidad 
                            FROM hashtags
                            GROUP BY Hashtag
                            ORDER BY Cantidad
                            DESC LIMIT 50")
  
  cantidad_hashtag <- sqldf("SELECT count(hashtags) 'Porcentaje Hashtags'
                            FROM aux WHERE hashtags!=''")
  porcentaje_hashtags <-round((cantidad_hashtag/total_filas)*100,2)
  
  tabla <- matrix(c(total_filas,cantidad_hashtag,porcentaje_hashtags,
                    'Total Filas','Total Hashtags','Porcentaje'), ncol = 2)
  colnames(tabla) <- c('Valores','Datos')
  tabla <- as.data.frame(tabla)
  
  # --- Grafico ranking Hashtag --- #
  I = try(ranking_hashtags$Hashtag, silent = TRUE)
  S = try(ranking_hashtags$Cantidad, silent = TRUE)
  
  imagen_hashtag <- plot_ly ( ranking_hashtags, y = c(S), x = c(I), type = "bar", name = "Ranking Hashtag")%>%
        layout(title ="Ranking de Hashtag", yaxis = list(title = 'Cantidad'), xaxis = list(title='Hashtag'))
  plotly_IMAGE(imagen_hashtag,format= "png",out_file= paste(carpeta,"Resultados","Comunidad","Movilizadores","RankingHashtag.png",sep = "/"))
  
  write.csv(porcentaje_hashtags, file = paste(carpeta,"Resultados","Comunidad","Movilizadores","Porcentaje_ranking.csv",sep = "/"),row.names=FALSE)
  write.csv(ranking_hashtags, file <- paste(carpeta,"Resultados","Comunidad","Movilizadores","ranking_hashtags.csv", sep = "/"), row.names = FALSE)
  
  # --- EFECTOS Y EXITOS --- #
  # --- MUESTRA --- #
  for(i in 1:length(nombres[,1])){
    
    muestra <- sqldf('SELECT *  FROM 	aux ')
    x <-sample(1:length(aux[,1]),1000, replace = TRUE)
    x <- as.data.frame(x)
    if(length(muestra[,1])>=1000)
    {
      muestra<-muestra[x[,1],]
      archivo_final<- paste(carpeta,"Resultados","EfectosyExitos", "Muestra", sep = "/")
      write.csv(muestra,file = paste(archivo_final,nombres$nombres[i],sep = "/"),row.names = FALSE)
    }else{
      muestra <- muestra[1:length(aux[,1]),]
      archivo_final<- paste(carpeta,"Resultados","EfectosyExitos", "Muestra", sep = "/")
      write.csv(muestra,file = paste(archivo_final,nombres$nombres[i],sep = "/"),row.names = FALSE)
    }
  }
  
  # --- ACEPTACION --- #
  aceptacion <- sqldf("SELECT user_id as ID_usuario,
                MAX(favorite_count) as Max_favoritos,
                MIN(favorite_count) as Min_favoritos,
                AVG(favorite_count) as Promedio FROM aux")
                
  aceptacionTweet <- sqldf("SELECT  user_id as ID_usuario,
                      MAX(favorite_count) as Max_favoritos,
                      text as Tweet FROM aux")
  
  write.csv(aceptacion, file = paste( ubicacion_resultados, "EfectosyExitos", "Aceptacion","aceptacion.csv",sep="/" ),row.names = FALSE)
  write.csv(aceptacionTweet, file = paste( ubicacion_resultados, "EfectosyExitos", "Aceptacion","aceptacionTweet.csv",sep="/" ),row.names = FALSE)

  # --- VIRALIZACION --- #
  viralizacionTweet <- sqldf("SELECT user_id as ID_usuario,
                        max(retweet_count) as Max_retweet,
                        text as Tweet
                        FROM aux")
                        
  viralizacion <- sqldf("SELECT user_id as ID_usuario,
                  max(retweet_count) as Max_retweet,
                  min(retweet_count) as Min_retweet,
                  avg(retweet_count) as Promedio
                  FROM aux")
  
  write.csv(viralizacion, file = paste( ubicacion_resultados, "EfectosyExitos","Viralizacion","viralizacion.csv",sep="/" ),row.names = FALSE)
  write.csv(viralizacionTweet, file = paste( ubicacion_resultados, "EfectosyExitos","Viralizacion","viralizacionTweet.csv",sep="/" ),row.names = FALSE)
  
  # --- CARACTERISTICAS TECNICAS --- #
  # ---  + - x CARACTERES  --- #
  cantidadCaracteres <- sqldf("SELECT max(display_text_width) as Max_caracteres,
                          min(display_text_width) as Min_caracteres, 
                          avg(display_text_width) as Promedio FROM aux")
  write.csv(cantidadCaracteres, file = paste( ubicacion_resultados, "CaracteristicasTecnicas", "Caracteres","NumeroCaracteres.csv",sep="/"),row.names = FALSE)

  # --- CONTENIDO MULTIMEDIA --- #
  try(cantidad_link <- sqldf("SELECT COUNT(urls_url) 'Porcentaje Links' FROM aux WHERE urls_url NOT LIKE ''"), silent = TRUE)
  
  try(ranking_url <- sqldf("SELECT urls_url Url, count(urls_url) Cantidad FROM aux
                  WHERE urls_url NOT LIKE ''
                  GROUP BY urls_url
                  ORDER BY cantidad DESC
                  LIMIT 5"), silent = TRUE)
  try(cantidad_fotos <-sqldf("SELECT count(media_type) 'Porcentaje Fotos' FROM aux WHERE  media_type != ''"), silent = TRUE)
  
  try(porcentaje_links <- round((cantidad_link/total_filas)*100,3), silent = TRUE)
  try(porcentaje_fotos <- round((cantidad_fotos/total_filas)*100,3), silent = TRUE)
  
  try(write.csv(ranking_url,file = paste(carpeta,"Resultados","CaracteristicasTecnicas","Multimedia","RankingUrl.csv",sep = "/"),row.names = FALSE), silent = TRUE)
  try(write.csv(porcentaje_links, file=paste(carpeta,"Resultados","CaracteristicasTecnicas","Multimedia","PorcentajeLink.csv",sep="/"),row.names = FALSE), silent = TRUE)
  try(write.csv(porcentaje_fotos, file=paste(carpeta,"Resultados","CaracteristicasTecnicas","Multimedia","PorcentajeFotos.csv",sep="/"),row.names = FALSE), silent = TRUE)
  
  # --- Grafico Ranking URLS --- #
  ranking_url <- as.data.frame(ranking_url)
  N <- ranking_url$Url
  U <- ranking_url$Cantidad
  
  imagen_ranking <- plot_ly ( ranking_url, y = c(U),x = c(N), type = "bar", name = "Ranking URL")%>%
    layout(title ="Ranking de URL", yaxis = list(title = 'Urls'), xaxis = list(title='Cantidad'))
  
  try(plotly_IMAGE(imagen_ranking, format = "png", out_file = paste(carpeta,"Resultados","CaracteristicasTecnicas","Multimedia","RankingUrl.png",sep = "/")),silent = TRUE)
  
  # --- CATEGORIZACION POR DISPOSITIVO --- #
  dispositivos <- sqldf( "SELECT COUNT(source) Total FROM aux")

  androids <- sqldf( "SELECT COUNT(source) Androids FROM aux WHERE source LIKE '%Android'")
  
  iphone <- sqldf( "SELECT COUNT(source) Iphone FROM aux WHERE source LIKE '%iPhone'")
  
  web <- sqldf( "SELECT COUNT(source) Web FROM aux WHERE source LIKE '%Web%'")
  
  otros <- sqldf("SELECT COUNT(source) Otros FROM aux WHERE source NOT LIKE '%Android' AND source NOT LIKE '%iPhone' AND source NOT LIKE '%Web%'")

  consulta_union <- sqldf("SELECT * FROM androids, iphone, web, otros, dispositivos")
  
  write.csv(consulta_union, file = paste( ubicacion_resultados, "CaracteristicasTecnicas","Dispositivos","UnionDispositivos.csv",sep = "/"),row.names = FALSE)

  A = consulta_union$Androids
  I = consulta_union$Iphone
  W = consulta_union$Web
  O = consulta_union$Otros
  TT = consulta_union$Total
  
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





