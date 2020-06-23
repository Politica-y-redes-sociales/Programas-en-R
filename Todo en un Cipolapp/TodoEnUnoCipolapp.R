#install.packages("dplyr")
#install.packages("sqldf")
#Actalizacion de paquetes 
#update.packages()

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
library(RSQLite)


carpeta = "/Users/alfonsoopazo/Desktop/Observatorio/Todo en un Cipolapp"
carpeta_base = paste(carpeta,"Bases",sep="/")
nombres = dir(carpeta_base)
nombres = as.data.frame(nombres)

pat ="(RT|via)(((?:\\b\\W*|)@\\w+)+)|,|:|(https|http)://t.co/[A-Za-z\\d]+|&amp;|http\\w*|@\\w+|(\\w+\\.|)\\w+\\.\\w+(/\\w+)*"
patref ="@[A-Za-z0-9]*[^\\s:_.<]"
patref2 ='c("[A-Za-z0-9áéíóú]*")'
patHashtag="#[A-Za-z0-9áéíóú]*"

Sys.setenv("plotly_username"="observatorio2019")
Sys.setenv("plotly_api_key"="BObHnm3fSEoIg5SCk3a2")


if(dir.exists(paste(carpeta, "Resultados", sep = "/")))
{}else
{
  dir.create(paste(carpeta, "Resultados", sep = "/"))

  #If para crear los directorios dentro de la carpeta Resultados
  if((dir.exists(paste(carpeta,"Resultados","Evolucion", sep = "/")))
     &(dir.exists(paste(carpeta,"Resultados","Comunidad", sep = "/")))
     &(dir.exists(paste(carpeta,"Resultados","Efectos", sep = "/")))
     &(dir.exists(paste(carpeta,"Resultados","CaracteristicasTecnicas", sep = "/")))
     &(dir.exists(paste(carpeta,"Resultados", "DeterminantesSemanticos", sep = "/")))
     &(dir.exists(paste(carpeta,"Resultados", "ResultadosGenerales", sep = "/"))))
  {}else
  {
    dir.create(paste( carpeta,"Resultados", "Evolucion", sep = "/"))
    dir.create(paste( carpeta,"Resultados", "Comunidad", sep = "/"))
    dir.create(paste( carpeta,"Resultados", "Efectos", sep = "/"))
    dir.create(paste( carpeta,"Resultados", "CaracteristicasTecnicas", sep = "/"))
    dir.create(paste( carpeta,"Resultados", "DeterminantesSemanticos", sep = "/"))
    dir.create(paste( carpeta,"Resultados", "ResultadosGenerales", sep = "/"))
  
    #if de las carpetas EVOLUCION Y SENTIDO
    if((dir.exists(paste(carpeta,"Resultados","Evolucion","Histograma", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","Evolucion","Grafico Torta", sep = "/"))))
    {}else
    {
      dir.create(paste(carpeta,"Resultados","Evolucion","Histograma", sep = "/"))
      
    }
    
    # if de las carpetas DETERMINANTES SEMANTICOS
    if((dir.exists(paste( carpeta,"Resultados","DeterminantesSemanticos","Nube", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","DeterminantesSemanticos","Bigrama", sep = "/"))))
    {}else
    {
      dir.create(paste( carpeta,"Resultados", "DeterminantesSemanticos", "Nube", sep = "/"))
      dir.create(paste( carpeta,"Resultados", "DeterminantesSemanticos", "Bigrama", sep = "/"))
      
    }
    # if de las carpetas COMUNIDADES 
    if((dir.exists(paste(carpeta,"Resultados","Comunidad","Referentes", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","Comunidad","Influenciadores", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","Comunidad","Movilizadores", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","Comunidad","Activistas", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","Comunidad","CategorizarSexo", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","Comunidad","Masificadores", sep = "/"))))
    {}else
    {
      dir.create(paste(carpeta,"Resultados","Comunidad","Referentes", sep = "/"))
      dir.create(paste(carpeta,"Resultados","Comunidad","Influenciadores", sep = "/"))
      dir.create(paste(carpeta,"Resultados","Comunidad","Movilizadores", sep = "/"))
      dir.create(paste(carpeta,"Resultados","Comunidad","CategorizarSexo", sep = "/"))
      dir.create(paste(carpeta,"Resultados","Comunidad","Masificadores", sep = "/"))
      dir.create(paste(carpeta,"Resultados","Comunidad","Activistas", sep = "/"))
    }
    # if de las carpetas EFECTOS Y EXITOS
    if((dir.exists(paste(carpeta,"Resultados","Efectos","Valoracion", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","Efectos","Categorizacion", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","Efectos","Muestra", sep = "/"))))
    {}else
    {
      dir.create(paste(carpeta,"Resultados","Efectos", "Valoracion", sep = "/"))
      dir.create(paste(carpeta,"Resultados","Efectos", "Categorizacion", sep = "/"))
      dir.create(paste(carpeta,"Resultados","Efectos", "Muestra", sep = "/"))
      
    }
    # if de las carpetas CARACTERISTICAS TECNICAS 
    if((dir.exists(paste(carpeta,"Resultados","CaracteristicasTecnicas","Caracteres", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","CaracteristicasTecnicas","Multimedia", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","CaracteristicasTecnicas","Dispositivos", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","CaracteristicasTecnicas","PorcentajeGeoreferencia", sep = "/")))
       & (dir.exists(paste(carpeta,"Resultados","CaracteristicasTecnicas","RankingGeoreferencia", sep = "/")))
    )
    {}else
    {
      dir.create(paste(carpeta,"Resultados","CaracteristicasTecnicas","Caracteres", sep = "/"))
      dir.create(paste(carpeta,"Resultados","CaracteristicasTecnicas","Multimedia", sep = "/"))
      dir.create(paste(carpeta,"Resultados","CaracteristicasTecnicas","Dispositivos", sep = "/"))
      dir.create(paste(carpeta,"Resultados","CaracteristicasTecnicas","PorcentajeGeoreferencia", sep = "/"))
      dir.create(paste(carpeta,"Resultados","CaracteristicasTecnicas","RankingGeoreferencia", sep = "/"))
    
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
  consulta$text=gsub("<f1>","ñ",consulta$text)#ñ
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
  consulta$text=gsub("<40>","@",consulta$text)#@
  
  
  #Nombre Archivos csv 
  archivolink = paste("porcentajelink",nombreResultado,".csv")
  archivoRanking = paste("ranking",nombreResultado,".csv")
  archivoCaracteres = paste("caracteres",nombreResultado,".csv")
  archivoValoracion = paste("Valoracion",nombreResultado,".csv")
  archivoValoracionTweet = paste("ValoracionTweet",nombreResultado,".csv")
  archivoViralizacion = paste("viralizacion",nombreResultado,".csv")
  archivoViralizacionTweet = paste("viralizacionTweet",nombreResultado,".csv")
  archivoReferentes = paste("referentes",nombreResultado,".csv")
  archivoInfluenciadores = paste("influenciadores",nombreResultado,".csv")
  archivoGeoreferencia = paste("georeferencia",nombreResultado,".csv")
  archivoRankingGeo = paste("ranking_paises",nombreResultado,".csv")
  archivoFotos = paste("cantidad_fotos", nombreResultado,".csv")
  archivoUnionDispositivos = paste("consulta_union",nombreResultado,".csv")
  archivoGenero = paste("genero",nombreResultado,".csv")
  archivoUnionSexo= paste("union_sexo",nombreResultado,".csv")
  archivoMasificadores = paste("masificadores",nombreResultado,".csv")
  archivoActivistas = paste("activistas",nombreResultado,".csv")
  archivoCategorizacion = paste("categorizacion",nombreResultado,".csv")
  
  aux <- read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  aux <- as.data.frame(aux)
  
  total_filas <- "SELECT count(user_id) total_filas FROM aux"
  try(total_filas <- sqldf(total_filas), silent = TRUE)
  
  # --- Evolucion y Sentido --- #
  # --- Histograma --- #
  histograma <- sqldf('SELECT  SUBSTR(created_at,1,10) FECHA,COUNT(SUBSTR(created_at,1,10)) CANTIDAD  
                       FROM aux 
                       GROUP BY substr(created_at,1,10) 
                       ORDER BY substr(created_at,1,10) 
                       DESC')
  
  try(write.csv(histograma,file <- paste(carpeta,"Resultados","Evolucion","Histograma","histogramax1dia.csv",sep = "/"),row.names=FALSE),silent = TRUE)
  try(write.csv(histograma,file <- paste(carpeta,"Resultados", "ResultadosGenerales","histogramax1diaA.csv",sep = "/"),row.names=FALSE),silent = TRUE)
  
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
  consulta_total <- "SELECT * FROM total_AA, total_NART, total_RT, total_RTAI, total_RTNAI, total_datos"
  try(total <- sqldf(consulta_total),silent = TRUE)
  
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
  
  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta,"Resultados","Evolucion","ReproduccionInteraccionProduccion.png",sep="/"),silent = TRUE))
  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta,"Resultados", "ResultadosGenerales","ReproduccionInteraccionProduccion.png",sep="/"),silent = TRUE))
  
  # --- Determinantes Semanticos --- #
  # --- Nube --- # Nueva nube
  conectores<-read.csv(paste(carpeta,"conectores.csv",sep = "/"), header = FALSE)
  tempora_nube<-sqldf("SELECT text FROM consulta")
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
  
  write.csv(nube, file = paste(carpeta,"Resultados","DeterminantesSemanticos","Nube","nube.csv",sep = "/"),row.names=FALSE)
  write.csv(nube, file = paste(carpeta,"Resultados", "ResultadosGenerales","nubeA.csv",sep = "/"),row.names=FALSE)
  
  # --- Bigrama --- #nuevo
  
  consulta <-sqldf("SELECT text FROM consulta")
  lol=consulta %>%
    
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
  write.csv(data_bigrama, file = paste( carpeta,"Resultados","DeterminantesSemanticos", "Bigrama","data_bigrama.csv",sep = "/"))
  write.csv(data_bigrama, file = paste(carpeta,"Resultados", "ResultadosGenerales","data_bigramaA.csv",sep = "/"))
  
  # --- Comunidades --- #
  # --- REFERENTES 2.0 ---#
  menciones <- unlist(regmatches(aux$text,gregexpr(patref2,aux$text)))
  menciones <- str_replace_all(menciones,'@','')
  menciones <-as.data.frame(menciones)
 
  referentes1 <- sqldf("SELECT DISTINCT(menciones) 'Usuarios Mencionados', COUNT(menciones) total_menciones
                           FROM menciones
                           WHERE menciones !=''
                           GROUP BY menciones
                           ORDER BY total_menciones DESC
                           LIMIT 30" )
  
  write.csv(referentes1, file = paste(carpeta,"Resultados","Comunidad","Referentes","Referentes2.0.csv",sep = "/"),row.names=FALSE)
  write.csv(referentes1, file = paste(carpeta,"Resultados", "ResultadosGenerales","Referentes2.0.csv",sep = "/"),row.names=FALSE)
  
  # --- Referentes antiguo --- HAY QUE MODIFICAR LAS MENCIONES EN FORMA DE VECTORES #
  try(referentes <- sqldf("SELECT DISTINCT(mentions_screen_name) as 'Usuarios mencionados' FROM aux
                           WHERE mentions_screen_name !=''"))
  
  write.csv(referentes, file = paste(carpeta,"Resultados","Comunidad","Referentes",archivoReferentes,sep = "/"),row.names=FALSE)
  write.csv(referentes, file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoReferentes,sep = "/"),row.names=FALSE)
  
  # --- Influenciadores --- #
  try(influenciadores <- sqldf("SELECT retweet_screen_name 'Usuario_Retweeado',count(retweet_screen_name) Cantidad_Retweet 
                      FROM aux 
                      WHERE is_retweet 
                      GROUP BY retweet_screen_name 
                      ORDER BY COUNT(retweet_screen_name) DESC
                      LIMIT 20"), silent =TRUE)
  
  write.csv(influenciadores,file <- paste(carpeta,"Resultados","Comunidad","Influenciadores",archivoInfluenciadores,sep = "/"),row.names = FALSE)
  write.csv(influenciadores,file <- paste(carpeta,"Resultados", "ResultadosGenerales",archivoInfluenciadores,sep = "/"),row.names = FALSE)
  
  # --- Movilizadores --- #
  archivoMovi = paste('resumen',nombreResultado,'.csv')
  archivoRanking = paste('ranking',nombreResultado,'.csv')
  
  filas = sqldf("SELECT COUNT(DISTINCT(status_id)) Total FROM aux")
  total = filas$Total
  
  all_words <- unlist(regmatches(aux$text, gregexpr(patHashtag, aux$text)))
  all_words <- str_replace_all(all_words,'#','')
  all_words <- as.data.frame(all_words)
  
  hashtags <- sqldf('SELECT distinct(all_words) Hashtag, COUNT(all_words) Suma
                    FROM all_words GROUP BY Hashtag ORDER BY suma DESC LIMIT 20')
  
  hashtagx <- sqldf("SELECT * FROM hashtags WHERE Hashtag NOT LIKE ''")
  
  filashashtag = sqldf('SELECT COUNT(Hashtag) Total FROM hashtagx')
  
  totalhashtags = filashashtag$Total
  porcentajeHashtag = trunc((totalhashtags/total)*100*10^2)/10^2
  
  
  ranking = sqldf('SELECT Hashtag, Suma FROM hashtagx LIMIT 10')
  
  tabla = matrix(c(total,totalhashtags, porcentajeHashtag,
                   'Total Filas','Total Hashtags','% Hashgtags'), ncol = 2)
 
  colnames(tabla) = c('Valores','Datos')
  tabla = as.data.frame(tabla)
  
  # --- Grafico ranking Hashtag --- #
  #Revisar influenciadores 
  ranking = as.data.frame(ranking)
  
  I = ranking$Hashtag
  S = ranking$Suma
  
  p <- plot_ly ( ranking, y = c(S),x = c(I), type = "bar", name = "Ranking Hashtag")%>%
    layout(title ="Ranking de Hashtag", yaxis = list(title = 'Cantidad'), xaxis = list(title='Hashtag'))
  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta,"Comunidad","Movilizadores","RankingHashtag.png",sep = "/")),silent = TRUE)
  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta,"ResultadosGenerales","RankingHashtagA.png")),silent =TRUE)
  
  write.csv(tabla, file = paste(carpeta,"Resultados","Comunidad","Movilizadores",archivoMovi,sep = "/"),row.names=FALSE)
  write.csv(ranking, file = paste(carpeta,"Resultados","Comunidad","Movilizadores", archivoRanking, sep = "/"), row.names = FALSE)
  #Ubicacion resultados generales
  write.csv(tabla, file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoMovi,sep = "/"),row.names=FALSE)
  write.csv(ranking, file = paste(carpeta,"Resultados", "ResultadosGenerales", archivoRanking, sep = "/"), row.names = FALSE)
  
  # --- Masificadores --- #
  #Usuarios que participan de  una conversacion y que tienen un gran numero de seguidores
  
  masificadores = 'SELECT distinct(screen_name) Cuenta, followers_count Cantidad_seguidores
                  FROM aux WHERE Cantidad_seguidores NOT LIKE "NA" 
                  GROUP BY Cuenta
                  ORDER BY Cantidad_seguidores DESC
                  LIMIT 20'
  masificadores = sqldf(masificadores)
  write.csv(masificadores,file = paste(carpeta,"Resultados","Comunidad","Masificadores",archivoMasificadores,sep = "/"),row.names=FALSE)

  # --- Activistas --- #
  
  activistas = 'SELECT screen_name Twitero,count(screen_name) Cantidad 
                FROM aux WHERE is_retweet = 0 
                GROUP BY screen_name 
                ORDER BY count(screen_name) desc'
  activistas = sqldf(activistas)
  write.csv(activistas, file = paste(carpeta,"Resultados","Comunidad","Activistas",archivoActivistas,sep = "/"),row.names = FALSE)
  
  
  # --- Contenidos multimedia --- #
  
  media <- "SELECT urls_expanded_url Media, COUNT(urls_expanded_url) Cantidad FROM aux WHERE Media NOT LIKE ' '"
 
  ranking_url <- "SELECT urls_url url, count(urls_url) cantidad FROM aux 
                  WHERE urls_url NOT LIKE ''
                  GROUP BY urls_url
                  ORDER BY cantidad DESC
                  LIMIT 5"
  
  columnas_link <- "SELECT (count(distinct(urls_url))-1) as Porcentaje FROM aux"
  
  cantidad_fotos <- "SELECT count(media_type) porcentaje_fotos FROM aux WHERE  media_type != ''"
  
  media =sqldf(media)
  ranking_url = sqldf(ranking_url)
  columnas_links= sqldf(columnas_link)
  porcentaje_links = round((columnas_links/total_filas)*100,3)
  cantidad_fotos = sqldf(cantidad_fotos) 
  fotos = round((cantidad_fotos/total_filas)*100,3)
  
  write.csv(ranking,file = paste(carpeta,"Resultados","CaracteristicasTecnicas","Multimedia",archivoRanking,sep = "/"),row.names = FALSE)
  write.csv(porcentaje_links, file=paste(carpeta,"Resultados","CaracteristicasTecnicas","Multimedia",archivolink,sep="/"),row.names = FALSE)
  write.csv(fotos, file=paste(carpeta,"Resultados","CaracteristicasTecnicas","Multimedia",archivoFotos,sep="/"),row.names = FALSE)
  #Ubicacion resultados generales
  write.csv(ranking,file =  paste(carpeta,"Resultados", "ResultadosGenerales",archivoRanking,sep = "/"),row.names = FALSE)
  write.csv(porcentaje_links, file= paste(carpeta,"Resultados", "ResultadosGenerales",archivolink,sep="/"),row.names = FALSE)
  write.csv(fotos, file= paste(carpeta,"Resultados", "ResultadosGenerales",archivoFotos,sep="/"),row.names = FALSE)
  
  # --- Grafico Ranking URLS --- #
   
  ranking_url <- as.data.frame(ranking_url)
  N <- ranking_url$url
  U <- ranking_url$cantidad
  
  p <- plot_ly ( ranking_url, y = c(U),x = c(N), type = "bar", name = "Ranking URL")%>%
    layout(title ="Ranking de URL", yaxis = list(title = 'Urls'), xaxis = list(title='Cantidad'))

  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta,"CaracteristicasTecnicas","Multimedia","RankingUrl.png",sep = "/")), silent = TRUE)
  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta,"ResultadosGenerales","RankingUrlA.png", sep = "/")),silent = TRUE)
  
  
  # --- Efectos y exitos --- #
  # --- Categorizacion --- #
  
  # --- Valoracion --- #
  Valoracion <- sqldf("SELECT screen_name as Usuario,
                MAX(favorite_count) as Max_favoritos,
                MIN(favorite_count) as Min_favoritos,
                AVG(favorite_count) as Promedio FROM aux")
  
  ValoracionTweet <- sqldf("SELECT  user_id as ID_usuario,
                      MAX(favorite_count) as Max_favoritos,
                      text as Tweet FROM aux")
  
  ValoracionFinal <- sqldf("SELECT * FROM Valoracion, ValoracionTweet")
  

  #ValoracionTweet <- sqldf(ValoracionTweet)
  
  write.csv(ValoracionFinal, file = paste(carpeta,"Resultados","Efectos","Valoracion",archivoValoracion,sep="/" ),row.names = FALSE)
  #write.csv(ValoracionTweet, file = paste(carpeta,"Resultados","Efectos","Valoracion",archivoValoracionTweet,sep="/" ),row.names = FALSE)
  #Ubiacion carpeta resultados generales
  write.csv(ValoracionFinal, file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoValoracion,sep="/" ),row.names = FALSE)
  #write.csv(ValoracionTweet, file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoValoracionTweet,sep="/" ),row.names = FALSE)
  
  # --- Muesta ---#
  for(i in 1:length(nombres[,1])){
    
    archivo <-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
    muestra <- sqldf('SELECT "screen_name","user_id","status_id","text", "retweet_text","is_retweet"  FROM 	consulta ')
    
    x <-sample(1:length(archivo[,1]),1000, replace = TRUE)
    x <- as.data.frame(x)
    
    
    if(length(muestra[,1])>=1000)
    {
      muestra<-muestra[x[,1],]
      archivo_final<- paste(carpeta,"Resultados","Efectos", "Muestra", sep = "/")
      write.csv(muestra,file = paste(archivo_final,nombres$nombres[i],sep = "/"),row.names = FALSE)
      
    }else{
      muestra <- muestra[1:length(archivo[,1]),]
      archivo_final<- paste(carpeta,"Resultados","Efectos", "Muestra", sep = "/")
      write.csv(muestra,file = paste(archivo_final,nombres$nombres[i],sep = "/"),row.names = FALSE)
    }
  }
  
  # --- Viralizacion --- #
  viralizacionTweet <- "SELECT user_id as ID_usuario,
                        MAX(retweet_count) as Max_retweet,
                        text as Tweet
                        FROM aux"
    
  
  viralizacion <- "SELECT user_id as ID_usuario,
                  MAX(retweet_count) as Max_retweet,
                  MIN(retweet_count) as Min_retweet,
                  AVG(retweet_count) as Promedio
                  FROM aux"
  
  viralizacion <- sqldf(viralizacion)
  viralizacionTweet <- sqldf(viralizacionTweet)
  
  #write.csv(viralizacion, file = paste(ubicacion_viralizacion,archivoViralizacion,sep="/" ),row.names = FALSE)
  #write.csv(viralizacionTweet, file = paste(ubicacion_viralizacion,archivoViralizacionTweet,sep="/" ),row.names = FALSE)
  
  # --- Caracteristicas Tecnicas --- #
  # ---  + - x Caracteres  --- #
  cantidadCaracteres <- "SELECT max(display_text_width) as Max_caracteres,
                        min(display_text_width) as Min_caracteres, 
                        avg(display_text_width) as Promedio FROM aux"
  try(cantidadCaracteres <- sqldf(cantidadCaracteres), silent = TRUE)
  
  write.csv(cantidadCaracteres, file = paste(carpeta,"Resultados","CaracteristicasTecnicas","Caracteres",archivoCaracteres,sep="/"),row.names = FALSE)
  write.csv(cantidadCaracteres, file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoCaracteres,sep="/"),row.names = FALSE)
  
  # --- Contenidos multimedia --- #
  # ---  Categorizacion por dispositivo --- #
  consulta_dispositivos <- "SELECT COUNT(source) Total FROM aux"
  try(dispositivos <- sqldf(consulta_dispositivos), silent = TRUE)
  
  dispositivos_android <- "SELECT COUNT(source) Androids FROM aux WHERE source LIKE '%Android'"
  try(androids <- sqldf(dispositivos_android), silent = TRUE)
  
  consulta_iphone <- "SELECT COUNT(source) Iphone FROM aux WHERE source LIKE '%iPhone'"
  try(iphone <- sqldf(consulta_iphone), silent = TRUE)
  
  consulta_web <- "SELECT COUNT(source) Web FROM aux WHERE source LIKE '%Web%'"
  try(web <- sqldf(consulta_web), silent=TRUE)
  
  consulta_otros <- "SELECT COUNT(source) Otros FROM aux WHERE source NOT LIKE '%Android' AND source NOT LIKE '%iPhone' AND source NOT LIKE '%Web%'"
  try(otros <- sqldf(consulta_otros), silent = TRUE)
  
  consulta_union <- "SELECT * FROM androids, iphone, web, otros, dispositivos"
  try(consulta_union <- sqldf(consulta_union), silent = TRUE)
  
  todos <- consulta_union
  
  write.csv(consulta_union, file = paste(carpeta,"Resultados","CaracteristicasTecnicas","Dispositivos",archivoUnionDispositivos,sep = "/"),row.names = FALSE)
  write.csv(consulta_union, file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoUnionDispositivos,sep = "/"),row.names = FALSE)
  
  A = todos$Androids
  I = todos$Iphone
  W = todos$Web
  O = todos$Otros
  TT = todos$Total
  
  porcentaje = matrix(c(trunc((A/TT)*100*10^2)/10^2,trunc((I/TT)*100*10^2)/10^2,trunc((W/TT)*100*10^2)/10^2,trunc((O/TT)*100*10^2)/10^2,'Android','Iphone','Web','Otros'),ncol = 2)
  colnames(porcentaje) = c('Porcentaje','Dispositivos')
  porcentaje = as.data.frame(porcentaje)
  
  data <- porcentaje[,c('Dispositivos', 'Porcentaje')]
  
  p <- plot_ly(data, labels = ~Dispositivos, values = ~Porcentaje, type = 'pie') %>%
    layout(title = 'Porcentaje de Dispositivos',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta,"Resultados","CaracteristicasTecnicas","Dispositivos","PorcentajeDispositivos.png",sep = "/")), silent = TRUE)
  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta,"Resultados","ResultadosGenerales","PorcentajeDispositivosA.png", sep = "/")), silent = TRUE)
  
  # --- Georeferencia --- #
  try(georeferencia <- sqldf("SELECT (count(distinct(country))-1) Porcentaje FROM aux"),silent = TRUE)
  
  # ---  Ranking de los 5 lugares mas mencionados --- #
  try(ranking_paises <- sqldf("SELECT count(country) Cantidad, place_full_name as Lugar,
                    place_type 'Tipo de lugar', country Paises, country_code 'Codigo Pais'
                    FROM aux WHERE Paises NOT LIKE ''
                    GROUP BY Paises 
                    ORDER BY Cantidad DESC
                    LIMIT 10"), silent = TRUE)
  
  porcentaje_georeferencia = round((georeferencia/total_filas)*100,2)
  
  # --- Grafico Ranking por paises --- #
  ranking_paises <- as.data.frame(ranking_paises)
  
  P = ranking_paises$Paises
  C = ranking_paises$Cantidad
  
  p <- plot_ly ( ranking_paises, y = c(C),x = c(P), type = "bar", name = "Ranking Paises")%>%
    layout(title ="Ranking de paises", yaxis = list(title = 'Cantidad'), xaxis = list(title='Paises'))
  
  #Ruta de salida de la imagen
  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta, "Resultados","CaracteristicasTecnicas","RankingGeoreferencia","RankingPaises.png", sep = "/")), silent = TRUE)
  try(plotly_IMAGE(p, format = "png", out_file = paste(carpeta,"Resultados","ResultadosGenerales","RankingPaisesA.png",sep = "/")), silent = TRUE)
  
  
  write.csv(porcentaje_georeferencia , file = paste(carpeta,"Resultados","CaracteristicasTecnicas","PorcentajeGeoreferencia",archivoGeoreferencia,sep = "/"),row.names = FALSE)
  write.csv(ranking_paises, file = paste(carpeta,"Resultados","CaracteristicasTecnicas","RankingGeoreferencia",archivoGeoreferencia,sep="/" ),row.names = FALSE)
  #Ubicacion resultados generales
  write.csv(porcentaje_georeferencia , file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoGeoreferencia,sep = "/"),row.names = FALSE)
  write.csv(ranking_paises, file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoRankingGeo,sep="/" ),row.names = FALSE)
  
  #---Categorizacion por genero---#
  genero = "SELECT sexo Sexo, count (sexo) Cantidad 
            FROM aux
            WHERE sexo = 'f' OR sexo = 'm' OR sexo ='na'
            GROUP BY sexo
            ORDER BY Cantidad DESC  
            "
  na = "SELECT count(sexo) as NA 
        FROM aux
        WHERE sexo LIKE 'na'"
  try(na <- sqldf(na), silent = TRUE)
  
  f = "SELECT count(sexo) as F
       FROM aux WHERE sexo LIKE 'f' "
  try(f <- sqldf(f),silent = TRUE)
  
  m = "SELECT count(sexo) as M
      FROM aux WHERE sexo LIKE 'm' "
  try(m <- sqldf(m),silent = TRUE)
  
  try(porcentaje_na <- round((na/total_filas)*100,3),silent = TRUE)
  try(porcentaje_f <- round((f/total_filas)*100,3),silent = TRUE)
  try(porcentaje_m <- round((m/total_filas)*100,3),silent = TRUE)
  
  union_sexo = "SELECT * FROM porcentaje_na, porcentaje_f, porcentaje_m"
  try(union_sexo <- sqldf(union_sexo), silent = TRUE)
  
  write.csv(genero,file = paste(carpeta,"Resultados","Comunidad","CategorizarSexo",archivoGenero,sep = "/"),row.names=FALSE)
  write.csv(union_sexo,file = paste(carpeta,"Resultados","Comunidad","CategorizarSexo",archivoUnionSexo,sep = "/"),row.names=FALSE)
  write.csv(genero,file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoGenero,sep = "/"),row.names=FALSE)
  write.csv(union_sexo,file = paste(carpeta,"Resultados", "ResultadosGenerales",archivoUnionSexo,sep = "/"),row.names=FALSE)
  
  
  i = i + 1
  
}





