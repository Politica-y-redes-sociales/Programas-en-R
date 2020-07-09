library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(tidytext)
library(tm)
library(colorspace)
library(sqldf)

Sys.getlocale()
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
pat="(RT|via)(((?:\\b\\W*|)@\\w+)+)|,|:|(https|http)://t.co/[A-Za-z\\d]+|&amp;|http\\w*|@\\w+|(\\w+\\.|)\\w+\\.\\w+(/\\w+)*"

memory.limit(9999999999)
carpeta<-"C:/Users/Ruben/Desktop/ProgramasR/TodoEnUno"
carpeta_Bases<-paste(carpeta,"Bases",sep="/")
nombres<-dir(carpeta_Bases)
nombres<-as.data.frame(nombres)

if(dir.exists(paste(carpeta,"Resultados",sep = "/")))
{}else{dir.create(paste(carpeta,"Resultados",sep = "/"))}
i=1
for(i in 2:length(nombres[,1]))
{
  archivo_temporal<-paste(carpeta_Bases,toString(nombres$nombres[i]),sep="/")
  nombre<-substr(toString(nombres$nombres[i]),1,(str_length(nombres$nombres[i])-4))
  nombre_carpeta<-paste(carpeta,"Resultados",sep = "/")
  nombre_carpeta<-paste(nombre_carpeta,nombre,sep = "/")
  
  if(dir.exists(nombre_carpeta))
  {}else{dir.create(nombre_carpeta)}
  
  consulta <-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-7")
  consulta<-sqldf('SELECT DISTINCT(status_id),* from consulta')
  consulta<-consulta[,-1]
  consulta$text=gsub("<f1>","?",consulta$text)
  consulta$text=gsub("<e1>","?",consulta$text)
  consulta$text=gsub("<c1>","?",consulta$text)
  consulta$text=gsub("<e9>","?",consulta$text)
  consulta$text=gsub("<ed>","?",consulta$text)
  consulta$text=gsub("<f3>","?",consulta$text)
  consulta$text=gsub("<d3>","?",consulta$text)
  
  consulta$text=gsub("<fa>","?",consulta$text)
  
  ###### Muestra ########
  
  largo<-length(consulta[,1])
  aleatorios <- as.data.frame(sample(1:10000000, largo, replace=TRUE))
  colnames(aleatorios)<-c("numeros")
  muestra<-sqldf("select created_at,screen_name,text, source from consulta")
  muestra<-cbind(muestra,aleatorios)
  muestra<-sqldf("select created_at,screen_name,text, source from muestra order by numeros")
  muestra<-sqldf("select * from muestra limit(1000)")
  
  write.csv(muestra, file = paste(nombre_carpeta,"muestra.csv",sep = "/"),row.names=FALSE)
  muestra<-""
  aleatorios<-""
  
  ###### TWITTEROS Y INFLUENCIADORES ########
  
  influenciadores<-sqldf('select retweet_screen_name USUARIO,count(retweet_screen_name) CANTIDAD from consulta where is_retweet group by retweet_screen_name order by count(retweet_screen_name) desc')
  twiteros<-sqldf('select screen_name,count(screen_name) from consulta where is_retweet=0 group by screen_name order by count(screen_name) desc')
  write.csv(influenciadores, file = paste(nombre_carpeta,"influenciadores.csv",sep="/"),row.names=FALSE)
  write.csv(twiteros, file = paste(nombre_carpeta,"twiteros.csv",sep = "/"),row.names=FALSE)
  influenciadores<-""
  twiteros<-""
  
  
  ####### HISTOGRAMA ###########
  
  histograma <- sqldf(' select  substr(created_at,1,10) FECHA,count(substr(created_at,1,10)) CANTIDAD  from consulta group by substr(created_at,1,10) ORDER BY substr(created_at,1,10) DESC')
  write.csv(histograma,file = paste(nombre_carpeta,"1dia.csv",sep = "/"),row.names=FALSE)
  histograma<-""
  ######## NUBE #############
  conectores<-read.csv(paste(carpeta,"conectores.csv",sep = "/"), header = FALSE)
  consulta<-sqldf("select text from consulta")
  tempora_nube<-consulta
  tempora_nube<-mutate(tempora_nube,text = str_replace_all(text,pat, ""))
  lis<-unnest_tokens(tempora_nube,word, text, token="ngrams",n=1 )
  tempora_nube<-""
  nube<-count(lis,word,sort=TRUE)
  lis<-""
  
  nube<-as.data.frame(nube)
  conectores<-as.data.frame(conectores)
  
  consulta_conectores=paste0(paste("select * from nube where word!=",conectores$V1[1],sep = "'" ),"' ")
  for(j in 2:length(conectores[,1]))
  {
    consulta_conectores=paste0(paste(consulta_conectores,conectores$V1[j],sep = " and word!='"),"'")
  }
  nube=sqldf(consulta_conectores)
  
  write.csv(nube, file = paste(nombre_carpeta,"nube.csv",sep = "/"),row.names=FALSE)
  
  nube<-""
  
  
  # 
  # #######  Bigramma ########
    consulta=sqldf("select text from consulta")
  # 
  # lol=consulta %>%
  #   
  #   mutate(text = str_replace_all(text,pat, "")) %>%
  #   unnest_tokens(word, text, token="ngrams",n=2 ) %>%
  #   count(word, sort=TRUE) %>% 
  #   separate(word, c("word1", "word2"), sep = " ") %>% 
  #   filter(!word1 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>% 
  #   filter(!word2 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>%
  #   filter(!str_detect(word1,"\\d+")) %>% 
  #   filter(!str_detect(word2,"\\d+")) %>% 
  #   unite(bigrama, word1, word2, sep = " ") %>%
  #   mutate(bigrama=reorder(bigrama,n)) %>% 
  #   top_n(13,n) %>%
  #   ungroup()
  # 
  # ##### GUARDA BIGRAMMA ##########

  data_bigrama=consulta %>%
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

  write.csv(data_bigrama, file = paste(nombre_carpeta,"data_bigrama.csv",sep = "/"),row.names=FALSE)

  }