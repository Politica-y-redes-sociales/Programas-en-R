library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(ggplot2)
library(tidytext)
library(tm)
library(colorspace)
library(xlsx)

Sys.getlocale()
pat="(RT|via)(((?:\\b\\W*|)@\\w+)+)|,|:|(https|http)://t.co/[A-Za-z\\d]+|&amp;|http\\w*|@\\w+|(\\w+\\.|)\\w+\\.\\w+(/\\w+)*"
carpeta<-"C:/Users/Administrador/Desktop/ProgramasR/Bigrama"
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
  
  if(dir.exists(nombre_carpeta))
  {}else{dir.create(nombre_carpeta)}
  
  temporal_bigrama<-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-8")
  temporal_bigrama<-select(temporal_bigrama,text)
  
  temporal_bigrama$text=gsub("<f1>","ñ",temporal_bigrama$text)
  
  temporal_bigrama$text=gsub("<e1>","á",temporal_bigrama$text)
  temporal_bigrama$text=gsub("<c1>","Á",temporal_bigrama$text)
  
  temporal_bigrama$text=gsub("<e9>","é",temporal_bigrama$text)
  
  temporal_bigrama$text=gsub("<ed>","í",temporal_bigrama$text)
  
  temporal_bigrama$text=gsub("<f3>","ó",temporal_bigrama$text)
  temporal_bigrama$text=gsub("<d3>","Ó",temporal_bigrama$text)
  
  temporal_bigrama$text=gsub("<fa>","ú",temporal_bigrama$text)
  #-----------------------------------Bigramma-----------------------------------
  
  lol=temporal_bigrama %>%
    
    
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
  # guardar bigrama ---------------------------------------------------------
  
  data_bigrama=temporal_bigrama %>%
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



