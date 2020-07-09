library(readr)
library(dplyr)#manejo de ficheros
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(tidytext)
library(tm)

pat="(RT|via)(((?:\\b\\W*|)@\\w+)+)|,|:|(https|http)://t.co/[A-Za-z\\d]+|&amp;|http\\w*|@\\w+|(\\w+\\.|)\\w+\\.\\w+(/\\w+)*"
carpeta<-"C:/Users/Administrador/Desktop/ProgramasR/Trigramma"
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
  
  temporal_trigrama<-read.csv(archivo_temporal,header = TRUE,sep = ",",encoding = "UTF-7")
  temporal_trigrama<-select(temporal_trigrama,text)
  #-----------------------------------Trigramma-----------------------------------
  
  lol=temporal_trigrama %>%
    
    
    mutate(text = str_replace_all(text,pat, "")) %>%
    
    unnest_tokens(word, text, token="ngrams",n=3 ) %>%
    
    count(word, sort=TRUE) %>% 
    
    separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
    
    filter(!word1 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>% 
    
    filter(!word2 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>%
    
    filter(!word3 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>%
    
    filter(!str_detect(word1,"\\d+")) %>% 
    
    filter(!str_detect(word2,"\\d+")) %>% 
    
    filter(!str_detect(word3,"\\d+")) %>% 
    
    
    unite(trigrama, word1, word2,word3, sep = " ") %>%
    mutate(trigrama=reorder(trigrama,n)) %>% 
    
    
    top_n(13,n) %>%
    
    ungroup()
  # guardar trigrama ---------------------------------------------------------
  
  data_trigrama=temporal_trigrama %>%
    mutate(text = str_replace_all(text,pat, "")) %>%
    unnest_tokens(word, text, token="ngrams",n=3 ) %>%
    count(word, sort=TRUE) %>% 
    separate(word, c("word1", "word2","word3" ), sep = " ") %>% 
    filter(!word1 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>% 
    
    filter(!word2 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>%
    
    filter(!word3 %in% c(stopwords("es"),"q","d","t","cc","x","html","posted","just","online","streaming","false","na")) %>%
    
    filter(!str_detect(word1,"\\d+")) %>% 
    
    filter(!str_detect(word2,"\\d+")) %>% 
    
    filter(!str_detect(word3,"\\d+")) %>% 
    
    unite(trigrama, word1, word2, word3, sep = " ") %>%
    mutate(trigrama=reorder(trigrama,n)) %>% 
    
    ungroup()
  
  write.csv(data_trigrama, file = paste(nombre_carpeta,"data_trigramma.csv",sep = "/"))
  
  
}
