library(readr)
library(stats)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidytext)

lista <-read.csv("C:/Users/Administrador/Desktop/ProgramasR/GrafoConInfluyentes/lista.csv", header = TRUE, sep = ",")
lista$text<-str_extract(lista$text,"RT @([a-zA-Z]|[0-9]|_){1,}:")
lista$text<-substring(lista$text,5,(nchar(lista$text)-1))
arreglo2<-count(lista,text)
colnames(arreglo2)[1]<-"screen_name"
arreglo<-count(lista,screen_name)
personas<-count(lista,Query)
colnames(personas)[1]<-"screen_name"

largo<-length(lista[,1])

arreglo<-rbind(arreglo,arreglo2)
arreglo<-rbind(arreglo,personas)
arreglo<-count(arreglo,screen_name)

arreglo<-as.data.frame(arreglo)
largo_nombres<-length(arreglo[,1])
if(is.na(arreglo[largo_nombres,1]))
{
  arreglo<-arreglo[-largo_nombres,]
  largo_nombres<-largo_nombres-1
}

matriz<-matrix(0,largo_nombres,largo_nombres)

colnames(matriz)<-as.character(arreglo[,1])
rownames(matriz)<-as.character(arreglo[,1])


#Primero Tweets
for(i in 1:largo)
{
  print(i)
  if(is.na(lista$text[i]))
  {
    matriz[toString(lista$screen_name[i]),toString(lista$Query[i])]<-matriz[toString(lista$screen_name[i]),toString(lista$Query[i])]+1
  }
}

#Segundo ReTweet
for(i in 1:largo)
{
  if(is.na(lista$text[i]))
  {
  }
  else
  {
    if(matriz[toString(lista$text[i]),toString(lista$Query[i])]==0)
    {matriz[toString(lista$text[i]),toString(lista$Query[i])]<-1}
    matriz[toString(lista$screen_name[i]),toString(lista$text[i])]<-matriz[toString(lista$screen_name[i]),toString(lista$text[i])]+1
  }
}

#A ponerle color no eficiente, pero sirve
personas<-as.data.frame(personas)
largo_personas<-length(personas[,1])

tablilla<-c("Source","Target","Weight")
for(i in 1:largo_nombres)
{
  for(j in 1:largo_nombres)
  {
    if(matriz[i,j]>0)
    {tablilla<-rbind(tablilla,c(i,j,matriz[i,j]))}
  }
  
}
tablilla<-as.data.frame(tablilla)

arreglo$nn<-as.character(arreglo$nn)
for(j in 1:(largo_nombres))
{
  influyente=0
  retweet=0
  y=1
  x=1
  while(y<=largo_personas)
  {
    if(matriz[j,toString(personas[y,1])]>=1 & influyente==0)
    {influyente=1}
    y<-y+1
  }
  
  esta<-filter(lista,lista$screen_name==toString(arreglo[j,1]) & is.na(lista$text)==FALSE)
  esta<-as.data.frame(esta)
  
  if(length(esta[,1])>0)
  {retweet=1}
  
  if(retweet==1 & influyente==1)
  { arreglo[j,2]="Rt y Tw"}
  else
  {
    if(retweet==1)
    {arreglo[j,2]="Rt"}
    
    else
    {arreglo[j,2]="Tw"}
  }
  
  while(x<=largo_personas)
  {
    if(toString(arreglo[j,1])==toString(personas[x,1]))
    {arreglo[j,2]="Query"}
    x<-x+1
  }
}

nombres<-cbind(1:largo_nombres,arreglo)
colnames(nombres)<-c("Id","Label","Category")

write.table(nombres, file = "C:/Users/Administrador/Desktop/Programas/GrafoConInfluyentes/nombres.csv",row.names=FALSE)
write.table(tablilla, file = "C:/Users/Administrador/Desktop/Programas/GrafoConInfluyentes/tabla.csv",row.names=FALSE,col.names = FALSE)
