
#install.packages("tidyverse")
##library(devtools)
##devtools::install_github("ExequielAndres/tweetbotornot")
#hacer esto con el github del observatorio

library(tweetbotornot)
library(rtweet)
library(dplyr)
library(sqldf)
token <- create_token ( 
  app<-"Observatorio Mexico 2", 
  consumer_key<-"0Dfgxid9fd99m89qXtDMB2AW8", 
  consumer_secret<-"oneySpIjrfZnbHPCvt5mSyGXHLVHWFk0zq4FPwn6DJQYWq5eUa",
  access_token<-"987408666319118336-wMO5baRXILTNAEkGUL6PPSA19dS9ZbD", 
  access_secret<-"5XqOhjn2xV1wFVKfMZkBhFazPjad5lV191ywsHSVs6cQ5"
)

carpeta <- "C:/programasR/DetectorBot"
base <- "C:/programasR/DetectorBot/cuentas.csv"
users <- read.csv(base,header = TRUE, encoding = "UTF-8") 
if(length(as.data.frame(users$screen_name)) == 0 ){
  datos<- read.csv("C:/programasR/DetectorBot/cuentas.csv",header = TRUE,sep = ";",encoding = "UTF-8")
}
users1 <- as.data.frame(users)

if(dir.exists(paste(carpeta, "Resultados", sep = "/")))
{}else
{dir.create(paste(carpeta, "Resultados", sep = "/"))}

#la funcion gsub se debe utilizar solo en caso de que los usuarios de la base contengan @ antes de su nombre de
users <- gsub("@","",users$screen_name)
data <- botornot(users, fast = TRUE)
data[order(data$prob_bot), ]
usuarios_bot <- data%>% filter(prob_bot <= 0.95)
usuarios_bot2 <- data%>% filter(prob_bot >= 0.95)
query <- sqldf("SELECT * FROM users1 s WHERE s.screen_name IN (SELECT screen_name FROM usuarios_bot)") #SELECT screen_name FROM usuarios_bot,SELECT * FROM users1 s WHERE(SELECT screen_name FROM usuarios_bot) = s.screen_name
write.csv(usuarios_bot2, file = paste(carpeta,"Resultados","usuarios_bot.csv",sep = "/"),row.names=FALSE)
write.csv(query, file = paste(carpeta,"Resultados","usuarios_filtrados.csv",sep = "/"),row.names=FALSE)
View(usuarios_bot)








