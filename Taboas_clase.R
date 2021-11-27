#Importamos los datos del aula
library(tidyr)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr)
datos<-read.csv2("1ESO_a.csv",header = T,encoding = "UTF-8")
#Arregalmos el data frame
datos$CUALIFICACIÓN<-as.numeric(datos$CUALIFICACIÓN)
datos<-datos%>%mutate(Nome_completo=paste(NOME,X1º.APELIDO,X2º.APELIDO),Cat_notas=case_when(CUALIFICACIÓN<5~"Aprobado",CUALIFICACIÓN>=5~"Suspenso"))%>%
  select(Nome_completo,Cat_notas)%>%na.omit()
datos<-group_by(datos,Nome_completo)
datos%>%summarise(N_aprobados=length(Cat_notas[Cat_notas=="Aprobado"]))

datos<-datos%>%ddply(.(Nome_completo),mutate,N_aprobados=length(Cat_notas[Cat_notas=="Aprobado"]))
datos<-datos%>%ddply(.(Nome_completo),mutate,N_suspensos=length(Cat_notas[Cat_notas=="Suspenso"]))
  



ddply(.(Nome_completo),mutate,N_suspensos=length(Cat_notas[Cat_notas=="Aprobado"]))
                                            
                                            
                                            
length(datos$Cat_notas[datos$Cat_notas=="Aprobado"])
mutate(N_aprobados=length(Cat_notas[Cat_notas=="Aprobado"]),
       N_suspensos=length(Cat_notas[Cat_notas=="Suspenso"]))
