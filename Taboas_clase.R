#Importamos los datos del aula
library(tidyr)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
detach(package:plyr)
#"ESO3_PMAR",
clase<-c("ESO1_A","ESO1_B","ESO1_C","ESO2_A","ESO2_B","ESO2_C","ESO3_A","ESO3_B","ESO4_A","ESO4_B","BAC1_A","BAC2_A","INFOFI1","INFOFI2","PEIEST1","PEIEST2")
j=1
resultados<-data.frame(Clase=character(),Aprobados=integer(),Suspensos_1_3=integer(),
                       Suspensos_4_6=integer(),
                       Suspensos_7_mas=integer(),
                       stringsAsFactors=F)
for(i in clase){
datos<-read.csv2(paste0(i,".csv"),header = T,encoding = "UTF-8")
#Arregalmos el data frame
datos$CUALIFICACIÓN<-as.numeric(datos$CUALIFICACIÓN)
datos<-datos%>%mutate(Nome_completo=paste(NOME,X1º.APELIDO,X2º.APELIDO),Cat_notas=case_when(CUALIFICACIÓN>5~"Aprobado",CUALIFICACIÓN<=5~"Suspenso"))%>%
    select(Nome_completo,Cat_notas)%>%na.omit()
datos$Nome_completo<-as.factor(datos$Nome_completo)
                   
datos1<-group_by(datos,Nome_completo)
datos1<-datos1%>%summarise(N_suspensos=length(Cat_notas[Cat_notas=="Suspenso"]))
datos1<-datos1%>%mutate(Casos=case_when(N_suspensos==0~"Aprobados",
              N_suspensos>=1&N_suspensos<=3~"Suspensos_1_3",
              N_suspensos>=4&N_suspensos<=6~"Suspensos_4_6",TRUE~"Suspensos_7_mas"))
ungroup(datos1)
datos1<-datos1%>%group_by(Casos)%>%summarise(Total=n())%>%pivot_wider(names_from = Casos,values_from=Total)
datos1$Clase<-paste(i)
datos1<-datos1[,c(ncol(datos1),1:ncol(datos1)-1)]
resultados[j,"Clase"]<-datos1[1,"Clase"]
if("Aprobados"%in% colnames(datos1)){
  resultados[j,"Aprobados"]<-datos1[1,"Aprobados"]
} else {
  resultados[j,"Aprobados"]<-0
}
if("Suspensos_1_3"%in% colnames(datos1)){
  resultados[j,"Suspensos_1_3"]<-datos1[1,"Suspensos_1_3"]
} else {
  resultados[j,"Suspensos_1_3"]<-0
}
if("Suspensos_4_6"%in% colnames(datos1)){
  resultados[j,"Suspensos_4_6"]<-datos1[1,"Suspensos_4_6"]
} else {
  resultados[j,"Suspensos_4_6"]<-0
}
if("Suspensos_7_mas"%in% colnames(datos1)){
  resultados[j,"Suspensos_7_mas"]<-datos1[1,"Suspensos_7_mas"]
}else{
  resultados[j,"Suspensos_7_mas"]<-0
}
j=j+1
}
write.csv2(resultados,"clases.csv")

