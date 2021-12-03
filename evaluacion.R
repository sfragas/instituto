library(tidyr)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr)
rm(list=ls())
#Importamos los datos
datos<-read.csv2("CLASES_ESO_BAC.csv",header=T)
datos<-datos[,2:8]
datos$Clase<-as.factor(datos$Clase)
datos$Trimestre<-as.factor(datos$Trimestre)
#Ordenamos los niveles de clase
datos$Clase<-factor(datos$Clase,levels=levels(datos$Clase)[c(3:13,1:2)])
datos$Curso<-as.factor(substr(datos$Clase,1,4))
datos$Curso<-factor(datos$Curso,levels=levels(datos$Curso)[c(3:6,1:2)])
datos1<-pivot_longer(datos,cols = 2:5,names_to = "Resultados",values_to = "numero")
datos1$Resultados<-as.factor(datos1$Resultados)
#Numero de suspensos por case y categorías (Global)
colores<-c("#097a10","#c2af06","#f75205","#a60c25")
#datos2<-datos%>%dplyr::group_by(Curso,Trimestre)%>%summarise(N_alumnos=sum(N_alumnos),Aprobados=sum(Aprobados),
 #               Suspensos_1_3=sum(Suspensos_1_3),Suspensos_4_6=sum(Suspensos_4_6),Suspensos_7_mas=sum(Suspensos_7_mas))
#datos2<-pivot_longer(datos2,cols = 4:7,names_to = "Resultados",values_to = "numero")
#Graficamos los cursos por trimestres
for(i in unique(datos1$Trimestre)){
  datos<-dplyr::filter(datos1,Trimestre==i)
  grafica<-ggplot(data=datos,aes(x=Curso,y=numero,fill=Resultados))+geom_bar(stat="identity",position=position_dodge())+
    theme_grey()+labs(title=paste("Resultados de los diferentes cursos trimestre ",i))+scale_fill_manual(values=colores)
  print(grafica)
}

#Creamos diagramas de barras por trimestres y clases. Separamos por niveles educativos
#Para los Cursos de ESO
for(j in unique(datos1$Trimestre)){
  dato<-filter(datos1,Trimestre==j)
  dato<-dato[grep("^ESO",datos1$Curso),]
  for(i in unique(dato$Clase)){
    dato1<-dplyr::filter(dato,Clase==i)
    fig<-ggplot(dato1,aes(x=Resultados,y=numero,fill=Resultados))+
      geom_bar(stat="identity",position=position_dodge())+geom_text(aes(label=paste(round((numero/N_alumnos)*100,1),"%")),vjust=1.6,color="black",size=3.5)+
      theme_bw()+labs(title=paste("Resultados de la clase",i,"trimestre",j))+scale_fill_manual(values=colores)
    
    print(fig)
    #ggsave(fig,file=paste(resultado,j,i,".jpeg",sep=""),height=11,width=18,dpi=180)
  }
}

#Para los Cursos de BAC
for(j in unique(datos1$Trimestre)){
  dato<-filter(datos1,Trimestre==j)
  dato<-dato[grep("^BAC",datos1$Curso),]
  for(i in unique(dato$Clase)){
    dato1<-dplyr::filter(dato,Clase==i)
    fig<-ggplot(dato1,aes(x=Resultados,y=numero,fill=Resultados))+
      geom_bar(stat="identity",position=position_dodge())+geom_text(aes(label=paste(round((numero/N_alumnos)*100,1),"%")),vjust=1.6,color="black",size=3.5)+
      theme_bw()+labs(title=paste("Resultados de la clase",i,"trimestre",j))+scale_fill_manual(values=colores)
    
    print(fig)
    #ggsave(fig,file=paste(resultado,j,i,".jpeg",sep=""),height=11,width=18,dpi=180)
  }
}
# Realizamos as gráficas de barras para cada trimestre e cada unha das clases agrupadas por niveis

## Resultados para as clases de ESO
dato<-datos1[grep("^ESO",datos1$Curso),]
for(j in unique(dato$Clase)){
  data<-dplyr::filter(dato,Clase==j & numero>0)
  #dato<-dato[grep("^ESO",datos1$Curso),]
  dato_sort<-arrange(data,Trimestre)
  dato_cumsum<-ddply(dato_sort,.(Trimestre),transform,label_ypos=rev(cumsum(rev(numero))))
  fig<-ggplot(dato_cumsum,aes(x=Trimestre,y=numero,group=Resultados),ylim(0,N_alumnos))+
    geom_bar(aes(fill=Resultados),stat="identity")+theme_bw()+labs(title=paste("Resultados da clase",j))+scale_fill_manual(values=colores)+
    geom_text(aes(y=label_ypos,label=numero),vjust=1.6,color="black",size=3.5)
  print(fig)
}

## Resultados para as clases de BAC
dato<-datos1[grep("^BAC",datos1$Curso),]
for(j in unique(dato$Clase)){
  data<-filter(dato,Clase==j & numero>0)
  dato_sort<-arrange(data,Trimestre)
  dato_cumsum<-ddply(dato_sort,.(Trimestre),transform,label_ypos=rev(cumsum(rev(numero))))
  fig<-ggplot(dato_cumsum,aes(x=Trimestre,y=numero,group=Resultados),ylim(0,N_alumnos))+
    geom_bar(aes(fill=Resultados),stat="identity")+theme_bw()+labs(title=paste("Resultados da clase",j))+scale_fill_manual(values=colores)+
    geom_text(aes(y=label_ypos,label=numero),vjust=1.6,color="black",size=3.5)
  print(fig)
}

#Para los Cursos de INFOFI E PEIEST#######################

#Importamos los datos
datos<-read.csv2("CLASES_INFOFI_PEIEST.csv",header=T)
datos<-datos[,2:7]
datos$Clase<-as.factor(datos$Clase)
datos$Trimestre<-as.factor(datos$Trimestre)
#Ordenamos los niveles de clase
datos$Clase<-factor(datos$Clase)
datos$Curso<-as.factor(substr(datos$Clase,1,7))
datos$Curso<-factor(datos$Curso)
datos1<-pivot_longer(datos,cols = 2:4,names_to = "Resultados",values_to = "numero")
datos1$Resultados<-as.factor(datos1$Resultados)
#Numero de suspensos por case y categorías (Global)
colores<-c("#097a10","#c2af06","#f75205","#a60c25")
#Graficamos los cursos por trimestres
for(i in unique(datos1$Trimestre)){
  datos<-dplyr::filter(datos1,Trimestre==i)
  grafica<-ggplot(data=datos,aes(x=Curso,y=numero,fill=Resultados))+geom_bar(stat="identity",position=position_dodge())+
    theme_grey()+labs(title=paste("Resultados de los diferentes cursos trimestre ",i))+scale_fill_manual(values=colores)
  print(grafica)
}
#GRAFICAMOS POR CLASE PARA INFOFI
for(j in unique(datos1$Trimestre)){
  dato<-filter(datos1,Trimestre==j)
  dato<-dato[grep("^INF",datos1$Curso),]
  for(i in unique(dato$Clase)){
    dato1<-dplyr::filter(dato,Clase==i)
    fig<-ggplot(dato1,aes(x=Resultados,y=numero,fill=Resultados))+
      geom_bar(stat="identity",position=position_dodge())+geom_text(aes(label=paste(round((numero/N_alumnos)*100,1),"%")),vjust=1.6,color="black",size=3.5)+
      theme_bw()+labs(title=paste("Resultados de la clase",i,"trimestre",j))+scale_fill_manual(values=colores)
    
    print(fig)
    #ggsave(fig,file=paste(resultado,j,i,".jpeg",sep=""),height=11,width=18,dpi=180)
  }
}

#GRAFICAMOS POR CLASE PARA PEIEST
for(j in unique(datos1$Trimestre)){
  dato<-filter(datos1,Trimestre==j)
  dato<-dato[grep("^PEI",datos1$Curso),]
  for(i in unique(dato$Clase)){
    dato1<-dplyr::filter(dato,Clase==i)
    fig<-ggplot(dato1,aes(x=Resultados,y=numero,fill=Resultados))+
      geom_bar(stat="identity",position=position_dodge())+geom_text(aes(label=paste(round((numero/N_alumnos)*100,1),"%")),vjust=1.6,color="black",size=3.5)+
      theme_bw()+labs(title=paste("Resultados de la clase",i,"trimestre",j))+scale_fill_manual(values=colores)
    
    print(fig)
    #ggsave(fig,file=paste(resultado,j,i,".jpeg",sep=""),height=11,width=18,dpi=180)
  }
}

#Generamos diagrama de barras por trimestre para cada clase

for(j in unique(datos1$Clase)){
  dato<-filter(datos1,Clase==j & numero>0)
  dato_sort<-arrange(dato,Trimestre)
  dato_cumsum<-ddply(dato_sort,.(Trimestre),transform,label_ypos=rev(cumsum(rev(numero))))
  fig<-ggplot(dato_cumsum,aes(x=Trimestre,y=numero,group=Resultados),ylim(0,N_alumnos))+
    geom_bar(aes(fill=Resultados),stat="identity")+theme_bw()+labs(title=paste("Resultados de la clase",j))+scale_fill_manual(values=colores)+
    geom_text(aes(y=label_ypos,label=numero),vjust=1.6,color="black",size=3.5)
    
    
    print(fig)
}
  


toc: true # table of content true
toc_depth: 3  # upto three depths of headings (specified by #, ## and ###)
number_sections: true  ## if you want number sections at each table header
theme: united  # many options for theme, this one is my favorite.
highlight: tango  # specifies the syntax highlighting style
