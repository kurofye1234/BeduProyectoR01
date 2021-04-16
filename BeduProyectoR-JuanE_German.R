#Preparacion de Ambiente
setwd("C:/Users/X1 Carbon/Documents/Juan E/Cursos/BEDU/Modulo02_ProgramacionEnR/Sesion09_Proyecto/Data")
DataReg1 <- read.csv("carpetas_completa.csv")
#Librerias
install.packages("scales")
install.packages("sp")
install.packages("ggplot2")
library(scales)
library(ggplot2)
library(dplyr)
library(sp)
library(data.table)
library(plotly)
library(TSA)
summary("DataReg1")
View (DataReg1)

#Se Limpian los nombres: 
DataReg1$fiscalia <- gsub('INVESTIGACIÃ“N', 'INVESTIGACION', DataReg1$fiscalia)
DataReg1$fiscalia <- gsub('JUÃREZ', 'JUAREZ', DataReg1$fiscalia)
DataReg1$fiscalia <- gsub('ÃLVARO OBREGÃ“N', 'ALVARO OBREGON', DataReg1$fiscalia)
DataReg1$fiscalia <- gsub('ATENCIÃ“N', 'ATENCION', DataReg1$fiscalia)
DataReg1$delito <- gsub('DAÃ‘O', 'DAÑO', DataReg1$delito)

#Se cambian los nombres de los meses:
DataReg1$mes_hechos <- gsub('Diciembre', '12', DataReg1$mes_hechos)
View(DataReg1)

#Se quitan columnas que no nos interesan: 
DataReg1Limpia = subset(DataReg1, select = -c(competencia,tempo,calle_hechos2,unidad_investigacion) )
View(DataReg1Limpia)

#Ordenamos por anho 
DataFiscaliaOrdenadaAnho <- DataReg1Limpia[order(DataReg1Limpia$ao_hechos),] # (DataReg1Limpia(ao_hechos,mes_hechos),decreasing = F)
View(DataFiscaliaOrdenadaAnho)

#HechosBasicos
#Media de Anho 
mediaAnho <- mean(DataFiscaliaOrdenadaAnho$ao_hechos)

#Nos damos cuenta que hay NAs por lo que limpiaremos 
DataSinNAOrdenada <- complete.cases(DataFiscaliaOrdenadaAnho)
View(DataSinNAOrdenada) #No nos traera nada por que hay que convertirlo a DataFrame
DataSinNAOrdenada2 <-DataFiscaliaOrdenadaAnho[DataSinNAOrdenada,] #Se Convierte a DF
View(DataSinNAOrdenada2)

mediaAnho <- mean(DataSinNAOrdenada2$ao_hechos)
mediaAnho


#Extraemos datos por anho 
Anho2021 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2021') & DataSinNAOrdenada2$ao_hechos<=('2021'),];
View(Anho2021)
Anho2020 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2020') & DataSinNAOrdenada2$ao_hechos<=('2020'),];
View(Anho2020)
Anho2019 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2019') & DataSinNAOrdenada2$ao_hechos<=('2019'),];
View(Anho2019)
Anho2018 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2018') & DataSinNAOrdenada2$ao_hechos<=('2018'),];
View(Anho2018)
Anho2017 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2017') & DataSinNAOrdenada2$ao_hechos<=('2017'),];
View(Anho2017)
Anho2016 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2016') & DataSinNAOrdenada2$ao_hechos<=('2016'),];
View(Anho2016)
Anho2015 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2015') & DataSinNAOrdenada2$ao_hechos<=('2015'),];
View(Anho2015)
Anho2014 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2014') & DataSinNAOrdenada2$ao_hechos<=('2014'),];
View(Anho2014)
Anho2013 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2013') & DataSinNAOrdenada2$ao_hechos<=('2013'),];
View(Anho2013)
Anho2012 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2012') & DataSinNAOrdenada2$ao_hechos<=('2012'),];
View(Anho2012)
Anho2011 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2011') & DataSinNAOrdenada2$ao_hechos<=('2011'),];
View(Anho2011)
Anho2010 <- DataSinNAOrdenada2[DataSinNAOrdenada2$ao_hechos>=('2010') & DataSinNAOrdenada2$ao_hechos<=('2010'),];
View(Anho2010)


#Vamos a utilizar 2021
#Agrupamos por mes y alcaldia, sacando el total de delitos reportados
Anho2020Grouped <- Anho2020 %>%
  group_by(Anho2020$alcaldia_hechos, Anho2020$mes_hechos) %>%
  mutate(count = n())
View(Anho2020Grouped)

#Anho2021Grouped$mes_hechos = factor(Annho2021Grouped, levels = month.name)

#Sacamos un Subset por Alcaldia
Anho2020Grouped.s1 <- Anho2020Grouped[Anho2020Grouped$alcaldia_hechos == "IZTAPALAPA" ,] #Se crea un subset
View(Anho2020Grouped.s1)

#Graficamos Numero de Delitos por mes en esa alcaldia 
Plot2020 <- ggplot(Anho2020Grouped.s1, aes(x=Anho2020Grouped.s1$mes_hechos, y=Anho2020Grouped.s1$count)) + 
  geom_line( color="blue") + 
  geom_point() +
  labs(x = "Mes", 
       y = "No Delitos",
       title = paste("Alcaldia",
                     Anho2020Grouped.s1$alcaldia_hechos)
  ) + 
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1)) 

ggplotly(Plot2020)

#Nos dimos cuenta que los meses son super importantes, entonces tendremos que pasarlos a: 
#Ingles o a Numericos por lo que convertimos los meses a numericos 
#en la linea 22

hist(x=Anho2020Grouped.s1$count, breaks=100, main="Histograma",
     xlab="Numero de delitos", ylab="2020")















