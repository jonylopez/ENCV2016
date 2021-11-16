#CODIGO FUENTE ENCUESTA NACIONAL DE CALIDAD DE VIDA 2016
#INVESTIGACION SOBRE DESIGUALDAD SOCIOECONOMICA Y ACCESO A LA JUSTICIA
#LINEA SISTTEMA JUDICIAL

rm(list=ls())
{#Paquetes requeridos
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("openxlsx")
#install.packages("ggplot2")
}#Activar si no estan instalados los paquetes
{
  library("tidyverse")
  library("dplyr")
  library("openxlsx")
  library("haven")
  library("ggplot2")
}#librerias
#Directorio de trabajo (en la ruta especificada deben estar los archivos del dataframe)
setwd("C:/Users/Admin/Desktop/ECV 2016/R")

#Modulos (se cargan los modulos de la encuesta de calidad de vida)
{cch=read.csv("caracteristicas y composicion del hogar.csv", sep = ";", dec = ",", header = T)%>%
    #mutate(P6087=ifelse(P6087==9,0,P6087))%>%#educacion padre
    #mutate(P6088=ifelse(P6088==9,0,P6088))%>%#educacion madre
    mutate(P6020=ifelse(P6020==2,0,P6020))}#sexo. modulo caracteristicas y composicion del hogar
    #esta transformacion se realiza para que los hombres queden con valor 1 y las mujeres con valor 0

{sh=as.data.frame(read_sav("Servicios del hogar.sav"))%>%
    mutate(LOGPERCAPITA=log(PERCAPITA))%>%
    mutate(quintil=PERCAPITA)
    sh$quintil[sh$quintil<=quantile(sh$quintil,probs=.2)]=1
    sh$quintil[sh$quintil<=quantile(sh$quintil,probs=.4) & sh$quintil>quantile(sh$quintil,probs=.2)]=2
    sh$quintil[sh$quintil<=quantile(sh$quintil,probs=.6) & sh$quintil>quantile(sh$quintil,probs=.4)]=3
    sh$quintil[sh$quintil<=quantile(sh$quintil,probs=.8) & sh$quintil>quantile(sh$quintil,probs=.6)]=4
    sh$quintil[sh$quintil<=quantile(sh$quintil,probs=1) & sh$quintil>quantile(sh$quintil,probs=.8)]=5
    names(sh)[1]="ï..DIRECTORIO"
    s_h=sh%>%
    mutate(q1=ifelse(sh$quintil==1,1,0))%>%
    mutate(q2=ifelse(sh$quintil==2,1,0))%>%
    mutate(q3=ifelse(sh$quintil==3,1,0))%>%
    mutate(q4=ifelse(sh$quintil==4,1,0))%>%
    mutate(q5=ifelse(sh$quintil==5,1,0))
    
}#ingreso por quintiles
#servicios del hogar
{cvhtb=read.csv("condiciones de vida del hogar y tenencia de bienes.csv", sep = ";", dec = ",", header = T)%>%
    mutate(P1075=ifelse(P1075==2,0,P1075))}#conexion a internet
#esta transformacion es para que la variable sea dicotomica con valores 0 y 1

{dv=read.csv("datos de la vivienda.csv", sep = ";", dec = ",", header = T)%>%
    mutate(P3=ifelse(P3==2,0,P3))%>% #ubicacion de la vivienda
    mutate(P3=ifelse(P3==3,0,P3))%>% #ubicacion de la vivienda
    #esta transformacion es para que la variable sea dicotomica con valores 0 y 1
    mutate(P8520S1A1=ifelse(P8520S1A1==0,NA,P8520S1A1))%>% #estrato
    mutate(P8520S1A1=ifelse(P8520S1A1==9,NA,P8520S1A1))%>% #estrato
    mutate(P8520S1A1=ifelse(P8520S1A1==8,NA,P8520S1A1))} #estrato
    #esta transformacion es para excluir las categorias de la variable que no proveen informacion relevante

{edu=read.csv("educacion.csv", sep = ";", dec = ",", header = T)%>%
    mutate(P6160=ifelse(P6160==2,0,P6160))%>% #leer y escribir
    #esta transformacion es para que la variable sea dicotomica con valores 0 y 1
    mutate(de1=ifelse(P8587%in% c(2,3),1,0))%>%
    mutate(de2=ifelse(P8587 %in% c(4,5),1,0))%>%
    mutate(de3=ifelse(P8587%in% c(seq(6,13,1)),1,0))}
    
    #mutate(P8587=ifelse(P8587==1,0,P8587))%>% #nivel educativo
    #mutate(P8587=ifelse(P8587 %in% c(2,3),1,P8587))%>% #nivel educativo
    #mutate(P8587=ifelse(P8587 %in% c(4,5),2,P8587))%>% #nivel educativo
    #mutate(P8587=ifelse(P8587 %in% c(seq(6,13,1)),3,P8587))} #nivel educativo
    #esta transformacion es para agrupar la categorizacion de la variable nivel educativo.

nj1=read.csv("necesidades juridicas.csv", sep = ";", dec = ",", header = T)

#ainm=read.csv("atencion integral niños y niñas menores a 5 años.csv", sep = ";", dec = ",", header = T)
#cvhs=read.csv("condiciones de vida del hogar subsidios.csv", sep = ";", dec = ",", header = T)
#ft=read.csv("fuerza de trabajo.csv", sep = ";", dec = ",", header = T)
#nj2=read.csv("necesidades juridicas 2.csv", sep = ";", dec = ",", header = T)
#sld=read.csv("salud.csv", sep = ";", dec = ",", header = T)
#tic=read.csv("tecnologia de comunicacion e informacion.csv", sep = ";", dec = ",", header = T)
#tfve=read.csv("tenencia y financiacion de la vivienda que ocupa el hogar (escritura personas).csv", sep = ";", dec = ",", header = T)
#tfv=read.csv("tenencia y financiacion de la vivienda que ocupa el hogar.csv", sep = ";", dec = ",", header = T)
#ti=read.csv("trabajo infantil.csv", sep = ";", dec = ",", header = T)

#Categorias varaible dependiente
{
  i1_4=nj1 %>%
    filter(P1672==1)%>%
    filter(P1676<=2)%>%
    filter(P1684==1)%>%
    filter(P1685==1)%>%
    mutate(CATEGORIA=3)
  
  i1_3=nj1 %>%
    filter(P1672==1)%>%
    filter(P1676<=2)%>%
    filter(P1684==1)%>%
    filter(P1685==2)%>%
    mutate(CATEGORIA=2)
  
  i1_2=nj1 %>%
    filter(P1672==1)%>%
    filter(P1676<=2)%>%
    filter(P1684==2)%>%
    mutate(CATEGORIA=2)
  
  i1_1=nj1 %>%
    filter(P1672==1)%>%
    filter(P1676==3)%>%
    mutate(CATEGORIA=1)
  
  i2_3=nj1 %>%
    filter(P1672==2)%>%
    filter(P1680==1)%>%
    filter(P1684==1)%>%
    filter(P1685==1)%>%
    mutate(CATEGORIA=3)
  
  i2_2=nj1 %>%
    filter(P1672==2)%>%
    filter(P1680==1)%>%
    filter(P1684==1)%>%
    filter(P1685==2)%>%
    mutate(CATEGORIA=2)
  
  i2_1=nj1 %>%
    filter(P1672==2)%>%
    filter(P1680==1)%>%
    filter(P1684==2)%>%
    mutate(CATEGORIA=2)
  
  i3_0=nj1 %>%
    filter(P1683 %in% c(2,3,5,6,7))%>%
    mutate(CATEGORIA=0)
  
  i3_00=nj1 %>%
    filter(P1681 %in% c(2,4,5))%>%
    mutate(CATEGORIA=0)
  
  i3_000=nj1 %>%
    filter(P1682 %in% c(1,3,4))%>%
    mutate(CATEGORIA=0)
  
  c=rbind( i1_1[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")], 
           i1_2[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")],
           i1_3[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")],
           i1_4[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")],
           i2_1[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")],
           i2_2[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")],
           i2_3[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")],
           i3_0[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")],
           i3_00[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")],
           i3_000[ , c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "CATEGORIA")])
}
#filtro encuesta por unidad de datos

#vivienda
datvi=dv[,c("ï..DIRECTORIO","SECUENCIA_ENCUESTA","P3","P8520S1A1")]

#hogar
datho=merge(cvhtb[, c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P","P1075")],
            s_h[ ,c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "PERCAPITA","LOGPERCAPITA","q2","q3","q4","q5")],
            by=c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P"))

#persona
datpe=merge(edu[,c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "de1","de2","de3","P6160")],
            cch[,c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "P6020")],
            by=c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P"))

#union de bases por personas
##vivienda-hogar
vh=merge(datho, datvi,by.x = c("ï..DIRECTORIO", "SECUENCIA_P"),
         by.y = c("ï..DIRECTORIO","SECUENCIA_ENCUESTA"))[,c(1,3,2,4,5,6,7,8,9,10,11,12)]  

##vivienda-hogar-personas
vhp=merge(datpe, vh, by.x=c("ï..DIRECTORIO","SECUENCIA_P"),
          by.y=c("ï..DIRECTORIO","SECUENCIA_ENCUESTA"))[,c(1,3,2,4,5,6,7,8,10,11,12,13,14,15,16,17,18)]
#data completa con categoria
{data=na.omit(merge(c,vhp, by.x = c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P"),
                    by.y = c("ï..DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P"),
                    all.x = T))[,c(seq(4,18,1))]
  #se omiten las observaciones con NA
  names(data)[2]="de1" #dummy educacion
  names(data)[3]="de2" #dummy educacion
  names(data)[4]="de3" #dummy educacion
  names(data)[5]="le" #saber leer y escribir P6160 1si 0no
  names(data)[6]="sexo" #sexo P6020 1 Hombre 0 Mujer
  names(data)[7]="cint" #conexion internet P1075 1si 0no
  names(data)[8]="ing" #ingreso percapita PERCPITA
  names(data)[9]="loging" #departamento P1_DEPARTAMENTO 
  names(data)[10]="q2"
  names(data)[11]="q3"
  names(data)[12]="q4"
  names(data)[13]="q5"
  names(data)[14]="cvi" #clase vivienda P3 1cabecera 0centropoblado-rural disperso
  names(data)[15]="est" #estrato
}

7+7

mean(data$ing)

str(data$ing)
sd(data$ing)
quantile(data$ing, probs = c(.33,.66,1))
max(data$ing)
data$edu
identical(data$CATEGORIA,data$edu)

data$est=factor(data$est, levels =c(1,2,3,4,5,6),ordered = T)
summary(data$est)

data$CATEGORIA=factor(data$CATEGORIA, levels =c(0,1,2,3),ordered = T)
summary(data$CATEGORIA)

data=write.xlsx(data,".xlsx")
saveWorkbook(data, file = "datadummy.xlsx", overwrite = TRUE)
#exportacion de base de datos a excel para trabajar en STATA.



#######################################################################################################
#datos descriptivos




identical(data$edu,data$CATEGORIA)

data$edu=factor(data$edu, levels =c(0,1,2,3),ordered = T)
summary(data$edu)

data$edu=as.integer(data$edu)
data$le=as.factor(data$le)
data$sexo=as.factor(data$sexo)
data$nep=as.integer(data$nep)
data$nem=as.integer(data$nem)
data$cint=as.factor(data$cint)
data$ing=as.numeric(data$ing)
data$dep=as.factor(data$dep)
data$cvi=as.factor(data$cvi)

#GRAFICO DESIGUALDAD Y ACCESO A LA JUSTICIA

setwd("C:/Users/Admin/Desktop/ECV 2016/R")
datos=read.csv(file="datos acceso a la justicia y desigualdad.csv",dec=",", sep=";", header=T)
plot(datos$Gini,datos$IAEJ)
Gini=datos$Gini
IAEJ=datos$IAEJ
reg=lm(IAEJ~Gini)
summary(reg)
plot(Gini,IAEJ, main="IAEJ vs Gini por departamentos")
abline(reg, lwd=3, col='red')
 

