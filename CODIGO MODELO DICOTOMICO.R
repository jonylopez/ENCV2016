
#CODIGO FUENTE ENCUESTA NACIONAL DE CALIDAD DE VIDA 2016
#INVESTIGACION SOBRE DESIGUALDAD SOCIOECONOMICA Y ACCESO A LA JUSTICIA
#CENTRO DE ESTUDIOS DE DERECHO, JUSTICIA Y SOCIEDAD
#LINEA SISTEMA JUDICIAL

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
} #Librerias requeridas

#Directorio de trabajo (en la ruta especificada deben estar los archivos del dataframe)
setwd("C:/Users/jonyp/Desktop/ECV2016/R")

#Modulos ENCV 2016
{cch=as.data.frame(read_sav("Características y composición del hogar.sav"))%>%
    mutate(P6020=ifelse(P6020==2,0,P6020))}#Caracteristicas y composicion del hogar, P6020-sexo dummy 1hombre 0mujer
{sh=as.data.frame(read_sav("Servicios del hogar.sav"))%>%
    mutate(LOGPERCAPITA=log(PERCAPITA))%>%
    mutate(quintil=PERCAPITA)
  sh$quintil[sh$quintil<=quantile(sh$quintil,probs=.2)]=1
  sh$quintil[sh$quintil<=quantile(sh$quintil,probs=.4) & sh$quintil>quantile(sh$quintil,probs=.2)]=2
  sh$quintil[sh$quintil<=quantile(sh$quintil,probs=.6) & sh$quintil>quantile(sh$quintil,probs=.4)]=3
  sh$quintil[sh$quintil<=quantile(sh$quintil,probs=.8) & sh$quintil>quantile(sh$quintil,probs=.6)]=4
  sh$quintil[sh$quintil<=quantile(sh$quintil,probs=1) & sh$quintil>quantile(sh$quintil,probs=.8)]=5
  s_h=sh%>%
    mutate(q1=ifelse(sh$quintil==1,1,0))%>%
    mutate(q2=ifelse(sh$quintil==2,1,0))%>%
    mutate(q3=ifelse(sh$quintil==3,1,0))%>%
    mutate(q4=ifelse(sh$quintil==4,1,0))%>%
    mutate(q5=ifelse(sh$quintil==5,1,0))
  
}# Servicios del hogar, variable ingreso percapita, log ingreso percapita y quintiles de ingreso percapita
{cvhtb=as.data.frame(read_sav("Condiciones de vida del hogar y tenencia de bienes.sav"))%>%
    mutate(P1075=ifelse(P1075==2,0,P1075))}#Condiciones de vida del hoagr y tenencia de bienes, P1075-conexion a internet dummy 1si 0no
{dv=as.data.frame(read_sav("Datos de la vivienda.sav"))%>%
    mutate(P3=ifelse(P3==2,0,P3))%>%
    mutate(P3=ifelse(P3==3,0,P3))}#Datos de la vivienda, ubicacion de la vivienda dummy 1cabecera 0centros poblados y rural dispersa
{edu=as.data.frame(read_sav("Educación.sav"))%>%
    mutate(ne1=ifelse(P8587%in% c(2,3),1,0))%>%
    mutate(ne2=ifelse(P8587 %in% c(4,5),1,0))%>%
    mutate(ne3=ifelse(P8587%in% c(seq(6,13,1)),1,0))}#educacion, P8587-nivel educativo  dummys ne1 primaria ne2 secundaria ne3 superior
{nj1=as.data.frame(read_sav("Necesidades jurídicas.sav"))%>%
  filter(P1672==1)%>%
  filter(P1676<=2)%>%
  mutate(P1685=ifelse(P1685==2,0,P1685))}#necesidades juridicas el problema se soluciono 1S 2N P1685

#filtro encuesta por unidad de datos

#vivienda
datvi=dv[,c("DIRECTORIO","SECUENCIA_ENCUESTA","P3")]

#hogar
datho=merge(cvhtb[, c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P","P1075")],
            s_h[ ,c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "PERCAPITA","LOGPERCAPITA","q2","q3","q4","q5")],
            by=c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P"))

#persona
datpe=merge(edu[,c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "ne1","ne2","ne3")],
            cch[,c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P", "P6020")],
            by=c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P"))

#union de bases por personas
##vivienda-hogar
vh=merge(datho, datvi,by.x = c("DIRECTORIO", "SECUENCIA_P"),
         by.y = c("DIRECTORIO","SECUENCIA_ENCUESTA"))[,c(1,3,2,4,5,6,7,8,9,10,11)]  

##vivienda-hogar-personas
vhp=merge(datpe, vh, by.x=c("DIRECTORIO","SECUENCIA_P"),
          by.y=c("DIRECTORIO","SECUENCIA_ENCUESTA"))[,c(1,3,2,4,5,6,7,9,10,11,12,13,14,15,16)]
#data completa con variable dependiente
data=na.omit(merge(nj1[,c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P","P1685")],vhp, by.x = c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P"),
                    by.y = c("DIRECTORIO","SECUENCIA_ENCUESTA", "SECUENCIA_P"),
                    all.x = T))[,c(seq(4,16,1))]
  #se omiten las observaciones con NA

names(data)[1]="Solpro" #solucion del problema
names(data)[2]="ne1" #dummy educacion
names(data)[3]="ne2" #dummy educacion
names(data)[4]="ne3" #dummy educacion
names(data)[5]="sexo" #sexo P6020 1 Hombre 0 Mujer
names(data)[6]="cint" #conexion internet P1075 1si 0no
names(data)[7]="ing" #ingreso percapita PERCPITA
names(data)[8]="loging" #departamento P1_DEPARTAMENTO 
names(data)[9]="q2"
names(data)[10]="q3"
names(data)[11]="q4"
names(data)[12]="q5"
names(data)[13]="cvi" #clase vivienda P3 1cabecera 0centropoblado-rural disperso


data=write.xlsx(data,".xlsx")
saveWorkbook(data, file = "dicotomico.xlsx", overwrite = TRUE)
