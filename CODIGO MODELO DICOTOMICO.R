
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

