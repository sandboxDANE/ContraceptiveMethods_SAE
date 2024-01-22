
################################################################################
##                     Proyecto MRP - Left No One Behind                      ##
## Title:        Indicador de planificación familiar D6 - Uso de algún método ##
##               de planificación                                             ##
## Returns:      Estimación Plugin por dominios de interés                    ##
## Author:       Felipe Molina & Andrés Gutiérrez & Diego Lemus               ##
## Institution:  CEPAL                                                        ##
## Date:         2021                                                         ##
## División:     División de Estadísticas                                     ##
## Disclaimer:   Estos códigos computacionales han sido programados con el    ##
##               fin de ejemplificar las metodologías propuestas por CEPAL.   ##
##               La responsabilidad del uso de los programas recae            ##
##               completamente sobre  los funcionarios a quienes se hace      ##
##               entrega. Se exime a la CEPAL de los errores que puedan ser   ##
##               ocasionados por el uso incorrecto de estos códigos.          ##
################################################################################

###--- Cleaning R environment ---###

#rm(list = ls())
#gc()
install.packages('corr')
#################
### Libraries ###
#################

library(tidyverse)
library(survey)
#library(srvyr)
#library(sae)
library(lme4)
library(data.table)
library(dplyr)
library(haven)
#library(formula.tools)
library(remotes)
library(corr)
#library(StatisticalModels)
library(MASS)

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(160000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

options(encoding = "UTF-8", scipen = 999)

# COMMAND ----------

mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"

###---------------------------------- ENDS ----------------------------------###

#Xencuesta <- readRDS("/Volumes/Macintosh HD/Backup_2021_Azure/Colombia-UNFPA/1. ConciliarBases/Output/XencuestaD.rds")  %>% filter(unida == 1)


Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds"))  %>% filter(unida == 1)

###------------ Anexando los Senate Weights a la base de la ENDS ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###--------------------------------- CENSO ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds"))  %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

#Xcenso = sample_n(Xencuesta, 10000)
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

###----- Identificador de Municipio - Departamento por persona del censo ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds")) 

###----- Variables diseño ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))



###------------- Anexando el código Divipola a la base del Censo ------------###
Xcenso <- Xcenso  %>% left_join(., Variables.disenio,
                              by = c("Divipola"= "Divipola"))

###--------------------- Listado de los 1122 municipios ---------------------###

Divipola <- Municipio %>% distinct(Divipola, .keep_all = T)
Divipola$departamento = as.character(as.numeric(Divipola$departamento))
Divipola$municipio = as.character(as.numeric(Divipola$municipio))

Xencuesta$departamento = as.character(as.numeric(Xencuesta$departamento))
Xencuesta$municipio = as.character(as.numeric(Xencuesta$municipio))

###------------ Anexando el código Divipola a la base de la ENDS ------------###

Xencuesta <- Xencuesta %>% left_join(., Variables.disenio,
                              by = c("Divipola"= "Divipola"))

###------------ Anexando el la variable dependiente categorica   ------------###

Xencuesta <- Xencuesta %>% mutate(larga_duracion = ifelse(tipo_met %in% c(6, 7),'4_Permanentes',
                                                          ifelse(tipo_met %in% c(11, 2),'3_Larga',
                                                                 ifelse(tipo_met %in% c(0),'1_NoUse', '2_Corta'))))
Xencuesta <- Xencuesta %>% filter(larga_duracion != '1_NoUse')

Xencuesta <- Xencuesta %>% mutate(larga_duracion_2 = 
                                    ifelse(larga_duracion %in% c('3_Larga','4_Permanentes'),'1_Larga', '2_Corta'))

Xencuesta3 <- Xencuesta %>% mutate(larga_duracion_2n = 
                                    ifelse(larga_duracion_2 %in% c('1_Larga'),1, 0))

table(Xencuesta$larga_duracion)
table(Xencuesta$larga_duracion_2)


Xencuesta2=Xencuesta %>% as.data.frame()
cor(Xencuesta2[,c("larga_duracion_2", "inasistencia", "ocupada", "edad_13_14", "edad_15_19", "edad_20_24", "edad_25_29", "edad_30_3", 
                 "edad_34_39", "edad_40_44", "hijos",  "escolaridad_primaria", "escolaridad_secundaria", "escolaridad_superio", 
                 "reconoce_etnia", "misma_etnia_1", "aguacocina_acueducto_publico_veredal_agua_embotellada", 
                 "matpiso_maderaburda_tabla_tierraarena", "matpared_guadua_maderaburda_esterilla", 
                 "internet_Si", "electricidad_si", "tipo_viv_casa_departamento")])

cor(Xencuesta3[,c("larga_duracion_2n", "inasistencia", "ocupada", "edad_13_14", "edad_15_19", "edad_40_44", "hijos", "electricidad_si", 
                  "tipo_viv_casa_departamento", "escolaridad_primaria")])


###--- Limpiando memoria ---###
rm(Municipio, Divipola)

#1 Pill                                 11 NorplantTM or implants
#2 IUD                                  12 Prolonged abstinence
#3 Injections                           13 Lactational amenorrhea
#4 Diaphragm                            14 Female condom
#5 Male condom                          15 Foam and jelly
#6 Female Sterilization                 16 Emergency contraception
#7 Male Sterilization                   17 Other modern method
#8 Periodic Abstinence (Rhythm)         18 Standard days method
#9 Withdrawal                           19 Country specific method 1
#10 Other traditional                   20 Country specific method 2 


###############################################################################
## uso y demanda de métodos anticonceptivos
###############################################################################
# uso metodos - Uso de metodos de larga duracion
ld <- Xencuesta  %>% #group_by(area_urbano) %>% #filter(v502==1) %>% # mujeres unidas
  mutate(ld = ifelse(larga_duracion %in% c('3_Larga'),v005,0)) %>%  # actualmente usa algún método
  summarise(per_uso=100*sum(ld)/sum(v005)) # 10.1274

ld

# ld depto

ld_depto <- Xencuesta %>%  group_by(departamento)%>% #filter(unida==1) %>% # mujeres unidas
  mutate(uso=ifelse(larga_duracion %in% c('1_NoUse'),v005,0)) %>%  # actualmente usa algún método moderno
  summarise(per_uso=100*sum(uso)/sum(v005)) %>% as.data.frame() # 75.9
ld_depto


################################################################################
                       ### Ajuste del modelo Plugin ###
################################################################################
 Xencuesta[is.na(Xencuesta)] = 0
library(ordinal)
pluginreg <- clmm2(factor(larga_duracion_2) ~ inasistencia  +   ocupada + 
               edad_13_14 +
               edad_15_19 + edad_20_24 + edad_25_29 + edad_30_34 +                   
               edad_34_39 + edad_40_44 + 
               hijos +  escolaridad_primaria + escolaridad_secundaria  + escolaridad_superior  + 
               reconoce_etnia + misma_etnia_1 + reconoce_etnia*misma_etnia_1+
               aguacocina_acueducto_publico_veredal_agua_embotellada +
               matpiso_maderaburda_tabla_tierraarena + 
               matpared_guadua_maderaburda_esterilla  +
               internet_Si   + 
               electricidad_si + tipo_viv_casa_departamento, random=as.factor(departamento_etnia), 
               data=Xencuesta, Hess=TRUE, weights = Sweights)

summary(pluginreg)

#------------------------------------------------------------------------------#
#----------------- Exportando salidas: Modelo Plugin ajustado -----------------#
#------------------------------------------------------------------------------#

saveRDS(pluginreg, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/PluginLD2SW_Mun.rds"))



pluginreg = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/LD/PluginLD2SW_Mun.rds"))




#------------------------------------------------------------------------------#
#------------- Exportando los efectos fijos comunes a los dominios ------------#
#------------------------------------------------------------------------------#

betas = as.matrix(pluginreg$beta)

#------------------------------------------------------------------------------#
#------- Exportando los efectos aleatorios para cada uno de los dominios ------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

udSW = cbind.data.frame(indice = levels(as.factor(Xencuesta$departamento_etnia)),
                        ud = pluginreg$ranef)

#udSW


#------------------------------------------------------------------------------#
#-------------- En esta sección se emplea solo el modelo saturado -------------#
#------------------------------------------------------------------------------#
select = dplyr::select
###------------ Construcción de la matriz censal sintética XBeta ------------###

mat_CensoSW <- cbind.data.frame(Xcenso$departamento_etnia, cbind(as.matrix(Xcenso %>%
                                    select(rownames(betas)))) %*% 
                                    betas)
colnames(mat_CensoSW) <- c("Divipola","XB")

#-  Interceptos para cada modelo                                                 -#

mat_CensoSW$Alpha1_2 = pluginreg$Alpha[[1]]
#mat_CensoSW$Alpha2_3 = pluginreg$Alpha[[2]]
#mat_CensoSW$Alpha3_4 = pluginreg$Alpha[[3]]

#- Creando el vector nulo para ser reemplazado en el ciclo de estimación Plugin -#

Xcenso$pluginLD_1 = rep(0,5980476)#numeric(5980476)
#Xcenso$pluginLD_2 = rep(0,5980476)
#Xcenso$pluginLD_3 = rep(0,5980476)
#-- Códigos de los municipios en el censo para el ciclo de estimación Plugin --#

Div = unique(Xcenso$departamento_etnia)



###---------- Ciclo de estimación plugin por individuo en el censo ----------###
###----------   En esta sección se emplea solo el modelo saturado  ----------###

for(i in 1:length(Div)){
  
  print(i)

  ### Identificación del dominio ###
  
  index = which(Xcenso$departamento_etnia == Div[i])
  
  ###--- Estimación en las áreas muestreadas por la ENDS ---###
  
  if(Div[i] %in% udSW$indice){
   
    ###--- Seleccionando el efecto aleatorio del dominio ---###
    
    u = udSW$ud[which(as.character(udSW$indice) == Div[i])]
    
    ###---Probabilidad de uso de algún método anticonceptivo ---###
    
    Xcenso$pluginLD_1[index] = exp(  mat_CensoSW$Alpha1_2[index] - mat_CensoSW$XB[index] + u )/
                                 (1 + exp( mat_CensoSW$Alpha1_2[index] - mat_CensoSW$XB[index] + u ))
    
    #Xcenso$pluginLD_2[index] = exp( mat_CensoSW$Alpha2_3[index] - mat_CensoSW$XB[index] + u)/
    #                             (1 + exp( mat_CensoSW$Alpha2_3[index] - mat_CensoSW$XB[index] + u))
    
    #Xcenso$pluginLD_3[index] = exp(  mat_CensoSW$Alpha3_4[index] - mat_CensoSW$XB[index] + u)/
    #                              (1 + exp( mat_CensoSW$Alpha3_4[index] - mat_CensoSW$XB[index] + u ))
    
   print(paste(Div[i], "muestreada"))
    
  ###--- Estimación en áreas no muestreadas por la ENDS ---###
    
  }else{
    
    ###---Probabilidad de uso de algún método anticonceptivo ---###
    
    Xcenso$pluginLD[index] = exp(mat_CensoSW$XB[index])/
                                 (1 + exp(mat_CensoSW$XB[index]))
    
    print(paste(Div[i],"No muestreada"))
  }
}

Xcenso = Xcenso %>% mutate(LD1 = pluginLD_1,
                           #LD2 = pluginLD_2 - pluginLD_1,
                           #LD3 = pluginLD_3 - pluginLD_2,
                            LD2 = 1 - pluginLD_1,)

###------------ Proporción de uso de algún método anticonceptivo ------------###
###--------------------- Estimación plugin departamental --------------------### 

depto <- Xcenso %>% group_by(departamento) %>% summarise(Larga = mean(LD1),
                                                         Corta = mean(LD2))

###----------------------- Estimación plugin municipal ----------------------###

municipio <- Xcenso %>% group_by(Divipola) %>%  summarise(Larga = mean(LD1),
                                                          Corta = mean(LD2))

###----------------------- Estimación plugin por etnia ----------------------###

etnia <- Xcenso %>% group_by(etnia) %>%  summarise(Larga = mean(LD1),
                                                   Corta = mean(LD2))


###----------------- Estimación plugin: Departamento X etnia ----------------###

municipio_etnia <- Xcenso %>% group_by(departamento, etnia) %>% summarise(Larga = mean(LD1),
                                                                          Corta = mean(LD2))



###--------------------------------------------------------------------------###
###    Proporción de uso de métodos anticonceptivos reportado por la ENDS    ###
###--------------------------------------------------------------------------###
 ### Funcion para cakcular medias por cada nivel###
mediasc = function(nivel){
  medias <- numeric(36)
  
  ###--- Nacional ---###
  Xencuesta = Xencuesta %>%  mutate( grupo = ifelse(larga_duracion_2 == nivel, 1, 0) )
  medias[1] <- Xencuesta  %>% 
    summarise(p = weighted.mean(grupo, fexp)) %>% 
    as.numeric()
  
  ###--- Urbana ---###
  
  medias[2] <- Xencuesta %>% filter(area_urbano == 1 ) %>% 
    summarise(p = weighted.mean(grupo, fexp)) %>% as.numeric()
  
  ###--- Rural ---###
  
  medias[3] <- Xencuesta %>% filter(area_rural == 1) %>% 
    summarise(p = weighted.mean(grupo,fexp)) %>% as.numeric()
  
  ###--- Departamento ---###
  
  medias[4:36] <- Xencuesta %>% 
    group_by(departamento) %>% 
    summarise(p = weighted.mean(grupo,fexp)) %>% mutate(depto = as.numeric(as.character(departamento))) %>% arrange(depto) %>% 
    select(p) %>% unlist(use.names = FALSE)
  
  return(medias)
}


#'2_Corta'       '1_Larga'
medias = mediasc('2_Corta')
medias


###-------------------------- Matriz de calibración -------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% #sample_n(100) %>%
             left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)
MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])

###------------------------ Función de pesos Benchmark ----------------------###

Benchmark <- function(CensoY, media){
  library(sampling)
  MatrizCalibrada2 = MatrizCalibrada
  for(i in 1:36){
    MatrizCalibrada2[,i] <- MatrizCalibrada[,i] * (CensoY - media[i])
 }
  a = calib(Xs = MatrizCalibrada2, 
           # d = runif( dim(MatrizCalibrada)[1], 0, 1), 
            d = rep(1, dim(MatrizCalibrada)[1]), 
            #q = media,
           total=media,
            method = c("logit"),
           # bounds=c(0,7),
            max_iter = 100,
           # description = TRUE
           ) 
  return(a)
}


###--- Anexando pesos Benchmark al censo ---###

total=c(t(rep(1,nrow(MatrizCalibrada)))%*%as.matrix(MatrizCalibrada))


medias = mediasc('1_Larga')
Xcenso$pesos_LagaD <- Benchmark(Xcenso$LD1,media = medias)
summary(Xcenso$pesos_LagaD)
gc()

medias = mediasc('2_Corta')
Xcenso$pesos_CortaD <- Benchmark(Xcenso$LD2,media = medias)
summary(Xcenso$pesos_CortaD)
gc()




#Xcenso = Xcenso %>% mutate(total_pesos  =  pesos_CortaD +pesos_LagaD ,
#                           pesos_CortaD = pesos_CortaD/total_pesos,
#                           pesos_LagaD  = pesos_LagaD/total_pesos)

#summary(Xcenso$pesos_CortaD)
#summary(Xcenso$pesos_LagaD)

###---------------------- Estimación benchmark Nacional ---------------------###
Corta_Nal <- Xcenso %>% summarise(Corta_2020 = mean(LD2),
                                  CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_Nal')

Larga_Nal <- Xcenso %>% summarise(Larga_2020 = mean(LD1),
                                  LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_Nal')
Nal= Reduce(bind_rows, list(Corta_Nal,Larga_Nal )) %>% as.data.table() %>% 
  melt.data.table(.,id.vars = c('gr')) %>% .[is.na(value) == F] 


###---------------------- Estimación benchmark Clase ---------------------###
Corta_clase <- Xcenso %>% group_by(area_urbano) %>%summarise(Corta_2020 = mean(LD2),
                                                             CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_clase')

Larga_clase <- Xcenso %>% group_by(area_urbano) %>%summarise(Larga_2020 = mean(LD1),
                                                             LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_clase')


Clase= Reduce(bind_rows, list(Corta_clase,Larga_clase)) %>% as.data.table() %>% 
                melt.data.table(.,id.vars = c('gr', 'area_urbano')) %>% .[is.na(value) == F] 

###---------------------- Estimación benchmark Edad ---------------------###




Corta_edad <- Xcenso %>% group_by(edad) %>%summarise(Corta_2020 = mean(LD2),
                                                             CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_edad')

Larga_edad <- Xcenso %>% group_by(edad) %>%summarise(Larga_2020 = mean(LD1),
                                                             LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_edad')


Edad = Reduce(bind_rows, list(Corta_edad,Larga_edad )) %>% as.data.table() %>% 
  melt.data.table(.,id.vars = c('gr','edad')) %>% .[is.na(value) == F] 


###-------------------- Estimación benchmark Municipal ------------------###

Corta_Mun  <- Xcenso %>% group_by(departamento,Divipola) %>%summarise(Corta_2020 = mean(LD2),
                                                     CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_Mun')

Larga_Mun  <- Xcenso %>%group_by(departamento,Divipola) %>%summarise(Larga_2020 = mean(LD1),
                                                     LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_Mun')


Municipio = Reduce(bind_rows, list(Corta_Mun,Larga_Mun )) %>% as.data.table() %>% 
  melt.data.table(.,id.vars = c('gr','departamento','Divipola')) %>% .[is.na(value) == F] 

###-------------------- Estimación benchmark por etnia ------------------###

Corta_Etnia  <- Xcenso %>% group_by(etnia) %>%summarise(Corta_2020 = mean(LD2),
                                                                      CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_Etnia')

Larga_Etnia  <- Xcenso %>% group_by(etnia) %>%summarise(Larga_2020 = mean(LD1),
                                                                     LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_Etnia')

Etnia = Reduce(bind_rows, list(Corta_Etnia,Larga_Etnia ))  %>% as.data.table() %>% 
  melt.data.table(.,id.vars = c('gr','etnia')) %>% .[is.na(value) == F] 

###-------------------- Estimación benchmark por departamento  etnia ------------------###


Corta_departamento_etnia  <- Xcenso %>% group_by(departamento_etnia) %>%summarise(Corta_2020 = mean(LD2),
                                                        CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_departamento_etnia')

Larga_departamento_etnia <- Xcenso %>% group_by(departamento_etnia) %>%summarise(Larga_2020 = mean(LD1),
                                                        LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_departamento_etnia')


departamento_etnia = Reduce(bind_rows, 
                            list(Corta_departamento_etnia,Larga_departamento_etnia ))  %>% as.data.table() %>% 
  melt.data.table(.,id.vars = c('gr','departamento_etnia')) %>% .[is.na(value) == F] 

rm( Corta_departamento_etnia, Larga_departamento_etnia)

#Cuadro 8.4.1.1 - Distribución % de mujeres unidas
#----------------- Exporting Plugin estimates for post-stratum ----------------#

###--- Etnia ---###

saveRDS(Etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Etnia.rds"))

###--- Municipio ---###

saveRDS(Municipio, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Municipio.rds"))

###--- Edad ---###

saveRDS(Edad, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Edad.rds"))

###--- Departamento x etnia ---###

saveRDS(departamento_etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Departamento_etnia.rds"))


###--- Nal ---###

saveRDS(Nal, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Nacional.rds"))


###--- Clase ---###

saveRDS(Clase, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Clase.rds"))

