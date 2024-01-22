################################################################################
##                     Proyecto MRP - Left No One Behind                      ##
## Title:        Indicador de planificación familiar NI - Necesidad de método ##
##               de planificación insatisfecha                                ##
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

rm(list = ls())
gc()

#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)
library(sae)
library(lme4)
library(data.table)
library(haven)
library(formula.tools)
library(remotes)
library(StatisticalModels)

select <- dplyr::select



###------------ Definiendo el límite de la memoria RAM a emplear ------------###

mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

options(encoding = "UTF-8", scipen = 999)

###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds"))  %>% filter(unida == 1)

###------------ Anexando los Senate Weights a la base de la ENDS ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###--------------------------------- CENSO ----------------------------------###
Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds"))  %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

###-------------------- Número de municipios en la ENDS ---------------------###

sum(unique(Xencuesta$Divipola) %in% unique(Xcenso$Divipola))

###----- Identificador de Municipio - Departamento por persona del censo ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###----- Variables diseño ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))

###------------- Anexando el código Divipola a la base del Censo ------------###
Xcenso <- Xcenso %>% left_join(., Variables.disenio,
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

###--- Limpiando memoria ---###
#rm(Municipio, Divipola)


# NI - comparar con la tabla # 10.5.1.2
ni <- Xencuesta %>% filter(unida==1) %>% # mujeres unidas
  mutate(uso=ifelse(nec_ins %in% c(1),v005,0)) %>%  # demanda: unmet & met need
  summarise(per_uso=100*sum(uso)/sum(v005)) # 6.7
ni

################################################################################
                       ### Ajuste del modelo Plugin ###
################################################################################

###----------------------------- Modelo saturado ----------------------------###

pluginregSW_sat  <- glmer(nec_ins ~  inasistencia  +   ocupada + 
  edad_13_14 +
  edad_15_19 + edad_20_24 + edad_25_29 + edad_30_34 +                   
  edad_34_39 + edad_40_44 + 
  hijos +  escolaridad_primaria + escolaridad_secundaria  + escolaridad_superior  + 
  reconoce_etnia + misma_etnia_1 + reconoce_etnia*misma_etnia_1+
  aguacocina_acueducto_publico_veredal_agua_embotellada +
  matpiso_maderaburda_tabla_tierraarena + 
  matpared_guadua_maderaburda_esterilla  +
  internet_Si   + 
  electricidad_si + tipo_viv_casa_departamento +
  (1|departamento_etnia), family = "binomial", weights = Sweights, data = Xencuesta) 



#------------------------------------------------------------------------------#
#----------------- Exportando salidas: Modelo Plugin ajustado -----------------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

saveRDS(pluginregSW_sat, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/PluginNI_SW_Sat_Mun.rds"))

################################################################################
#----------------------- Estimación Plugin: indicador NI ----------------------#
################################################################################

pluginregSW_sat = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/NI/PluginNI_SW_Sat_Mun.rds"))

#------------------------------------------------------------------------------#
#------------- Exportando los efectos fijos comunes a los dominios ------------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

betasSW_sat = as.matrix(fixef(pluginregSW_sat))

#------------------------------------------------------------------------------#
#------- Exportando los efectos aleatorios para cada uno de los dominios ------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

udSW_sat = cbind.data.frame(indice = rownames(ranef(pluginregSW_sat)$departamento_etnia),
                            ud = ranef(pluginregSW_sat)$departamento_etnia[[1]])

#------------------------------------------------------------------------------#
#-------------- En esta sección se emplea solo el modelo saturado -------------#
#------------------------------------------------------------------------------#

###------------ Construcción de la matriz censal sintética XBeta ------------###

mat_CensoSW_sat <- cbind.data.frame(Xcenso$departamento_etnia, cbind(1,as.matrix(Xcenso %>%
                                    select(rownames(betasSW_sat)[-1]))) %*% 
                                    betasSW_sat)
colnames(mat_CensoSW_sat) <- c("Divipola","XB")

#- Creando el vector nulo para ser reemplazado en el ciclo de estimación Plugin -#

Xcenso$pluginNI = numeric(5980476)

#-- Códigos de los municipios en el censo para el ciclo de estimación Plugin --#

Div = unique(Xcenso$departamento_etnia)

###---------- Ciclo de estimación plugin por individuo en el censo ----------###

for(i in 1:length(Div)){
  
  print(i)
  
  ### Identificación del dominio ###
  
  index = which(Xcenso$departamento_etnia == Div[i])
  
  ###--- Estimación en las áreas muestreadas por la ENDS ---###
  
  if(Div[i] %in% udSW_sat$indice){
    
    ###--- Seleccionando el efecto aleatorio del dominio ---###
    
    u = udSW_sat$ud[which(as.character(udSW_sat$indice) == Div[i])]
    
    ###---Probabilidad de uso de algún método anticonceptivo ---###
    
    Xcenso$pluginNI[index] = exp(mat_CensoSW_sat$XB[index] + u)/
                             (1 + exp(mat_CensoSW_sat$XB[index] + u))
    
    print(paste(Div[i], "muestreada"))
    
    ###--- Estimación en áreas no muestreadas por la ENDS ---###
    
  }else{
    
    ###---Probabilidad de uso de algún método anticonceptivo ---###
    
    Xcenso$pluginNI[index] = exp(mat_CensoSW_sat$XB[index])/
                             (1 + exp(mat_CensoSW_sat$XB[index]))
    
    print(paste(Div[i],"No muestreada"))
  }
}

###-- Proporción de mujeres con necesidades de planificación insatisfechas --###
###--------------------- Estimación plugin departamental --------------------### 

depto <- Xcenso %>% group_by(departamento) %>% summarise(NI = mean(pluginNI))


###----------------------- Estimación plugin municipal ----------------------###

municipio <- Xcenso %>% group_by(Divipola) %>%
  summarise(NI = mean(pluginNI, na.rm = T))

###----------------------- Estimación plugin por etnia ----------------------###

etnia <- Xcenso %>% group_by(etnia) %>%
  summarise(NI = mean(pluginNI, na.rm = T))

###----------------- Estimación plugin: Departamento X etnia ----------------###

municipio_etnia <- Xcenso %>% group_by(Divipola, etnia) %>%
  summarise(NI = mean(pluginNI, na.rm = T))



################################################################################
###                                  Benchmark                                 #
################################################################################

###--------------------------------------------------------------------------###
###   Proporción de mujeres con necesidades de planificación insatisfechas   ### 
###                           reportado por la ENDS                          ###                         
###--------------------------------------------------------------------------###

medias <- numeric(36)

###--- Nacional ---###

medias[1] <- Xencuesta %>% summarise(p = weighted.mean(nec_ins, fexp)) %>% 
  as.numeric()

###--- Urbana ---###

medias[2] <- Xencuesta %>% filter(area_urbano == 1) %>% 
  summarise(p = weighted.mean(nec_ins, fexp)) %>% as.numeric()

###--- Rural ---###

medias[3] <- Xencuesta %>% filter(area_rural == 1) %>% 
  summarise(p = weighted.mean(nec_ins,fexp)) %>% as.numeric()

###--- Departamento ---###

 medias[4:36] <- Xencuesta %>% group_by(departamento) %>% 
   summarise(p = weighted.mean(nec_ins,fexp)) %>% mutate(depto = as.numeric(as.character(departamento))) %>% arrange(depto) %>% 
select(p) %>% unlist(use.names = FALSE)


###-------------------------- Matriz de calibración -------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% as.data.frame() %>% #sample_n(100) %>%
             left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)

MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])
names(MatrizCalibrada)


###------------------------ Función de pesos Benchmark ----------------------###

Benchmark <- function(CensoY){
  library(sampling)
  MatrizCalibrada2 = MatrizCalibrada
  for(i in 1:36){
    MatrizCalibrada2[,i] <- MatrizCalibrada[,i] * (CensoY - medias[i])
  }
  a = calib(Xs = MatrizCalibrada2, 
            d = rep(1, dim(MatrizCalibrada)[1]), 
            total = medias,
            method = c("logit")
           ) 
  return(a)
}


###--- Anexando pesos Benchmark al censo ---###
Xcenso$pesos_NI <- Benchmark(Xcenso$pluginNI)
summary(Xcenso$pesos_NI)


###---------------------- Estimación benchmark Nacional ---------------------###

NI_Nal <- Xcenso %>% summarise(NI_2020 = mean(pluginNI),
                               NIB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()


###-------------------- Estimación benchmark Departamental ------------------###

NI_depto <- Xcenso %>% group_by(departamento) %>%
  summarise(NI_2020 = mean(pluginNI),
            NIB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()

###-------------------- Estimación benchmark Municipal ------------------###

NI_Mun <- Xcenso %>% group_by(departamento, Divipola) %>%
  summarise(NI_2020 = mean(pluginNI),
            NIB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()


###---------------------- Estimación benchmark Clase ---------------------###

NI_clase <- Xcenso %>% group_by(area_urbano) %>% summarise(D6_2020 = mean(pluginNI),
                               D6B_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()


###---------------------- Estimación benchmark Edad ---------------------###

NI_edad <- Xcenso %>% group_by(edad) %>% summarise(D6_2020 = mean(pluginNI),
                               D6MB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()



###---------------------- Estimación benchmark por etnia --------------------###

NI_dominio <- Xcenso %>% group_by(etnia) %>% 
              summarise(NI_2020 = mean(pluginNI),
                        NIB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()


###-------------------- Estimación benchmark por departamento  etnia ------------------###

NI_departamento_etnia <- Xcenso %>% group_by(departamento_etnia) %>% 
  summarise(NI_2020 = mean(pluginNI),
            NIB_2020 = weighted.mean(pluginNI, pesos_NI)) %>% as.data.frame()


#----------------- Exporting Plugin estimates for post-stratum ----------------#
###--- Nal ---###

saveRDS(NI_Nal, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Nacional.rds"))

###--- Etnia ---###

saveRDS(NI_dominio, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Etnia.rds"))

###--- Municipio ---###

saveRDS(NI_Mun, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Municipio.rds"))

###--- Departamento ---###

saveRDS(NI_depto, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Departamento.rds"))

###--- Departamento x etnia ---###

saveRDS(NI_departamento_etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Departamento_etnia.rds"))


###--- Edad ---###

saveRDS(NI_edad, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Edad.rds"))

###--- Edad ---###

saveRDS(NI_clase, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Clase.rds"))

NI_clase
