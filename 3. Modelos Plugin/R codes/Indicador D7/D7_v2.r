################################################################################
##                     Proyecto MRP - Left No One Behind                      ##
## Title:        Indicador de planificación familiar D6m - Uso de métodos     ##
##               modernos de planificación                                    ##
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
## Modificado:   Sebastián Oviedo                                             ##
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

options(encoding = "UTF-8", scipen = 999)

mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"

###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds"))  %>% filter(unida == 1)

###------------ Anexando los Senate Weights a la base de la ENDS ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###--------------------------------- CENSO ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds"))  %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

###----- Identificador de Municipio - Departamento por persona del censo ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###----- Variables diseño ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))


Divipola <- Municipio %>% distinct(Divipola, .keep_all = T)

Divipola$departamento = as.character(as.numeric(Divipola$departamento))
Divipola$municipio = as.character(as.numeric(Divipola$municipio))

Xencuesta$departamento = as.character(as.numeric(Xencuesta$departamento))
Xencuesta$municipio = as.character(as.numeric(Xencuesta$municipio))


Xencuesta <- Xencuesta %>%
                    left_join(., Variables.disenio,
                              by = c("Divipola"= "Divipola"))

###------------- Anexando el código Divipola a la base del Censo ------------###
Xcenso <- Xcenso %>% left_join(., Variables.disenio,
                              by = c("Divipola"= "Divipola"))

###--- Limpiando memoria ---###

rm(Municipio, Divipola)



###------------ Anexando los dominios ------------###

Xencuesta = Xencuesta %>% mutate(departamento_etnia = paste0(departamento,"_", etnia))
Xcenso = Xcenso %>% mutate(departamento_etnia = paste0(departamento,"_", etnia))


# Demanda - comparar con la tabla 
de <- Xencuesta %>% #filter(unida==1) %>% # mujeres unidas
  transmute(num=ifelse(usamoderno == 1,v005,0), den=ifelse(usametodo == 1 | nec_ins == 1,v005,0)) %>%  
  summarise(per_uso=100*sum(num)/sum(den)) # 86.6
de


################################################################################
                      ### Ajuste del modelo Plugin ###
################################################################################

###--------------------------- Modelo saturado D6 ---------------------------###

pluginreg1 <- glmer(usametodo ~  inasistencia  +   ocupada + 
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

###--------------------------- Modelo saturado D6M --------------------------###

pluginreg2 <- glmer(usamoderno ~  inasistencia  +   ocupada + 
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

###---------------------------- Modelo saturado NI --------------------------###

pluginreg3 <- glmer(nec_ins ~ inasistencia  +   ocupada + 
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
#---------------- Exportando salidas: Modelos Plugin ajustados ----------------#
#------------------------------------------------------------------------------#
saveRDS(pluginreg1, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6UMunicipio.rds"))
saveRDS(pluginreg2, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6MUMunicipio.rds"))
saveRDS(pluginreg3, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginNIUMunicipio.rds"))



pluginreg1 = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6UMunicipio.rds"))
pluginreg2 = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6MUMunicipio.rds"))
pluginreg3 = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginNIUMunicipio.rds"))


#------------------------------------------------------------------------------#
#------- Exportando los efectos fijos comunes a los dominios por modelo -------#
#------------------------------------------------------------------------------#

#--- Modelo saturado: D6 ---#

betas1 = as.matrix(fixef(pluginreg1))
betas1

#--- Modelo saturado: D6M ---#

betas2 = as.matrix(fixef(pluginreg2))
betas2

#--- Modelo saturado: NI ---#

betas3 = as.matrix(fixef(pluginreg3))
betas3


###------------ Construcción de la matriz censal sintética XBeta ------------###
a1 = rownames(betas1)[-1]
a2 = rownames(betas2)[-1]
a3 = rownames(betas3)[-1]
mat_Censo_SW_sat <- cbind.data.frame(Xcenso$departamento_etnia, 
                                cbind(1, as.matrix(Xcenso %>% as.data.table %>%.[,..a1,])) %*% betas1,
                                cbind(1, as.matrix(Xcenso %>% as.data.table %>%.[,..a2,])) %*% betas2,
                                cbind(1, as.matrix(Xcenso %>% as.data.table %>%.[,..a3,])) %*% betas3)

colnames(mat_Censo_SW_sat) <- c("Divipola", "XB1", "XB2", "XB3")
head(mat_Censo_SW_sat)


#-- Códigos de los municipios en el censo para el ciclo de estimación Plugin --#

Div = unique(Xcenso$departamento_etnia)

#------------------------------------------------------------------------------#
#------- Exportando los efectos aleatorios para cada uno de los dominios ------#
#------------------------------------------------------------------------------#

udSW_sat =  data.frame(departamento_etnia = rownames(ranef(pluginreg1)$departamento_etnia), 
                 D6 = ranef(pluginreg1)$departamento_etnia[[1]], 
                 D6M = ranef(pluginreg2)$departamento_etnia[[1]],
                 NI = ranef(pluginreg3)$departamento_etnia[[1]])
rownames(udSW_sat) <- NULL


#-- Creando vector de efectos aleatorios para todos los dominios del país -#

udSW_sat = data.frame(departamento_etnia = Div) %>% left_join(udSW_sat, by = "departamento_etnia")

#----- Si el dominio no fue encuestado tendrá un efecto aleatorio de 0 ----#

udSW_sat$D6M[is.na(udSW_sat$D6M)] <- 0
udSW_sat$D6[is.na(udSW_sat$D6)] <- 0
udSW_sat$NI[is.na(udSW_sat$NI)] <- 0


#-------- Creando los vectores nulos para ser reemplazados en el ciclo --------# 
#--------                     de estimación Plugin                     --------#

Xcenso$pluginD6U = numeric(dim(Xcenso)[1])
Xcenso$pluginD6MU = numeric(dim(Xcenso)[1])
Xcenso$pluginNIU = numeric(dim(Xcenso)[1])
Xcenso$pluginD7 = numeric(dim(Xcenso)[1])

# COMMAND ----------

###---------- Ciclo de estimación plugin por individuo en el censo ----------###

for(i in 1:length(Div)){
  
  print(i)
  
  ### Identificación del dominio ###
  
  index = which(Xcenso$departamento_etnia == Div[i])
  
  ###---Probabilidad de uso de algún método anticonceptivo ---###
  
  ###--- Indicador D6 ---###
  
  Xcenso$pluginD6U[index] = exp(mat_Censo_SW_sat$XB1[index] + udSW_sat$D6[i])/
                            (1 + exp(mat_Censo_SW_sat$XB1[index] + udSW_sat$D6[i]))
  
  ###--- Indicador D6M ---###
  
  Xcenso$pluginD6MU[index] = exp(mat_Censo_SW_sat$XB2[index] + udSW_sat$D6M[i])/
                             (1 + exp(mat_Censo_SW_sat$XB2[index] + udSW_sat$D6M[i]))
  
  ###--- Indicador NI ---###
  
  Xcenso$pluginNIU[index] = exp(mat_Censo_SW_sat$XB3[index] + udSW_sat$NI[i])/
                             (1 + exp(mat_Censo_SW_sat$XB3[index] + udSW_sat$NI[i]))
  
  ###--- Indicador D7 ---###
  
  Xcenso$pluginD7[index] = Xcenso$pluginD6MU[index]/
                            (Xcenso$pluginD6U[index] + Xcenso$pluginNIU[index])
 
  print(paste(Div[i],"No muestreada")) 
}



###------------ Proporción de uso de algún método anticonceptivo ------------###
###--------------------- Estimación plugin departamental --------------------### 

depto_D7 <- Xcenso %>% group_by(departamento) %>% summarise(D7 = mean(pluginD7))
depto_D7

###----------------------- Estimación plugin municipal ----------------------###

municipio_D7 <- Xcenso %>% group_by(Divipola) %>% summarise(D7 = mean(pluginD7))
municipio_D7

###----------------------- Estimación plugin por etnia ----------------------###

###--- Recodificando la variable etnia según los niveles de MrP ---##

etnia_D7 <- Xcenso %>% group_by(etnia) %>% summarise(D7 = mean(pluginD7))
etnia_D7

###----------------- Estimación plugin: Departamento X etnia ----------------###

depto_etnia_D7 <- Xcenso %>% group_by(departamento, etnia) %>% 
                 summarise(D7 = mean(pluginD7))
depto_etnia_D7

###------------------- Estimación plugin: Municipio X etnia -----------------###

municipio_etnia_D7 <- Xcenso %>% group_by(Divipola, etnia) %>%
                      summarise(D7 = mean(pluginD7))
municipio_etnia_D7

################################################################################
###                                  Benchmark                                 #
################################################################################
library(srvyr)
###--------------------------------------------------------------------------###
##Proporción de mujeres que no quiere iniciar maternidad reportado por la ENDS##
###--------------------------------------------------------------------------###

medias <- numeric(36)

dis_paso <- Xencuesta %>% srvyr::as_survey_design(ids = idpers, strata = departamento, 
                                           weights = fexp)

###--- Nacional ---###

medias[1] <- (dis_paso %>% summarise(p = survey_ratio(usamoderno, 
                                                      usametodo + nec_ins)))$p

###--- Urbana ---###

medias[2] <- (dis_paso %>% filter(area_urbano == 1) %>% 
              summarise(p = survey_ratio(usamoderno, usametodo + nec_ins)))$p

###--- Rural ---###

medias[3] <- (dis_paso %>% filter(area_rural == 1) %>% 
                summarise(p = survey_ratio(usamoderno, usametodo + nec_ins)))$p

###--- Departamento ---###

 medias[4:36] <- Xencuesta %>% group_by(departamento) %>% 
  transmute(num=ifelse(usamoderno == 1,v005,0), den=ifelse(usametodo == 1 | nec_ins == 1,v005,0)) %>%  
  summarise(per_uso=sum(num)/sum(den)) %>% mutate(depto = as.numeric(as.character(departamento))) %>% arrange(depto) %>% 
  select(per_uso) %>% unlist(use.names = FALSE)


###-------------------------- Matriz de calibración -------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% #sample_n(100) %>%
  left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)
MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])

###------------------------ Función de pesos Benchmark ----------------------###

Benchmark <- function(CensoY){
  library(sampling)
  MatrizCalibrada2 = copy(MatrizCalibrada)
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
Xcenso$pesos_D7 <- Benchmark(Xcenso$pluginD7)
summary(Xcenso$pesos_D7)

summary(Xcenso$pluginD7)


###---------------------- Estimación benchmark Nacional ---------------------###

D7_Nal <- Xcenso %>% summarise(D7_2020 = mean(pluginD7),
                               D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()

###---------------------- Estimación benchmark Clase ---------------------###

D7_clase <- Xcenso %>% group_by(area_urbano) %>% 
                                summarise(D7_2020 = mean(pluginD7),
                                          D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()

###---------------------- Estimación benchmark Edad ---------------------###

D7_edad <- Xcenso %>% group_by(edad) %>% 
                          summarise(D7_2020 = mean(pluginD7),
                                    D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()


###-------------------- Estimación benchmark Departamental ------------------###

D7_depto <- Xcenso %>% group_by(departamento) %>% 
                                   summarise(D7_2020 = mean(pluginD7), 
                                             D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()


###-------------------- Estimación benchmark Municipal ------------------###

D7_Mun <- Xcenso %>% group_by(Divipola) %>% 
            summarise(D7_2020 = mean(pluginD7), 
            D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()

###---------------------- Estimación benchmark por etnia --------------------###

D7_dominio <- Xcenso %>% group_by(etnia) %>%
                         summarise(D7_2020 = mean(pluginD7), 
                                   D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()

###------------ Estimación benchmark por departamento etnia ------------------###

D7_depto_etnia <- Xcenso %>% group_by(departamento_etnia) %>%
                         summarise(D7_2020 = mean(pluginD7), 
                                   D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()


#----------------- Exporting Plugin estimates for post-stratum ----------------#

###--- Nal ---###
saveRDS(D7_Nal, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Nacional.rds"))


###--- Clase ---###
saveRDS(D7_clase, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Clase.rds"))

###--- Etnia ---###
saveRDS(D7_dominio, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Etnia.rds"))

###--- Municipio ---###
saveRDS(D7_Mun, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Municipio.rds"))

###--- Departamento ---###
saveRDS(D7_depto, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Departamento.rds"))

###---- Depto_etnia ----###
saveRDS(D7_depto_etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Departamento_etnia.rds"))

###--- Edad ---###
saveRDS(D7_edad, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Edad.rds"))


