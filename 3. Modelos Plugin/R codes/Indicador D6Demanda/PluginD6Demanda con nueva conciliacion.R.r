# Databricks notebook source
# MAGIC %python
# MAGIC 
# MAGIC ## Entrada de los datos
# MAGIC storage_account_name = "stminticpobrezamultidim"
# MAGIC storage_account_access_key ="fWAjdSD0oMRt80Wv7DKW3Fl4s/CIpOX053MqqdCg2kV0t8uJw97n1qIfWHaNRYAW8ATp89SwBovYe3PhkFUOWQ=="
# MAGIC 
# MAGIC ### Set path of directory (that contains subfolders "code", "input", "output" and "plots")
# MAGIC working_directory = "/dbfs/mnt/pobreza/changeanalysis/func-mintic-pobrezamultidimensio/CNPV2018"
# MAGIC 
# MAGIC ### Container 
# MAGIC container = "azure-webjobs-hosts"
# MAGIC 
# MAGIC ### Configure blob storage account access key globally
# MAGIC spark.conf.set(
# MAGIC   "fs.azure.account.key."+storage_account_name+".blob.core.windows.net",
# MAGIC   storage_account_access_key)

# COMMAND ----------

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
################################################################################

###--- Cleaning R environment ---###

#rm(list = ls())
#gc()

#################
### Libraries ###
#################

library(dplyr)
library(survey)
#library(srvyr)
library(sae)
library(lme4)
library(data.table)
library(haven)
library(formula.tools)
library(remotes)
#library(StatisticalModels)
select <- dplyr::select

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(640000000)

# COMMAND ----------

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

mount_point = "/dbfs/mnt/pobreza/changeanalysis/func-mintic-pobrezamultidimensio/Colombia-UNFPA"
memory.limit(640000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

options(encoding = "UTF-8", scipen = 999)

###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds")) %>% filter(unida == 1)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
###------------ Anexando los Senate Weights a la base de la ENDS ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)

###--------------------------------- CENSO ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds")) %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

###-------------------- Número de municipios en la ENDS ---------------------###

sum(unique(Xencuesta$Divipola) %in% unique(Xcenso$Divipola))

###----- Identificador de Municipio - Departamento por persona del censo ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###----- Variables diseño ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))


# COMMAND ----------


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

# COMMAND ----------

# Demanda - comparar con la tabla 
de <- Xencuesta %>% filter(unida==1) %>% # mujeres unidas
  transmute(num=ifelse(usamoderno == 1,v005,0), den=ifelse(usametodo == 1 | nec_ins == 1,v005,0)) %>%  
  summarise(per_uso=100*sum(num)/sum(den)) # 86.6
de

# COMMAND ----------


################################################################################
                         ### Ajuste del modelo Plugin ###
################################################################################

###----------------------------- Modelo saturado ----------------------------###

pluginregSW_sat <- glmer(demanda  ~ ind_hacinamiento  + ocupada + 
                   inasistencia  + 
                   edad_13_14 +
                   edad_15_19 + edad_20_24 + edad_25_29 + edad_30_34 +                   
                   edad_34_39 + edad_40_44 + 
                   hijos +  escolaridad_primaria + escolaridad_secundaria  + escolaridad_superior + migracion_migra_int + 
                   reconoce_etnia + misma_etnia_1 + reconoce_etnia*misma_etnia_1+
                   aguacocina_acueducto_publico_veredal_agua_embotellada +
                   matpiso_maderaburda_tabla_tierraarena + 
                   matpared_guadua_maderaburda_esterilla  +
                   internet_Si   + 
                   electricidad_si + tipo_viv_casa_departamento +
                   (1|departamento_etnia), family = "binomial", weights = Sweights, data = Xencuesta) 



# COMMAND ----------

summary(pluginregSW_sat)

# COMMAND ----------


#------------------------------------------------------------------------------#
#----------------- Exportando salidas: Modelo Plugin ajustado -----------------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

saveRDS(pluginregSW_sat, file = "PluginD6Demanda_SW_Sat_Mun.rds")

# COMMAND ----------

getwd()

# COMMAND ----------

# MAGIC %python
# MAGIC 
# MAGIC mount_point = "/mnt/pobreza/changeanalysis/func-mintic-pobrezamultidimensio/Colombia-UNFPA/"
# MAGIC sample_img_dir1 = "PluginD6Demanda_SW_Sat_Mun.rds"   
# MAGIC dbutils.fs.cp("file:/tmp/Rserv/conn29342/" + sample_img_dir1, mount_point + "3. Modelos PlugIn/Output/NI/") 
# MAGIC dbutils.fs.rm("file:/tmp/Rserv/conn29342/" + sample_img_dir1 + "")

# COMMAND ----------

#------------------------------------------------------------------------------#
#------------- Exportando los efectos fijos comunes a los dominios ------------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

betasSW_sat = as.matrix(fixef(pluginregSW_sat))
betasSW_sat


#------------------------------------------------------------------------------#
#------- Exportando los efectos aleatorios para cada uno de los dominios ------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

udSW_sat = cbind.data.frame(indice = rownames(ranef(pluginregSW_sat)$departamento_etnia),
                            ud = ranef(pluginregSW_sat)$departamento_etnia[[1]])


# COMMAND ----------



#------------------------------------------------------------------------------#
#-------------- En esta sección se emplea solo el modelo saturado -------------#
#------------------------------------------------------------------------------#

###------------ Construcción de la matriz censal sintética XBeta ------------###

mat_CensoSW_sat <- cbind.data.frame(Xcenso$Divipola,cbind(1,as.matrix(Xcenso %>%
                                    select(rownames(betasSW_sat)[-1]))) %*% 
                                    betasSW_sat)
colnames(mat_CensoSW_sat) <- c("Divipola","XB")


# COMMAND ----------


#- Creando el vector nulo para ser reemplazado en el ciclo de estimación Plugin -#

Xcenso$pluginD6M = numeric(nrow(Xcenso))

#-- Códigos de los municipios en el censo para el ciclo de estimación Plugin --#

Div = unique(Xcenso$departamento_etnia)

# COMMAND ----------



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
    
    Xcenso$pluginD6M[index] = exp(mat_CensoSW_sat$XB[index] + u)/
                                  (1 + exp(mat_CensoSW_sat$XB[index] + u))
    
    print(paste(Div[i], "muestreada"))
    
    ###--- Estimación en áreas no muestreadas por la ENDS ---###
    
  }else{
    
    ###---Probabilidad de uso de algún método anticonceptivo ---###
    
    Xcenso$pluginD6M[index] = exp(mat_CensoSW_sat$XB[index])/
                                  (1 + exp(mat_CensoSW_sat$XB[index]))
    
    print(paste(Div[i],"No muestreada"))
  }
}

# COMMAND ----------

#------------------------------------------------------------------------------#
#--------------- Exportando salidas: Censo con estimación Plugin --------------#
#------------------------------------------------------------------------------#

saveRDS(Xcenso, file = "XcensoD6DemandaDivipola.rds")



# COMMAND ----------

# MAGIC %python
# MAGIC mount_point = "/mnt/pobreza/changeanalysis/func-mintic-pobrezamultidimensio/Colombia-UNFPA/"
# MAGIC sample_img_dir1 = "XcensoD6DemandaDivipola.rds"   
# MAGIC dbutils.fs.cp("file:/tmp/Rserv/conn29342/" + sample_img_dir1, mount_point + "3. Modelos PlugIn/Output/D6M/") 
# MAGIC dbutils.fs.rm("file:/tmp/Rserv/conn29342/" + sample_img_dir1 + "")

# COMMAND ----------


###--------- Proporción de uso de métodos anticonceptivos modernos ----------###
###--------------------- Estimación plugin departamental --------------------### 

deptoD6M <- Xcenso %>% group_by(departamento) %>% summarise(D6M = mean(pluginD6M))
deptoD6M 

###----------------------- Estimación plugin municipal ----------------------###

municipioD6M <- Xcenso %>% group_by(Divipola) %>% summarise(D6M = mean(pluginD6M))
municipioD6M

###----------------------- Estimación plugin por etnia ----------------------###

etniaD6M <- Xcenso %>% group_by(etnia) %>% summarise(D6M = mean(pluginD6M))
etniaD6M

###----------------- Estimación plugin: Departamento X etnia ----------------###

depto_etniaD6M <- Xcenso %>% group_by(departamento, etnia) %>%
                             summarise(D6M = mean(pluginD6M))
depto_etniaD6M

###------------------- Estimación plugin: Municipio X etnia -----------------###

municipio_etniaD6M <- Xcenso %>% group_by(Divipola, etnia) %>%
                                 summarise(D6M = mean(pluginD6M))
municipio_etniaD6M

# COMMAND ----------


################################################################################
###                                  Benchmark                                 #
################################################################################

###--------------------------------------------------------------------------###
###   Proporción de mujeres con necesidades de planificación insatisfechas   ### 
###                           reportado por la ENDS                          ###                         
###--------------------------------------------------------------------------###

medias <- numeric(36)

###--- Nacional ---###

medias[1] <- Xencuesta %>% summarise(p = weighted.mean(demanda, fexp)) %>% 
  as.numeric()

###--- Urbana ---###

medias[2] <- Xencuesta %>% filter(area_urbano == 1) %>% 
  summarise(p = weighted.mean(demanda, fexp)) %>% as.numeric()

###--- Rural ---###

medias[3] <- Xencuesta %>% filter(area_rural == 1) %>% 
  summarise(p = weighted.mean(demanda,fexp)) %>% as.numeric()

###--- Departamento ---###

 medias[4:36] <- Xencuesta %>% group_by(departamento) %>% 
   summarise(p = weighted.mean(demanda,fexp)) %>% select(p) %>% unlist(use.names = FALSE)
medias

# COMMAND ----------

###-------------------------- Matriz de calibración -------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% as.data.frame() %>% #sample_n(100) %>%
             left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)



# COMMAND ----------

MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])
names(MatrizCalibrada)

# COMMAND ----------


###------------------------ Función de pesos Benchmark ----------------------###

Benchmark <- function(CensoY){
  library(sampling)
  MatrizCalibrada2 = MatrizCalibrada
  for(i in 1:36){
    MatrizCalibrada2[,i] <- MatrizCalibrada[,i] * (CensoY - medias[i])
  }
  a = calib(Xs = MatrizCalibrada2, 
            d = rep(1, dim(MatrizCalibrada)[1]), 
       
            total = medias,#rep(0, 36), 
            method = c("logit")#,bounds=c(0.5,1.5)
           ) 
  return(a)
}

# COMMAND ----------


###--- Anexando pesos Benchmark al censo ---###

Xcenso$pesos_D6Demanda <- Benchmark(Xcenso$pluginD6M)
summary(Xcenso$pesos_D6Demanda)

# COMMAND ----------


###---------------------- Estimación benchmark Nacional ---------------------###

D6Demanda_Nal <- Xcenso %>% summarise(D6M_2020 = mean(pluginD6M_fex),
                               D6MB_2020 = weighted.mean(pluginD6M_fex, pesos_D6Demanda)
                               )


###-------------------- Estimación benchmark Clase ------------------###

D6Demanda_clase <- Xcenso %>% group_by(area_urbano) %>%
                        summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )
###-------------------- Estimación benchmark Departamental ------------------###

D6Demanda_depto <- Xcenso %>% group_by(departamento) %>%
                        summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )


###---------------------- Estimación benchmark Municipal --------------------###

D6Demanda_Mun <- Xcenso %>% group_by(departamento,Divipola) %>%
                      summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )


###---------------------- Estimación benchmark por etnia --------------------###

D6Demanda_dominio <- Xcenso %>% group_by(etnia) %>% 
                          summarise(D6M_2020 = mean(pluginD6M_fex),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )

###---------------------- Estimación benchmark por etnia --------------------###

D6Demanda_etnia_depto <- Xcenso %>% group_by(departamento_etnia) %>% 
                          summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )



# COMMAND ----------

D6Demanda_Nal %>% as.data.frame()

# COMMAND ----------

D6Demanda_clase %>% as.data.frame()

# COMMAND ----------

D6Demanda_depto %>% as.data.frame()

# COMMAND ----------

#write.table(D6M_Mun, '/dbfs/FileStore/Salidas/Clasificacion/D6_Mun.csv', dec = ",", sep = ";")

D6Demanda_Mun %>% as.data.frame()

# COMMAND ----------

D6Demanda_dominio %>% as.data.frame()

# COMMAND ----------


#----------------- Exporting Plugin estimates for post-stratum ----------------#

###--- Etnia ---###

saveRDS(D6M_dominio, file = "D6M_plugin_dominio.rds")

###--- Municipio ---###

saveRDS(D6M_Mun, file = "D6M_plugin_Mun.rds")

###--- Departamento ---###

saveRDS(D6M_depto, file = "D6M_plugin_Departamento.rds")

# COMMAND ----------

write.table(D6M_depto, '/dbfs/FileStore/Salidas/Clasificacion/D6M_plugin_Departamento.csv')

# COMMAND ----------

# MAGIC %python
# MAGIC mount_point = "/mnt/pobreza/changeanalysis/func-mintic-pobrezamultidimensio/Colombia-UNFPA/"
# MAGIC sample_img_dir1 = "D6M_plugin_dominio.rds"   
# MAGIC dbutils.fs.cp("file:/tmp/Rserv/conn1872/" + sample_img_dir1, mount_point + "3. Modelos PlugIn/Output/D6M/") 
# MAGIC dbutils.fs.rm("file:/tmp/Rserv/conn1872/" + sample_img_dir1 + "")

# COMMAND ----------

# MAGIC %python
# MAGIC mount_point = "/mnt/pobreza/changeanalysis/func-mintic-pobrezamultidimensio/Colombia-UNFPA/"
# MAGIC sample_img_dir1 = "D6M_plugin_Mun.rds"   
# MAGIC dbutils.fs.cp("file:/tmp/Rserv/conn1872/" + sample_img_dir1, mount_point + "3. Modelos PlugIn/Output/D6M/") 
# MAGIC dbutils.fs.rm("file:/tmp/Rserv/conn1872/" + sample_img_dir1 + "")

# COMMAND ----------

# MAGIC %python
# MAGIC mount_point = "/mnt/pobreza/changeanalysis/func-mintic-pobrezamultidimensio/Colombia-UNFPA/"
# MAGIC sample_img_dir1 = "D6M_plugin_Departamento.rds"   
# MAGIC dbutils.fs.cp("file:/tmp/Rserv/conn1872/" + sample_img_dir1, mount_point + "3. Modelos PlugIn/Output/D6M/") 
# MAGIC dbutils.fs.rm("file:/tmp/Rserv/conn1872/" + sample_img_dir1 + "")

# COMMAND ----------

