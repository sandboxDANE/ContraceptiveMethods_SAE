######################################################################################################## 
## Title:        Family Planning Indicator D7 - Use of long-lasting anticonceptual methods.           ##
## Returns:      Estimation Plugin by Domains of Interest                                             ##  
## The code was modified by Lina Sánchez, Sebastián Oviedo, DANE.                                     ## 
## Carlos Rámirez,  Université de Montréal, Juliana Guerrero, World Bank.                             ##
## The original code was developed by Felipe Molina, Andrés Gutiérrez and Diego Lemus in the MRP      ##                                               
## Project - Left No One Behind, Institution: ECLAC, Division: Statistics Division.                   ##                
########################################################################################################

###--- Cleaning R environment ---###

#rm(list = ls())
#gc()

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

###------------ Defining the limit of RAM memory to be used------------###

memory.limit(160000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

options(encoding = "UTF-8", scipen = 999)


mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"

###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds"))  %>% filter(unida == 1)

###------------   Senate Weights                                 ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###--------------------------------- CENSUS ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds"))  %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

###----- Identification of Municipality - Department per census person ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds")) 

###------------- Desing variables -----------------------------------------###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))



###------------- Appending the Divipola code to the Census database -------------###
Xcenso <- Xcenso  %>% left_join(., Variables.disenio,
                              by = c("Divipola"= "Divipola"))

###------------- List of the 1122 municipalities -------------###

Divipola <- Municipio %>% distinct(Divipola, .keep_all = T)
Divipola$departamento = as.character(as.numeric(Divipola$departamento))
Divipola$municipio = as.character(as.numeric(Divipola$municipio))

Xencuesta$departamento = as.character(as.numeric(Xencuesta$departamento))
Xencuesta$municipio = as.character(as.numeric(Xencuesta$municipio))

###------------ Appending the Divipola code to the ENDS database  ------------###

Xencuesta <- Xencuesta %>% left_join(., Variables.disenio,
                              by = c("Divipola"= "Divipola"))

###------------ Annexing the categorical dependent variable   ------------###

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


###--- Clearing Memory ---###
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
## use and demand for contraceptive methods
###############################################################################
# uso metodos - Use of long term methodologies
ld <- Xencuesta  %>% #group_by(area_urbano) %>% #filter(v502==1) %>% # mujeres unidas
  mutate(ld = ifelse(larga_duracion %in% c('3_Larga'),v005,0)) %>%  # actualmente usa algún método
  summarise(per_uso=100*sum(ld)/sum(v005)) # 10.1274

ld

# ld by depto

ld_depto <- Xencuesta %>%  group_by(departamento)%>% #filter(unida==1) %>% # mujeres unidas
  mutate(uso=ifelse(larga_duracion %in% c('1_NoUse'),v005,0)) %>%  # actualmente usa algún método moderno
  summarise(per_uso=100*sum(uso)/sum(v005)) %>% as.data.frame() # 75.9
ld_depto

################################################################################
                         ### Fitting the Plug-in model###
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
#----------------- Exporting outputs: Plugin model fitted     -----------------#
#------------------------------------------------------------------------------#

saveRDS(pluginreg, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/PluginLD2SW_Mun.rds"))



pluginreg = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/LD/PluginLD2SW_Mun.rds"))




#------------------------------------------------------------------------------#
#------------- Exporting common fixed effects to domains           ------------#
#------------------------------------------------------------------------------#

betas = as.matrix(pluginreg$beta)

#------------------------------------------------------------------------------#
#------- Exporting the random effects for each of the domains            ------#
#------------------------------------------------------------------------------#

###--- Saturated Model ---###

udSW = cbind.data.frame(indice = levels(as.factor(Xencuesta$departamento_etnia)),
                        ud = pluginreg$ranef)


#------------------------------------------------------------------------------#
#-------------- In this section only the saturated model is used. -------------#
#------------------------------------------------------------------------------#
select = dplyr::select
###------------ Construction of the Synthetic Census Matrix XBeta ------------###

mat_CensoSW <- cbind.data.frame(Xcenso$departamento_etnia, cbind(as.matrix(Xcenso %>%
                                    select(rownames(betas)))) %*% 
                                    betas)
colnames(mat_CensoSW) <- c("Divipola","XB")

#-  Intercepts for each model                                                 -#

mat_CensoSW$Alpha1_2 = pluginreg$Alpha[[1]]

#- Creating the Null Vector to be Replaced in the Plugin Estimation Cycle -#

Xcenso$pluginLD_1 = rep(0,5980476)

#-- Municipality Codes in the Census for the Plugin Estimation Cycle --#

Div = unique(Xcenso$departamento_etnia)

###---------- Individual Census Plugin Estimation Cycle ----------###

for(i in 1:length(Div)){
  
  print(i)

  ###--- Domain Identification ---###
  
  index = which(Xcenso$departamento_etnia == Div[i])
  
  ###--- Estimation in the Areas Sampled by the ENDS ---###
  
  if(Div[i] %in% udSW$indice){
    
  ###--- Selecting the Random Effect of the Domain ---###
    
    u = udSW$ud[which(as.character(udSW$indice) == Div[i])]
    
    ###--- Probability of Using Any Contraceptive Method ---###
    
    Xcenso$pluginLD_1[index] = exp(  mat_CensoSW$Alpha1_2[index] - mat_CensoSW$XB[index] + u )/
                                 (1 + exp( mat_CensoSW$Alpha1_2[index] - mat_CensoSW$XB[index] + u ))
        
   print(paste(Div[i], "muestreada"))
    
  ###--- Estimación en áreas no muestreadas por la ENDS ---###
    
  }else{
    
    ###--- Estimation in Non-Sampled Areas by the ENDS ---###
    
    Xcenso$pluginLD[index] = exp(mat_CensoSW$XB[index])/
                                 (1 + exp(mat_CensoSW$XB[index]))
    
    print(paste(Div[i],"No muestreada"))
  }
}

Xcenso = Xcenso %>% mutate(LD1 = pluginLD_1,
                            LD2 = 1 - pluginLD_1,)


###--------------------- Departmental Plugin Estimation --------------------###


depto <- Xcenso %>% group_by(departamento) %>% summarise(Larga = mean(LD1),
                                                         Corta = mean(LD2))

###--------------------- Municipality Plugin Estimation --------------------###

municipio <- Xcenso %>% group_by(Divipola) %>%  summarise(Larga = mean(LD1),
                                                          Corta = mean(LD2))

###----------------------- Plugin Estimation by Ethnicity ----------------------###

etnia <- Xcenso %>% group_by(etnia) %>%  summarise(Larga = mean(LD1),
                                                   Corta = mean(LD2))


###----------------- Plugin Estimation: Department X Ethnicity ----------------###

municipio_etnia <- Xcenso %>% group_by(departamento, etnia) %>% summarise(Larga = mean(LD1),
                                                                          Corta = mean(LD2))



###--------------------------------------------------------------------------###
###    Proportion of contraceptive method use reported by the ENDS           ###
###--------------------------------------------------------------------------###
 ### Function to calculate averages for each level ###
mediasc = function(nivel){
  medias <- numeric(36)
  
  ###--- National ---###
  Xencuesta = Xencuesta %>%  mutate( grupo = ifelse(larga_duracion_2 == nivel, 1, 0) )
  medias[1] <- Xencuesta  %>% 
    summarise(p = weighted.mean(grupo, fexp)) %>% 
    as.numeric()
  
  ###--- Urban ---###
  
  medias[2] <- Xencuesta %>% filter(area_urbano == 1 ) %>% 
    summarise(p = weighted.mean(grupo, fexp)) %>% as.numeric()
  
  ###--- Rural ---###
  
  medias[3] <- Xencuesta %>% filter(area_rural == 1) %>% 
    summarise(p = weighted.mean(grupo,fexp)) %>% as.numeric()
  
  ###--- Department ---###
  
  medias[4:36] <- Xencuesta %>% 
    group_by(departamento) %>% 
    summarise(p = weighted.mean(grupo,fexp)) %>% mutate(depto = as.numeric(as.character(departamento))) %>% arrange(depto) %>% 
    select(p) %>% unlist(use.names = FALSE)
  
  return(medias)
}


#'2_Corta'       '1_Larga'
medias = mediasc('2_Corta')
medias


###-------------------------- Calibration Matrix -------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% #sample_n(100) %>%
             left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)
MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])

###------------------------Weigths Benchmark Function ----------------------###

Benchmark <- function(CensoY, media){
  library(sampling)
  MatrizCalibrada2 = MatrizCalibrada
  for(i in 1:36){
    MatrizCalibrada2[,i] <- MatrizCalibrada[,i] * (CensoY - media[i])
 }
  a = calib(Xs = MatrizCalibrada2, 
           # d = runif( dim(MatrizCalibrada)[1], 0, 1), 
            d = rep(1, dim(MatrizCalibrada)[1]), 
           
           total=media,
            method = c("logit"),
           # bounds=c(0,7),
            max_iter = 100,
           # description = TRUE
           ) 
  return(a)
}


###------------ Appending Benchmark Weights to the Census -------------###

total=c(t(rep(1,nrow(MatrizCalibrada)))%*%as.matrix(MatrizCalibrada))


medias = mediasc('1_Larga')
Xcenso$pesos_LagaD <- Benchmark(Xcenso$LD1,media = medias)
summary(Xcenso$pesos_LagaD)
gc()

medias = mediasc('2_Corta')
Xcenso$pesos_CortaD <- Benchmark(Xcenso$LD2,media = medias)
summary(Xcenso$pesos_CortaD)
gc()


###----------------------  Benchmark Estimation Nacional ---------------------###

Corta_Nal <- Xcenso %>% summarise(Corta_2020 = mean(LD2),
                                  CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_Nal')

Larga_Nal <- Xcenso %>% summarise(Larga_2020 = mean(LD1),
                                  LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_Nal')
Nal= Reduce(bind_rows, list(Corta_Nal,Larga_Nal )) %>% as.data.table() %>% 
  melt.data.table(.,id.vars = c('gr')) %>% .[is.na(value) == F] 


###--------------------  Benchmark Estimation Class ------------------###

Corta_clase <- Xcenso %>% group_by(area_urbano) %>%summarise(Corta_2020 = mean(LD2),
                                                             CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_clase')

Larga_clase <- Xcenso %>% group_by(area_urbano) %>%summarise(Larga_2020 = mean(LD1),
                                                             LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_clase')


Clase= Reduce(bind_rows, list(Corta_clase,Larga_clase)) %>% as.data.table() %>% 
                melt.data.table(.,id.vars = c('gr', 'area_urbano')) %>% .[is.na(value) == F] 

###--------------------  Benchmark Estimation Age ------------------###




Corta_edad <- Xcenso %>% group_by(edad) %>%summarise(Corta_2020 = mean(LD2),
                                                             CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_edad')

Larga_edad <- Xcenso %>% group_by(edad) %>%summarise(Larga_2020 = mean(LD1),
                                                             LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_edad')


Edad = Reduce(bind_rows, list(Corta_edad,Larga_edad )) %>% as.data.table() %>% 
  melt.data.table(.,id.vars = c('gr','edad')) %>% .[is.na(value) == F] 


###--------------------  Benchmark Estimation Municipality ------------------###

Corta_Mun  <- Xcenso %>% group_by(departamento,Divipola) %>%summarise(Corta_2020 = mean(LD2),
                                                     CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_Mun')

Larga_Mun  <- Xcenso %>%group_by(departamento,Divipola) %>%summarise(Larga_2020 = mean(LD1),
                                                     LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_Mun')


Municipio = Reduce(bind_rows, list(Corta_Mun,Larga_Mun )) %>% as.data.table() %>% 
  melt.data.table(.,id.vars = c('gr','departamento','Divipola')) %>% .[is.na(value) == F] 

###--------------------  Benchmark Estimation Ethnicity ------------------###

Corta_Etnia  <- Xcenso %>% group_by(etnia) %>%summarise(Corta_2020 = mean(LD2),
                                                                      CortaB_2020 = weighted.mean(LD2, pesos_CortaD), gr = 'Corta_Etnia')

Larga_Etnia  <- Xcenso %>% group_by(etnia) %>%summarise(Larga_2020 = mean(LD1),
                                                                     LargaB_2020 = weighted.mean(LD1, pesos_LagaD), gr = 'Larga_Etnia')

Etnia = Reduce(bind_rows, list(Corta_Etnia,Larga_Etnia ))  %>% as.data.table() %>% 
  melt.data.table(.,id.vars = c('gr','etnia')) %>% .[is.na(value) == F] 

###--------------------  Benchmark Estimation department x Ethnicity ------------------###


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

###--- Ethnicity ---###

saveRDS(Etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Etnia.rds"))

###--- Municipality ---###

saveRDS(Municipio, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Municipio.rds"))

###--- Age ---###

saveRDS(Edad, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Edad.rds"))

###--- department x Ethnicity ---###

saveRDS(departamento_etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Departamento_etnia.rds"))


###--- National ---###

saveRDS(Nal, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Nacional.rds"))


###--- Class ---###

saveRDS(Clase, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Clase.rds"))

