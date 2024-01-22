######################################################################################################## 
## Title:        Family Planning Indicator D6 - Use of Any Planning Method                            ##
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
library(haven)
#library(formula.tools)
library(remotes)
#library(StatisticalModels)

###------------ Defining the limit of RAM memory to be used ------------###

memory.limit(160000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

options(encoding = "UTF-8", scipen = 999)


mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"

###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds"))  %>% filter(unida == 1)

###------------ Anexando los Senate Weights a la base de la ENDS ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

Xencuesta = Xencuesta %>% mutate(indigena = ifelse(etnia == "indigena", 1, 0))
Xencuesta = Xencuesta %>% mutate(afro = ifelse(etnia == "palanquero_mulato_afrodescendiente", 1, 0))

###--------------------------------- CENSUS ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds"))  %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

Xcenso = Xcenso %>% mutate(indigena = ifelse(etnia == "indigena", 1, 0))
Xcenso = Xcenso %>% mutate(afro = ifelse(etnia == "palanquero_mulato_afrodescendiente", 1, 0))

###----- Census Person-Level Identifier for Municipality and Department ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds")) 

###----- Design Variables ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))



###------------- Appending the Divipola code to the Census database ------------###
Xcenso <- Xcenso  %>% left_join(., Variables.disenio,
                              by = c("Divipola"= "Divipola"))

###--------------------- List of the 1122 municipalities ---------------------###

Divipola <- Municipio %>% distinct(Divipola, .keep_all = T)
Divipola$departamento = as.character(as.numeric(Divipola$departamento))
Divipola$municipio = as.character(as.numeric(Divipola$municipio))

Xencuesta$departamento = as.character(as.numeric(Xencuesta$departamento))
Xencuesta$municipio = as.character(as.numeric(Xencuesta$municipio))

###------------ Appending the Divipola code to the ENDS database ------------###

Xencuesta <- Xencuesta %>% left_join(., Variables.disenio,
                              by = c("Divipola"= "Divipola"))


###--- Clearing memory ---###
rm(Municipio, Divipola)




###############################################################################
## Contraceptive Methods Use and Demand
###############################################################################
# Use of methods - Satisfied need for contraceptive methods

uso <- Xencuesta  %>% #group_by(area_urbano) %>% #filter(v502==1) %>% # mujeres unidas
  mutate(uso=ifelse(usametodo %in% c(1),v005,0)) %>%  # Currently using any method
  
  summarise(per_uso=100*sum(uso)/sum(v005)) # 80.9

uso

# demand
demanda <- Xencuesta %>% #filter(v502==1) %>% # Married women
  mutate(uso=ifelse(demanda %in% c(1),v005,0)) %>%  # demanda: unmet & met need
  summarise(per_uso=100*sum(uso)/sum(v005)) # 87.6
demanda



# uso métodos 
uso_m <- Xencuesta %>%  group_by(departamento)%>% #filter(unida==1) %>% # Married women
  mutate(uso=ifelse(usametodo %in% c(1),v005,0)) %>%  # Currently using any method
  summarise(per_uso=100*sum(uso)/sum(v005)) %>% as.data.frame() # 75.9
uso_m


################################################################################
### Plugin Model fitting                                                     ###
################################################################################


pluginreg <- glmer(usametodo ~ inasistencia  +   ocupada + 
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

####################################################################################
### Plugin Model fit including Ethnicity as a fixed effect variable       ###
### This is done to verify that the effect of contraceptive use does not depend ###
### on ethnicity but on age, education level, and so on.                         ###
####################################################################################


pluginreg_1 <- glmer(usametodo ~ inasistencia  +   ocupada + 
                     edad_13_14 +
                     edad_15_19 + edad_20_24 + edad_25_29 + edad_30_34 +                   
                     edad_34_39 + edad_40_44 + 
                     hijos +  escolaridad_primaria + escolaridad_secundaria  + escolaridad_superior  + 
                     #reconoce_etnia + misma_etnia_1 + reconoce_etnia*misma_etnia_1+
                     indigena + afro +
                     aguacocina_acueducto_publico_veredal_agua_embotellada +
                     matpiso_maderaburda_tabla_tierraarena + 
                     matpared_guadua_maderaburda_esterilla  +
                     internet_Si   + 
                     electricidad_si + tipo_viv_casa_departamento +
                     (1|departamento_etnia), family = "binomial", weights = Sweights, data = Xencuesta) 
summary(pluginreg)

#------------------------------------------------------------------------------#
#----------------- Exporting Outputs: Fitted Plugin Model ----------------------#
#------------------------------------------------------------------------------#

saveRDS(pluginreg_1, file = file.path(mount_point,"3. Modelos Plugin/Output/D6/Model_PluginD6_etnia_fixed.rds"))

saveRDS(pluginreg, file = file.path(mount_point,"3. Modelos Plugin/Output/D6/Model_PluginD6SW_Mun.rds"))



pluginreg = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D6/Model_PluginD6SW_Mun.rds"))



#------------------------------------------------------------------------------#
#------------- Exporting fixed effects common to domains ---------------------#
#------------------------------------------------------------------------------#


betas = as.matrix(fixef(pluginreg))

#------------------------------------------------------------------------------#
#-------- Exporting random effects for each of the domains -------------------#
#------------------------------------------------------------------------------#


###--- Saturated Model ---###


udSW = cbind.data.frame(indice = rownames(ranef(pluginreg)$departamento_etnia),
                            ud = ranef(pluginreg)$departamento_etnia[[1]])


udSW


#------------------------------------------------------------------------------#
#-------- In this section, only the saturated model is used ------------------#
#------------------------------------------------------------------------------#

###--- Construction of the synthetic census matrix XBeta ---###


mat_CensoSW <- cbind.data.frame(Xcenso$departamento_etnia, cbind(1,as.matrix(Xcenso %>%
                                    select(rownames(betas)[-1]))) %*% 
                                    betas)
colnames(mat_CensoSW) <- c("Divipola","XB")

#- Creating the null vector to be replaced in the Plugin estimation loop -#

Xcenso$pluginD6 = numeric(5980476)

#-- Municipality codes in the census for the Plugin estimation loop --#

Div = unique(Xcenso$departamento_etnia)



###---------- Plugin estimation loop per individual in the census ----------###
###---------- In this section, only the saturated model is used ----------###


for(i in 1:length(Div)){
  
  print(i)

  ### Domain identification ###
  
  index = which(Xcenso$departamento_etnia == Div[i])
  
  ###--- Estimation in the sampled areas by ENDS ---###
  
  
  if(Div[i] %in% udSW$indice){
   
    ###--- Selecting the random effect of the domain ---###
    
    u = udSW$ud[which(as.character(udSW$indice) == Div[i])]
    
    ###--- Probability of contraceptive method use ---###
    
    Xcenso$pluginD6[index] = exp(mat_CensoSW$XB[index] + u)/
                                 (1 + exp(mat_CensoSW$XB[index] + u))
    
   print(paste(Div[i], "muestreada"))
    
   ###--- Estimation in areas not covered by the ENDS ---###
    
  }else{
    
    ###--- Probability of using any contraceptive method ---###
    
    Xcenso$pluginD6[index] = exp(mat_CensoSW$XB[index])/
                                 (1 + exp(mat_CensoSW$XB[index]))
    
    print(paste(Div[i],"No muestreada"))
  }
}


###--- Proportion of using any contraceptive method ---###
###----- Plugin estimation by department -----###

depto <- Xcenso %>% group_by(departamento) %>% summarise(D6 = mean(pluginD6))

###--- Plugin Estimation by Municipality ---###

municipio <- Xcenso %>% group_by(Divipola) %>%
  summarise(D6 = mean(pluginD6, na.rm = T))

###--- Plugin Estimation by Ethnicity ---###


etnia <- Xcenso %>% group_by(etnia) %>%
  summarise(D6 = mean(pluginD6, na.rm = T))


###--- Plugin Estimation: Department X Ethnicity ---###

municipio_etnia <- Xcenso %>% group_by(Divipola, etnia) %>%
  summarise(D6 = mean(pluginD6, na.rm = T))



###------------------------ Proportion of Contraceptive Method Use Reported by ENDS ------------------------###


medias <- numeric(36)

###--- National ---###

medias[1] <- Xencuesta %>% summarise(p = weighted.mean(usametodo, fexp)) %>% 
             as.numeric()

###--- Urban ---###

medias[2] <- Xencuesta %>% filter(area_urbano == 1) %>% 
             summarise(p = weighted.mean(usametodo, fexp)) %>% as.numeric()

###--- Rural ---###

medias[3] <- Xencuesta %>% filter(area_rural == 1) %>% 
             summarise(p = weighted.mean(usametodo,fexp)) %>% as.numeric()

###--- Department ---###

 medias[4:36] <- Xencuesta %>% group_by(departamento) %>% 
   summarise(p = weighted.mean(usametodo,fexp)) %>% mutate(depto = as.numeric(as.character(departamento))) %>% arrange(depto) %>% 
select(p) %>% unlist(use.names = FALSE)



medias



###------------------------ Calibration Matrix ------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% #sample_n(100) %>%
             left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)
MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])

###------------------------ Benchmark Weights Function ----------------------###


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


###--- Appending Benchmark Weights to the Census ---###

Xcenso$pesos_D6 <- Benchmark(Xcenso$pluginD6)
summary(Xcenso$pesos_D6)


###-------------- National Benchmark Estimation -----------------###

D6_Nal <- Xcenso %>% summarise(D6_2020 = mean(pluginD6),
                               D6B_2020 = weighted.mean(pluginD6, pesos_D6)) %>%  as.data.frame()
###-------------- Benchmark Estimation for Class ---------------###

D6_clase <- Xcenso %>% group_by(area_urbano) %>% summarise(D6_2020 = mean(pluginD6),
                               D6B_2020 = weighted.mean(pluginD6, pesos_D6)) %>% as.data.frame()
###-------------------- Departmental Benchmark Estimation ------------------###

D6_depto <- Xcenso %>% group_by(departamento) %>%
  summarise(D6_2020 = mean(pluginD6), 
            D6B_2020 = weighted.mean(pluginD6, pesos_D6)) %>% as.data.frame()

###-------------------- Municipal Benchmark Estimation ------------------###


D6_Mun <- Xcenso %>% group_by(departamento,Divipola) %>%
  summarise(D6_2020 = mean(pluginD6), 
            D6B_2020 = weighted.mean(pluginD6, pesos_D6)) %>% as.data.frame()

###-------------------- Age Benchmark Estimation ------------------###


D6_edad <- Xcenso %>% group_by(edad) %>% summarise(D6_2020 = mean(pluginD6),
                               D6B_2020 = weighted.mean(pluginD6, pesos_D6))  %>% as.data.frame()


###-------------------- Ethnicity Benchmark Estimation ------------------###

D6_dominio <- Xcenso %>% group_by(etnia) %>% 
                         summarise(D6_2020 = mean(pluginD6),
                                   D6B_2020 = weighted.mean(pluginD6, pesos_D6)) %>% as.data.frame()

###---------- Benchmark Estimation by Department and Ethnicity ----------###

D6_departamento_etnia <- Xcenso %>% group_by(departamento_etnia) %>% 
                         summarise(D6_2020 = mean(pluginD6),
                                   D6B_2020 = weighted.mean(pluginD6, pesos_D6)) %>% as.data.frame()





#Table 8.4.1.1 - Distribution % of married women
#----------------- Exporting Plugin estimates for post-stratum ----------------#

###--- Nal ---###

saveRDS(D6_Nal, file = file.path(mount_point,"3. Modelos Plugin/Output/D6/Nacional.rds"))


###--- Urban-Rural Class ---###

saveRDS(D6_clase, file = file.path(mount_point,"3. Modelos Plugin/Output/D6/Clase.rds"))


###--- Department x Ethnicity ---###

saveRDS(D6_departamento_etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/D6/Departamento_etnia.rds"))


###--- Ethnicity ---###

saveRDS(D6_dominio, file = file.path(mount_point,"3. Modelos Plugin/Output/D6/D6_plugin_dominio.rds"))

###--- Municipality ---###

saveRDS(D6_Mun, file = file.path(mount_point,"3. Modelos Plugin/Output/D6/Municipio.rds"))

###--- Department ---###

saveRDS(D6_depto, file = file.path(mount_point,"3. Modelos Plugin/Output/D6/Departamento.rds"))


###--- Age ---###

saveRDS(D6_edad, file = file.path(mount_point,"3. Modelos Plugin/Output/D6/Edad.rds"))


