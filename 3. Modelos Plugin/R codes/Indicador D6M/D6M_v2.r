######################################################################################################## 
## Title:        Family Planning Indicator D6m - Use of Modern Methods.                               ##
## Returns:      Estimation Plugin by Domains of Interest                                             ##  
## The code was modified by Lina Sánchez, Sebastián Oviedo, DANE.                                     ## 
## Carlos Rámirez,  Université de Montréal, Juliana Guerrero, World Bank.                             ##
## The original code was developed by Felipe Molina, Andrés Gutiérrez and Diego Lemus in the MRP      ##                                               
## Project - Left No One Behind, Institution: ECLAC, Division: Statistics Division.                   ##                                                 
########################################################################################################

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
#library(StatisticalModels)
select <- dplyr::select

###------------ Defining the limit of RAM memory to be used------------###

memory.limit(320000000)

mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

options(encoding = "UTF-8", scipen = 999)

###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds"))  %>% filter(unida == 1)

###------------   Senate Weights                                 ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###--------------------------------- CENSUS ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds"))  %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

###----- Identification of Municipality - Department per census person ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds")) 

###----- Desing variables ----###

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


###--- Clearing memory ---###
rm(Municipio, Divipola)

# Modern meethods use
uso_m <- Xencuesta %>%  # Married women
  mutate(uso=ifelse(usamoderno %in% c(1),v005,0)) %>%  # Currenly uses any kind of method
  summarise(per_uso=100*sum(uso)/sum(v005)) # 75.9
uso_m

################################################################################
                         ### Fitting the Plug-in model###
################################################################################

###----------------------------- Saturated Model ----------------------------###

pluginregSW_sat <- glmer(usamoderno  ~ inasistencia  +   ocupada + 
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
#----------------- Exporting outputs: Plugin model fitted     -----------------#
#------------------------------------------------------------------------------#

###--- Saturated Model  ---###

saveRDS(pluginregSW_sat, file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/PluginD6M_model.rds"))



pluginregSW_sat = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D6M/PluginD6M_model.rds"))

#------------------------------------------------------------------------------#
#------------- Exporting common fixed effects to domains           ------------#
#------------------------------------------------------------------------------#

###--- Saturated Model ---###

betasSW_sat = as.matrix(fixef(pluginregSW_sat))
betasSW_sat


#------------------------------------------------------------------------------#
#------- Exporting the random effects for each of the domains            ------#
#------------------------------------------------------------------------------#

###--- Saturated Model ---###

udSW_sat = cbind.data.frame(indice = rownames(ranef(pluginregSW_sat)$departamento_etnia),
                            ud = ranef(pluginregSW_sat)$departamento_etnia[[1]])



#------------------------------------------------------------------------------#
#-------------- In this section, only the saturated model is used -------------#
#------------------------------------------------------------------------------#

###------------ Construction of the Synthetic Census Matrix XBeta ------------###

mat_CensoSW_sat <- cbind.data.frame(Xcenso$departamento_etnia,cbind(1,as.matrix(Xcenso %>%
                                    select(rownames(betasSW_sat)[-1]))) %*% 
                                    betasSW_sat)
colnames(mat_CensoSW_sat) <- c("Divipola","XB")




#- Creating the Null Vector to be Replaced in the Plugin Estimation Cycle -#

Xcenso$pluginD6M = numeric(nrow(Xcenso))

#-- Municipality Codes in the Census for the Plugin Estimation Cycle --#

Div = unique(Xcenso$departamento_etnia)




###---------- Individual Census Plugin Estimation Cycle ----------###

for(i in 1:length(Div)){
  
  print(i)
  
  ###--- Domain Identification ---###
  
  index = which(Xcenso$departamento_etnia == Div[i])
  
  ###--- Estimation in the Areas Sampled by the ENDS ---###
  
  if(Div[i] %in% udSW_sat$indice){
    
    ###--- Selecting the Random Effect of the Domain ---###
    
    u = udSW_sat$ud[which(as.character(udSW_sat$indice) == Div[i])]
    

    ###--- Probability of Using Any Contraceptive Method ---###
    
    Xcenso$pluginD6M[index] = exp(mat_CensoSW_sat$XB[index] + u)/
                                  (1 + exp(mat_CensoSW_sat$XB[index] + u))
    
    print(paste(Div[i], "muestreada"))
    
    ###--- Estimation in Non-Sampled Areas by the ENDS ---###
    
  }else{
    
    ###--- Probability of Using Any Contraceptive Method ---###
    
    Xcenso$pluginD6M[index] = exp(mat_CensoSW_sat$XB[index])/
                                  (1 + exp(mat_CensoSW_sat$XB[index]))
    
    print(paste(Div[i],"No muestreada"))
  }
}


###--------- Proportion of Modern Contraceptive Use               ----------###
###--------------------- Departmental Plugin Estimation --------------------###

deptoD6M <- Xcenso %>% group_by(departamento) %>% summarise(D6M = mean(pluginD6M))
deptoD6M 

###----------------------- Municipal Plugin Estimation ----------------------###

municipioD6M <- Xcenso %>% group_by(Divipola) %>% summarise(D6M = mean(pluginD6M))
municipioD6M

###----------------------- Plugin Estimation by Ethnicity ----------------------###

etniaD6M <- Xcenso %>% group_by(etnia) %>% summarise(D6M = mean(pluginD6M))
etniaD6M

###----------------- Plugin Estimation: Department X Ethnicity ----------------###

depto_etniaD6M <- Xcenso %>% group_by(departamento, etnia) %>%
                             summarise(D6M = mean(pluginD6M))
depto_etniaD6M

###------------------- Plugin Estimation: Municipality X Ethnicity -----------------###

municipio_etniaD6M <- Xcenso %>% group_by(Divipola, etnia) %>%
                                 summarise(D6M = mean(pluginD6M))
deptoD6M %>%  as.data.frame()



################################################################################
###                                  Benchmark                                 #
################################################################################

###--------------------------------------------------------------------------###
###   Proportion of Women with Unmet Family Planning Needs                   ### 
###                      Reported by the ENDS                                ###                         
###--------------------------------------------------------------------------###

medias <- numeric(36)

###--- National ---###

medias[1] <- Xencuesta %>% summarise(p = weighted.mean(usamoderno, fexp)) %>% 
  as.numeric()

###--- Urban ---###

medias[2] <- Xencuesta %>% filter(area_urbano == 1) %>% 
  summarise(p = weighted.mean(usamoderno, fexp)) %>% as.numeric()

###--- Rural ---###

medias[3] <- Xencuesta %>% filter(area_rural == 1) %>% 
  summarise(p = weighted.mean(usamoderno,fexp)) %>% as.numeric()

###--- Department ---###

 medias[4:36] <- Xencuesta %>% group_by(departamento) %>% 
   summarise(p = weighted.mean(usamoderno,fexp)) %>% mutate(depto = as.numeric(as.character(departamento))) %>% arrange(depto) %>% 
select(p) %>% unlist(use.names = FALSE)
medias



###-------------------------- Calibration Matrix -------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% as.data.frame() %>% #sample_n(100) %>%
             left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)

MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])


###------------------------Weigths Benchmark Function ----------------------###

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
Xcenso$pesos_D6M <- Benchmark(Xcenso$pluginD6M)
summary(Xcenso$pesos_D6M)

###----------------------  Benchmark Estimation Nacional ---------------------###

D6M_Nal <- Xcenso %>% summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6M) ) %>% as.data.frame()

###--------------------  Benchmark Estimation Department ------------------###

D6M_depto <- Xcenso %>% group_by(departamento) %>%
  summarise(D6M_2020 = mean(pluginD6M),
            D6MB_2020 = weighted.mean(pluginD6M, pesos_D6M))  %>% as.data.frame()


###----------------------  Benchmark Estimation Municipal --------------------###

D6M_Mun <- Xcenso %>% group_by(departamento,Divipola) %>%
  summarise(D6M_2020 = mean(pluginD6M),
            D6MB_2020 = weighted.mean(pluginD6M, pesos_D6M) ) %>% as.data.frame()

###----------------------  Benchmark Estimation Urban_Rural ---------------------###

D6M_clase <- Xcenso %>% group_by(area_urbano) %>% 
                     summarise(D6_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6M))  %>% as.data.frame()

###---------------------- Age Benchmark Estimation --------------------------###

D6M_edad <- Xcenso %>% group_by(edad) %>% 
                     summarise(D6_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6M))  %>% as.data.frame()



###---------------------- Ethnicity Benchmark Estimation ------------------------###

D6M_dominio <- Xcenso %>% group_by(etnia) %>% 
                          summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6M))  %>% as.data.frame()

###-------------------- Departmental Benchmark Estimation - Ethnicity ------------------###

D6M_departamento_etnia <- Xcenso %>% group_by(departamento_etnia) %>% 
  summarise(D6M_2020 = mean(pluginD6M),
            D6MB_2020 = weighted.mean(pluginD6M, pesos_D6M))  %>% as.data.frame()

                              

#----------------- Exporting Plugin estimates for post-stratum ----------------#

###--- Nal ---###

saveRDS(D6M_Nal, file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/Nacional.rds"))


###--- Urban - Rural ---###

saveRDS(D6M_clase, file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/Clase.rds"))


###--- Department x Ethnicity ---###

saveRDS(D6M_departamento_etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/Departamento_etnia.rds"))

###--- Ethnicity ---###

saveRDS(D6M_dominio, file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/D6_plugin_dominio.rds"))

###--- Municipality ---###

saveRDS(D6M_Mun, file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/Municipio.rds"))

###--- Department ---###

saveRDS(D6M_depto, file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/Departamento.rds"))


###--- Age ---###

saveRDS(D6M_edad, file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/Edad.rds"))


