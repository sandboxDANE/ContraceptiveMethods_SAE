######################################################################################################## 
## Title:        Family Planning Indicator NI - Unmet Need for Planning Method.                       ##
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
library(StatisticalModels)

select <- dplyr::select



###------------ Defining the limit of RAM memory to be used------------###

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

###-------------------- Total number of municipalities in the NSDS ---------------------###

sum(unique(Xencuesta$Divipola) %in% unique(Xcenso$Divipola))

###----- Identification of Municipality - Department per census person ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###----- Desing variables ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))

###------------- Appending the Divipola code to the Census database -------------###
Xcenso <- Xcenso %>% left_join(., Variables.disenio,
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

#rm(Municipio, Divipola)


# NI - To compare with the table # 10.5.1.2
ni <- Xencuesta %>% filter(unida==1) %>% # mujeres unidas
  mutate(uso=ifelse(nec_ins %in% c(1),v005,0)) %>%  # demanda: unmet & met need
  summarise(per_uso=100*sum(uso)/sum(v005)) # 6.7
ni

################################################################################
                         ### Fitting the Plug-in model###
################################################################################

###----------------------------- Saturated Model ----------------------------###

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
#----------------- Exporting outputs: Plugin model fitted     -----------------#
#------------------------------------------------------------------------------#

###--- Saturated Model  ---###

saveRDS(pluginregSW_sat, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/PluginNI_SW_Sat_Mun.rds"))

#################################################################################
#-----------------------  Plugin estimation: indicador NI ----------------------#
#################################################################################

pluginregSW_sat = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/NI/PluginNI_SW_Sat_Mun.rds"))

#------------------------------------------------------------------------------#
#------------- Exporting common fixed effects to domains           ------------#
#------------------------------------------------------------------------------#

betasSW_sat = as.matrix(fixef(pluginregSW_sat))

#------------------------------------------------------------------------------#
#------- Exporting the random effects for each of the domains            ------#
#------------------------------------------------------------------------------#


udSW_sat = cbind.data.frame(indice = rownames(ranef(pluginregSW_sat)$departamento_etnia),
                            ud = ranef(pluginregSW_sat)$departamento_etnia[[1]])

#------------------------------------------------------------------------------#
#-------------- In this section, only the saturated model is used -------------#
#------------------------------------------------------------------------------#

###------------ Construction of the Synthetic Census Matrix XBeta ------------###

mat_CensoSW_sat <- cbind.data.frame(Xcenso$departamento_etnia, cbind(1,as.matrix(Xcenso %>%
                                    select(rownames(betasSW_sat)[-1]))) %*% 
                                    betasSW_sat)
colnames(mat_CensoSW_sat) <- c("Divipola","XB")

#- Creating the Null Vector to be Replaced in the Plugin Estimation Cycle -#

Xcenso$pluginNI = numeric(5980476)

#-- Municipality Codes in the Census for the Plugin Estimation Cycle -----#

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
    
    Xcenso$pluginNI[index] = exp(mat_CensoSW_sat$XB[index] + u)/
                             (1 + exp(mat_CensoSW_sat$XB[index] + u))
    
    print(paste(Div[i], "muestreada"))
    
    ###--- Estimation in Non-Sampled Areas by the ENDS ---###
    
  }else{
    
    ###--- Probability of Using Any Contraceptive Method ---###
    
    Xcenso$pluginNI[index] = exp(mat_CensoSW_sat$XB[index])/
                             (1 + exp(mat_CensoSW_sat$XB[index]))
    
    print(paste(Div[i],"No muestreada"))
  }
}

###------------- Proportion of women with unmet planning needs  -------------###
###--------------------- Departmental Plugin Estimation --------------------###

depto <- Xcenso %>% group_by(departamento) %>% summarise(NI = mean(pluginNI))


###-----------------------  Plugin Estimation by Municipal ----------------------###

municipio <- Xcenso %>% group_by(Divipola) %>%
  summarise(NI = mean(pluginNI, na.rm = T))

###-----------------------  Plugin Estimation by Ethnicity ----------------------###

etnia <- Xcenso %>% group_by(etnia) %>%
  summarise(NI = mean(pluginNI, na.rm = T))

###----------------- Plugin Estimation: Department X Ethnicity ------------------###

municipio_etnia <- Xcenso %>% group_by(Divipola, etnia) %>%
  summarise(NI = mean(pluginNI, na.rm = T))



################################################################################
###                                  Benchmark                                 #
################################################################################

###--------------------------------------------------------------------------###
###    Proportion of women with unmet planning needs as reported by the NSDS ###                         
###--------------------------------------------------------------------------###

medias <- numeric(36)

###--- National ---###

medias[1] <- Xencuesta %>% summarise(p = weighted.mean(nec_ins, fexp)) %>% 
  as.numeric()

###--- Urban ---###

medias[2] <- Xencuesta %>% filter(area_urbano == 1) %>% 
  summarise(p = weighted.mean(nec_ins, fexp)) %>% as.numeric()

###--- Rural ---###

medias[3] <- Xencuesta %>% filter(area_rural == 1) %>% 
  summarise(p = weighted.mean(nec_ins,fexp)) %>% as.numeric()

###--- Department ---###

 medias[4:36] <- Xencuesta %>% group_by(departamento) %>% 
   summarise(p = weighted.mean(nec_ins,fexp)) %>% mutate(depto = as.numeric(as.character(departamento))) %>% arrange(depto) %>% 
select(p) %>% unlist(use.names = FALSE)


###-------------------------- Calibration Matrix -------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% as.data.frame() %>% #sample_n(100) %>%
             left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)

MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])
names(MatrizCalibrada)


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


###-------------------- Appending Benchmark Weights to the Census -----------###

Xcenso$pesos_NI <- Benchmark(Xcenso$pluginNI)
summary(Xcenso$pesos_NI)


###----------------------  Benchmark Estimation Nacional ---------------------###

NI_Nal <- Xcenso %>% summarise(NI_2020 = mean(pluginNI),
                               NIB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()


###----------------------  Benchmark Estimation Department ---------------------###


NI_depto <- Xcenso %>% group_by(departamento) %>%
  summarise(NI_2020 = mean(pluginNI),
            NIB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()

###----------------------  Benchmark Estimation Municipality -------------------###

NI_Mun <- Xcenso %>% group_by(departamento, Divipola) %>%
  summarise(NI_2020 = mean(pluginNI),
            NIB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()


###----------------------  Benchmark Estimation Class ---------------------###

NI_clase <- Xcenso %>% group_by(area_urbano) %>% summarise(D6_2020 = mean(pluginNI),
                               D6B_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()


###----------------------  Benchmark Estimation Age ---------------------###

NI_edad <- Xcenso %>% group_by(edad) %>% summarise(D6_2020 = mean(pluginNI),
                               D6MB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()



###----------------------  Benchmark Estimation Ethnicity ---------------------###

NI_dominio <- Xcenso %>% group_by(etnia) %>% 
              summarise(NI_2020 = mean(pluginNI),
                        NIB_2020 = weighted.mean(pluginNI, pesos_NI))  %>% as.data.frame()


###----------------------  Benchmark Estimation Department x Ethnicity ---------------------###

NI_departamento_etnia <- Xcenso %>% group_by(departamento_etnia) %>% 
  summarise(NI_2020 = mean(pluginNI),
            NIB_2020 = weighted.mean(pluginNI, pesos_NI)) %>% as.data.frame()


#----------------- Exporting Plugin estimates for post-stratum ----------------#
###--- Nal ---###

saveRDS(NI_Nal, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Nacional.rds"))

###--- Ethnicity ---###

saveRDS(NI_dominio, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Etnia.rds"))

###--- Municipality ---###

saveRDS(NI_Mun, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Municipio.rds"))

###--- Department ---###

saveRDS(NI_depto, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Departamento.rds"))

###--- Department x Ethnicity ---###

saveRDS(NI_departamento_etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Departamento_etnia.rds"))


###--- Age ---###

saveRDS(NI_edad, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Edad.rds"))

###--- Class ---###

saveRDS(NI_clase, file = file.path(mount_point,"3. Modelos Plugin/Output/NI/Clase.rds"))

NI_clase
