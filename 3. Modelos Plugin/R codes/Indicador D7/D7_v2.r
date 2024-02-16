######################################################################################################## 
## Title:        Family Planning Indicator D7 - Use of Any Planning Method                            ##
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

###------------ Defining the limit of RAM memory to be used ------------###

options(encoding = "UTF-8", scipen = 999)

mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds"))  %>% filter(unida == 1)

###------------ Senate Weights                                   ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###--------------------------------- CENSUS ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds"))  %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

###----- Census Person-Level Identifier for Municipality and Department ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###----- Design Variables ----###

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

###------------- Appending the Divipola code to the Census database ------------###
Xcenso <- Xcenso %>% left_join(., Variables.disenio,
                              by = c("Divipola"= "Divipola"))

###--- Clearing memory ---###

rm(Municipio, Divipola)



###------------ Adding the new domains ------------###

Xencuesta = Xencuesta %>% mutate(departamento_etnia = paste0(departamento,"_", etnia))
Xcenso = Xcenso %>% mutate(departamento_etnia = paste0(departamento,"_", etnia))


# Demand - As shown in the table
de <- Xencuesta %>% #filter(unida==1) %>% # mujeres unidas
  transmute(num=ifelse(usamoderno == 1,v005,0), den=ifelse(usametodo == 1 | nec_ins == 1,v005,0)) %>%  
  summarise(per_uso=100*sum(num)/sum(den)) # 86.6
de


################################################################################
### Plugin Model fitting                                                     ###
################################################################################

###--------------------------- Saturated model D6 ---------------------------###

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

###--------------------------- Saturated model D6M --------------------------###

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

###---------------------------- Saturated model NI --------------------------###

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

#-------------------------------------------------------------------------------#
#----------------- Exporting Outputs: Fitted Plugin Model ----------------------#
#-------------------------------------------------------------------------------#

saveRDS(pluginreg1, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6UMunicipio.rds"))
saveRDS(pluginreg2, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6MUMunicipio.rds"))
saveRDS(pluginreg3, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginNIUMunicipio.rds"))



pluginreg1 = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6UMunicipio.rds"))
pluginreg2 = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6MUMunicipio.rds"))
pluginreg3 = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginNIUMunicipio.rds"))


#------------------------------------------------------------------------------#
#------------- Exporting fixed effects common to domains ---------------------#
#------------------------------------------------------------------------------#

#--- Saturated model: D6 ---#

betas1 = as.matrix(fixef(pluginreg1))
betas1

#--- Saturated model: D6M ---#

betas2 = as.matrix(fixef(pluginreg2))
betas2

#--- Saturated model: NI ---#

betas3 = as.matrix(fixef(pluginreg3))
betas3


###--- Construction of the synthetic census matrix XBeta ---###

a1 = rownames(betas1)[-1]
a2 = rownames(betas2)[-1]
a3 = rownames(betas3)[-1]
mat_Censo_SW_sat <- cbind.data.frame(Xcenso$departamento_etnia, 
                                cbind(1, as.matrix(Xcenso %>% as.data.table %>%.[,..a1,])) %*% betas1,
                                cbind(1, as.matrix(Xcenso %>% as.data.table %>%.[,..a2,])) %*% betas2,
                                cbind(1, as.matrix(Xcenso %>% as.data.table %>%.[,..a3,])) %*% betas3)

colnames(mat_Censo_SW_sat) <- c("Divipola", "XB1", "XB2", "XB3")
head(mat_Censo_SW_sat)


#-- Municipality codes in the census for the Plugin estimation loop --#

Div = unique(Xcenso$departamento_etnia)

#------------------------------------------------------------------------------#
#------- Exporting the random effects for each of the domains            ------#
#------------------------------------------------------------------------------#

udSW_sat =  data.frame(departamento_etnia = rownames(ranef(pluginreg1)$departamento_etnia), 
                 D6 = ranef(pluginreg1)$departamento_etnia[[1]], 
                 D6M = ranef(pluginreg2)$departamento_etnia[[1]],
                 NI = ranef(pluginreg3)$departamento_etnia[[1]])
rownames(udSW_sat) <- NULL


#----------- Creating vector of random effects for all country domains -------------#

udSW_sat = data.frame(departamento_etnia = Div) %>% left_join(udSW_sat, by = "departamento_etnia")

#-------- If the domain was not surveyed it will have a random effect of 0. --------#

udSW_sat$D6M[is.na(udSW_sat$D6M)] <- 0
udSW_sat$D6[is.na(udSW_sat$D6)] <- 0
udSW_sat$NI[is.na(udSW_sat$NI)] <- 0

#------- Creating the null vector to be replaced in the Plugin estimation loop ----#

Xcenso$pluginD6U = numeric(dim(Xcenso)[1])
Xcenso$pluginD6MU = numeric(dim(Xcenso)[1])
Xcenso$pluginNIU = numeric(dim(Xcenso)[1])
Xcenso$pluginD7 = numeric(dim(Xcenso)[1])

###---------- Plugin estimation loop per individual in the census ----------###
###---------- In this section, only the saturated model is used   ----------###

for(i in 1:length(Div)){
  
  print(i)
  
  ### Domain identification ###
  
  index = which(Xcenso$departamento_etnia == Div[i])
  
  ###------------Probability of using any contraceptive method ------------###
  
  ###---  D6  Index ---###
  
  Xcenso$pluginD6U[index] = exp(mat_Censo_SW_sat$XB1[index] + udSW_sat$D6[i])/
                            (1 + exp(mat_Censo_SW_sat$XB1[index] + udSW_sat$D6[i]))
  
  ###--- D6M   Index  ---###
  
  Xcenso$pluginD6MU[index] = exp(mat_Censo_SW_sat$XB2[index] + udSW_sat$D6M[i])/
                             (1 + exp(mat_Censo_SW_sat$XB2[index] + udSW_sat$D6M[i]))
  
  ###--- NI  Index  ---###
  
  Xcenso$pluginNIU[index] = exp(mat_Censo_SW_sat$XB3[index] + udSW_sat$NI[i])/
                             (1 + exp(mat_Censo_SW_sat$XB3[index] + udSW_sat$NI[i]))
  
  ###---  D7  Index---###
  
  Xcenso$pluginD7[index] = Xcenso$pluginD6MU[index]/
                            (Xcenso$pluginD6U[index] + Xcenso$pluginNIU[index])
 
  print(paste(Div[i],"No muestreada")) 
}


###---------------------- Plugin estimation by department -------------------###

depto_D7 <- Xcenso %>% group_by(departamento) %>% summarise(D7 = mean(pluginD7))
depto_D7

###---------------------- Plugin estimation by Municipality -----------------###

municipio_D7 <- Xcenso %>% group_by(Divipola) %>% summarise(D7 = mean(pluginD7))
municipio_D7

###---------------------- Plugin estimation by Ethnicity -------------------###


etnia_D7 <- Xcenso %>% group_by(etnia) %>% summarise(D7 = mean(pluginD7))
etnia_D7

###----------------- Plugin estimation: department X Ethnicity --------------###

depto_etnia_D7 <- Xcenso %>% group_by(departamento, etnia) %>% 
                 summarise(D7 = mean(pluginD7))
depto_etnia_D7

###----------------- Plugin estimation: Municipality X Ethnicity --------------###

municipio_etnia_D7 <- Xcenso %>% group_by(Divipola, etnia) %>%
                      summarise(D7 = mean(pluginD7))
municipio_etnia_D7

################################################################################
###                                  Benchmark                                 #
################################################################################
library(srvyr)
###--------------------------------------------------------------------------###
##      Proportion of women who do not want to start childbearing            ###
##              reported by the ENDS                                         ###
###--------------------------------------------------------------------------###

medias <- numeric(36)

dis_paso <- Xencuesta %>% srvyr::as_survey_design(ids = idpers, strata = departamento, 
                                           weights = fexp)

###-------------------------- National --------------------------------###

medias[1] <- (dis_paso %>% summarise(p = survey_ratio(usamoderno, 
                                                      usametodo + nec_ins)))$p

###------------------------------- Urban ------------------------------###

medias[2] <- (dis_paso %>% filter(area_urbano == 1) %>% 
              summarise(p = survey_ratio(usamoderno, usametodo + nec_ins)))$p

###------------------------- Rural ------------------------------------###

medias[3] <- (dis_paso %>% filter(area_rural == 1) %>% 
                summarise(p = survey_ratio(usamoderno, usametodo + nec_ins)))$p

###------------------------ Department --------------------------------###

 medias[4:36] <- Xencuesta %>% group_by(departamento) %>% 
  transmute(num=ifelse(usamoderno == 1,v005,0), den=ifelse(usametodo == 1 | nec_ins == 1,v005,0)) %>%  
  summarise(per_uso=sum(num)/sum(den)) %>% mutate(depto = as.numeric(as.character(departamento))) %>% arrange(depto) %>% 
  select(per_uso) %>% unlist(use.names = FALSE)



###------------------------ Calibration Matrix ------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% #sample_n(100) %>%
  left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)
MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])

###------------------------------- Benchmark Weights Function --------------------------------###

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


###--------------------------- Appending Benchmark Weights to the Census ----------------------###

Xcenso$pesos_D7 <- Benchmark(Xcenso$pluginD7)
summary(Xcenso$pesos_D7)

summary(Xcenso$pluginD7)


###---------------------------- National Benchmark Estimation ---------------------------------###

D7_Nal <- Xcenso %>% summarise(D7_2020 = mean(pluginD7),
                               D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()

###---------------------------- Benchmark Estimation by Class ---------------------------------###

D7_clase <- Xcenso %>% group_by(area_urbano) %>% 
                                summarise(D7_2020 = mean(pluginD7),
                                          D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()

###---------------------------- Benchmark Estimation by Age -----------------------------------###

D7_edad <- Xcenso %>% group_by(edad) %>% 
                          summarise(D7_2020 = mean(pluginD7),
                                    D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()


###----------------------------- Benchmark Estimation by Department --------------------------###

D7_depto <- Xcenso %>% group_by(departamento) %>% 
                                   summarise(D7_2020 = mean(pluginD7), 
                                             D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()


###-------------------- Benchmark Estimation by Municipality ---------------------------------###

D7_Mun <- Xcenso %>% group_by(Divipola) %>% 
            summarise(D7_2020 = mean(pluginD7), 
            D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()

###--------------------------- Benchmark Estimation by Ethnicity -----------------------------###

D7_dominio <- Xcenso %>% group_by(etnia) %>%
                         summarise(D7_2020 = mean(pluginD7), 
                                   D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()

###---------------------- Benchmark Estimation by Department x Ethnicity --------------------###

D7_depto_etnia <- Xcenso %>% group_by(departamento_etnia) %>%
                         summarise(D7_2020 = mean(pluginD7), 
                                   D7B_2020 = weighted.mean(pluginD7, pesos_D7)) %>% as.data.frame()


###--------------- Exporting Plugin estimates for post-stratum ------------------------------###

###--- National ---###
saveRDS(D7_Nal, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Nacional.rds"))


###--- Class      ---------------------###
saveRDS(D7_clase, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Clase.rds"))

###--- Ethnicity   --------------------###
saveRDS(D7_dominio, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Etnia.rds"))

###--- Municipality     ---------------###
saveRDS(D7_Mun, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Municipio.rds"))

###--- Department       ---------------###
saveRDS(D7_depto, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Departamento.rds"))

###---- Department x  Ethnicity    ----###
saveRDS(D7_depto_etnia, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Departamento_etnia.rds"))

###--- Age       ----------------------###
saveRDS(D7_edad, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/Edad.rds"))


