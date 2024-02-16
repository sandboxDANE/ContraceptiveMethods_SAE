######################################################################################################## 
## Title:        Family Planning Indicator D6M - Use of modern Planning Method                         ##
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
#library(srvyr)
library(sae)
library(lme4)
library(data.table)
library(haven)
library(formula.tools)
library(remotes)
#library(StatisticalModels)
select <- dplyr::select

###------------ Defining the limit of RAM memory to be used ------------###


options(encoding = "UTF-8", scipen = 999)


mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"
memory.limit(640000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################


###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds")) %>% filter(unida == 1)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###------------ Senate Weights                                   ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)

###--------------------------------- CENSUS ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds")) %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)

###-------------------- Número de municipios en la ENDS ---------------------###

sum(unique(Xencuesta$Divipola) %in% unique(Xcenso$Divipola))

###----- Census Person-Level Identifier for Municipality and Department ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###----- Design Variables ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))


###------------- Appending the Divipola code to the Census database ------------###

Xcenso <- Xcenso %>% left_join(., Variables.disenio,
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

###--- Clearing memory ---####
rm(Municipio, Divipola)

###############################################################################
## Contraceptive Methods Demand
###############################################################################

# Demanda - comparar con la tabla - Value reported in ENDS
de <- Xencuesta %>% filter(unida==1) %>% # mujeres unidas
  transmute(num=ifelse(usamoderno == 1,v005,0), den=ifelse(usametodo == 1 | nec_ins == 1,v005,0)) %>%  
  summarise(per_uso=100*sum(num)/sum(den)) # 86.6
de


################################################################################
### Plugin Model fitting                                                     ###
################################################################################

###----------------------------- Saturated model ----------------------------###

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


summary(pluginregSW_sat)

#------------------------------------------------------------------------------#
#----------------- Exporting Outputs: Fitted Plugin Model ----------------------#
#------------------------------------------------------------------------------#

###--- Saturated model ---###

saveRDS(pluginregSW_sat, file = "PluginD6Demanda_SW_Sat_Mun.rds")


pluginregSW_sat = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D6/PluginD6Demanda_SW_Sat_Mun.rds"))

#------------------------------------------------------------------------------#
#------------- Exporting fixed effects common to domains ---------------------#
#------------------------------------------------------------------------------#

betasSW_sat = as.matrix(fixef(pluginregSW_sat))
betasSW_sat


#------------------------------------------------------------------------------#
#-------- Exporting random effects for each of the domains -------------------#
#------------------------------------------------------------------------------#

###--- Modelo saturado ---###

udSW_sat = cbind.data.frame(indice = rownames(ranef(pluginregSW_sat)$departamento_etnia),
                            ud = ranef(pluginregSW_sat)$departamento_etnia[[1]])


#------------------------------------------------------------------------------#
#-------- In this section, only the saturated model is used -------------------#
#------------------------------------------------------------------------------#

###--- ------Construction of the synthetic census matrix XBeta --------------###

mat_CensoSW_sat <- cbind.data.frame(Xcenso$Divipola,cbind(1,as.matrix(Xcenso %>%
                                    select(rownames(betasSW_sat)[-1]))) %*% 
                                    betasSW_sat)
colnames(mat_CensoSW_sat) <- c("Divipola","XB")


#- Creating the null vector to be replaced in the Plugin estimation loop -#

Xcenso$pluginD6M = numeric(nrow(Xcenso))

#-- Municipality codes in the census for the Plugin estimation loop --#

Div = unique(Xcenso$departamento_etnia)

###---------- Plugin estimation loop per individual in the census ----------###
###---------- In this section, only the saturated model is used ------------###

for(i in 1:length(Div)){
  
  print(i)
  
  ### Domain identification ###
  
  index = which(Xcenso$departamento_etnia == Div[i])
  
  ###--- Estimation in the sampled areas by ENDS ---###
  
  if(Div[i] %in% udSW_sat$indice){
    
    ###--- Selecting the random effect of the domain ---###
    
    u = udSW_sat$ud[which(as.character(udSW_sat$indice) == Div[i])]
    
    ###--- Probability of contraceptive method use ---###
    
    Xcenso$pluginD6M[index] = exp(mat_CensoSW_sat$XB[index] + u)/
                                  (1 + exp(mat_CensoSW_sat$XB[index] + u))
    
    print(paste(Div[i], "muestreada"))
    
   ###--- Estimation in areas not covered by the ENDS ---###
    
  }else{
    
    ###--- Probability of using any contraceptive method ---###
    
    Xcenso$pluginD6M[index] = exp(mat_CensoSW_sat$XB[index])/
                                  (1 + exp(mat_CensoSW_sat$XB[index]))
    
    print(paste(Div[i],"No muestreada"))
  }
}


###------ Proportion of using any contraceptive modern     method  ----------###
###--------------------- Plugin estimation by department --------------------### 

deptoD6M <- Xcenso %>% group_by(departamento) %>% summarise(D6M = mean(pluginD6M))
deptoD6M 

###----------------------- Plugin estimation by municipality -----------------###

municipioD6M <- Xcenso %>% group_by(Divipola) %>% summarise(D6M = mean(pluginD6M))
municipioD6M

###----------------------- Plugin estimation by Ethnicity ----------------------###

etniaD6M <- Xcenso %>% group_by(etnia) %>% summarise(D6M = mean(pluginD6M))
etniaD6M

###-------------------- Plugin Estimation: Department X Ethnicity -------------###

depto_etniaD6M <- Xcenso %>% group_by(departamento, etnia) %>%
                             summarise(D6M = mean(pluginD6M))
depto_etniaD6M

###-------------------- Plugin Estimation: Municipality X Ethnicity -------------###

municipio_etniaD6M <- Xcenso %>% group_by(Divipola, etnia) %>%
                                 summarise(D6M = mean(pluginD6M))
municipio_etniaD6M


################################################################################
###                                  Benchmark                                 #
################################################################################

###--------------------------------------------------------------------------###
###   Proportion of women with unmet planning needs as reported by the NSDS  ###                          
###--------------------------------------------------------------------------###

medias <- numeric(36)

###--- National ---###

medias[1] <- Xencuesta %>% summarise(p = weighted.mean(demanda, fexp)) %>% 
  as.numeric()

###--- Urban ---###

medias[2] <- Xencuesta %>% filter(area_urbano == 1) %>% 
  summarise(p = weighted.mean(demanda, fexp)) %>% as.numeric()

###--- Rural ---###

medias[3] <- Xencuesta %>% filter(area_rural == 1) %>% 
  summarise(p = weighted.mean(demanda,fexp)) %>% as.numeric()

###--- Department ---###

 medias[4:36] <- Xencuesta %>% group_by(departamento) %>% 
   summarise(p = weighted.mean(demanda,fexp)) %>% select(p) %>% unlist(use.names = FALSE)
medias


###--------------------------  Calibration Matrix  -------------------------###

MatrizCalibrada <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/MatrizCalibrada.rds")) %>% as.data.frame() %>% as.data.frame() %>% #sample_n(100) %>%
             left_join(Xcenso %>% select(idpers,unida), by = "idpers") %>% filter(is.na(unida) == F)


MatrizCalibrada$unos = 1
MatrizCalibrada = MatrizCalibrada %>% select(unos,names(MatrizCalibrada)[3:37])
names(MatrizCalibrada)



###------------------------ Benchmark Weights Function ----------------------###

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

###--- Appending Benchmark Weights to the Census ---###

Xcenso$pesos_D6Demanda <- Benchmark(Xcenso$pluginD6M)
summary(Xcenso$pesos_D6Demanda)

###-------------- National Benchmark Estimation -----------------###

D6Demanda_Nal <- Xcenso %>% summarise(D6M_2020 = mean(pluginD6M_fex),
                               D6MB_2020 = weighted.mean(pluginD6M_fex, pesos_D6Demanda)
                               )

###-------------- Benchmark Estimation for Class ---------------###

D6Demanda_clase <- Xcenso %>% group_by(area_urbano) %>%
                        summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )
###-------------------- Departmental Benchmark Estimation ------------------###

D6Demanda_depto <- Xcenso %>% group_by(departamento) %>%
                        summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )


###-------------------- Municipality Benchmark Estimation ------------------###

D6Demanda_Mun <- Xcenso %>% group_by(departamento,Divipola) %>%
                      summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )


###-------------------- Ethnicity Benchmark Estimation ------------------###

D6Demanda_dominio <- Xcenso %>% group_by(etnia) %>% 
                          summarise(D6M_2020 = mean(pluginD6M_fex),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )

###-------------------- Depatment x Ethnicity Benchmark Estimation ------------------###

D6Demanda_etnia_depto <- Xcenso %>% group_by(departamento_etnia) %>% 
                          summarise(D6M_2020 = mean(pluginD6M),
                               D6MB_2020 = weighted.mean(pluginD6M, pesos_D6Demanda)
                               )


D6Demanda_Nal %>% as.data.frame()

D6Demanda_clase %>% as.data.frame()

D6Demanda_depto %>% as.data.frame()

D6Demanda_Mun %>% as.data.frame()

D6Demanda_dominio %>% as.data.frame()


#----------------- Exporting Plugin estimates for post-stratum ----------------#
###--- Ethnicity ---###

saveRDS(D6M_dominio, file = "D6M_plugin_dominio.rds")

###--- Municipality ---###

saveRDS(D6M_Mun, file = "D6M_plugin_Mun.rds")

###--- Department ---###

saveRDS(D6M_depto, file = "D6M_plugin_Departamento.rds")


write.table(D6M_depto, '/dbfs/FileStore/Salidas/Clasificacion/D6M_plugin_Departamento.csv')
