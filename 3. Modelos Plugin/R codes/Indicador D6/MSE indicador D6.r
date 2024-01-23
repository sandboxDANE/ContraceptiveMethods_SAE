######################################################################################################## 
## Title:        Family Planning Indicator D6 - Use of Any Planning Method                            ##
## Returns:      Estimation MSE by Domains of Interest                                                ##  
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

library(sae)
library(dplyr)
library(nlme)
library(TeachingSampling)
library(dtplyr)
library(data.table)
library(purrr)

mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"


##  Original estimator
###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds")) %>% filter(unida == 1)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###------------ Anexando los Senate Weights a la base de la ENDS ------------###


Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)

###----- Identifier of Municipality - Department per person from the census ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###--------------------------------- CENSUS ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds")) %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)


###----- Design Variables ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))


###--------------------- List of the 1122 municipalities ---------------------###
Divipola <- Municipio %>% distinct(Divipola, .keep_all = T)

Divipola$departamento = as.character(as.numeric(Divipola$departamento))
Divipola$municipio = as.character(as.numeric(Divipola$municipio))


Xencuesta$departamento = as.character(as.numeric(Xencuesta$departamento))
Xencuesta$municipio = as.character(as.numeric(Xencuesta$municipio))


###------------ Appending the Divipola code to the ENDS database ------------###
Xencuesta <- Xencuesta %>% left_join(., Variables.disenio,`copy` = TRUE,
                                     by = c("Divipola"= "Divipola"))


# Domains
Div = unique(Xcenso$departamento_etnia)

# ENDS and Population Sample Size

n_d = Xencuesta %>% group_by(departamento_etnia) %>% summarise(nd = n()) %>%
  mutate_all(~replace(.,is.na(.),0)) %>% as.data.frame()

N_d = Xcenso %>% lazy_dt() %>% group_by(departamento_etnia) %>% summarise(Nd = n()) %>% 
  as.data.frame()


# CENSUS MODEL
################################################################################
### Plugin Model Adjustment ###
################################################################################

###----------------------------- Saturated Model ----------------------------###

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


summary(pluginreg)



###----------- Exporting Fixed Effects Common to Domains ----------###

betas = as.matrix(fixef(pluginreg))

###--- MSE ---###

sd.u = pluginreg@theta


################################################################################
#### Model with Random Effects of Department_Ethnicity
################################################################################

###------------ Building the Synthetic Census Matrix XBeta ------------###


matriz <- cbind.data.frame(Xcenso$departamento_etnia, cbind(1,as.matrix(Xcenso %>%
                                                                          dplyr::select(rownames(betas)[-1]))) %*% betas)

colnames(matriz) <- c("Divipola","XB")


###----------- List to store bootstrap iterations -----------###

plugin.estrella = list()

B = 2000

###--- Total Municipalities ---###

Div = unique(Xcenso$departamento_etnia)

###--- Municipalities in the survey ---###

Div2 = unique(Xencuesta$departamento_etnia)


for(b in 1:B){
  print(paste0('Iteracion:',b))
  
  #----------------------------------------------------------------------------#
  #--- Step 1: generate random effects for municipalities in the ENDS --#
  #----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Div2, ud = rnorm(length(Div2), 0, sd.u))
  
  #-- Creating random effects vector for all municipalities in the country -#
  
  ud = data.frame(Divipola = Div) %>% left_join(ud, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0 ----#
  
  ud$ud[is.na(ud$ud)] <- 0
  
  #------------------------------------------------------------------------------#
  #-- Step 2: Generate pseudocensus and parameter estimator from this --#
  #------------------------------------------------------------------------------#
  
  #---     Location in the list to store the estimated theta in the       ---#
  #---     pseudocensus and the one obtained in the selected sample       ---#
  
  
  plugin.estrella[[b]] = cbind(theta.censo = numeric(nrow(ud)), 
                               theta.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  # Censo with variable to generate the estimation of the probability of NI #
  censoydi = cbind.data.frame(Xcenso$departamento_etnia, NA, Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola", "Ydi", colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  #--- For each woman in each municipality, the probability of NI is generated. ---#
  
  
  for(i in 1:length(Div)){
    print(paste0('Dominio:',i))
    #--- Municipality code ---#
    
    index = Div[i]
    
    #--- Total population of the municipality ---#
    
    N.d = N_d[N_d$departamento_etnia == index,]$Nd
    
    #- Position of synthetic census matrix XBeta corresponding to the municipality -#
    
    pos = which(matriz$Divipola == index)
    
    #- Probability that a woman in the municipality has planning NI. -# 
    
    theta.di = (exp(matriz[pos,2] + rep(ud[i,2], N.d))/
                  (1 + exp(matriz[pos,2] + rep(ud[i,2], N.d))))
    
    #- Position in the census base corresponding to the municipality -#
    
    pos2 = which(censoydi$Divipola == index)
    
    #- Generating the dummy response variable based on the probability -#
    #- that a woman in the municipality will have NI                   -#
    
    censoydi[pos2, 2] = rbinom(N.d, 1, theta.di)
    
    #- Estimation of the probability of NI in women -#
    #- of the municipality in the pseudocensus      -# 
    
    plugin.estrella[[b]][i,1] = (1/N.d) * sum(censoydi[pos2, 2])
  }
  
  
  #----------------------------------------------------------------------------#
  #-- Step 3: Select sample from the pseudocensus and parameter estimator    --#
  #--         from the pseudosample                                          --#
  #----------------------------------------------------------------------------#
  
  #-- Pseudocensus sample with size that matches the number of women         --# 
  #-- selected in the ENDS sample municipalities                             --#
  
  muestra <- censoydi %>% left_join(n_d, by = c("Divipola"= "departamento_etnia")) %>%
    mutate(id_orig = 1:dim(censoydi)[1], Aleatorio = runif(dim(censoydi)[1])) %>%  
    group_by(departamento_etnia) %>% arrange(departamento_etnia, Aleatorio) %>% 
    mutate(id_efecto = row_number()) %>% 
    mutate(Seleccionado = ifelse(id_efecto <= nd, "Seleccionado", "")) %>% 
    filter(Seleccionado == "Seleccionado") %>% ungroup() %>% 
    arrange(id_orig) %>% dplyr::select(names(censoydi)) %>% as.data.frame()
  
  #--------- Model fit for the pseudo-sample ---------#
  
  pluginbootstrap <- glmer(Ydi ~ inasistencia  +   ocupada + 
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
                             (1|departamento_etnia), family = "binomial", data = muestra) 
  
  ###---    Exporting common fixed effects in the bootstrap model   ---###
  
  betasB = as.matrix(fixef(pluginbootstrap))
  
  ###---- Exporting the random effects for each of the domains ---###
  
  udB =  data.frame(Divipola = rownames(ranef(pluginbootstrap)$departamento_etnia), 
                    ud = ranef(pluginbootstrap)$departamento_etnia[[1]])
  
  rownames(udB) <- NULL

  
  #-- Creating a vector of random effects for all the municipalities in the country -#
  
  udB = data.frame(Divipola = Div) %>% left_join(udB, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0. ----#
  
  udB$ud[is.na(udB$ud)] <- 0
  
  ##------------ Construction of the synthetic census matrix XBeta ------------##
  
  matrizCenso <- cbind.data.frame(censoydi$Divipola, 
                                  cbind(1, as.matrix(censoydi %>% dplyr::select(rownames(betasB)[-1]))) %*% betasB)
  
  colnames(matrizCenso) <- c("Divipola","XB")
  
  censoydi$pluginB <- NA
  
  #--- Boostrap estimates for each domain ---#
  
  for(i in 1:length(Div)){
    
    index = which(censoydi$Divipola == Div[i])
    
    #- Probability that a woman in the municipality has NI per individual -#
    
    censoydi$pluginB[index] = exp(matrizCenso$XB[index] + udB[i,2])/
      (1 + exp(matrizCenso$XB[index] + udB[i,2]))
    
    #- Municipal probability that a woman will have Method Use -#
    
    plugin.estrella[[b]][i,2] =  mean(censoydi$pluginB[index])
  }
}


#------------------------------------------------------------------------------#
#------------------------------ Estimación del ECM ----------------------------#
#------------------------------------------------------------------------------#

#-- Average of the differences pseudocensus plugin - bootstrap pseudosample --# 
uso_mse <- plugin.estrella %>% map_df(~.x %>% data.frame() %>% 
                                       mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2))

uso_mse %>% as.data.frame()

saveRDS(uso_mse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/D6/D6_MSE_departamento_etnia.rds"))




#-----------------------------------------------------------------------------#
#---------------------Municipality - BT -------------------------------------#
#-----------------------------------------------------------------------------#

n_d = Xencuesta %>% group_by(Divipola) %>% summarise(nd = n()) 

N_d = Xcenso %>% lazy_dt() %>% group_by(Divipola) %>% summarise(Nd = n()) %>% 
  as.data.frame()


###----------- List where the bootstrap iterations will be stored -----------###

plugin.estrella2 = list()

B = 1000

###--- Total of municipalities ---###

Divv = unique(Xcenso$Divipola)

###--- Municipalities in the survey ---###

Divv2 = unique(Xencuesta$Divipola)

###------------ Construcción de la matriz censal sintética XBeta ------------###

matriz <- cbind.data.frame(Xcenso$Divipola, cbind(1,as.matrix(Xcenso %>%
                                                                select(rownames(betas)[-1]))) %*% 
                             betas)

colnames(matriz) <- c("Divipola","XB")

# COMMAND ----------

for(b in 1:B){
  print(b)
  
  #----------------------------------------------------------------------------#
  #--- Step 1: generate random effects for the municipalities in the NSDS    --#
  #----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Divv2, ud = rnorm(length(Divv2), 0, sd.u))
  
  #-- Creating a vector of random effects for all the municipalities in the country -#
  
  ud = data.frame(Divipola = Divv) %>% left_join(ud, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0. ----#
  
  ud$ud[is.na(ud$ud)] <- 0
  
  #----------------------------------------------------------------------------#
  #-- Step 2: Generate pseudocensus and parameter estimator from it          --#
  #----------------------------------------------------------------------------#
  
  #---     Location in the list to store the theta estimated in the          --#
  #--- pseudocensus and that obtained in the selected sample of the          --#
  #---   pseudocensus                                                        --#
  
  plugin.estrella2[[b]] = cbind(theta.censo = numeric(nrow(ud)), 
                                theta.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  # Census with a variable to generate the estimation of the probability of NI #
  censoydi = cbind.data.frame(Xcenso$Divipola, NA, Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola", "Ydi", colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  #--- For each woman in each municipality, probabilities of NI are generated ---#
  
  
  
  for(i in 1:length(Divv)){
    print(i)
    #--- Municipality code ---#
    
    index = Divv[i]
    
    #--- Total population of the municipality ---#
    
    N.d = N_d[N_d$Divipola == index,]$Nd
    
    #- Position in the synthetic census matrix XBeta corresponding to the municipality -#
    
    pos = which(matriz$Divipola == index)
    
    #- Probability that a woman from the municipality has an unmet need for family planning -#
    
    theta.di = (exp(matriz[pos,2] + rep(ud[i,2], N.d))/
                  (1 + exp(matriz[pos,2] + rep(ud[i,2], N.d))))
    
    #- Position in the census database corresponding to the municipality -#
    
    pos2 = which(censoydi$Divipola == index)
    
    #- Generating the simulated response variable based on the probability   -#
    #-           that a woman from the municipality has no intention         -#
    
    censoydi[pos2, 2] = rbinom(N.d, 1, theta.di)
    
    #- Estimating the probability of no intention in women         -# 
    #-          from the municipality in the pseudocensus          -#
    
    plugin.estrella2[[b]][i,1] = (1/N.d) * sum(censoydi[pos2, 2])
  }
  
  
  #----------------------------------------------------------------------------#
  #-- Step 3: Select sample from the pseudocensus and parameter estimator --#
  #--         based on the pseudosample                                     --#
  #----------------------------------------------------------------------------#
  
  #- Sample from the pseudocensus with a size that matches the number of women -#
  #-              selected in the sample municipalities of the ENDS            -#
  
  muestra <- censoydi %>% left_join(n_d, by = c("Divipola"= "Divipola")) %>%
    mutate(id_orig = 1:dim(censoydi)[1], Aleatorio = runif(dim(censoydi)[1])) %>%  
    group_by(Divipola) %>% arrange(Divipola, Aleatorio) %>% 
    mutate(id_efecto = row_number()) %>% 
    mutate(Seleccionado = ifelse(id_efecto <= nd, "Seleccionado", "")) %>% 
    filter(Seleccionado == "Seleccionado") %>% ungroup() %>% 
    arrange(id_orig) %>% select(names(censoydi)) %>% as.data.frame()
  
  #--------- Fit the model for the pseudo-sample ---------#
  
  pluginbootstrap <- glmer(Ydi ~ inasistencia  +   ocupada + 
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
                             (1|departamento_etnia), family = "binomial", data = muestra) 
  
  ###--- Exporting common fixed effects in the bootstrap model   ---###
  
  betasB = as.matrix(fixef(pluginbootstrap))
  
  ###---- Exporting the random effects for each of the domains ---###
  
  udB =  data.frame(Divipola = as.character(Divv) ,
                    #rownames(ranef(pluginbootstrap)$Divipola), #aqui cambiar
                    ud = 0#ranef(pluginbootstrap)$Divipola[[1]]
  )#aqui cambiar
  
  rownames(udB) <- NULL
  #udB$Divipola <- as.numeric(udB$Divipola)
  
  #-- Creating a vector of random effects for all municipalities in the country -#
  
  udB = data.frame(Divipola = as.character(Divv)) %>% left_join(udB, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0 ----#
  
  udB$ud[is.na(udB$ud)] <- 0
  
  ##------------ Construction of the synthetic census matrix XBeta ------------##
  
  matrizCenso <- cbind.data.frame(censoydi$Divipola, 
                                  cbind(1, as.matrix(censoydi %>% 
                                                       select(rownames(betasB)[-1]))) %*% betasB)
  
  colnames(matrizCenso) <- c("Divipola","XB")
  
  censoydi$pluginB <- NA
  
  #--- Bootstrap estimates for each municipality ---#
  
  for(i in 1:length(Divv)){
    
    index = which(censoydi$Divipola == Divv[i])
    
    #- Probability that a woman in the municipality has unmet need by individual -#
    
    censoydi$pluginB[index] = exp(matrizCenso$XB[index] + udB[i,2])/
      (1 + exp(matrizCenso$XB[index] + udB[i,2]))
    
    #-  Probability that a woman has unmet need -#
    
    plugin.estrella[[b]][i,2] =  mean(censoydi$pluginB[index])
  }
}

#--------------------------------------------------------------------------------#
#---------------------- Estimation of the Mean Squared Error --------------------#
#--------------------------------------------------------------------------------#

#-- Average of the differences between plugin pseudocensus and bootstrap pseudosample --#

D6_mse <- plugin.estrella2 %>% map_df(~.x %>% data.frame() %>% 
                                        mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2))

#------------------------------------------------------------------------------#
#---------------------- Exporting Outputs: MSE Estimation ---------------------#
#------------------------------------------------------------------------------#


saveRDS(D6_mse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/D6/D6_MSE_Divipola.rds"))







