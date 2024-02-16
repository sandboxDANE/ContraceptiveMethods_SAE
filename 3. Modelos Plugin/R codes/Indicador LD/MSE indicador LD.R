
######################################################################################################## 
## Title:        Family Planning Indicator LD - Use of long-term contraceptive methods                ##
## Returns:      Estimation MSE by Domains of Interest                                                ##  
## The code was modified by Lina Sánchez, Sebastián Oviedo, DANE.                                     ## 
## Carlos Rámirez,  Université de Montréal, Juliana Guerrero, World Bank.                             ##
## The original code was developed by Felipe Molina, Andrés Gutiérrez and Diego Lemus in the MRP      ##                                         
## Project - Left No One Behind, Institution: ECLAC, Division: Statistics Division.                   ##                                                 
########################################################################################################

rm(list = ls())
gc()
library(sae)
library(dplyr)
library(nlme)
library(TeachingSampling)
library(dtplyr)
library(data.table)
library(purrr)
library(ordinal)

mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"


## Estimador original
###---------------------------------- ENDS ----------------------------------###

Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds")) %>% filter(unida == 1)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###------------  Senate Weights ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)

###----- Municipality - Department Identifier per Census Person ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###--------------------------------- CENSO ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds")) %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)


###----- Desing Variables ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))


###--------------------- List of the 1122 Municipalities ---------------------###
Divipola <- Municipio %>% distinct(Divipola, .keep_all = T)

Divipola$departamento = as.character(as.numeric(Divipola$departamento))
Divipola$municipio = as.character(as.numeric(Divipola$municipio))


Xencuesta$departamento = as.character(as.numeric(Xencuesta$departamento))
Xencuesta$municipio = as.character(as.numeric(Xencuesta$municipio))


###------------ Appending Divipola Code to the ENDS Database ------------###
Xencuesta <- Xencuesta %>% left_join(., Variables.disenio,`copy` = TRUE,
                                     by = c("Divipola"= "Divipola"))


# Domains

Div = unique(Xcenso$departamento_etnia)

# Sample sizes for Ends and Population

n_d = Xencuesta %>% group_by(departamento_etnia) %>% summarise(nd = n()) %>%
  mutate_all(~replace(.,is.na(.),0)) %>% as.data.frame()

N_d = Xcenso %>% lazy_dt() %>% group_by(departamento_etnia) %>% summarise(Nd = n()) %>% 
  as.data.frame()

###------------ Adding the categorical dependent variable.  ------------###

Xencuesta <- Xencuesta %>% mutate(larga_duracion = ifelse(tipo_met %in% c(6, 7),'4_Permanentes',
                                                          ifelse(tipo_met %in% c(11, 2),'3_Larga',
                                                                 ifelse(tipo_met %in% c(0),'1_NoUse', '2_Corta'))))
Xencuesta <- Xencuesta %>% filter(larga_duracion != '1_NoUse')

Xencuesta <- Xencuesta %>% mutate(larga_duracion_2 = 
                                    ifelse(larga_duracion %in% c('3_Larga','4_Permanentes'),'1_Larga', '2_Corta'))

table(Xencuesta$larga_duracion)
table(Xencuesta$larga_duracion_2)

################################################################################
### Ajuste del modelo Plugin ###
################################################################################

###----------------------------- Saturated model----------------------------###


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

###----------- Exportando los efectos fijos comunes a los dominios ----------###
saveRDS(pluginreg, file = file.path(mount_point,"3. Modelos Plugin/Output/LD/PluginLD2SW_Mun.rds"))

pluginreg = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/LD/PluginLD2SW_Mun.rds"))




betas = as.matrix(pluginreg$beta)

###--- MSE ---###

sd.u = pluginreg$stDev


################################################################################
### Model with Random Effects of Department_Ethnicity
################################################################################

###------------ Construction of the Synthetic Census Matrix XBeta ------------###



matriz <- cbind.data.frame(Xcenso$departamento_etnia, cbind(as.matrix(Xcenso %>%
                                                                          dplyr::select(rownames(betas)))) %*% betas)

colnames(matriz) <- c("Divipola","XB")
matriz$Alpha1_2 = pluginreg$Alpha[[1]]

###----------- List where Bootstrap Iterations will be Saved -----------###

plugin.estrella = list()
plugin.estrella2 = list()
plugin.estrella3 = list()
plugin.estrella4 = list()

B =500

###--- Total Municipalities ---###

Div = unique(Xcenso$departamento_etnia)

###--- Municipalities in the Survey ---###

Div2 = unique(Xencuesta$departamento_etnia)

#Div = Div2
for(b in 1:B){
  print(paste0('Iteracion:',b))
  
  #-----------------------------------------------------------------------------#
  #--- Step 1: Generate Random Effects for Municipalities in the ENDS ----------#
  #-----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Div2, ud = rnorm(length(Div2), 0, sd.u))
  
  #-- Creating a vector of random effects for all Municipalities in the country -#
  
  ud = data.frame(Divipola = Div) %>% left_join(ud, by = "Divipola")
  
  #----- If the Municipality was not surveyed, it will have a random effect of 0 ----#
  
  ud$ud[is.na(ud$ud)] <- 0
  
#------------------------------------------------------------------------------#
#-- Step 2: Generate Pseudocensus and Estimate the Parameter from it ----------#
#------------------------------------------------------------------------------#

#---     Location in the list to store the estimated theta in the       ---#
#---     pseudocensus and obtained in the selected sample of this       ---#
  
  plugin.estrella[[b]] = cbind(theta.censo = numeric(nrow(ud)), 
                               theta.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  plugin.estrella2[[b]] = cbind(theta.censo = numeric(nrow(ud)), 
                               theta.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  plugin.estrella3[[b]] = cbind(theta.censo = numeric(nrow(ud)), 
                               theta.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  plugin.estrella4[[b]] = cbind(theta.censo = numeric(nrow(ud)), 
                               theta.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  #- Census with variable to generate the estimate of the probability of LD -#
  censoydi = cbind.data.frame(Xcenso$departamento_etnia, NA, Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola", "Ydi", colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  #--- For each woman in each municipality, the probability of LD is generated. ---#
  
  
  for(i in 1:length(Div)){

    #--- Municipality code ---#
    
    index = Div[i]
    
    #--- Total Population of the Municipality ---#
    
    N.d = N_d[N_d$departamento_etnia == index,]$Nd
    
    #- Position in the Synthetic Census Matrix XBeta corresponding to the municipality -#
    
    pos = which(matriz$Divipola == index)
    
    #- Posición en la base censal que corresponde al municipio -#
    
    pos2 = which(censoydi$Divipola == index)
    
    #- Generate the dummy response variable from the probability that a woman in the municipality has LD. -#
    
    censoydi[pos2, 2] = rbinom(N.d, 4, c(0.2, 0.32, 0.1, 0.38))
    
    #-  Estimation of the probability of LD among women in the municipality in the pseudocensus.     -#
    is.na(censoydi$Ydi[pos]) = 0
    plugin.estrella[[b]][i,1] = (1/N.d) * sum(censoydi$Ydi[pos2] == 1 & is.na(censoydi$Ydi[pos2]) == F)
    plugin.estrella2[[b]][i,1] = (1/N.d) * sum(censoydi$Ydi[pos2] == 2 & is.na(censoydi$Ydi[pos2]) == F)
    plugin.estrella3[[b]][i,1] = (1/N.d) * sum(censoydi$Ydi[pos2] == 3 & is.na(censoydi$Ydi[pos2]) == F)
    plugin.estrella4[[b]][i,1] = (1/N.d) * sum(censoydi$Ydi[pos2] == 4 & is.na(censoydi$Ydi[pos2]) == F)
  }
  
#------------------------------------------------------------------------------#
#-- Step 3: Select a sample from the pseudocensus and estimate the parameter --#
#--         from the pseudosample                                            --#
#------------------------------------------------------------------------------#
  
  #- Sample from the pseudocensus with a size that matches the number of women -#
  #-          selected in the sample of municipalities from the ENDS           -#
  
  muestra <- censoydi  %>% left_join(n_d, by = c("Divipola"= "departamento_etnia")) %>%
    mutate(id_orig = 1:dim(censoydi)[1], Aleatorio = runif(dim(censoydi)[1])) %>%  
    group_by(departamento_etnia) %>% arrange(departamento_etnia, Aleatorio) %>% 
    mutate(id_efecto = row_number()) %>% 
    mutate(Seleccionado = ifelse(id_efecto <= nd, "Seleccionado", "")) %>% 
    filter(Seleccionado == "Seleccionado") %>% ungroup() %>% 
    arrange(id_orig) %>% dplyr::select(names(censoydi)) %>% as.data.frame() %>% 
    filter(Ydi %in% 1:4)
  
  muestra <- muestra %>% mutate(Ydi = ifelse(Ydi %in% c(4,3),'4_Permanentes',
                                                            ifelse(Ydi %in% c(3),'3_Larga',
                                                                   ifelse(Ydi %in% c(1),'1_NoUse', '2_Corta'))))
  

  #--------- Model fitting for the pseudo-sample ---------#
  
  pluginbootstrap <- clmm2(factor(Ydi) ~ inasistencia  +   ocupada + 
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
                     data=muestra, Hess=TRUE)
  
  ###--- Exporting the common fixed effects in the bootstrap model ---###
  
  betasB = as.matrix(pluginbootstrap$beta)
  
   ###---- Exporting the random effects for each of the domains ---###
  
  udB =  data.frame(Divipola = levels(as.factor(Xencuesta$departamento_etnia)), 
                    ud = pluginreg$ranef)

  
  rownames(udB) <- NULL

  
  #-- Creating a vector of random effects for all municipalities in the country -#
  
  udB = data.frame(Divipola = Div) %>% left_join(udB, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0 ----#
  
  udB$ud[is.na(udB$ud)] <- 0
  
  ##------------ Construction of the Synthetic Census Matrix XBeta ------------##
  
  matrizCenso <- cbind.data.frame(censoydi$Divipola, 
                                  cbind(as.matrix(censoydi %>% dplyr::select(rownames(betasB)))) %*% betasB)
  
  colnames(matrizCenso) <- c("Divipola","XB")
  
  #-  Intercepts for each model                                        -#
  
  matrizCenso$Alpha1_2 = pluginbootstrap$Alpha[[1]]
  matrizCenso$Alpha2_3 = pluginbootstrap$Alpha[[2]]
  matrizCenso$Alpha3_4 = pluginbootstrap$Alpha[[3]]
  censoydi$pluginB <- NA

  ###-------- Boostrap estimates for each domain ----------###
  
  for(i in 1:length(Div)){
    
    index = which(censoydi$Divipola == Div[i])
    
    #- Probability that a woman in the municipality has LD per individual. -#
    
    matrizCenso$pluginLD_1 = exp( matrizCenso$Alpha1_2 - matrizCenso$XB + udB[i,2])/
      (1 + exp( matrizCenso$Alpha1_2 - matrizCenso$XB + udB[i,2]))
    
    matrizCenso$pluginLD_2 = exp( matrizCenso$Alpha2_3 - matrizCenso$XB + udB[i,2])/
      (1 + exp( matrizCenso$Alpha2_3 - matrizCenso$XB + udB[i,2]))
    
    matrizCenso$pluginLD_3 = exp(  matrizCenso$Alpha3_4 - matrizCenso$XB + udB[i,2])/
      (1 + exp( matrizCenso$Alpha3_4 - matrizCenso$XB + udB[i,2] ))
    
    censoydiT = matrizCenso %>% mutate(LD1 = pluginLD_1,
                                     LD2 = pluginLD_2 - pluginLD_1,
                                     LD3 = pluginLD_3 - pluginLD_2,
                                     LD4 = 1 - pluginLD_3 )
    
    
    plugin.estrella[[b]][i,2]  =  mean(censoydiT$LD1)
    plugin.estrella2[[b]][i,2] =  mean(censoydiT$LD2)
    plugin.estrella3[[b]][i,2] =  mean(censoydiT$LD3)
    plugin.estrella4[[b]][i,2] =  mean(censoydiT$LD4)
  }
  gc()
}


#------------------------------------------------------------------------------#
#------------------------------  ECM  Estimation   ----------------------------#
#------------------------------------------------------------------------------#

#-- Average of the differences between plugin pseudocensus and bootstrap pseudosample --#
uso_mse <- plugin.estrella %>% map_df(~.x %>% data.frame() %>% 
                                       mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2)) %>%  as.data.frame()

uso_mse2 <- plugin.estrella2 %>% map_df(~.x %>% data.frame() %>% 
                                        mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2)) %>%  as.data.frame()

uso_mse3 <- plugin.estrella3 %>% map_df(~.x %>% data.frame() %>% 
                                        mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2)) %>%  as.data.frame()

uso_mse4 <- plugin.estrella4 %>% map_df(~.x %>% data.frame() %>% 
                                        mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2)) %>%  as.data.frame()


saveRDS(uso_mse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/LD/NoUse_MSE_departamento_etnia.rds"))
saveRDS(uso_mse2 %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/LD/CortaD_MSE_departamento_etnia.rds"))
saveRDS(uso_mse3 %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/LD/LargaD_MSE_departamento_etnia.rds"))
saveRDS(uso_mse4 %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/LD/Uso_Permanente_MSE_departamento.rds"))




#-----------------------------------------------------------------------------#
#---------------------Con Municipio - BT -------------------------------------#
#-----------------------------------------------------------------------------#

n_d = Xencuesta %>% group_by(Divipola) %>% summarise(nd = n()) 

N_d = Xcenso %>% lazy_dt() %>% group_by(Divipola) %>% summarise(Nd = n()) %>% 
  as.data.frame()


###-------------- List where Bootstrap Iterations will be Saved -----------###

plugin.estrella2 = list()

B = 1000

###--- Total Municipalities ---###

Divv = unique(Xcenso$Divipola)

###--- Municipalities in the Survey ---###

Divv2 = unique(Xencuesta$Divipola)


###------------ Construction of the Synthetic Census Matrix XBeta ------------###

matriz <- cbind.data.frame(Xcenso$Divipola, cbind(1,as.matrix(Xcenso %>%
                                                                select(rownames(betas)[-1]))) %*% 
                             betas)

colnames(matriz) <- c("Divipola","XB")


for(b in 1:B){
  print(b)
  
  #----------------------------------------------------------------------------#
  #--- Step 1: Generate Random Effects for Municipalities in the ENDS ----------#
  #----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Divv2, ud = rnorm(length(Divv2), 0, sd.u))
  
  #-- Creating a vector of random effects for all municipalities in the country -#
  
  ud = data.frame(Divipola = Divv) %>% left_join(ud, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0 ----#
  
  ud$ud[is.na(ud$ud)] <- 0
  
  #------------------------------------------------------------------------------#
  #-- Step 2: Generate Pseudocensus and Estimate the Parameter from it ----------#
  #------------------------------------------------------------------------------#
  
  #---     Location in the list to store the estimated theta in the       ---#
  #---     pseudocensus and obtained in the selected sample of this       ---#
  
  plugin.estrella2[[b]] = cbind(theta.censo = numeric(nrow(ud)), 
                                theta.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  #- Census with variable to generate the estimate of the probability of LD -#
  censoydi = cbind.data.frame(Xcenso$Divipola, NA, Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola", "Ydi", colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  #--- For each woman in each municipality, the probability of LD is generated.---#
  
  
  for(i in 1:length(Divv)){
    print(i)
    #--- Municipality Code ---#
    
    index = Divv[i]
    
   #--- Total Population of the Municipality ---#
    
    N.d = N_d[N_d$Divipola == index,]$Nd
    
    #- Position in the Synthetic Census Matrix XBeta corresponding to the municipality -#
    
    pos = which(matriz$Divipola == index)
    
    #- Probability that a woman in the municipality has Family Planning Unmet Need -#
    
    theta.di = (exp(matriz[pos,2] + rep(ud[i,2], N.d))/
                  (1 + exp(matriz[pos,2] + rep(ud[i,2], N.d))))
    
    #- Position in the Census Database corresponding to the municipality -#
    
    pos2 = which(censoydi$Divipola == index)
    
    #- The dummy response variable is based on the probability that a woman in the municipality has LD.    -#
    
    censoydi[pos2, 2] = rbinom(N.d, 1, theta.di)
    
    #-  Estimation of the probability of NI among women in the municipality in the pseudocensus.    -#
    
    plugin.estrella2[[b]][i,1] = (1/N.d) * sum(censoydi[pos2, 2])
  }
  
  
  #------------------------------------------------------------------------------##
  #-- Step 3: Select a sample from the pseudocensus and estimate the parameter --##
  #--         from the pseudosample                                            --##
  #------------------------------------------------------------------------------##
  
  #- Sample from the pseudocensus with a size that matches the number of women -#
  #-          selected in the sample of municipalities from the ENDS          -#
  
  muestra <- censoydi %>% left_join(n_d, by = c("Divipola"= "Divipola")) %>%
    mutate(id_orig = 1:dim(censoydi)[1], Aleatorio = runif(dim(censoydi)[1])) %>%  
    group_by(Divipola) %>% arrange(Divipola, Aleatorio) %>% 
    mutate(id_efecto = row_number()) %>% 
    mutate(Seleccionado = ifelse(id_efecto <= nd, "Seleccionado", "")) %>% 
    filter(Seleccionado == "Seleccionado") %>% ungroup() %>% 
    arrange(id_orig) %>% select(names(censoydi)) %>% as.data.frame()
  
  #--------- Fitting the model for the pseudo-sample ---------#
  
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
  
  ###--- Exporting the common fixed effects in the bootstrap model ---###
  
  betasB = as.matrix(fixef(pluginbootstrap))
  
   ###---- Exporting the random effects for each of the domains ---###
  
  udB =  data.frame(Divipola = as.character(Divv) ,
                    #rownames(ranef(pluginbootstrap)$Divipola), #aqui cambiar
                    ud = 0#ranef(pluginbootstrap)$Divipola[[1]]
  )
  
  rownames(udB) <- NULL
  
  #-- Creating a vector of random effects for all municipalities in the country -#
  
  udB = data.frame(Divipola = as.character(Divv)) %>% left_join(udB, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0 ----#
  
  udB$ud[is.na(udB$ud)] <- 0
  
  ##------------ Construction of the Synthetic Census Matrix XBeta ------------##
  
  matrizCenso <- cbind.data.frame(censoydi$Divipola, 
                                  cbind(1, as.matrix(censoydi %>% 
                                                       select(rownames(betasB)[-1]))) %*% betasB)
  
  colnames(matrizCenso) <- c("Divipola","XB")
  
  censoydi$pluginB <- NA
  
  #--- Bootstrap Estimates for Each Municipality ---#
  
  for(i in 1:length(Divv)){
    
    index = which(censoydi$Divipola == Divv[i])
    
    #- Probability that a woman in the municipality has LD per individual -#
    
    censoydi$pluginB[index] = exp(matrizCenso$XB[index] + udB[i,2])/
      (1 + exp(matrizCenso$XB[index] + udB[i,2]))
    
    #- Municipal probability of a woman having LD -#
    
    plugin.estrella[[b]][i,2] =  mean(censoydi$pluginB[index])
  }
}

#------------------------------------------------------------------------------#
#------------------------------  ECM Estimation    ----------------------------#
#------------------------------------------------------------------------------#

#-- Average of the differences between plugin pseudocensus and bootstrap pseudosample --#
D6_mse <- plugin.estrella2 %>% map_df(~.x %>% data.frame() %>% 
                                        mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2))

#------------------------------------------------------------------------------#
#--------------------- Exporting Outputs: ECM Estimation ----------------------#
#------------------------------------------------------------------------------#

saveRDS(D6_mse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/D6/D6_MSE_Divipola.rds"))







