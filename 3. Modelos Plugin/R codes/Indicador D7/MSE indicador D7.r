######################################################################################################## 
## Title:        Family Planning Indicator D7 - Use of any contranceptive method.                     ##
## Returns:      Estimation MSE by Domains of Interest                                                ##  
## The code was modified by Lina Sánchez, Sebastián Oviedo, DANE.                                     ## 
## Carlos Rámirez,  Université de Montréal, Juliana Guerrero, World Bank.                             ##
## The original code was developed by Felipe Molina, Andrés Gutiérrez and Diego Lemus in the MRP      ##                                               
## Project - Left No One Behind, Institution: ECLAC, Division: Statistics Division.                   ##                                                 
########################################################################################################
options(encoding = "UTF-8", scipen = 999)
rm(list = ls())
gc()
library(sae)
library(dplyr)
library(nlme)
library(TeachingSampling)
library(dtplyr)
library(data.table)

mount_point = "D:/SAE anticonceptivos/Colombia-UNFPA/"


##  Original Estimator
###---------------------------------- ENDS ----------------------------------###


Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds")) %>% filter(unida == 1)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))

###------------------------------  Senate Weights --------------------------### 

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)

###-------- Municipality - Department Identifier per Census Person ---------###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###--------------------------------- CENSUS ----------------------------------###

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


###-------------- Appending Divipola Code to the ENDS Database ---------------###
Xencuesta <- Xencuesta %>% left_join(., Variables.disenio,`copy` = TRUE,
                                     by = c("Divipola"= "Divipola"))


# Domains

Div = unique(Xcenso$departamento_etnia)

# Sample sizes for Ends and Population

n_d = Xencuesta %>% group_by(departamento_etnia) %>% summarise(nd = n()) %>%
  mutate_all(~replace(.,is.na(.),0)) %>% as.data.frame()

N_d = Xcenso %>% lazy_dt() %>% group_by(departamento_etnia) %>% summarise(Nd = n()) %>% 
  as.data.frame()
# Census model

pluginreg1 <- glmer(usametodo ~ inasistencia  +   ocupada + 
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

pluginreg2 <- glmer(usamoderno ~ inasistencia  +   ocupada + 
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

pluginreg1 = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6UMunicipio.rds"))
pluginreg2 = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginD6MUMunicipio.rds"))
pluginreg3 = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D7/PluginNIUMunicipio.rds"))


# fixed and random effects

betas1 = as.matrix(fixef(pluginreg1))
betas2 = as.matrix(fixef(pluginreg2))
betas3 = as.matrix(fixef(pluginreg3))

###------------ Construction of the Synthetic Census Matrix XBeta ------------###

matriz <- cbind.data.frame(Xcenso$departamento_etnia, cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas1)[-1])))%*%betas1,
                                cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas2)[-1])))%*%betas2,
                                cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas3)[-1])))%*%betas3)

colnames(matriz) <- c("Divipola","XB1","XB2","XB3")


###------------ Clear Memory ------------###

rm(Municipio)
gc()
###----------- List where Bootstrap Iterations will be Saved -----------###

plugin.estrellaD7 = list()

B = 500

###----------------- Total Municipalities -----------------###

Div = unique(Xcenso$departamento_etnia)

###----------------- Municipalities in the Survey -----------------###

Div2 = unique(Xencuesta$departamento_etnia)

## ------------  The for loop for the bootstrap goes here  -----------------##

# Sd1 2 y 3  are the standard errors of the estimated random effects in each model
 sd1.u = pluginreg1@theta
 sd2.u = pluginreg2@theta
 sd3.u = pluginreg3@theta
 
for(b in 1:B){
  print(paste0('Iteracion:',b))
  
  #-----------------------------------------------------------------------------#
  #--- Step 1: Generate Random Effects for Municipalities in the ENDS ----------#
  #-----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Div2,ud1 = rnorm(length(Div2),0,sd1.u),
                  ud2 = rnorm(length(Div2),0,sd2.u),
                  ud3 = rnorm(length(Div2),0,sd3.u))
  #-- Creating a vector of random effects for all Municipalities in the country -#
  
  ud = data.frame(Divipola = Div) %>%
                  left_join(ud, by = "Divipola")
  
  #----- If the Municipality was not surveyed, it will have a random effect of 0 ----#
  ud$ud1[is.na(ud$ud1)] <- 0
  ud$ud2[is.na(ud$ud2)] <- 0
  ud$ud3[is.na(ud$ud3)] <- 0
  
#------------------------------------------------------------------------------#
#-- Step 2: Generate Pseudocensus and Estimate the Parameter from it ----------#
#------------------------------------------------------------------------------#

#---     Location in the list to store the estimated theta in the       ---#
#---     pseudocensus and obtained in the selected sample of this       ---#
  
  plugin.estrellaD7[[b]] = cbind(D7.censo = numeric(nrow(ud)),
                                 D7.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  
  print("Generando pseudocenso")
  #- Census with a variable to generate the estimation of the D7 probability -#
  censoydi = cbind.data.frame(Xcenso$departamento_etnia,NA,NA,NA,Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola","Ydi1","Ydi2","Ydi3",colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  ###-----   #For each Divipola and each individual, their probability of use estimation is generated.  ------###
  ###------With these probabilities, pseudocensuses are generated.   --------###
  for(i in 1:length(Div)){
    #--- Municipality code ---#
    index = Div[i]
    
    #--- Total Population of the Municipality ---#
  
    N.d = N_d[N_d$departamento_etnia == index,]$Nd
    
    #- Position in the Synthetic Census Matrix XBeta corresponding to the municipality -#
    pos = which(matriz$Divipola==index)
    
    
    #- PProbability that a woman in the municipality has  Unmeet Needs -# 
    #D6M
    theta.di1 = (exp(matriz[pos,2] + rep(ud[i,2],N.d))/(1+exp(matriz[pos,2] + rep(ud[i,2],N.d))))
    #D6
    theta.di2 = (exp(matriz[pos,3] + rep(ud[i,3],N.d))/(1+exp(matriz[pos,3] + rep(ud[i,3],N.d))))
    #NI
    theta.di3 = (exp(matriz[pos,4] + rep(ud[i,4],N.d))/(1+exp(matriz[pos,4] + rep(ud[i,4],N.d))))
    
    
    #- Position in the Census Database corresponding to the municipality -#
    pos2 = which(censoydi$Divipola==index)
    
    #- Generating the simulated response variable based on the probability -#
    #-                that a woman in the municipality has D7              -#
    censoydi[pos2,2] = rbinom(N.d,1,theta.di1)
    censoydi[pos2,3] = rbinom(N.d,1,theta.di2)
    censoydi[pos2,4] = rbinom(N.d,1,theta.di3)
    

    
    #-  estimation of the probability of D7 among women in the municipality in the pseudocensus. -#
    
    media1 = (1/N.d)*sum(censoydi[pos2,2])
    media2 = (1/N.d)*sum(censoydi[pos2,3])
    media3 = (1/N.d)*sum(censoydi[pos2,4])
    
    plugin.estrellaD7[[b]][i,1] = media1/(media2+media3)
    
    
  }
  
  
  print("Generando pseudo muestra")
#------------------------------------------------------------------------------#
#-- Step 3: Select a sample from the pseudocensus and estimate the parameter --#
#--         from the pseudosample                                            --#
#------------------------------------------------------------------------------#
  
  #- Sample from the pseudocensus with a size that matches the number of women -#
  #-          selected in the sample of municipalities from the ENDS           -#
  
  muestra <- censoydi %>% left_join(n_d, by = c("Divipola" = "departamento_etnia") ) %>%
    mutate(id_orig = 1:dim(censoydi)[1],Aleatorio = runif(dim(censoydi)[1])) %>% 
    group_by(departamento_etnia) %>% arrange(departamento_etnia,Aleatorio) %>% 
    mutate(id_efecto = row_number()) %>% 
    mutate(Seleccionado = ifelse(id_efecto <= nd, "Seleccionado", "")) %>% 
    filter(Seleccionado == "Seleccionado") %>% ungroup() %>% 
    arrange(id_orig) %>% select(names(censoydi)) %>% as.data.frame()
  
  
  #--------- Ajuste del modelo para la pseudo-muestra ---------#
  
  print("Modelos bootstrap")
  modelo1 <- glmer(Ydi1 ~ inasistencia  +   ocupada + 
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
  
  modelo2 <- glmer(Ydi2 ~  inasistencia  +   ocupada + 
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
  
  modelo3 <- glmer(Ydi3 ~  inasistencia  +   ocupada + 
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
  betasB1 = as.matrix(fixef(modelo1))
  betasB2 = as.matrix(fixef(modelo2))
  betasB3 = as.matrix(fixef(modelo3))
  
 ###---- Exporting the random effects for each of the domains ---###
  udB =  data.frame(Divipola = rownames(ranef(modelo1)$departamento_etnia), 
                    ud1 = ranef(modelo1)$departamento_etnia[[1]],
                    ud2 = ranef(modelo2)$departamento_etnia[[1]],
                    ud3 = ranef(modelo3)$departamento_etnia[[1]] )
  rownames(udB) <- NULL
  
  #-- Creating a vector of random effects for all municipalities in the country -#
  udB = data.frame(Divipola = Div) %>% left_join(udB, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0 ----#
  udB$ud1[is.na(udB$ud1)] <- 0
  udB$ud2[is.na(udB$ud2)] <- 0
  udB$ud3[is.na(udB$ud3)] <- 0
  
  ##------------ Construction of the Synthetic Census Matrix XBeta ------------##
  matrizCenso <- cbind.data.frame(censoydi$Divipola, 
                                  cbind(1, as.matrix(censoydi %>% dplyr::select(rownames(betasB1)[-1]))) %*% betasB1,
                                  cbind(1, as.matrix(censoydi %>% dplyr::select(rownames(betasB2)[-1]))) %*% betasB2,
                                  cbind(1, as.matrix(censoydi %>% dplyr::select(rownames(betasB3)[-1]))) %*% betasB3)
  colnames(matrizCenso) <- c("Divipola","XB1","XB2","XB3")
  
  #D6M
  censoydi$pluginB1 <- numeric(dim(censoydi)[1])
  #D6
  censoydi$pluginB2 <- numeric(dim(censoydi)[1])
  # NI
  censoydi$pluginB3 <- numeric(dim(censoydi)[1])
  print("Estimaciones bootstrap")
  
   #--- Bootstrap Estimates for Each Domain ---#
  for(i in 1:length(Div)){
    
    index = which(censoydi$Divipola==Div[i])
    
    #- Probability that a woman in the municipality has D7 per individual -#
    
    censoydi$pluginB1[index] = exp(matrizCenso$XB1[index]+udB[i,2])/(1+exp(matrizCenso$XB1[index]+udB[i,2]))
    censoydi$pluginB2[index] = exp(matrizCenso$XB2[index]+udB[i,3])/(1+exp(matrizCenso$XB2[index]+udB[i,3]))
    censoydi$pluginB3[index] = exp(matrizCenso$XB3[index]+udB[i,4])/(1+exp(matrizCenso$XB3[index]+udB[i,4]))
    
    media1 = mean(censoydi$pluginB1[index])
    media2 = mean(censoydi$pluginB2[index])
    media3 = mean(censoydi$pluginB3[index])
    
    #- Municipal probability that a woman  -#
    plugin.estrellaD7[[b]][i,2] = media1/(media2+media3)

  }
}

save(plugin.estrella, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/plugin.estrella.D7"))

#warnings()
#load(file = "3. Modelos Plug-In/plugin.estrella.D7")


#------------------------------------------------------------------------------#
#------------------------------  ECM  Estimation   ----------------------------#
#------------------------------------------------------------------------------#
 
#-- Average of the differences between plugin pseudocensus and bootstrap pseudosample --#
 library(purrr)
d7_mse <- plugin.estrellaD7 %>% map_df(~.x %>% data.frame() %>% 
                                         mutate(dif2 = (D7.censo - D7.muestra)^2)) %>% 
   group_by(Codmun) %>% summarise(ecm = mean(dif2))
 
d7_mse %>% as.data.frame()
 
saveRDS(d7_mse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/D7/D7_departamento_etnia.rds"))
 
#-----------------------------------------------------------------------------#
#---------------------Con Municipio - BT -------------------------------------#
#-----------------------------------------------------------------------------#

###------------ Construction of the synthetic census matrix XBeta ------------###

matriz <- cbind.data.frame(Xcenso$Divipola, cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas1)[-1])))%*%betas1,
                           cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas2)[-1])))%*%betas2,
                           cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas3)[-1])))%*%betas3)

colnames(matriz) <- c("Divipola","XB1","XB2","XB3")


###------------ Clearing Memory ------###
rm(censoydi,matriz,matrizCenso, muestra,n_d,N_d,plugin.estrella,pluginreg,ud,udB, Divipola, pluginbootstrap,Municipio)

gc()


n_d = Xencuesta %>% group_by(Divipola) %>% summarise(nd = n()) %>%
  mutate_all(~replace(.,is.na(.),0)) %>% as.data.frame()

N_d = Xcenso %>% lazy_dt() %>% group_by(Divipola) %>% summarise(Nd = n()) %>% 
  as.data.frame()

plugin.estrella2_D7 = list()

B = 2#500

###--- Total Municipalities ---###

Divv = unique(Xcenso$Divipola)

###--- Municipalities in the Survey ---###

Divv2 = unique(Xencuesta$Divipola)

sd1.u = pluginreg1@theta
sd2.u = pluginreg2@theta
sd3.u = pluginreg3@theta

for(b in 1:B){
  print(paste0('Iteracion:',b))
  
  #----------------------------------------------------------------------------#
  #--- Step 1: Generate Random Effects for Municipalities in the ENDS ----------#
  #----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Divv2,
                  ud1 = rnorm(length(Divv2),0,sd1.u),
                  ud2 = rnorm(length(Divv2),0,sd2.u),
                  ud3 = rnorm(length(Divv2),0,sd3.u))
   #-- Creating a vector of random effects for all municipalities in the country -#
  
  ud = data.frame(Divipola = Divv) %>%
    left_join(ud, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0 ----#
  ud$ud1[is.na(ud$ud1)] <- 0
  ud$ud2[is.na(ud$ud2)] <- 0
  ud$ud3[is.na(ud$ud3)] <- 0
  
  #------------------------------------------------------------------------------#
  #-- Step 2: Generate Pseudocensus and Estimate the Parameter from it ----------#
  #------------------------------------------------------------------------------#
  
  #---     Location in the list to store the estimated theta in the       ---#
  #---     pseudocensus and obtained in the selected sample of this       ---#
  
  plugin.estrella2_D7[[b]] = cbind(D7.censo = numeric(nrow(ud)),
                                 D7.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  print("Generando pseudocenso")
  #- Census with variable to generate the estimate of the probability of D7#
  censoydi = cbind.data.frame(Xcenso$Divipola,NA,NA,NA,Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola","Ydi1","Ydi2","Ydi3",colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  #--- For each woman in each municipality, the probability of D7 is generated. ---#
  
  for(i in 1:length(Divv)){
    #--- Municipality Code ---#
    index = Divv[i]
    
    #--- Total Population of the Municipality ---#
    
    N.d = N_d[N_d$Divipola == index,]$Nd
    
    #- Position in the Synthetic Census Matrix XBeta corresponding to the municipality -#
    pos = which(matriz$Divipola==index)
    
    #- Probability that a woman in the municipality has Family Planning Unmet Need -#

    #D6M
    theta.di1 = (exp(matriz[pos,2] + rep(ud[i,2],N.d))/(1+exp(matriz[pos,2] + rep(ud[i,2],N.d))))
    #D6
    theta.di2 = (exp(matriz[pos,3] + rep(ud[i,3],N.d))/(1+exp(matriz[pos,3] + rep(ud[i,3],N.d))))
    #NI
    theta.di3 = (exp(matriz[pos,4] + rep(ud[i,4],N.d))/(1+exp(matriz[pos,4] + rep(ud[i,4],N.d))))
    
    
     #- Position in the Census Database corresponding to the municipality -#
    pos2 = which(censoydi$Divipola==index)
    
    #- Generating the dummy response variable from the probability that a woman in the municipality has D7.       -#
    censoydi[pos2,2] = rbinom(N.d,1,theta.di1)
    censoydi[pos2,3] = rbinom(N.d,1,theta.di2)
    censoydi[pos2,4] = rbinom(N.d,1,theta.di3)
    
    
    
    #-  Estimation of the probability of D7 among women in the municipality in the pseudocensus.          -#
    
    media1 = (1/N.d)*sum(censoydi[pos2,2])
    media2 = (1/N.d)*sum(censoydi[pos2,3])
    media3 = (1/N.d)*sum(censoydi[pos2,4])
    
    plugin.estrella2_D7[[b]][i,1] = media1/(media2+media3)
    
    
  }
  
  print("Generando pseudo muestra")
  #----------------------------------------------------------------------------#
  #-- Step 3: Select a sample from the pseudocensus and estimate the parameter --
  #--         from the pseudosample                                           --
  #----------------------------------------------------------------------------#
  
  #- Sample from the pseudocensus with a size that matches the number of women -#
  #-          selected in the sample of municipalities from the ENDS          -#
  
  muestra <- censoydi %>% left_join(n_d, by = c("Divipola" = "Divipola") ) %>%
    mutate(id_orig = 1:dim(censoydi)[1],Aleatorio = runif(dim(censoydi)[1])) %>% 
    group_by(Divipola) %>% arrange(Divipola,Aleatorio) %>% 
    mutate(id_efecto = row_number()) %>% 
    mutate(Seleccionado = ifelse(id_efecto <= nd, "Seleccionado", "")) %>% 
    filter(Seleccionado == "Seleccionado") %>% ungroup() %>% 
    arrange(id_orig) %>% select(names(censoydi)) %>% as.data.frame()
  
  #--------- Fitting the model for the pseudo-sample ---------#
  
  print("Modelos bootstrap")
  modelo1 <- glmer(Ydi1 ~ inasistencia  +   ocupada + 
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
  
  modelo2 <- glmer(Ydi2 ~  inasistencia  +   ocupada + 
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
  
  modelo3 <- glmer(Ydi3 ~  inasistencia  +   ocupada + 
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
  betasB1 = as.matrix(fixef(modelo1))
  betasB2 = as.matrix(fixef(modelo2))
  betasB3 = as.matrix(fixef(modelo3))
  
  ###---- Exporting the random effects for each of the domains ---###
  udB =  data.frame(Divipola = as.character(Divv) ,
                    ud1 = 0,
                    ud2 = 0,
                    ud3 = 0 )
  rownames(udB) <- NULL
  
  #-- Creating a vector of random effects for all municipalities in the country -#
  udB = data.frame(Divipola = as.character(Divv)) %>% left_join(udB, by = "Divipola")
  
  #----- If the municipality was not surveyed, it will have a random effect of 0 ----#
  udB$ud1[is.na(udB$ud1)] <- 0
  udB$ud2[is.na(udB$ud2)] <- 0
  udB$ud3[is.na(udB$ud3)] <- 0
  
  ##------------ Construction of the Synthetic Census Matrix XBeta ------------##
  matrizCenso <- cbind.data.frame(censoydi$Divipola, 
                                  cbind(1, as.matrix(censoydi %>% dplyr::select(rownames(betasB1)[-1]))) %*% betasB1,
                                  cbind(1, as.matrix(censoydi %>% dplyr::select(rownames(betasB2)[-1]))) %*% betasB2,
                                  cbind(1, as.matrix(censoydi %>% dplyr::select(rownames(betasB3)[-1]))) %*% betasB3)
  colnames(matrizCenso) <- c("Divipola","XB1","XB2","XB3")
  
  #D6M
  censoydi$pluginB1 <- numeric(dim(censoydi)[1])
  #D6
  censoydi$pluginB2 <- numeric(dim(censoydi)[1])
  # NI
  censoydi$pluginB3 <- numeric(dim(censoydi)[1])
  print("Estimaciones bootstrap")
  
  #--- Bootstrap Estimates for Each Municipality ---#
  for(i in 1:length(Divv)){
    
    index = which(censoydi$Divipola==Divv[i])
    
    #- Probability that a woman in the municipality has D7 per individual -#
    
    censoydi$pluginB1[index] = exp(matrizCenso$XB1[index]+udB[i,2])/(1+exp(matrizCenso$XB1[index]+udB[i,2]))
    censoydi$pluginB2[index] = exp(matrizCenso$XB2[index]+udB[i,3])/(1+exp(matrizCenso$XB2[index]+udB[i,3]))
    censoydi$pluginB3[index] = exp(matrizCenso$XB3[index]+udB[i,4])/(1+exp(matrizCenso$XB3[index]+udB[i,4]))
    
    media1 = mean(censoydi$pluginB1[index])
    media2 = mean(censoydi$pluginB2[index])
    media3 = mean(censoydi$pluginB3[index])
    
    plugin.estrella2_D7[[b]][i,2] = media1/(media2+media3)
  
  }
}

#save(plugin.estrella, file = "3. Modelos Plug-In/plugin.estrella.D7")
#warnings()
#load(file = "3. Modelos Plug-In/plugin.estrella.D7")


#------------------------------------------------------------------------------#
#------------------------------  ECM Estimation    ----------------------------#
#------------------------------------------------------------------------------#

#-- Average of the differences between plugin pseudocensus and bootstrap pseudosample --# 

d72_mse <- plugin.estrella2_D7 %>% map_df(~.x %>% data.frame() %>% 
                                         mutate(dif2 = (D7.censo - D7.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2))

d72_mse %>% as.data.frame()

saveRDS(d72_mse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/D7/D7_Divipola.rds"))

