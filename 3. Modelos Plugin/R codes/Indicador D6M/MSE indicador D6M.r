######################################################################################################## 
## Title:        Family Planning Indicator D6m - Use of Modern Methods.                               ##
## Returns:      Estimation MSE by Domains of Interest                                             ##  
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
###------------  Senate Weights ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)

###----- Municipality - Department Identifier per Census Person ----###

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

# MODELO PARA CENSO


pluginreg <- glmer(usamoderno ~ inasistencia  +   ocupada + 
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


saveRDS(pluginreg, file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/PluginD6M_model.rds"))


pluginreg = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D6M/PluginD6M_model.rds"))

# fixed and random effects

betas = as.matrix(fixef(pluginreg))

###--- MSE ---###

sd.u = pluginreg@theta

################################################################################
### Model with Random Effects of Department_Ethnicity
################################################################################

###------------ Construction of the Synthetic Census Matrix XBeta ------------###

matriz <- cbind.data.frame(Xcenso$departamento_etnia, cbind(1,as.matrix(Xcenso %>%
                                                                          dplyr::select(rownames(betas)[-1]))) %*% betas)

colnames(matriz) <- c("Divipola","XB")

################################################################################
###------------ Clear Memory ------------###

rm(Municipio)

###----------- List where Bootstrap Iterations will be Saved -----------###
plugin.estrella = list()

###--- Total Municipalities ---###

Div = unique(Xcenso$departamento_etnia)

###--- Municipalities in the Survey ---###

Div2 = unique(Xencuesta$departamento_etnia)
B = 1000


## The for loop for the bootstrap goes here


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
  
#- Census with a variable to generate the estimation of the D6M probability -#
  censoydi = cbind.data.frame(Xcenso$departamento_etnia, NA, Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola", "Ydi", colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  

  #For each Divipola and each individual, their probability of use estimation is generated.
  #With these probabilities, pseudocensuses are generated.
  for(i in 1:length(Div)){
    print(paste0('Dominio:',i))
    #--- Municipality code ---#
    index = Div[i]
    
    #--- Total Population of the Municipality ---#
    N.d = N_d[N_d$departamento_etnia == index,]$Nd
    
    #- Position in the Synthetic Census Matrix XBeta corresponding to the municipality -#
    pos = which(matriz$Divipola==index)
    
    
    #- Probability that a woman in the municipality has Family Planning Unmet Need -#
    theta.di = (exp(matriz[pos,2] + rep(ud[i,2],N.d))/(1+exp(matriz[pos,2] + rep(ud[i,2],N.d))))
    
    
    #- Position in the Census Database corresponding to the municipality -#
    pos2 = which(censoydi$Divipola==index)
    
    #- Generating the simulated response variable based on the probability -#
    #-        that a woman in the municipality has Family Planning Unmet Need       -#
    censoydi[pos2,2] = rbinom(N.d,1,theta.di)
    

    #- Estimation of the probability of Family Planning Unmet Need in women -#
    #- of the municipality in the pseudocensus -#
    plugin.estrella[[b]][i,1] = (1/N.d)*sum(censoydi[pos2,2])
  }
  
#------------------------------------------------------------------------------#
#-- Step 3: Select a sample from the pseudocensus and estimate the parameter --#
#--         from the pseudosample                                            --#
#------------------------------------------------------------------------------#
  
  #- Sample from the pseudocensus with a size that matches the number of women -#
  #-          selected in the sample of municipalities from the ENDS           -#
  
  muestra <- censoydi %>% left_join(n_d, by = c("Divipola" = "departamento_etnia")) %>%
    mutate(id_orig = 1:dim(censoydi)[1],Aleatorio = runif(dim(censoydi)[1])) %>% 
    group_by(departamento_etnia) %>% arrange(departamento_etnia,Aleatorio) %>% 
    mutate(id_efecto = row_number()) %>% 
    mutate(Seleccionado = ifelse(id_efecto <= nd, "Seleccionado", "")) %>% 
    filter(Seleccionado == "Seleccionado") %>% ungroup() %>% 
    arrange(id_orig) %>% select(names(censoydi)) %>% as.data.frame()
  
  
  # modelo con esta muestra
  
  pluginbootstrap <- glmer(Ydi  ~ inasistencia  +   ocupada + 
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
  
  udB =  data.frame(Divipola = rownames(ranef(pluginbootstrap)$departamento_etnia), 
                    ud = ranef(pluginbootstrap)$departamento_etnia[[1]])
  
  rownames(udB) <- NULL
  
  
  #-- Creando vector de efectos aleatorios para todos los municipios del país -#
  
  udB = data.frame(Divipola = Div) %>% left_join(udB, by = "Divipola")
  
  #----- Si el municipio no fue encuestado tendrá un efecto aleatorio de 0 ----#
  
  udB$ud[is.na(udB$ud)] <- 0
  
  ##------------ Construcción de la matriz censal sintética XBeta ------------##
  
  matrizCenso <- cbind.data.frame(censoydi$Divipola, 
                                  cbind(1, as.matrix(censoydi %>% dplyr::select(rownames(betasB)[-1]))) %*% betasB)
  
  colnames(matrizCenso) <- c("Divipola","XB")
  
  censoydi$pluginB <- NA
  
  #--- Estimaciones boostrap para cada dominio ---#
  
  for(i in 1:length(Div)){
    
    index = which(censoydi$Divipola == Div[i])
    
    #- Probabilidad de que una mujer del municipio tenga NI por individuo -#
    
    censoydi$pluginB[index] = exp(matrizCenso$XB[index] + udB[i,2])/
      (1 + exp(matrizCenso$XB[index] + udB[i,2]))
    
    #- Probabilidad municipal de que una mujer tenga Uso de metodo -#
    
    plugin.estrella[[b]][i,2] =  mean(censoydi$pluginB[index])
  }

}

#save(plugin.estrella, file = "3. Modelos Plug-In/plugin.estrella.D6M")
#warD6Mngs()
#load(file = "3. Modelos Plug-In/plugin.estrella.D6M")


#------------------------------------------------------------------------------#
#------------------------------ Estimación del ECM ----------------------------#
#------------------------------------------------------------------------------#

#-- Promedio de las diferencias plugin pseudocenso - pseudomuestra bootstrap --# 
library(purrr)
D6Mmse <- plugin.estrella %>% map_df(~.x %>% data.frame() %>% 
                                        mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2))

D6Mmse %>% as.data.frame()

saveRDS(D6Mmse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/D6M_MSE_departamento_etnia.rds"))

#18:16pm -03/Feb/2022

rm(censoydi,matriz,matrizCenso, muestra,n_d,N_d,plugin.estrella,pluginreg,ud,udB, Divipola, pluginbootstrap,Municipio)

#-----------------------------------------------------------------------------#
#---------------------Con Municipio - BT -------------------------------------#
#-----------------------------------------------------------------------------#

pluginreg = readRDS(file.path(mount_point,"3. Modelos Plugin/Output/D6M/PluginD6M_model.rds"))


# fixed and random effects

betas = as.matrix(fixef(pluginreg))

###--- MSE ---###

sd.u = pluginreg@theta

n_d = Xencuesta %>% group_by(Divipola) %>% summarise(nd = n()) 

N_d = Xcenso %>% lazy_dt() %>% group_by(Divipola) %>% summarise(Nd = n()) %>% 
  as.data.frame()


###----------- Lista donde se guardarán las iteraciones bootstrap -----------###

plugin.estrella2 = list()

B = 500#1000

###--- Total de Municipios ---###

Divv = unique(Xcenso$Divipola)

###--- Municipios en la encuesta ---###

Divv2 = unique(Xencuesta$Divipola)


###------------ Construcción de la matriz censal sintética XBeta ------------###

matriz <- cbind.data.frame(Xcenso$Divipola, cbind(1,as.matrix(Xcenso %>%
                                                                select(rownames(betas)[-1]))) %*% 
                             betas)

colnames(matriz) <- c("Divipola","XB")


for(b in 1:B){
  print(paste0("Iteracion: ",b))
  
  #----------------------------------------------------------------------------#
  #--- Paso 1: generar los efectos aleatorios para los municipios en la ENDS --#
  #----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Divv2, ud = rnorm(length(Divv2), 0, sd.u))
  
  #-- Creando vector de efectos aleatorios para todos los municipios del país -#
  
  ud = data.frame(Divipola = Divv) %>% left_join(ud, by = "Divipola")
  
  #----- Si el municipio no fue encuestado tendrá un efecto aleatorio de 0 ----#
  
  ud$ud[is.na(ud$ud)] <- 0
  
  #----------------------------------------------------------------------------#
  #-- Paso 2: Generar pseudocenso y estimador del parámetro a partir de este --#
  #----------------------------------------------------------------------------#
  
  #---     Ubicación en la lista para almacenar el theta estimado en el     ---#
  #---     pseudocenso y el obtenido en la muestra seleccionada de este     ---#
  
  plugin.estrella2[[b]] = cbind(theta.censo = numeric(nrow(ud)), 
                                theta.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  #- Censo con variable para generar la estimación de la probabilidad de NI -#
  censoydi = cbind.data.frame(Xcenso$Divipola, NA, Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola", "Ydi", colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  #--- Para cada mujer del cada municipio se generan probabilidad de NI ---#
  
  
  for(i in 1:length(Divv)){
    #print(i)
    #--- Código del municipio ---#
    
    index = Divv[i]
    
    #--- Total poblacional del municipio ---#
    
    N.d = N_d[N_d$Divipola == index,]$Nd
    
    #- Posición de matriz censal sintética XBeta que corresponde al municipio -#
    
    pos = which(matriz$Divipola == index)
    
    #- Probabilidad de que una mujer del municipio tenga NI de planificación -# 
    
    theta.di = (exp(matriz[pos,2] + rep(ud[i,2], N.d))/
                  (1 + exp(matriz[pos,2] + rep(ud[i,2], N.d))))
    
    #- Posición en la base censal que corresponde al municipio -#
    
    pos2 = which(censoydi$Divipola == index)
    
    #- Generando la variable respuesta simulada a partir de la probabilidad -#
    #-                de que una mujer del municipio tenga NI               -#
    
    censoydi[pos2, 2] = rbinom(N.d, 1, theta.di)
    
    #-  Estimación de la probabilidad de NI en las mujeres   -# 
    #-              del municipio en el pseudocenso          -#
    
    plugin.estrella2[[b]][i,1] = (1/N.d) * sum(censoydi[pos2, 2])
  }
  
  
  #----------------------------------------------------------------------------#
  #-- Paso 3: Seleccionar muestra del pseudocenso y estimador del parámetro  --#
  #--         a partir de la pseudomuestra                                   --#
  #----------------------------------------------------------------------------#
  
  #- Muestra del pseudocenso con tamaño que coincide con el número de mujeres -#
  #-              seleccionada en la muestra municipios de la ENDS            -#
  
  muestra <- censoydi %>% left_join(n_d, by = c("Divipola"= "Divipola")) %>%
    mutate(id_orig = 1:dim(censoydi)[1], Aleatorio = runif(dim(censoydi)[1])) %>%  
    group_by(Divipola) %>% arrange(Divipola, Aleatorio) %>% 
    mutate(id_efecto = row_number()) %>% 
    mutate(Seleccionado = ifelse(id_efecto <= nd, "Seleccionado", "")) %>% 
    filter(Seleccionado == "Seleccionado") %>% ungroup() %>% 
    arrange(id_orig) %>% select(names(censoydi)) %>% as.data.frame()
  
  #--------- Ajuste del modelo para la pseudo-muestra ---------#
  
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
  
  ###---    Exportando los efectos fijos comunes en el modelo bootstrap   ---###
  
  betasB = as.matrix(fixef(pluginbootstrap))
  
  ###---- Exportando los efectos aleatorios para cada uno de los dominios ---###
  
  udB =  data.frame(Divipola = as.character(Divv) ,
                    #rownames(ranef(pluginbootstrap)$Divipola), #aqui cambiar
                    ud = 0#ranef(pluginbootstrap)$Divipola[[1]]
  )#aqui cambiar
  
  rownames(udB) <- NULL
  #udB$Divipola <- as.numeric(udB$Divipola)
  
  #-- Creando vector de efectos aleatorios para todos los municipios del país -#
  
  udB = data.frame(Divipola = as.character(Divv)) %>% left_join(udB, by = "Divipola")
  
  #----- Si el municipio no fue encuestado tendrá un efecto aleatorio de 0 ----#
  
  udB$ud[is.na(udB$ud)] <- 0
  
  ##------------ Construcción de la matriz censal sintética XBeta ------------##
  
  matrizCenso <- cbind.data.frame(censoydi$Divipola, 
                                  cbind(1, as.matrix(censoydi %>% 
                                                       select(rownames(betasB)[-1]))) %*% betasB)
  
  colnames(matrizCenso) <- c("Divipola","XB")
  
  censoydi$pluginB <- NA
  rm(pluginbootstrap)
  #--- Estimaciones boostrap para cada municipio ---#
  
  for(i in 1:length(Divv)){
    
    index = which(censoydi$Divipola == Divv[i])
    
    #- Probabilidad de que una mujer del municipio tenga NI por individuo -#
    
    censoydi$pluginB[index] = exp(matrizCenso$XB[index] + udB[i,2])/
      (1 + exp(matrizCenso$XB[index] + udB[i,2]))
    
    #- Probabilidad municipal de que una mujer tenga NI -#
    
    plugin.estrella2[[b]][i,2] =  mean(censoydi$pluginB[index])
  }
  rm(matrizCenso)
}

#------------------------------------------------------------------------------#
#------------------------------ Estimación del ECM ----------------------------#
#------------------------------------------------------------------------------#

#-- Promedio de las diferencias plugin pseudocenso - pseudomuestra bootstrap --# 
library(purrr)
D6M_mse <- plugin.estrella2 %>% map_df(~.x %>% data.frame() %>% 
                                        mutate(dif2 = (theta.censo - theta.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2))

#------------------------------------------------------------------------------#
#--------------------- Exportando salidas: Estimación ECM ---------------------#
#------------------------------------------------------------------------------#

saveRDS(D6M_mse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/D6M/D6M_MSE_Divipola.rds"))

