
################################################################################
##                     Proyecto MRP - Left No One Behind                      ##
## Title:        Indicador de plaD6Mficación familiar D6 - Uso de algún método ##
##               de plaD6Mficación                                             ##
## Returns:      Estimación MSE por domiD6Mos de interés                       ##
## Author:       Felipe Molina & Andrés Gutiérrez & Diego Lemus               ##
## Institution:  CEPAL                                                        ##
## Date:         2021                                                         ##
## División:     División de Estadísticas                                     ##
## Disclaimer:   Estos códigos computacionales han sido programados con el    ##
##               fin de ejemplificar las metodologías propuestas por CEPAL.   ##
##               La responsabilidad del uso de los programas recae            ##
##               completamente sobre  los funcionarios a quienes se hace      ##
##               entrega. Se exime a la CEPAL de los errores que puedan ser   ##
##               ocasionados por el uso incorrecto de estos códigos.          ##
## Modificado:   Sebastián Oviedo - Feb/2022                                  ##       
################################################################################

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


## Estimador original
###---------------------------------- ENDS ----------------------------------###


Xencuesta <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XencuestaD.rds")) %>% filter(unida == 1)
Xencuesta = Xencuesta %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
###------------ Anexando los Senate Weights a la base de la ENDS ------------###

Xencuesta$Sweights <- nrow(Xencuesta) * Xencuesta$fexp/sum(Xencuesta$fexp)

###----- Identificador de Municipio - Departamento por persona del censo ----###

Municipio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/Municipio.rds"))

###--------------------------------- CENSO ----------------------------------###

Xcenso <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/XcensoD.rds")) %>% filter(unida == 1)
Xcenso = Xcenso %>% mutate(reconoce_etnia = ifelse(etnia == "Ningun_grupo", 0, 1))
Xcenso = Xcenso %>% mutate(`reconoce_etnia:misma_etnia_1` = reconoce_etnia*misma_etnia_1)


###----- Variables diseño ----###

Variables.disenio <- readRDS(file.path(mount_point,"1. ConciliarBases/Output/variable_disenio_ENDS.rds"))
Variables.disenio = Variables.disenio %>% mutate(log_Total_p = log(Total_p))


###--------------------- Listado de los 1122 municipios ---------------------###
Divipola <- Municipio %>% distinct(Divipola, .keep_all = T)

Divipola$departamento = as.character(as.numeric(Divipola$departamento))
Divipola$municipio = as.character(as.numeric(Divipola$municipio))


Xencuesta$departamento = as.character(as.numeric(Xencuesta$departamento))
Xencuesta$municipio = as.character(as.numeric(Xencuesta$municipio))


###------------ Anexando el código Divipola a la base de la ENDS ------------###
Xencuesta <- Xencuesta %>% left_join(., Variables.disenio,`copy` = TRUE,
                                     by = c("Divipola"= "Divipola"))


# DomiD6Mos

Div = unique(Xcenso$departamento_etnia)

# tamaño de muestra Ends y poblacionales

n_d = Xencuesta %>% group_by(departamento_etnia) %>% summarise(nd = n()) %>%
  mutate_all(~replace(.,is.na(.),0)) %>% as.data.frame()

N_d = Xcenso %>% lazy_dt() %>% group_by(departamento_etnia) %>% summarise(Nd = n()) %>% 
  as.data.frame()
# MODELO PARA CENSO

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

###------------ Construcción de la matriz censal sintética XBeta ------------###

matriz <- cbind.data.frame(Xcenso$departamento_etnia, cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas1)[-1])))%*%betas1,
                                cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas2)[-1])))%*%betas2,
                                cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas3)[-1])))%*%betas3)

colnames(matriz) <- c("Divipola","XB1","XB2","XB3")


###------------ Limpiiar memoria ------###

rm(Municipio)
gc()
####----------- Lista donde se guardarán las iteraciones bootstrap -----------###

plugin.estrellaD7 = list()

B = 500

###--- Total de Municipios ---###

Div = unique(Xcenso$departamento_etnia)

###--- Municipios en la encuesta ---###

Div2 = unique(Xencuesta$departamento_etnia)

# Aquí va el for para el bootstrap

# Sd1 2 y 3 son los errores estandar de los efectos aleatorios estimados en cada modelo
 sd1.u = pluginreg1@theta
 sd2.u = pluginreg2@theta
 sd3.u = pluginreg3@theta
 
for(b in 1:B){
  print(paste0('Iteracion:',b))
  
  #----------------------------------------------------------------------------#
  #--- Paso 1: generar los efectos aleatorios para los municipios en la ENDS --#
  #----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Div2,ud1 = rnorm(length(Div2),0,sd1.u),
                  ud2 = rnorm(length(Div2),0,sd2.u),
                  ud3 = rnorm(length(Div2),0,sd3.u))
  #-- Creando vector de efectos aleatorios para todos los municipios del país -#
  
  ud = data.frame(Divipola = Div) %>%
                  left_join(ud, by = "Divipola")
  
  #----- Si el municipio no fue encuestado tendrá un efecto aleatorio de 0 ----#
  ud$ud1[is.na(ud$ud1)] <- 0
  ud$ud2[is.na(ud$ud2)] <- 0
  ud$ud3[is.na(ud$ud3)] <- 0
  
  #----------------------------------------------------------------------------#
  #-- Paso 2: Generar pseudocenso y estimador del parámetro a partir de este --#
  #----------------------------------------------------------------------------#
  
  #---     Ubicación en la lista para almacenar el theta estimado en el     ---#
  #---     pseudocenso y el obtenido en la muestra seleccionada de este     ---#
  
  plugin.estrellaD7[[b]] = cbind(D7.censo = numeric(nrow(ud)),
                                 D7.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  
  print("Generando pseudocenso")
  #- Censo con variable para generar la estimación de la probabilidad de D7 -#
  censoydi = cbind.data.frame(Xcenso$departamento_etnia,NA,NA,NA,Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola","Ydi1","Ydi2","Ydi3",colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  #--- Para cada mujer del cada municipio se generan probabilidad de NI ---#
  for(i in 1:length(Div)){
    #--- Código del municipio ---#
    index = Div[i]
    
    #--- Total poblacional del municipio ---#
  
    N.d = N_d[N_d$departamento_etnia == index,]$Nd
    
    #- Posición de matriz censal sintética XBeta que corresponde al municipio -#
    pos = which(matriz$Divipola==index)
    
    
    #- Probabilidad de que una mujer del municipio tenga NI de planificación -# 
    #D6M
    theta.di1 = (exp(matriz[pos,2] + rep(ud[i,2],N.d))/(1+exp(matriz[pos,2] + rep(ud[i,2],N.d))))
    #D6
    theta.di2 = (exp(matriz[pos,3] + rep(ud[i,3],N.d))/(1+exp(matriz[pos,3] + rep(ud[i,3],N.d))))
    #NI
    theta.di3 = (exp(matriz[pos,4] + rep(ud[i,4],N.d))/(1+exp(matriz[pos,4] + rep(ud[i,4],N.d))))
    
    
    #- Posición en la base censal que corresponde al municipio -#
    pos2 = which(censoydi$Divipola==index)
    
    #- Generando la variable respuesta simulada a partir de la probabilidad -#
    #-                de que una mujer del municipio tenga D7               -#
    censoydi[pos2,2] = rbinom(N.d,1,theta.di1)
    censoydi[pos2,3] = rbinom(N.d,1,theta.di2)
    censoydi[pos2,4] = rbinom(N.d,1,theta.di3)
    

    
    #-  Estimación de la probabilidad de D7 en las mujeres   -# 
    #-              del municipio en el pseudocenso          -#
    
    media1 = (1/N.d)*sum(censoydi[pos2,2])
    media2 = (1/N.d)*sum(censoydi[pos2,3])
    media3 = (1/N.d)*sum(censoydi[pos2,4])
    
    plugin.estrellaD7[[b]][i,1] = media1/(media2+media3)
    
    
  }
  
  
  print("Generando pseudo muestra")
  #----------------------------------------------------------------------------#
  #-- Paso 3: Seleccionar muestra del pseudocenso y estimador del parámetro  --#
  #--         a partir de la pseudomuestra                                   --#
  #----------------------------------------------------------------------------#
  
  #- Muestra del pseudocenso con tamaño que coincide con el número de mujeres -#
  #-              seleccionada en la muestra municipios de la ENDS            -#
  
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
  
  
  
  ###---    Exportando los efectos fijos comunes en el modelo bootstrap   ---###
  betasB1 = as.matrix(fixef(modelo1))
  betasB2 = as.matrix(fixef(modelo2))
  betasB3 = as.matrix(fixef(modelo3))
  
  ###---- Exportando los efectos aleatorios para cada uno de los dominios ---###
  udB =  data.frame(Divipola = rownames(ranef(modelo1)$departamento_etnia), 
                    ud1 = ranef(modelo1)$departamento_etnia[[1]],
                    ud2 = ranef(modelo2)$departamento_etnia[[1]],
                    ud3 = ranef(modelo3)$departamento_etnia[[1]] )
  rownames(udB) <- NULL
  
  #-- Creando vector de efectos aleatorios para todos los municipios del país -#
  udB = data.frame(Divipola = Div) %>% left_join(udB, by = "Divipola")
  
  #----- Si el municipio no fue encuestado tendrá un efecto aleatorio de 0 ----#
  udB$ud1[is.na(udB$ud1)] <- 0
  udB$ud2[is.na(udB$ud2)] <- 0
  udB$ud3[is.na(udB$ud3)] <- 0
  
  ##------------ Construcción de la matriz censal sintética XBeta ------------##
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
  
  #--- Estimaciones boostrap para cada dominio ---#
  for(i in 1:length(Div)){
    
    index = which(censoydi$Divipola==Div[i])
    
    #- Probabilidad de que una mujer del municipio tenga D7 por individuo -#
    
    censoydi$pluginB1[index] = exp(matrizCenso$XB1[index]+udB[i,2])/(1+exp(matrizCenso$XB1[index]+udB[i,2]))
    censoydi$pluginB2[index] = exp(matrizCenso$XB2[index]+udB[i,3])/(1+exp(matrizCenso$XB2[index]+udB[i,3]))
    censoydi$pluginB3[index] = exp(matrizCenso$XB3[index]+udB[i,4])/(1+exp(matrizCenso$XB3[index]+udB[i,4]))
    
    media1 = mean(censoydi$pluginB1[index])
    media2 = mean(censoydi$pluginB2[index])
    media3 = mean(censoydi$pluginB3[index])
    
    #- Probabilidad municipal de que una mujer  -#
    plugin.estrellaD7[[b]][i,2] = media1/(media2+media3)

  }
}

save(plugin.estrella, file = file.path(mount_point,"3. Modelos Plugin/Output/D7/plugin.estrella.D7"))

#warnings()
#load(file = "3. Modelos Plug-In/plugin.estrella.D7")


 #------------------------------------------------------------------------------#
 #------------------------------ Estimación del ECM ----------------------------#
 #------------------------------------------------------------------------------#
 
 #-- Promedio de las diferencias plugin pseudocenso - pseudomuestra bootstrap --# 
 library(purrr)
d7_mse <- plugin.estrellaD7 %>% map_df(~.x %>% data.frame() %>% 
                                         mutate(dif2 = (D7.censo - D7.muestra)^2)) %>% 
   group_by(Codmun) %>% summarise(ecm = mean(dif2))
 
d7_mse %>% as.data.frame()
 
saveRDS(d7_mse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/D7/D7_departamento_etnia.rds"))
 
#-----------------------------------------------------------------------------#
#---------------------Con Municipio - BT -------------------------------------#
#-----------------------------------------------------------------------------#

###------------ Construcción de la matriz censal sintética XBeta ------------###

matriz <- cbind.data.frame(Xcenso$Divipola, cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas1)[-1])))%*%betas1,
                           cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas2)[-1])))%*%betas2,
                           cbind(1,as.matrix(Xcenso %>% dplyr::select(rownames(betas3)[-1])))%*%betas3)

colnames(matriz) <- c("Divipola","XB1","XB2","XB3")


###------------ Limpiar memoria ------###
rm(censoydi,matriz,matrizCenso, muestra,n_d,N_d,plugin.estrella,pluginreg,ud,udB, Divipola, pluginbootstrap,Municipio)

gc()


n_d = Xencuesta %>% group_by(Divipola) %>% summarise(nd = n()) %>%
  mutate_all(~replace(.,is.na(.),0)) %>% as.data.frame()

N_d = Xcenso %>% lazy_dt() %>% group_by(Divipola) %>% summarise(Nd = n()) %>% 
  as.data.frame()

plugin.estrella2_D7 = list()

B = 2#500

###--- Total de Municipios ---###

Divv = unique(Xcenso$Divipola)

###--- Municipios en la encuesta ---###

Divv2 = unique(Xencuesta$Divipola)

sd1.u = pluginreg1@theta
sd2.u = pluginreg2@theta
sd3.u = pluginreg3@theta

for(b in 1:B){
  print(paste0('Iteracion:',b))
  
  #----------------------------------------------------------------------------#
  #--- Paso 1: generar los efectos aleatorios para los municipios en la ENDS --#
  #----------------------------------------------------------------------------#
  
  ud = data.frame(Divipola = Divv2,
                  ud1 = rnorm(length(Divv2),0,sd1.u),
                  ud2 = rnorm(length(Divv2),0,sd2.u),
                  ud3 = rnorm(length(Divv2),0,sd3.u))
  #-- Creando vector de efectos aleatorios para todos los municipios del país -#
  
  ud = data.frame(Divipola = Divv) %>%
    left_join(ud, by = "Divipola")
  
  #----- Si el municipio no fue encuestado tendrá un efecto aleatorio de 0 ----#
  ud$ud1[is.na(ud$ud1)] <- 0
  ud$ud2[is.na(ud$ud2)] <- 0
  ud$ud3[is.na(ud$ud3)] <- 0
  
  #----------------------------------------------------------------------------#
  #-- Paso 2: Generar pseudocenso y estimador del parámetro a partir de este --#
  #----------------------------------------------------------------------------#
  
  #---     Ubicación en la lista para almacenar el theta estimado en el     ---#
  #---     pseudocenso y el obtenido en la muestra seleccionada de este     ---#
  
  plugin.estrella2_D7[[b]] = cbind(D7.censo = numeric(nrow(ud)),
                                 D7.muestra = numeric(nrow(ud))) %>% 
    data.frame() %>% mutate(Codmun = ud$Divipola)
  
  print("Generando pseudocenso")
  #- Censo con variable para generar la estimación de la probabilidad de D7 -#
  censoydi = cbind.data.frame(Xcenso$Divipola,NA,NA,NA,Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,])
  
  colnames(censoydi) <- c("Divipola","Ydi1","Ydi2","Ydi3",colnames(Xcenso %>% as.data.table() %>% .[,c("Divipola") := NULL,]))
  
  #--- Para cada mujer del cada municipio se generan probabilidad de NI ---#
  
  for(i in 1:length(Divv)){
    #--- Código del municipio ---#
    index = Divv[i]
    
    #--- Total poblacional del municipio ---#
    
    N.d = N_d[N_d$Divipola == index,]$Nd
    
    #- Posición de matriz censal sintética XBeta que corresponde al municipio -#
    pos = which(matriz$Divipola==index)
    
    
    #- Probabilidad de que una mujer del municipio tenga NI de planificación -# 
    #D6M
    theta.di1 = (exp(matriz[pos,2] + rep(ud[i,2],N.d))/(1+exp(matriz[pos,2] + rep(ud[i,2],N.d))))
    #D6
    theta.di2 = (exp(matriz[pos,3] + rep(ud[i,3],N.d))/(1+exp(matriz[pos,3] + rep(ud[i,3],N.d))))
    #NI
    theta.di3 = (exp(matriz[pos,4] + rep(ud[i,4],N.d))/(1+exp(matriz[pos,4] + rep(ud[i,4],N.d))))
    
    
    #- Posición en la base censal que corresponde al municipio -#
    pos2 = which(censoydi$Divipola==index)
    
    #- Generando la variable respuesta simulada a partir de la probabilidad -#
    #-                de que una mujer del municipio tenga D7               -#
    censoydi[pos2,2] = rbinom(N.d,1,theta.di1)
    censoydi[pos2,3] = rbinom(N.d,1,theta.di2)
    censoydi[pos2,4] = rbinom(N.d,1,theta.di3)
    
    
    
    #-  Estimación de la probabilidad de D7 en las mujeres   -# 
    #-              del municipio en el pseudocenso          -#
    
    media1 = (1/N.d)*sum(censoydi[pos2,2])
    media2 = (1/N.d)*sum(censoydi[pos2,3])
    media3 = (1/N.d)*sum(censoydi[pos2,4])
    
    plugin.estrella2_D7[[b]][i,1] = media1/(media2+media3)
    
    
  }
  
  # 4) Obtener muestra del Censo anterior para generar nuevo modelo
  # Censo sin Divipolas muestreadas
  
  print("Generando pseudo muestra")
  #----------------------------------------------------------------------------#
  #-- Paso 3: Seleccionar muestra del pseudocenso y estimador del parámetro  --#
  #--         a partir de la pseudomuestra                                   --#
  #----------------------------------------------------------------------------#
  
  #- Muestra del pseudocenso con tamaño que coincide con el número de mujeres -#
  #-              seleccionada en la muestra municipios de la ENDS            -#
  
  muestra <- censoydi %>% left_join(n_d, by = c("Divipola" = "Divipola") ) %>%
    mutate(id_orig = 1:dim(censoydi)[1],Aleatorio = runif(dim(censoydi)[1])) %>% 
    group_by(Divipola) %>% arrange(Divipola,Aleatorio) %>% 
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
  
  
  
  ###---    Exportando los efectos fijos comunes en el modelo bootstrap   ---###
  betasB1 = as.matrix(fixef(modelo1))
  betasB2 = as.matrix(fixef(modelo2))
  betasB3 = as.matrix(fixef(modelo3))
  
  ###---- Exportando los efectos aleatorios para cada uno de los dominios ---###
  udB =  data.frame(Divipola = as.character(Divv) ,
                    ud1 = 0,
                    ud2 = 0,
                    ud3 = 0 )
  rownames(udB) <- NULL
  
  #-- Creando vector de efectos aleatorios para todos los municipios del país -#
  udB = data.frame(Divipola = as.character(Divv)) %>% left_join(udB, by = "Divipola")
  
  #----- Si el municipio no fue encuestado tendrá un efecto aleatorio de 0 ----#
  udB$ud1[is.na(udB$ud1)] <- 0
  udB$ud2[is.na(udB$ud2)] <- 0
  udB$ud3[is.na(udB$ud3)] <- 0
  
  ##------------ Construcción de la matriz censal sintética XBeta ------------##
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
  
  #--- Estimaciones boostrap para cada dominio ---#
  for(i in 1:length(Divv)){
    
    index = which(censoydi$Divipola==Divv[i])
    
    #- Probabilidad de que una mujer del municipio tenga D7 por individuo -#
    
    censoydi$pluginB1[index] = exp(matrizCenso$XB1[index]+udB[i,2])/(1+exp(matrizCenso$XB1[index]+udB[i,2]))
    censoydi$pluginB2[index] = exp(matrizCenso$XB2[index]+udB[i,3])/(1+exp(matrizCenso$XB2[index]+udB[i,3]))
    censoydi$pluginB3[index] = exp(matrizCenso$XB3[index]+udB[i,4])/(1+exp(matrizCenso$XB3[index]+udB[i,4]))
    
    media1 = mean(censoydi$pluginB1[index])
    media2 = mean(censoydi$pluginB2[index])
    media3 = mean(censoydi$pluginB3[index])
    
    #- Probabilidad municipal de que una mujer  -#
    plugin.estrella2_D7[[b]][i,2] = media1/(media2+media3)
  
  }
}

#save(plugin.estrella, file = "3. Modelos Plug-In/plugin.estrella.D7")
#warnings()
#load(file = "3. Modelos Plug-In/plugin.estrella.D7")


#------------------------------------------------------------------------------#
#------------------------------ Estimación del ECM ----------------------------#
#------------------------------------------------------------------------------#

#-- Promedio de las diferencias plugin pseudocenso - pseudomuestra bootstrap --# 

d72_mse <- plugin.estrella2_D7 %>% map_df(~.x %>% data.frame() %>% 
                                         mutate(dif2 = (D7.censo - D7.muestra)^2)) %>% 
  group_by(Codmun) %>% summarise(ecm = mean(dif2))

d72_mse %>% as.data.frame()

saveRDS(d72_mse %>% as.data.frame(), file = file.path(mount_point,"3. Modelos Plugin/Output/D7/D7_Divipola.rds"))

