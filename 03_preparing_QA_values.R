
# Leer tabla QA2 
tabla <- list.files("~/PROJECTS/16b_trapananda_TS/table", pattern = glob2rx("*QA2.csv"), full.names = T)
name <- list.files("~/PROJECTS/16b_trapananda_TS/table", pattern = glob2rx("*QA2.csv"), full.names = F)

qa <- read.csv(tabla, stringsAsFactors=FALSE)
qa$QA_word2[is.na(qa$QA_word2)] <- "no_data"

outname <- paste("~/PROJECTS/16b_trapananda_TS/table", "/", "bin_", name, sep = "" )


############################################# no cambiar ##########################################################

# Crear vectores
qa_1 <- c(1:length(qa$value))
qa_2 <- c(1:length(qa$value))
qa_3 <- c(1:length(qa$value))
qa_4 <- c(1:length(qa$value))
qa_5 <- c(1:length(qa$value))
qa_6 <- c(1:length(qa$value))
qa_7 <- c(1:length(qa$value))
qa_8 <- c(1:length(qa$value))



# Primer ciclo para "MODLAND_QA" QA
for (i in 1:length(qa$value)){
  if (qa$QA_word1[i]=="VI_produced_good_quality"){
    qa_1[i] <- 1
  }  else if (qa$QA_word1[i]== "VI_produced_but_check_other_QA"){
    qa_1[i] <- 2
  }  else if (qa$QA_word1[i]== "Pixel_not_produced_other_reasons_than_clouds" | qa$QA_word1[i]== "Pixel_produced_but_most_probably_cloud"){
    qa_1[i] <- 0
  }
}
  
# Segundo ciclo para "VI usefulness", "Aerosol quantity", "Adjacent cloud detected", "Land/water mask" & "Mixed cloud" QAs
for (i in 1:length(qa$value)){
  if (qa_1[i]==2 & qa$QA_word2[i]=="Highest_quality"){
    qa_2[i] <- 1
  }  else if (qa_1[i]==2 & qa$QA_word2[i]=="no_data"){
    qa_2[i] <- 1
  }  else if (qa_1[i]==2 & qa$QA_word2[i]=="Lower_quality"){
    qa_2[i] <- 1
  }  else if (qa_1[i]==2 & qa$QA_word2[i]=="Decreasing_quality"){
    qa_2[i] <- 1
  } else {
    qa_2[i] <- 0
  }
  if (qa$QA_word3[i]=="Aer_high"){
    qa_3[i] <- 0
  }  else {
    qa_3[i] <- 1
  }
  if (qa$QA_word4[i]=="Adj_cloud_yes"){
    qa_4[i] <- 0
  }  else {
    qa_4[i] <- 1
  }
  if (qa$QA_word6[i]=="Mix_cloud_yes"){
    qa_5[i] <- 0
  }  else {
    qa_5[i] <- 1
  }
  if (qa$QA_word7[i]=="LWF_land"){
    qa_6[i] <- 1
  }  else {
    qa_6[i] <- 0
  }
  if (qa$QA_word8[i]=="Snow_ice_yes"){
    qa_7[i] <- 0
  }  else {
    qa_7[i] <- 1
  }
  if (qa$QA_word9[i]=="Shadow_yes"){
    qa_8[i] <- 0
  }  else {
    qa_8[i] <- 1
  }
}

# Crear vectores finales 
qa_pre_final <-as.data.frame(cbind(qa_1, qa_2, qa_3, qa_4, qa_5, qa_6, qa_7, qa_8))

qa_final1 <- c(1:length(qa$value))
qa_final2 <- c(1:length(qa$value))
qa_final <- c(1:length(qa$value))

# Ciclo para discriminar los valores de la utilidad de los píxeles
for (i in 1:length(qa$value)){
  if (qa_pre_final$qa_1[i]==1){
    qa_final1[i] <- 1
  } else {
    qa_final1[i] <- 0
  }
  if (qa_pre_final$qa_2[i]+qa_pre_final$qa_3[i]+qa_pre_final$qa_4[i]+qa_pre_final$qa_5[i]+qa_pre_final$qa_6[i]+qa_pre_final$qa_7[i]+qa_pre_final$qa_8[i]==7){
    qa_final2[i] <- 1
  } else {
    qa_final2[i] <- 0
  }
}

# Ciclo para crear columna final con los píxeles a dejar y quitar
for (i in 1:length(qa$value)){
  if (qa_final1[i]+qa_final2[i]==0){
    qa_final[i] <- 0
  } else {
    qa_final[i] <- 1
  }
}

############################################ cambiar sólo dirección de salida de tabla ###########################################################

# Manejo de tabla & suma de píxeles finales 
tabla <- cbind(qa, qa_final)
write.csv(tabla, outname)

lngth <- length(tabla$count)
pix_inout <- aggregate(tabla$count, by = list(Category = tabla$qa_final), FUN = sum) # 0 = out, 1 = in
pix_inout[1,2] <- pix_inout$x[1]-tabla$count[lngth]
pix_inout
pixout_porcentaje <- (pix_inout$x[1]/(pix_inout$x[1]+pix_inout$x[2]))*100 # porcentaje de píxeles que se van 
pixout_porcentaje

# Filtrar valores de QA para eliminar
outvalues <- subset(tabla$value, tabla$qa_final==0)

tab <- paste("r2[QA2==", outvalues, "]", " <- ", "NA", sep = "")
tab[1]

# copiar resultados de acá para poder pegar en el script "data_prep_anomaly_EVI"
for (i in 1:length(tab)){
  cat(tab[i], "\n")
}







