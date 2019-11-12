#install.packages("foreign")
library(foreign)

vif2008 <- read.spss("/home/ranchobojon/Documents/Data Mining/Proyecto/vif2008.sav",to.data.frame=T,stringAsFactors=F)
vif2009 <- read.spss("/home/ranchobojon/Documents/Data Mining/Proyecto/vif2009.sav",to.data.frame=T,stringAsFactors=F)
vif2010 <- read.spss("/home/ranchobojon/Documents/Data Mining/Proyecto/vif2010.sav",to.data.frame=T,stringAsFactors=F)

vif2008_values <- read.spss("/home/ranchobojon/Documents/Data Mining/Proyecto/vif2008.sav",use.value.labels=F,to.data.frame=T,stringAsFactors=F)
vif2009_values <- read.spss("/home/ranchobojon/Documents/Data Mining/Proyecto/vif2009.sav",use.value.labels=F,to.data.frame=T,stringAsFactors=F)
vif2010_values <- read.spss("/home/ranchobojon/Documents/Data Mining/Proyecto/vif2010.sav",use.value.labels=F,to.data.frame=T,stringAsFactors=F)

#Revisar que MEDIDAS_SEGURIDAD si sea la ultima columna para que esto funcione

library(dplyr)

vif2008 <- vif2008 %>% rename(AGR_GRUPET = "AGR_GURPET")
vif2009 <- vif2009 %>% rename(AGR_GRUPET = "AGR_GURPET")


vif2008$HEC_DEPTOMCPIO <- vif2008_values$HEC_DEPTOMCPIO
vif2009$HEC_DEPTOMCPIO <- vif2009_values$HEC_DEPTOMCPIO
vif2010$HEC_DEPTOMCPIO <- vif2010_values$HEC_DEPTOMCPIO

vif2008$NUM_MES_EMISION <- vif2008_values$MES_EMISION
vif2009$NUM_MES_EMISION <- vif2009_values$MES_EMISION
vif2010$NUM_MES_EMISION <- vif2010_values$MES_EMISION

vif <- rbind(vif2008,vif2009[,names(vif2009) != "MEDIDAS_SEGURIDAD"],vif2010[,names(vif2010) != "MEDIDAS_SEGURIDAD"])

# Etnia, nivel de ed, anio, mes, tipo de agresion, alfabetismo, vic_trabaja, agr_trabaja, sexo, edad, relacion victima agresor, dpto, hec_area, otras_vic, otras vic mujeres, 
vif_filtrado <- vif %>% select(ANO_EMISION, MES_EMISION, NUM_MES_EMISION, HEC_ANO, HEC_MES, HEC_AREA, HEC_DEPTOMCPIO, HEC_TIPAGRE, VIC_SEXO, AGR_SEXO, VIC_REL_AGR, VIC_TRABAJA, VIC_EDAD, VIC_ALFAB, VIC_ESCOLARIDAD, VIC_DISC, TIPO_DISCAQ, VIC_GRUPET, AGR_EDAD, AGR_ALFAB, AGR_ESCOLARIDAD, AGR_TRABAJA, AGR_GRUPET, QUIEN_REPORTA, INST_DONDE_DENUNCIO, HEC_RECUR_DENUN, OTRAS_VICTIMAS, VIC_OTRAS_MUJ) %>% filter(VIC_SEXO == "Mujeres")
vif_filtrado_chombres <- vif %>% select(ANO_EMISION, MES_EMISION, NUM_MES_EMISION, HEC_ANO, HEC_MES, HEC_AREA, HEC_DEPTOMCPIO, HEC_TIPAGRE, VIC_SEXO, AGR_SEXO, VIC_REL_AGR, VIC_TRABAJA, VIC_EDAD, VIC_ALFAB, VIC_ESCOLARIDAD, VIC_DISC, TIPO_DISCAQ, AGR_EDAD, AGR_ALFAB, AGR_ESCOLARIDAD, AGR_TRABAJA, QUIEN_REPORTA, INST_DONDE_DENUNCIO, HEC_RECUR_DENUN, OTRAS_VICTIMAS, VIC_OTRAS_MUJ)

#101-117 Guatemala
#201-208 El Progreso
#301-316 Sacatepequez
#401-416 Chimaltenango
#501-513 Escuintla
#601-614 Santa Rosa
#701-719 Solola
#801-808 Totonicapan
#901-924 Quetzaltenango
#1001-1020 Suchitepequez
#1101-1109 Retalhuleu
#1201-1229 San Marcos
#1301-1332 Huehuetenango
#1401-1421 Quiche
#1501-1508 Baja Verapaz
#1601-1617 Alta Verapaz
#1701-1712 Peten
#1801-1805 Izabal
#1901-1910 Zacapa
#2001-2011 Chiquimula
#2101-2107 Jalapa
#2201-2217 Jutiapa
departamentos <- c("Guatemala", "El Progreso", "Sacatepéquez", "Chimaltenango", "Escuintla", "Santa Rosa", "Sololá", "Totonicapán", "Quetzaltenango", "Suchitepéquez", "Retalhuleu", "San Marcos", "Huehuetenango", "Quiché", "Baja Verapaz", "Alta Verapaz", "Petén", "Izabal", "Zacapa", "Chiquimula", "Jalapa", "Jutiapa")

vif_final <- vif_filtrado

vif_final$HEC_DEPTOMCPIO <- cut(vif_filtrado$HEC_DEPTOMCPIO, breaks=c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300),labels=departamentos)
vif_final$VIC_REL_AGR[vif_final$VIC_REL_AGR == "Esposos(a)"] <- "Esposos(as)"
droplevels(vif_final$VIC_REL_AGR)

levels(vif_final$VIC_ESCOLARIDAD)[levels(vif_final$VIC_ESCOLARIDAD) == "Divesificado grado ignorado"] <- "Diversificado grado ignorado"
levels(vif_final$AGR_ESCOLARIDAD)[levels(vif_final$AGR_ESCOLARIDAD) == "Divesificado grado ignorado"] <- "Diversificado grado ignorado"
levels(vif_final$VIC_OTRAS_MUJ)[levels(vif_final$VIC_OTRAS_MUJ)=="Ninguna"] <- 0
levels(vif_final$VIC_OTRAS_MUJ) = as.integer(levels(vif_final$VIC_OTRAS_MUJ))

library(ggplot2)
'%not in%' <- Negate('%in%')
ggplot(vif_final[which(vif_final$HEC_ANO %not in% c("200","Ignorado")),], aes(x=HEC_MES, color=HEC_ANO)) + geom_point(stat="count") + theme(axis.text.x = element_text(angle = 90)) + facet_grid(~ANO_EMISION)

write.csv(vif_final,"violencia_a_mujeres_con_etnias.csv",row.names=F)

vif_final_chombres <- vif_filtrado_chombres
vif_final_chombres$HEC_DEPTOMCPIO <- cut(vif_filtrado$HEC_DEPTOMCPIO, breaks=c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300),labels=departamentos)
vif_final_chombres$VIC_REL_AGR[vif_final$VIC_REL_AGR == "Esposos(a)"] <- "Esposos(as)"
droplevels(vif_final_chombres$VIC_REL_AGR)

levels(vif_final_chombres$VIC_ESCOLARIDAD)[levels(vif_final_chombres$VIC_ESCOLARIDAD) == "Divesificado grado ignorado"] <- "Diversificado grado ignorado"
levels(vif_final_chombres$AGR_ESCOLARIDAD)[levels(vif_final_chombres$AGR_ESCOLARIDAD) == "Divesificado grado ignorado"] <- "Diversificado grado ignorado"

vif_clusters <- vif_final
vif_clusters$QUIEN_REPORTA <- as.numeric(vif_clusters$QUIEN_REPORTA)
vif_clusters$VIC_ESCOLARIDAD <- as.numeric(vif_clusters$VIC_ESCOLARIDAD)#-1 #correccion porque empieza en 1
vif_clusters$AGR_ESCOLARIDAD <- as.numeric(vif_clusters$AGR_ESCOLARIDAD)#-1 
vif_clusters$VIC_EDAD <- as.numeric(vif_clusters$VIC_EDAD)
vif_clusters$AGR_EDAD <- as.numeric(vif_clusters$AGR_EDAD)
levels(vif_clusters$OTRAS_VICTIMAS)[levels(vif_clusters$OTRAS_VICTIMAS)=="Ninguna"] <- 0
vif_clusters$OTRAS_VICTIMAS <- as.integer((vif_clusters$OTRAS_VICTIMAS))
vif_clusters$OTRAS_VICTIMAS[vif_clusters$OTRAS_VICTIMAS==22] <- NA
