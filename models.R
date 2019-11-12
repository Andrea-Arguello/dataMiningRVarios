vif_data <- read.csv("/home/ranchobojon/violencia_a_mujeres_CORREGIDO.csv")
str(vif_data)


#train y test
#train <- sample(nrow(vif_data), 0.7*nrow(vif_data), replace = FALSE)
#vif_train <- vif_data[train,]
#vif_test <- vif_data[-train,]

#write.csv(vif_train, "vif_train.csv", row.names = F)
#write.csv(vif_test, "vif_test.csv", row.names = F)
vif_train <- read.csv("vif_train.csv")
vif_test <- read.csv("vif_test.csv")

#sexo, relacion, sin y con institucion, trabaja vic y agr, niv_educ ambos, departamento, area, quien reporta -> tip_agre
library(e1071)
modeloNB <- naiveBayes(HEC_TIPAGRE~AGR_SEXO+VIC_REL_AGR+HEC_DEPTOMCPIO+HEC_AREA,data=vif_train)
prediccion<-predict(modeloNB, newdata=vif_test, type="raw")
predictionNB<-as.data.frame(prediccion)
predictionNB$res <- colnames(predictionNB)[apply(predictionNB,1,which.max)]
resultsNB <- table(vif_test$HEC_TIPAGRE,predictionNB$res)
resultsNB
accuracyNB<-sum(diag(resultsNB))/sum(resultsNB)
accuracyNB

modeloNB2 <- naiveBayes(HEC_TIPAGRE~AGR_SEXO+VIC_REL_AGR+HEC_DEPTOMCPIO+HEC_AREA+QUIEN_REPORTA+INST_DONDE_DENUNCIO+VIC_TRABAJA+AGR_TRABAJA,data=vif_train)
prediccion2<-predict(modeloNB2, newdata=vif_test, type="raw")

predictionNB2<-as.data.frame(prediccion2[-which(is.nan(prediccion2)),])
predictionNB2$res <- colnames(predictionNB2)[apply(predictionNB2,1,which.max)]
resultsNB2 <- table(vif_test[-which(is.nan(prediccion2)),]$HEC_TIPAGRE,predictionNB2$res)
resultsNB2
accuracyNB2<-sum(diag(resultsNB2))/sum(resultsNB2)
accuracyNB2

modeloNB3 <- naiveBayes(HEC_TIPAGRE~AGR_SEXO+VIC_REL_AGR+HEC_DEPTOMCPIO+HEC_AREA+VIC_TRABAJA+AGR_TRABAJA,data=vif_train)
prediccion3<-predict(modeloNB3, newdata=vif_test, type="raw")
predictionNB3<-as.data.frame(prediccion3)
predictionNB3$res <- colnames(predictionNB3)[apply(predictionNB3,1,which.max)]
resultsNB3 <- table(vif_test$HEC_TIPAGRE,predictionNB3$res)
resultsNB3
accuracyNB3<-sum(diag(resultsNB3))/sum(resultsNB3)
accuracyNB3

modeloNB4 <- naiveBayes(HEC_TIPAGRE~AGR_SEXO+VIC_REL_AGR+HEC_DEPTOMCPIO+HEC_AREA+VIC_ESCOLARIDAD+AGR_ESCOLARIDAD,data=vif_train)
prediccion4<-predict(modeloNB4, newdata=vif_test, type="raw")
predictionNB4<-as.data.frame(prediccion4)
predictionNB4$res <- colnames(predictionNB4)[apply(predictionNB4,1,which.max)]
resultsNB4 <- table(vif_test$HEC_TIPAGRE,predictionNB4$res)
resultsNB4
accuracyNB4<-sum(diag(resultsNB4))/sum(resultsNB4)
accuracyNB4

# Regresion lineal: Mes y cantidad de denuncias anual
vif_rl <- vif_data
vif_rl$NUM_MES_EMISION <- ifelse(vif_data$NUM_MES_EMISION < 10, paste(0,vif_data$NUM_MES_EMISION,sep=""),vif_data$NUM_MES_EMISION)
vif_rl$ANO_MES <- paste(vif_rl$ANO_EMISION,vif_rl$NUM_MES_EMISION,sep="")

library(dplyr)
vif_sum_ano_mes <- vif_rl %>% group_by(as.numeric(ANO_MES)) %>% summarise(cuenta = count(ANO_MES))
vif_sum_ano_mes <- vif_sum_ano_mes$cuenta
vif_sum_ano_mes$x_val <- as.integer(vif_sum_ano_mes$x)
str(vif_sum_ano_mes)

denunciasRL <- lm(freq~x_val, data=vif_sum_ano_mes)
denunciasRL

library(ggplot2)
ggplot(vif_sum_ano_mes, aes(x_val, freq)) + geom_point() + stat_smooth(method = lm) + scale_x_continuous(name="Meses (1 = enero 2008)")


# Arboles de decision
library(rpart)
library(rpart.plot)
# Reduciendolo a las dos mas comunes, fisica y psicologica
vif_train_2agres <- vif_train[which(vif_train$HEC_TIPAGRE == "Psicológica" | vif_train$HEC_TIPAGRE == "Física-psicológica"), ]
vif_test_2agres <- vif_test[which(vif_test$HEC_TIPAGRE == "Psicológica" | vif_test$HEC_TIPAGRE == "Física-psicológica"), ]
droplevels(vif_train_2agres$HEC_TIPAGRE)
droplevels(vif_test_2agres$HEC_TIPAGRE)

modeloDT<-rpart(HEC_TIPAGRE~., method="class", data=vif_train_2agres[,c("HEC_TIPAGRE","VIC_REL_AGR","VIC_GRUPET","AGR_GRUPET","AGR_SEXO","HEC_DEPTOMCPIO","OTRAS_VICTIMAS")], model=T)
rpart.plot(modeloDT,box.palette="Grays")
predDT<-predict(modeloDT, newdata=vif_test_2agres, type="class")
resultsDT <- table(vif_test_2agres$HEC_TIPAGRE,predDT)
resultsDT
accuracyDT<-sum(diag(resultsDT))/sum(resultsDT)
accuracyDT

'%not in%' <- Negate('%in%')
vif_data2 <- vif_data[which(vif_data$HEC_ANO %not in% c("200","Ignorado")),]
vif_data2$HEC_ANO <- as.integer(paste(vif_data2$HEC_ANO))
ggplot(vif_data2, aes(x=as.factor(ANO_EMISION), y=HEC_ANO)) + geom_boxplot()

ggplot(vif_data2, aes(y=HEC_ANO)) + geom_boxplot()
