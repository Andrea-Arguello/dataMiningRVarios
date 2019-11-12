
library(dplyr)
library(tidyr)
library(purrr)
library(cluster)
library(caret)
library(ggplot2)
library(dendextend) #para funciones de cluster jerarquico
library(mclust)
library(fpc)

vif_for_clusters <- vif_clusters %>% select(VIC_EDAD,VIC_ESCOLARIDAD,QUIEN_REPORTA,AGR_EDAD,AGR_ESCOLARIDAD,OTRAS_VICTIMAS,VIC_OTRAS_MUJ,HEC_ANO,ANO_EMISION,NUM_MES_EMISION)
vif_for_clusters$HEC_ANO <- as.integer(as.character(vif_for_clusters$HEC_ANO))
#vif_for_clusters$HEC_MES <- as.integer(as.character(vif_for_clusters$HEC_MES))
vif_for_clusters$ANO_EMISION <- as.integer(as.character(vif_for_clusters$ANO_EMISION))
vif_for_clusters$VIC_OTRAS_MUJ <- as.integer(as.character(vif_for_clusters$VIC_OTRAS_MUJ))

vif_for_clusters$VIC_EDAD[vif_for_clusters$VIC_EDAD == 99] <- NA
vif_for_clusters$AGR_EDAD[vif_for_clusters$AGR_EDAD == 99] <- NA

vif_for_clusters$VIC_ESCOLARIDAD[vif_for_clusters$VIC_ESCOLARIDAD == 24] <- NA
vif_for_clusters$AGR_ESCOLARIDAD[vif_for_clusters$AGR_ESCOLARIDAD == 24] <- NA

vif_for_clusters_nona <- na.omit(vif_for_clusters)
vif_for_clusters_nona_2008 <- vif_for_clusters_nona %>% filter(ANO_EMISION==2008)
vif_for_clusters_nona_2009 <- vif_for_clusters_nona %>% filter(ANO_EMISION==2009)
vif_for_clusters_nona_2010 <- vif_for_clusters_nona %>% filter(ANO_EMISION==2010)


seventy <- createDataPartition(y=vif_for_clusters_nona_2008$ANO_EMISION, p=0.7,list=FALSE)
vif_for_clusters_nona_2008_70 <- vif_for_clusters_nona_2008[seventy,]

#vif_for_clusters$OTRAS_VICTIMAS[is.na(vif_for_clusters$OTRAS_VICTIMAS)] <- -1
#vif_for_clusters$VIC_OTRAS_MUJ[is.na(vif_for_clusters$VIC_OTRAS_MUJ)] <- -1
##vif_for_clusters$HEC_ANO[is.na(vif_for_clusters$HEC_ANO)] <- -1
##vif_for_clusters$HEC_MES[is.na(vif_for_clusters$HEC_MES)] <- -1
#vif_for_clusters$VIC_ESCOLARIDAD[is.na(vif_for_clusters$VIC_ESCOLARIDAD)] <- -1
#vif_for_clusters$AGR_ESCOLARIDAD[is.na(vif_for_clusters$AGR_ESCOLARIDAD)] <- -1


tot_withinss <- map_dbl(2:10,  function(k){
  model <- kmeans(x = vif_for_clusters_nona, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 2:10 ,
  tot_withinss = tot_withinss
)

# Grafica de codo
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

# Varios modelos con distinta k
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = vif_for_clusters_nona_2008_70, k = k)
  model$silinfo$avg.width
})

# Dataframe k y silueta
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

# Silueta vs k
ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)



model <- kmeans(vif_for_clusters_nona, centers = 3)
cluster_k <- model$cluster

segment_vif_k <- cbind(vif_for_clusters_nona,cluster_k)
count(segment_vif_k, cluster_k)

clusplot(segment_vif_k,segment_vif_k$cluster_k,shape=TRUE,color=TRUE,labels=5)

ggplot(segment_vif_k,aes(x=cluster_k,y=VIC_EDAD,group=cluster_k)) + geom_boxplot()
ggplot(segment_vif_k,aes(x=cluster_k,y=AGR_EDAD,group=cluster_k)) + geom_boxplot()

ggplot(segment_vif_k,aes(x=cluster_k,y=HEC_ANO,group=cluster_k)) + geom_boxplot()
ggplot(segment_vif_k,aes(x=QUIEN_REPORTA)) + geom_histogram(stat="count") + facet_grid(~cluster_k) + ggtitle("Frecuencia de quién reporta, separado por clusters")

table(segment_vif_k$cluster_k,segment_vif_k$ANO_EMISION)
table(segment_vif_k$cluster_k,segment_vif_k$HEC_ANO)

ggplot(segment_vif_k,aes(x=cluster_k,y=VIC_ESCOLARIDAD,group=cluster_k)) + geom_boxplot() #no indican mucho
ggplot(segment_vif_k,aes(x=cluster_k,y=AGR_ESCOLARIDAD,group=cluster_k)) + geom_boxplot() #Tampoco indican mucho

ggplot(segment_vif_k,aes(x=cluster_k,y=OTRAS_VICTIMAS,group=cluster_k)) + geom_boxplot() #no indican mucho
ggplot(segment_vif_k,aes(x=cluster_k,y=VIC_OTRAS_MUJ,group=cluster_k)) + geom_boxplot() #Tampoco indican mucho

segment_vif_k %>% group_by(cluster_k) %>% summarize(SumaVictimas = sum(OTRAS_VICTIMAS), PromedioVictimas = SumaVictimas/n(), SumaVicMujeres = sum(VIC_OTRAS_MUJ), PromedioVicMujeres = SumaVicMujeres/n())
segment_vif_k %>% group_by(cluster_k) %>% summarize(promedioEscolaridadVic = mean(VIC_ESCOLARIDAD),promedioEscolaridadAgr = mean(AGR_ESCOLARIDAD), medianaEscolaridadVic = median(VIC_ESCOLARIDAD), medianaEscolaridadAgr = median(AGR_ESCOLARIDAD))

#write.csv(segment_vif_k,"infoclusters2.csv")
