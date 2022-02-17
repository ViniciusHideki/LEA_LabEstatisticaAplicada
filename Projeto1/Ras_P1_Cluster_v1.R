### graficos
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)
if(!require(ggrepel)){install.packages(ggrepel)}
library(ggrepel)
if(!require(cowplot)){install.packages("cowplot")}
library(cowplot)
if(!require(RColorBrewer)){install.packages("RColorBrewer")}
library(RColorBrewer)
if(!require(corrplot)){install.packages("corrplot")}
library(corrplot)


### manipulacao de dados
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)


### Agrupamento
if(!require(factoextra)){install.packages("factorextra")}
library(factoextra)
if(!require(FactoMineR)){install.packages("FactoMineR")}
library(FactoMineR)
if(!require(NbClust)){install.packages("NbClust")}
library(NbClust)

### Dataframe to latex
if(!require(xtable)){install.packages("xtable")}
library(xtable)


### Para a silhueta
if(!require(cluster)){install.packages("cluster")}
library(cluster)

### Para o  Calinski-Harabasz index
if(!require(fpc)){install.packages("fpc")}
library(fpc)

### Para o davies
if(!require(clusterSim)){install.packages("clusterSim")}
library(clusterSim)

### Para o Dunn
if(!require(clValid)){install.packages("clValid")}
library(clValid)



library(readxl)
D3 <- read_excel("D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/D3.xlsx")

#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(D3,2,pMiss)

dados = matrix(unlist(lapply(D3[,2:16],as.numeric)),
               ncol = ncol(D3[,2:16]), 
               nrow = nrow(D3))

dados = data.frame(dados)

names(dados) = c("Alcool", "Adstringente","Pimenta",
                 "Amargo", "Doce","Vegetal",
                 "Frutado","Terroso","Eucalipto",
                 "Floral","Hortela","Carvalho",
                 "FrutasVermelhas","Defumado",
                 "Acidez")


dados_p = scale(dados)

#=============================================
# Matriz de distancias
#=============================================

### Agrupamento 
### Matriz de Distância

### Padronizado os dados
dista <- dist(dados_p)
dista



#=============================================
#  Fazendo hierárquico
#=============================================

#=============================================
#  Metodo de ward
#=============================================

par(mfrow = c(1,1))

dista.ward = hclust(d = dista,
                    method = "ward.D")

dista.ward

dend_ward = 
  fviz_dend(dista.ward, cex=0.8, horiz = T) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  
  labs(title = "Método de Ward",
       y = "Pesos")

dend_ward


ggsave(filename = "dend_ward_1.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


dend_ward2 = 
  fviz_dend(dista.ward, cex=0.8, horiz = T,
            rect = T,k=3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Método de Ward",y = "Pesos")

dend_ward2

ggsave(filename = "dend_ward_2.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")






dend_ward3 = 
  fviz_dend(dista.ward, cex=0.8, horiz = T,
            rect = T,k=2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Método de Ward",y = "Pesos")

dend_ward3

ggsave(filename = "dend_ward_3.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")








#=============================================
#  Metodo do Centroide
#=============================================

par(mfrow = c(1,1))

dista.centroid = hclust(d = dista,
                        method = "centroid")

dend_centroid = 
  fviz_dend(dista.centroid, cex=1, horiz = T) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Método do Centroide",
       y = "Pesos") 

dend_centroid



#=============================================
#  Metodo do vizinho mais proximo
#=============================================

par(mfrow = c(1,1))

dista.single = hclust(d = dista,
                      method = "single")

dista.single

dend_single = 
  fviz_dend(dista.single, cex=1, horiz = T) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Método do Vizinho mais próximo",
       y = "Pesos") 

dend_single


#=============================================
#  Metodo do vizinho mais longe
#=============================================

par(mfrow = c(1,1))

dista.complete = hclust(d = dista,
                        method = "complete")

dend_complete = 
  fviz_dend(dista.complete, cex=1, horiz = T) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Método do Vizinho mais longe",
       y = "Pesos") 

dend_complete



#=============================================
#  Metodo do vizinho medio
#=============================================

par(mfrow = c(1,1))

dista.average = hclust(d = dista,
                       method = "average")

dend_average = 
  fviz_dend(dista.average, cex=1, horiz = T) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Método do Vizinho médio",
       y = "Pesos") 

dend_average


#=============================================
#  Metodo do vizinho mediano
#=============================================

par(mfrow = c(1,1))

dista.median = hclust(d = dista,
                      method = "median")

dend_median = 
  fviz_dend(dista.median, cex=1, horiz = T) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Método do Vizinho mediano",
       y = "Pesos") 

dend_median


#=============================================
#  savar os necessarios
#=============================================



ggarrange(dend_centroid, 
          dend_single,
          dend_complete,
          labels = LETTERS[1:5],
          ncol = 3)


ggsave(filename = "dend_ruins1.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")


ggarrange(dend_median,
          dend_average,
          labels = LETTERS[1:5],
          ncol = 2)


ggsave(filename = "dend_ruins2.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 20.75, 
       height = 15.5, 
       units = "cm")


#=============================================
#  Numero ideal de grupos
#=============================================


NbClust(data = dados, distance = "euclidean", min.nc = 2, max.nc = 5,
        index = "all", method = "ward.D")

aa = NbClust(data = dados, distance = "euclidean", min.nc = 2, max.nc = 5,
             index = "all", method = "ward.D")

aa$Best.nc[,aa$Best.nc[1,]==2]

aa$Best.nc[,aa$Best.nc[1,]==3]

aa$Best.nc[,aa$Best.nc[1,]==4]

aa$Best.nc[,aa$Best.nc[1,]==5]

NbClust(data = dados, distance = "euclidean", min.nc = 2, max.nc = 5,
        index = "all", method = "centroid")

NbClust(data = dados, distance = "euclidean", min.nc = 2, max.nc = 5,
        index = "all", method = "single")

NbClust(data = dados, distance = "euclidean", min.nc = 2, max.nc = 5,
        index = "all", method = "complete")

NbClust(data = dados, distance = "euclidean", min.nc = 2, max.nc = 5,
        index = "all", method = "average")

NbClust(data = dados, distance = "euclidean", min.nc = 2, max.nc = 5,
        index = "all", method = "median")




#=============================================
# Kmeans - 3 grupos
#=============================================

cut_Ward <- cutree(dista.ward, k = 3)

## Calculando medias dos grupos
clust.Ward <- function(i, dat, cut_Ward){
  ind <- (cut_Ward == i)
  colMeans(dat[ind,])
}  

m_Ward <- t(as.matrix(sapply(unique(cut_Ward), clust.Ward, 
                             dados_p, cut_Ward)))

### chutes do ward para o kmeans
round(m_Ward, 3)
xtable(m_Ward[,1:8])
xtable(m_Ward[,9:15])

### Ajustando o Kmeans
kmeans_Ward <- kmeans(dados_p, m_Ward, nstart = 1,
                      trace = T)
kmeans_Ward


### Validando o K-means
### Incluir a coluna do RÃ³tulo
data_PosKm <- cbind(dados, Grupos = kmeans_Ward$cluster) %>%
  tibble()
data_PosKm


#=============================================
# Boxplot - 3 grupos
#=============================================
data_PosKm$Grupos <- as.character(data_PosKm$Grupos)

data_PosKm_box = melt(data_PosKm)
data_PosKm_box$variable = 
  factor(data_PosKm_box$variable,
         labels =names(dados))

head(data_PosKm_box)

box_plot_grupo1_2_3 =  
  ggplot(data = data_PosKm_box, aes(x=variable, y=value, fill=Grupos)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        axis.text.x.bottom = element_blank(),
        legend.position = "none")+
  xlab("") + ylab("Valor") + ggtitle("Variáveis")


box_plot_grupo1_2_3

  


names(dados)

filter(data_PosKm_box,
       variable==names(dados)[c(1,2,4,5,15)])

subset(data_PosKm_box,
       variable == names(dados)[c(1,2,4,5,15)]  )

subset(data_PosKm_box,
       variable %in% names(dados)[c(1,2,4,5,15)]  )

### Textura
ggplot(data = 
         subset(data_PosKm_box,
                variable %in% names(dados)[c(1,2,4,5,15)]), 
       aes(x=variable, y=value, fill=Grupos)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        axis.text.x.bottom = element_blank(),
        legend.position = c(0.85,0.27))+
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits=c(min(as.matrix(dados))+0.2,
                              max(as.matrix(dados))-0.2))+
  
  xlab("") + ylab("") + ggtitle("")


ggsave(filename = "PosKM_Boxplot_textura.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


### Aroma
ggplot(data = 
         subset(data_PosKm_box,
                variable %in% names(dados)[c(3,6,7,8,9)]), 
       aes(x=variable, y=value, fill=Grupos)) + 
  geom_boxplot() +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits=c(1,3.2))+
  
  facet_wrap(~variable, scale="free") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        axis.text.x.bottom = element_blank(),
        legend.position = c(0.85,0.27))+
  xlab("") + ylab("Valor") + ggtitle("")

ggsave(filename = "PosKM_Boxplot1_aroma.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")





ggplot(data = 
         subset(data_PosKm_box,
                variable %in% names(dados)[10:14]), 
       
       aes(x=variable, y=value, fill=Grupos)) + 
  geom_boxplot() +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits=c(1,4))+
  
  facet_wrap(~variable, scale="free") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        axis.text.x.bottom = element_blank(),
        legend.position = c(0.85,0.27))+
  xlab("") + ylab("Valor") + ggtitle("")

ggsave(filename = "PosKM_Boxplot2_aroma.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")





#=============================================
# ACP e pontos
#=============================================

graf_biplot_kmeans = 
  
  fviz_cluster(kmeans_Ward,
               data = dados_p,
               geom = "point",
               pointsize = 2,
               main = "Grupos criado pelo Kmeans na ACP",
               repel = T) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        legend.position = "right",
        axis.ticks.x=element_blank())+
  
  geom_text_repel(aes(label = row.names(dados) ),size = 3)  +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits=c(-4,4))+
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits=c(-4,6))+
  
  geom_hline(yintercept = 0,linetype = "longdash",
             color = 'black',size=0.7,alpha =0.4)+
  
  geom_vline(xintercept = 0,linetype = "longdash",
             color = 'black',size=0.7,alpha =0.4)


graf_biplot_kmeans




graf_biplot=
fviz_pca_biplot(PCA(dados_p),
                label ="var",
                col.var = 'black',
                col.ind = "blue",
                alpha.ind=0.2) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        strip.background = element_rect(colour="black",
                                        fill="white"),
        legend.position = "right",
        axis.ticks.x=element_blank())+
  
  labs(title="Biplot da ACP")+
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits=c(-4,4))+
  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits=c(-4,6))+
  
  geom_hline(yintercept = 0,linetype = "longdash",
             color = 'red',size=0.1,alpha =0.2)+
  
  geom_vline(xintercept = 0,linetype = "longdash",
             color = 'red',size=0.1,alpha =0.2)

graf_biplot










#=============================================
# ACP 
#=============================================


Analise_CP = PCA(dados_p)

graf_acp = 
fviz_pca_var(Analise_CP,
             title="ACP das variáveis",
             repel=T,
             col.var='cos2',
             gradient.cols=brewer.pal(n=9,"PuBu")[-c(1:4)]) + 
  theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, family ="serif"))

graf_acp




ggsave(filename = "ACP_Geral.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 20.75, 
       height = 15.5,
       units = "cm")



#=============================================
# Juntando os dois - 3 grupos e ACP
#=============================================

ggarrange(graf_biplot,
          graf_biplot_kmeans,
          labels = c("A","B"))

ggsave(filename = "ACP_Kmeans.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 30.75, 
       height = 15.5, 
       units = "cm")













#=============================================
# Kmeans - 2 grupos
#=============================================

cut_Ward2 <- cutree(dista.ward, k = 2)

## Calculando medias dos grupos
clust.Ward <- function(i, dat, cut_Ward){
  ind <- (cut_Ward == i)
  colMeans(dat[ind,])
}  

m_Ward2 <- t(as.matrix(sapply(unique(cut_Ward2), clust.Ward, 
                             dados_p, cut_Ward2)))
round(m_Ward2, 3)



xtable(m_Ward2[,1:8])

xtable(m_Ward2[,9:15])

###
kmeans_Ward2 <- kmeans(dados_p, m_Ward2, nstart = 1,
                      trace = T)
kmeans_Ward2


### Validando o K-means
### Incluir a coluna do RÃ³tulo
data_PosKm2 <- cbind(dados, Grupos = kmeans_Ward2$cluster) %>%
  tibble()
data_PosKm2


#=============================================
# Boxplot - 2 grupos
#=============================================
data_PosKm2$Grupos <- as.character(data_PosKm2$Grupos)

data_PosKm_box2 = melt(data_PosKm2)
data_PosKm_box2$variable = 
  factor(data_PosKm_box2$variable,
         labels =names(dados))

head(data_PosKm_box2)


### Textura
ggplot(data = 
         subset(data_PosKm_box2,
                variable %in% names(dados)[c(1,2,4,5,15)]), 
       aes(x=variable, y=value, fill=Grupos)) + 
  geom_boxplot() +
  facet_wrap(~variable, scale="free") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        axis.text.x.bottom = element_blank(),
        legend.position = c(0.85,0.27))+
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits=c(min(as.matrix(dados))+0.2,
                              max(as.matrix(dados))-0.2))+
  
  xlab("") + ylab("") + ggtitle("")


ggsave(filename = "PosKM2_Boxplot_textura.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


### Aroma
ggplot(data = 
         subset(data_PosKm_box2,
                variable %in% names(dados)[c(3,6,7,8,9)]), 
       aes(x=variable, y=value, fill=Grupos)) + 
  geom_boxplot() +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits=c(1,3.2))+
  
  facet_wrap(~variable, scale="free") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        axis.text.x.bottom = element_blank(),
        legend.position = c(0.85,0.27))+
  xlab("") + ylab("Valor") + ggtitle("")

ggsave(filename = "PosKM2_Boxplot1_aroma.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")





ggplot(data = 
         subset(data_PosKm_box2,
                variable %in% names(dados)[10:14]), 
       
       aes(x=variable, y=value, fill=Grupos)) + 
  geom_boxplot() +
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits=c(1,4))+
  
  facet_wrap(~variable, scale="free") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 14,
                            family ="serif"),
        axis.text.x.bottom = element_blank(),
        legend.position = c(0.85,0.27))+
  xlab("") + ylab("Valor") + ggtitle("")

ggsave(filename = "PosKM2_Boxplot2_aroma.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")

















#=============================================
# Comparacao de medidas para os grupos
#=============================================

### Uso os dados padronizados ou em suas
## escalas originais

### Silhueta --> Maior melhor
### Calinshi hararasz --> Maior melhor
### Davies Bouldin --> Menor melhor
### Dunn --> Maior melhor


vetor_silhueta = 
  c(mean(silhouette(cut_Ward,dist(dados) )[,3]),
    mean(silhouette(cut_Ward2,dist(dados))[,3]),
    mean(silhouette(kmeans_Ward$cluster,dist(dados))[,3]),
    mean(silhouette(kmeans_Ward2$cluster,dist(dados))[,3]))

vetor_calinski =
  c(calinhara(dados,cut_Ward),
    calinhara(dados,cut_Ward2),
    calinhara(dados,kmeans_Ward$cluster),
    calinhara(dados,kmeans_Ward2$cluster))

vetor_davies =
  c(index.DB(dados,cut_Ward)$DB,
    index.DB(dados,cut_Ward2)$DB,
    index.DB(dados,kmeans_Ward$cluster)$DB,
    index.DB(dados,kmeans_Ward2$cluster)$DB)

vetor_dunn = 
  c(dunn(Data=dados,clusters=cut_Ward),
    dunn(Data=dados,clusters=cut_Ward2),
    dunn(Data=dados,clusters=kmeans_Ward$cluster),
    dunn(Data=dados,clusters=kmeans_Ward2$cluster))


mat_avaliacao = 
  data.frame(round(vetor_silhueta,digits=3),
             round(vetor_calinski,digits=3),
             round(vetor_davies,digits=3),
             round(vetor_dunn,digits=3))

names(mat_avaliacao) = 
  c("Silhueta", "Calinshi hararasz",
    "Davies Bouldin","Dunn")

xtable::xtable(mat_avaliacao)

  
