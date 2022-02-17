
### Criacao de figuras
if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2)
if(!require(RColorBrewer)){install.packages('RColorBrewer')}
library(RColorBrewer)
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)
if(!require(qqplotr)){install.packages("qqplotr")}
library(qqplotr)
if(!require(ggcorrplot)){install.packages("ggcorrplot")}
library(ggcorrplot)
if(!require(GGally)){install.packages("GGally")}
library(GGally)

### Manipular bases de dados
if(!require(reshape)){install.packages("reshape")}
library(reshape)
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

### dataframe pra latex
if(!require(xtable)){install.packages("xtable")}
library(xtable)


library(readxl)
D3 <- read_excel("D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/D3.xlsx")

#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(D3,2,pMiss)


#############################
# Analise descritiva - Correlacao linear
#############################


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

ggcorrplot(cor(dados), hc.order = TRUE, 
           type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_bw,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab_size = 5, digits = 2)

ggsave(filename = "corr_linear.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")





#############################
# Medidas descritivas
#############################

# Minimo, Q1, Mediana,
# Media, Q3, Maximo,
# DP, CP

fun_MedidasDesc = function(vetor){
  
  medidas = 
    c(min(vetor),
      quantile(vetor,0.25),
      median(vetor),
      mean(vetor),
      quantile(vetor,0.75),
      max(vetor),
      sd(vetor),
      (sd(vetor)/mean(vetor)) )
  
  return(medidas)
}

lista_Desc = list()

### Descritivas no geral

for(i in 1:ncol(dados)){
  lista_Desc[[i]]=fun_MedidasDesc(dados[,i])
}

names(dados)
round(lista_Desc[[1]],digits=3)


### Descritivas da Textura
desc_textura = 
  data.frame(matrix(c( round(lista_Desc[[1]],digits=3),
                       round(lista_Desc[[2]],digits=3),
                       round(lista_Desc[[4]],digits=3),
                       round(lista_Desc[[5]],digits=3),
                       round(lista_Desc[[15]],digits=3)),
                    byrow=T,ncol=8))

xtable(desc_textura)


### Descritivas do Aroma1
desc_aroma1 = 
  data.frame(matrix(c( round(lista_Desc[[3]],digits=3),
                       round(lista_Desc[[6]],digits=3),
                       round(lista_Desc[[7]],digits=3),
                       round(lista_Desc[[8]],digits=3),
                       round(lista_Desc[[9]],digits=3)),
                    byrow=T,ncol=8))

xtable(desc_aroma1)
c(3,6,7,8,9)





### Descritivas do Aroma2
desc_aroma2 = 
  data.frame(matrix(c( round(lista_Desc[[10]],digits=3),
                       round(lista_Desc[[11]],digits=3),
                       round(lista_Desc[[12]],digits=3),
                       round(lista_Desc[[13]],digits=3),
                       round(lista_Desc[[14]],digits=3)),
                    byrow=T,ncol=8))

xtable(desc_aroma2)
10:14



#############################
# Boxplots - Univaridas
#############################

dados_melt = melt(dados)


box_textura <- 
  ggplot(data = subset(dados_melt,
                       variable %in% names(dados)[c(1,2,4,5,15)])) +
  
  geom_boxplot(mapping = aes(y = value,x=variable,fill=variable), 
               color = 'black',
               show.legend = T) +
  
  labs(x = "", y = "Notas", 
       title = "",
       fill= "") +
  
  scale_fill_brewer(palette="Set1")+
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

box_textura

ggsave(filename = "textura_box1.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")



box_aroma1 <- 
  ggplot(data = subset(dados_melt,
                       variable
                       %in% 
                         names(dados)[c(3,6,7,8,9)])) +
  
  geom_boxplot(mapping = aes(y = value,x=variable,fill=variable), 
               color = 'black',
               show.legend = T) +
  
  labs(x = "", y = "Notas", 
       title = "",
       fill= "") +
  
  scale_fill_brewer(palette="Set1")+
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

box_aroma1

ggsave(filename = "aroma_box1.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


box_aroma2 <- 
  ggplot(data = subset(dados_melt,
                       variable
                       %in% 
                         names(dados)[c(10:14)])) +
  
  geom_boxplot(mapping = aes(y = value,x=variable,fill=variable), 
               color = 'black',
               show.legend = T) +
  
  labs(x = "", y = "Notas", 
       title = "",
       fill= "") +
  
  scale_fill_brewer(palette="Set1")+
  
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

box_aroma2

ggsave(filename = "aroma_box2.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto1/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")
