#############################################
# Pacotes
#############################################

### figuras 
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(GGally)){install.packages("GGally")}
library(GGally)
if(!require(RColorBrewer)){install.packages('RColorBrewer')}
library(RColorBrewer)
if(!require(ggpubr)){install.packages('ggpubr')}
library(ggpubr)
if(!require("qqplotr")){install.packages("qqplotr")}
library(qqplotr)

### trata dados
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

### ACM
if(!require(FactoMineR)){install.packages("FactoMineR")}
library(FactoMineR)
if(!require(factoextra)){install.packages("factoextra")}
library(factoextra)
if(!require(corrplot)){install.packages("corrplot")}
library(corrplot)



#############################################
# Pacotes
#############################################

library(readxl)
Dados <- 
  read_excel("D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Dados.xlsx")


Dados_servicos = 
  Dados %>%
  select(Região,
         UF,
         
         q15,q21,q23,q24,
         
         q32,q34,q37,q42,
         
         q41_1_1,
         q41_2_1,
         q41_3_1,
         q41_4_1)

Dados_servicos$Região =
  gsub("Região ","",Dados_servicos$Região)

names(Dados_servicos)=
  c("Regiao","UF",
    
    "Proteçao_Idosos",
    "Abordagem_Especializada",
    "Criança_Acolhimento",
    "Família_Guardiões",
    
    "Abrigo_Calamidade",
    "Proteção_Imigrantes",
    "ACESSUAS_Trabalho",
    "Criança_Feliz",
    
    "Beneficio_Morte",
    "Beneficio_Natalidade",
    "Beneficio_Calamidade",
    "Beneficio_Vulnerabilidade")


names(Dados_servicos)=
  c("Regiao","UF",
    
    "Prote_Idosos",
    "Abordagem_Especial",
    "Criança_Acolhe",
    "Guardiões",
    
    "Calamidade",
    "Imigrantes",
    "ACESSUAS",
    "Criança_Feliz",
    
    "Bene_Morte",
    "Bene_Natalidade",
    "Bene_Calamidade",
    "Bene_Vulnerabilidade")

#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(Dados_servicos,2,pMiss)






#############################################
# ACM
#############################################


res.mca <- MCA(Dados_servicos[,-c(1,2)], 
               graph = FALSE)

# Autovalores
eig.val <- get_eigenvalue(res.mca);
eig.val

fviz_screeplot(res.mca, addlabels = TRUE)+
  theme_bw() +
  labs(y = "Porcentagem da variância explicada", 
       x = "Dimensões",
       title = "")+
  theme(axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"))


ggsave(filename = "MCA_ScreeplotEigen.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


# eig.val2 = 
#   data.frame("Autovalor"=round(get_eigenvalue(res.mca)[,1],digits=2),
#              "Dimensao"=paste(c(1:12)) )
# 
# eig.val2$Dimensao = as.numeric(eig.val2$Dimensao)
# 
# 
# ggplot(eig.val2,aes(x=factor(Dimensao),y=Autovalor))+
#   geom_bar(stat = "identity", color = "white",fill="steelblue")+
#   
#   geom_text(label = with(eig.val2, Autovalor), vjust=-1.5,size=4.5)+
#   scale_y_continuous(limits=c(0,0.16),
#                      breaks = scales::pretty_breaks(n = 5))+
#   geom_line(group=1,size=1.1,alpha=0.5) +
#   geom_point(size=4)+
#   xlab("Dimensão") + 
#   ylab("Autorvalor") +
#   ggtitle("")+
#   scale_fill_brewer(palette="Dark2")+
#   theme_bw()+
#   theme(text = element_text(size = 14, 
#                             family ="serif"),
#         plot.title = element_text(hjust = 0.5),
#         legend.title = element_blank(),
#         legend.position = "none")


# Resultados para as variÃ¡veis
var <- get_mca_var(res.mca)

#===============================================
# Qualidade de representacao
#===============================================

var <- get_mca_var(res.mca)

ggplot(data = melt(var$cos2[,-5]), 
       aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  labs(x = "", y = "", fill = "cos2") +
  geom_text(aes(x = Var2, y = Var1, label = round(value, 2)), color = "black", 
            fontface = "bold", size = 4.5)+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(9,"Blues")[-c(1,7:9)])+
  theme(text = element_text(size = 16, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))

ggsave(filename = "MCA_Co2_Dims.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


#===============================================
# Correlacao
#===============================================

options(ggrepel.max.overlaps = Inf) 

#set.seed(10)
fviz_mca_var(res.mca, choice = "mca.cor", repel = TRUE,
             axes = c(1,2),
             ggtheme = theme_bw(),
             col.quanti.sup = "firebrick3",
             col.var = "dodgerblue3")+
  labs(title="")+
  theme(plot.title=element_text(hjust=0.5, 
                                size = 15, 
                                family ="serif"),
        text = element_text(size=13, family ="serif"))


ggsave(filename = "MCA_Dim12_Corr.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


set.seed(10)
fviz_mca_var(res.mca, choice = "mca.cor", repel = TRUE,
             axes = c(1,3),
             ggtheme = theme_bw(),
             col.quanti.sup = "firebrick3",
             col.var = "dodgerblue3")+
  labs(title="")+
  theme(plot.title=element_text(hjust=0.5, 
                                size = 15, 
                                family ="serif"),
        text = element_text(size=13, family ="serif"))



set.seed(10)
fviz_mca_var(res.mca, choice = "mca.cor", repel = TRUE,
             axes = c(2,3),
             ggtheme = theme_bw(),
             col.quanti.sup = "firebrick3",
             col.var = "dodgerblue3")+
  labs(title="")+
  theme(plot.title=element_text(hjust=0.5, 
                                size = 15, 
                                family ="serif"),
        text = element_text(size=13, family ="serif"))

#===============================================
# Plano 1-2
#===============================================



# legenda_box = 
#   paste0(c(". q15: Proteçao de Idosos \n"),
#          c(". q21: Abordagem Especializada \n"),
#          c(". q23: Acolhimento de Crianças \n"),
#          c(". q24: Suporte a Famílias Guardiães \n"),
#          
#          c(". q32: Abrigo a Calamidade \n"),
#          c(". q34: Proteção para Imigrantes \n"),
#          c(". q37: ACESSUAS e Trabalho \n"),
#          c(". q42: Criança Feliz \n"),
#          
#          c(". q41_1_1: Beneficio de Morte \n"),
#          c(". q41_2_1: Beneficio de Natalidade \n"),
#          c(". q41_3_1: Beneficio de Calamidade \n"),
#          c(". q41_4_1: Beneficio de Vulnerabilidade"))



# names(Dados_servicos) = 
#   c("Regiao",
#     "UF",
#     
#     "q15","q21","q23","q24",
#     
#     "q32","q34","q37","q42",
#     
#     "q41_1_1",
#     "q41_2_1",
#     "q41_3_1",
#     "q41_4_1")

set.seed(10)
fviz_mca_var(MCA(Dados_servicos[,-2], 
                 graph = FALSE,
                 quali.sup = 1), 
             repel = TRUE,
             axes = c(1,2),
             ggtheme = theme_bw(),
             col.quali.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")

# +
#   annotate(geom="label", x=-Inf, y=-Inf, 
#            label=legenda_box,
#            hjust=-1,vjust=-1,
#            color="black")


ggsave(filename = "MCA_Dim12_Plano.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


set.seed(10)
fviz_mca_var(MCA(Dados_servicos[,-2], 
                 graph = FALSE,
                 quali.sup = 1), 
             repel = TRUE,
             axes = c(1,2),
             ggtheme = theme_bw(),
             select.var = list(cos2=10),
             col.quali.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")


ggsave(filename = "MCA_Dim12_Plano_Regiao_TOP10.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")




set.seed(10)
fviz_mca_var(MCA(Dados_servicos[,-1], 
                 graph = FALSE,
                 quali.sup = 1), 
             repel = TRUE,
             axes = c(1,2),
             select.var = list(cos2=10),
             ggtheme = theme_bw(),
             col.quali.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")

ggsave(filename = "MCA_Dim12_Plano_UF_TOP10.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")



set.seed(10)
fviz_mca_ind(MCA(Dados_servicos[,-2], 
                 graph = FALSE,
                 quali.sup = 1),
             label="none",
             habillage = "Regiao",
             axes = c(1,2),
             ggtheme = theme_bw())+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")


ggsave(filename = "MCA_Dim12_Regiao_Ind.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")




#===============================================
# Plano 1-3
#===============================================


set.seed(10)
fviz_mca_var(MCA(Dados_servicos[,-2], 
                 graph = FALSE,
                 quali.sup = 1), 
             repel = TRUE,
             axes = c(1,3),
             ggtheme = theme_bw(),
             col.quali.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")

ggsave(filename = "MCA_Dim13_Plano.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


set.seed(10)
fviz_mca_var(MCA(Dados_servicos[,-2], 
                 graph = FALSE,
                 quali.sup = 1), 
             repel = TRUE,
             axes = c(1,3),
             ggtheme = theme_bw(),
             select.var = list(cos2=10),
             col.quali.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")


ggsave(filename = "MCA_Dim13_Plano_Regiao_TOP10.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")




set.seed(10)
fviz_mca_var(MCA(Dados_servicos[,-1], 
                 graph = FALSE,
                 quali.sup = 1), 
             repel = TRUE,
             axes = c(1,3),
             select.var = list(cos2=10),
             ggtheme = theme_bw(),
             col.quali.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")

ggsave(filename = "MCA_Dim13_Plano_UF_TOP10.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")



set.seed(10)
fviz_mca_ind(MCA(Dados_servicos[,-2], 
                 graph = FALSE,
                 quali.sup = 1),
             label="none",
             habillage = "Regiao",
             axes = c(1,3),
             ggtheme = theme_bw())+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")


ggsave(filename = "MCA_Dim13_Regiao_Ind.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")









#===============================================
# Plano 2-3
#===============================================


set.seed(10)
fviz_mca_var(MCA(Dados_servicos[,-2], 
                 graph = FALSE,
                 quali.sup = 1), 
             repel = TRUE,
             axes = c(2,3),
             ggtheme = theme_bw(),
             col.quali.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")

ggsave(filename = "MCA_Dim23_Plano.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


set.seed(10)
fviz_mca_var(MCA(Dados_servicos[,-2], 
                 graph = FALSE,
                 quali.sup = 1), 
             repel = TRUE,
             axes = c(2,3),
             ggtheme = theme_bw(),
             select.var = list(cos2=10),
             col.quali.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")


ggsave(filename = "MCA_Dim23_Plano_Regiao_TOP10.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")




set.seed(10)
fviz_mca_var(MCA(Dados_servicos[,-1], 
                 graph = FALSE,
                 quali.sup = 1), 
             repel = TRUE,
             axes = c(2,3),
             select.var = list(cos2=10),
             ggtheme = theme_bw(),
             col.quali.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")

ggsave(filename = "MCA_Dim23_Plano_UF_TOP10.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")



set.seed(10)
fviz_mca_ind(MCA(Dados_servicos[,-2], 
                 graph = FALSE,
                 quali.sup = 1),
             label="none",
             habillage = "Regiao",
             axes = c(2,3),
             ggtheme = theme_bw())+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="")


ggsave(filename = "MCA_Dim23_Regiao_Ind.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")

