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

### modelagem
if(!require(MASS)){install.packages("MASS")}
library(MASS)
if(!require(stats4)){install.packages("stats4")}
library(stats4)
if(!require(survival)){install.packages("survival")}
library(survival)
if(!require(parfm)){install.packages("parfm")}
library(parfm)


### latex
if(!require(xtable)){install.packages("xtable")}
library(xtable)

#############################################
# Lendo os dados
#############################################

## Lendo os dados
library(readr)
Dados_telecom <- read_csv("Dados_telecom.csv")

variaveis_indices = 
  c(match("clienduracao",names(Dados_telecom)),
    match("churn01",names(Dados_telecom)),
    match("idade",names(Dados_telecom)),
    match("residduracao",names(Dados_telecom)),
    match("renda",names(Dados_telecom)),
    match("educacao",names(Dados_telecom)),
    
    match("churn",names(Dados_telecom)))

dados = Dados_telecom[,variaveis_indices]

names(table(dados$educacao))

dados$educacao = factor(dados$educacao,
                        levels = c("Inc_medio","Inc_sup","medio",
                                   "posgrad","superior"),
                        labels = c("Inc_Medio","Inc_Superior","Medio",
                                   "Superior+","Superior+"))

#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dados,2,pMiss)

#############################################
# Descritiva - Churn e Quanti
#############################################

box_idade = 
  ggplot(data=dados,
         aes(x=churn,y=idade,fill=churn))+
  geom_boxplot()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  xlab("Churn") + 
  ylab("Anos") +
  ggtitle("Idade do cliente")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")+
  coord_flip()



box_renda = 
  ggplot(data=dados,
         aes(x=churn,y=renda,fill=churn))+
  geom_boxplot()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  xlab("Churn") + 
  ylab("Milhar") +
  ggtitle("Renda familiar anual")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")+
  coord_flip()



box_residduracao = 
  ggplot(data=dados,
         aes(x=churn,y=residduracao,fill=churn))+
  geom_boxplot()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  xlab("Churn") + 
  ylab("Anos") +
  ggtitle("Tempo na residência")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")+
  coord_flip()



box_clienduracao = 
  ggplot(data=dados,
         aes(x=churn,y=clienduracao,fill=churn))+
  geom_boxplot()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  
  xlab("Churn") + 
  ylab("Mêses") +
  ggtitle("Tempo como cliente")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")+
  coord_flip()




ggarrange(box_clienduracao,
          box_idade,
          box_residduracao,
          box_renda,
          labels = LETTERS[1:4])


ggsave(filename = "Desc_Churn_e_Quanti.png",
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Atividade4_SobrevivenciaFragilidade/Figuras",
       width = 19.75,
       height = 12.5,
       units = "cm")


#############################################
# Descritiva - Churn e Quali
#############################################

ggplot(dados, aes(x= churn,  group=educacao)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  
  xlab("Churn") + 
  ylab("Porcentagem") +
  ggtitle("Escolaridade")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")+
  
  facet_grid(~educacao) +
  scale_y_continuous(labels = scales::percent,
                     limits=c(0,1))


ggsave(filename = "Desc_Churn_e_Quali.png",
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Atividade4_SobrevivenciaFragilidade/Figuras",
       width = 19.75,
       height = 12.5,
       units = "cm")
