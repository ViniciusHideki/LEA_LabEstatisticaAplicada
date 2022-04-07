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
### nuvem de palavras
if(!require(wordcloud)){install.packages("wordcloud")}
library(wordcloud)


### trata dados
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)
if(!require(GDAtools)){install.packages("GDAtools")}
if(!require(forcats)){install.packages("forcats")}
library(forcats)


### ACM
if(!require(leaflet)){install.packages("leaflet")}
library(leaflet)
if(!require(sf)){install.packages("sf")}
library(sf)


### To Latex

if(!require(xtable)){install.packages("xtable")}
library(xtable)



#############################################
# Pacotes
#############################################

### Variaveis escolhidas pela Thais

# [1] "IBGE"                   "UF"                    
# [3] "Municipio"              "Area"                  
# [5] "Pop_Urb_2010"           "Pop_Rur_2010"          
# [7] "Porte_2010"             "Sexo"                  
# [9] "Nivel_escolaridade"     "Profissao"             
# [11] "Cargo"                  "Principal_area_atuacao"
# [13] "Idade"                  "Gestao_SUAS"


Dados <- 
  read_excel("D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Dados_RH.xlsx")


library(readxl)

Dados_RH = 
  Dados %>%
  select(IBGE, IBGE7, UF, 
         
         Município, area,
         
         Pop_Urbana2010, Pop_Rural2010,
         
         q65_3,
         
         q65_9, q65_10,
         
         q65_12, q65_15,
         
         Idade)

names(Dados_RH) = 
  c("IBGE", "IBGE7", "UF",
    
    "Municipio", "Area", 
    
    "Pop_Urb_2010","Pop_Rur_2010",
    
    "Sexo",
    
    "Nivel_escolaridade", "Profissao",
    
    "Cargo","Principal_area_atuacao",
    
    "Idade")


Dados_RH$Nivel_escolaridade=
  factor(Dados_RH$Nivel_escolaridade,
         levels = c("Sem Escolaridade",
                    "Ensino Fundamental Incompleto",
                    "Ensino Médio Incompleto",
                    "Ensino Médio Completo",
                    "Ensino Superior Incompleto",
                    "Ensino Superior Completo",
                    "Especialização",
                    "Mestrado",
                    "Doutorado"))
  

#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(Dados_RH,2,pMiss)



manipula_dados_bar =function(variavel){
  data.frame(Nome=names(table(variavel)),
             Freq=paste(table(variavel)),
             Porc=paste0(round(prop.table(table(variavel)),
                               digits=3)*100,"%"),
             Perc=as.numeric(round(prop.table(table(variavel)),
                                   digits=3)))
}


fun_MedidasDesc = function(vetor){
  
  medidas = 
    c(min(vetor,na.rm=T),
      quantile(vetor,0.25,na.rm=T),
      median(vetor,na.rm=T),
      mean(vetor,na.rm=T),
      quantile(vetor,0.75,na.rm=T),
      max(vetor,na.rm=T),
      sd(vetor,na.rm=T),
      (sd(vetor,na.rm=T)/mean(vetor,na.rm=T)) )
  
  names(medidas) = 
    c("Min","Q1","Med","Media",
      "Q3","Max","Dp","CV")
  
  return(medidas)
}


#############################################
# Plano 1-2
#############################################

names(Dados_RH)

#####
# Roraima, Amazonas, RJ --> Abordagem Especial Sim
#####

Dados_RH_Dim12_S = Dados_RH %>%
  filter(UF == "Amazonas" | UF == "Roraima" | UF == "Rio de Janeiro")


### Sexo

sexo_Dim12_S = 
ggplot(manipula_dados_bar(Dados_RH_Dim12_S$Sexo),
       aes(x=Nome,y=Perc,fill=Nome))+
  geom_bar(stat = "identity", color = "black")+
  
  geom_text(label = with(manipula_dados_bar(Dados_RH_Dim12_S$Sexo), 
                         paste(Porc)), 
            vjust=-1)+
  scale_y_continuous(limits=c(0,1.05),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("") +
  ggtitle("Amazonas, Roraima,\nRio de Janeiro")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=14))

sexo_Dim12_S

### Escolaridade

escola_Dim12_S = 
  ggplot(Dados_RH_Dim12_S,aes(x=Nivel_escolaridade,
                              fill=Nivel_escolaridade))+
  geom_bar(aes(y = (..count..)/sum(..count..)),
           colour="black")+
  
  scale_y_continuous(limits=c(0,0.5),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("Porcentagem") +
  ggtitle("Amazonas, Roraima, Rio de Janeiro")+
  scale_fill_brewer(palette="Paired")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=12))+
  coord_flip()

escola_Dim12_S


### Cargo

cargo_Dim12_S = 
  ggplot(Dados_RH_Dim12_S,aes(x=Cargo,
                              fill=Cargo))+
  geom_bar(aes(y = (..count..)/sum(..count..)),
           colour="black")+
  
  scale_y_continuous(limits=c(0,0.3),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("Porcentagem") +
  ggtitle("Amazonas, Roraima, Rio de Janeiro")+
  scale_fill_brewer(palette="Paired")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15))+
  coord_flip()

cargo_Dim12_S


### Profissoes

table(Dados_RH_Dim12_S$Profissao)

freq_words_Dim12_S = as.numeric(table(Dados_RH_Dim12_S$Profissao))
name_words_Dim12_S = names(table(Dados_RH_Dim12_S$Profissao))
data_words_Dim12_S = 
  data.frame(freq_words_Dim12_S,name_words_Dim12_S)


data_words_Dim12_S <- data_words_Dim12_S %>% 
  mutate(rank = dense_rank(desc(freq_words_Dim12_S)))

graf_names_freq_Dim12_S = 
  ggplot(data= data_words_Dim12_S[which(data_words_Dim12_S$rank <= 10),], 
         aes(x = reorder(name_words_Dim12_S, 
                         freq_words_Dim12_S), 
             y =  freq_words_Dim12_S,
             fill = reorder(name_words_Dim12_S, 
                            -freq_words_Dim12_S))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=7))+
  geom_bar(stat="identity",width=0.6,
           color="black") +
  coord_flip(clip = "off")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  theme(axis.text.y = element_text(face="bold", color="black", 
                                   size=15)) +
  labs(title = "Amazonas, Roraima, Rio de Janeiro", 
       x = "", y = "Frequência absoluta")

graf_names_freq_Dim12_S

### Idade


summary(Dados_RH_Dim12_S$Idade[Dados_RH_Dim12_S$Idade<100])






#####
# Piaui, Paraíba --> Abordagem Especial Nao
#####

table(Dados_RH$UF)

Dados_RH_Dim12_N = Dados_RH %>%
  filter(UF == "Piaui" | UF == "Paraíba")


### Sexo


sexo_Dim12_N = 
  ggplot(manipula_dados_bar(Dados_RH_Dim12_N$Sexo),
         aes(x=Nome,y=Perc,fill=Nome))+
  geom_bar(stat = "identity", color = "black")+
  
  geom_text(label = with(manipula_dados_bar(Dados_RH_Dim12_N$Sexo), 
                         paste(Porc)), 
            vjust=-1)+
  scale_y_continuous(limits=c(0,1.05),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("") +
  ggtitle("Piauí,\nParaíba")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=14))



### Escolaridade

escola_Dim12_N = 
  ggplot(Dados_RH_Dim12_N,aes(x=Nivel_escolaridade,
                              fill=Nivel_escolaridade))+
  geom_bar(aes(y = (..count..)/sum(..count..)),
           colour="black")+
  
  scale_y_continuous(limits=c(0,0.5),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("Porcentagem") +
  ggtitle("Piauí, Paraíba")+
  scale_fill_brewer(palette="Paired")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15))+
  coord_flip()

escola_Dim12_N


### Cargo

cargo_Dim12_N = 
  ggplot(Dados_RH_Dim12_N,aes(x=Cargo,
                              fill=Cargo))+
  geom_bar(aes(y = (..count..)/sum(..count..)),
           colour="black")+
  
  scale_y_continuous(limits=c(0,0.3),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("Porcentagem") +
  ggtitle("Piauí, Paraíba")+
  scale_fill_brewer(palette="Paired")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15))+
  coord_flip()

cargo_Dim12_N


### Profissoes

table(Dados_RH_Dim12_N$Profissao)

freq_words_Dim12_N = as.numeric(table(Dados_RH_Dim12_N$Profissao))
name_words_Dim12_N = names(table(Dados_RH_Dim12_N$Profissao))
data_words_Dim12_N = 
  data.frame(freq_words_Dim12_N,name_words_Dim12_N)

data_words_Dim12_N <- data_words_Dim12_N %>% 
  mutate(rank = dense_rank(desc(freq_words_Dim12_N)))

graf_names_freq_Dim12_N = 
  ggplot(data= data_words_Dim12_N[which(data_words_Dim12_N$rank <= 10),], 
         aes(x = reorder(name_words_Dim12_N, 
                         freq_words_Dim12_N), 
             y =  freq_words_Dim12_N,
             fill = reorder(name_words_Dim12_N, 
                            -freq_words_Dim12_N))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=7),
                     limits = c(0,1000))+
  geom_bar(stat="identity",width=0.6,
           color="black") +
  coord_flip(clip = "off")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  theme(axis.text.y = element_text(face="bold", color="black", 
                                   size=15)) +
  labs(title = "Piauí, Paraíba", 
       x = "", y = "Frequência absoluta")

graf_names_freq_Dim12_N

### Idade


summary(Dados_RH_Dim12_S$Idade[Dados_RH_Dim12_S$Idade<100&Dados_RH_Dim12_S$Idade>16])
summary(Dados_RH_Dim12_N$Idade[Dados_RH_Dim12_N$Idade<100&Dados_RH_Dim12_N$Idade>16])

xtable(
  matrix(c(names(fun_MedidasDesc(Dados_RH_Dim12_S$Idade[Dados_RH_Dim12_S$Idade<100&Dados_RH_Dim12_S$Idade>16])),
           round(fun_MedidasDesc(Dados_RH_Dim12_S$Idade[Dados_RH_Dim12_S$Idade<100&Dados_RH_Dim12_S$Idade>16]),
                 digits=3),
           round(fun_MedidasDesc(Dados_RH_Dim12_N$Idade[Dados_RH_Dim12_N$Idade<100&Dados_RH_Dim12_N$Idade>16]),
                 digits=3)),
         byrow=T,ncol=8))


#####
# Salvando os graficos
#####

ggarrange(sexo_Dim12_S,
          sexo_Dim12_N)

ggsave(filename = "Desc_RH_Dim12_Sexo.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")




ggarrange(escola_Dim12_S,
          escola_Dim12_N)

ggsave(filename = "Desc_RH_Dim12_Escola.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 45.75, 
       height = 12.5, 
       units = "cm")




ggarrange(cargo_Dim12_S,
          cargo_Dim12_N)

ggsave(filename = "Desc_RH_Dim12_Cargo.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 45.75, 
       height = 12.5, 
       units = "cm")




ggarrange(graf_names_freq_Dim12_S,
          graf_names_freq_Dim12_N)

ggsave(filename = "Desc_RH_Dim12_Profissoes.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 45.75, 
       height = 12.5, 
       units = "cm")






















#############################################
# Plano 1-3
#############################################

names(Dados_RH)

#####
# "Bahia, Piaui, Maranhão, Pernambuco, Ceará" 
# --> Crianca_Feliz_Sim
#####

table(Dados_RH$UF)

Dados_RH_Dim13_S = Dados_RH %>%
  filter(UF == "Bahia" | UF == "Piaui" 
         | UF == "Maranhão"| UF == "Pernambuco"
         | UF == "Ceará")


### Sexo

sexo_Dim13_S = 
  ggplot(manipula_dados_bar(Dados_RH_Dim13_S$Sexo),
         aes(x=Nome,y=Perc,fill=Nome))+
  geom_bar(stat = "identity", color = "black")+
  
  geom_text(label = with(manipula_dados_bar(Dados_RH_Dim13_S$Sexo), 
                         paste(Porc)), 
            vjust=-1)+
  scale_y_continuous(limits=c(0,1.05),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("") +
  ggtitle("Bahia, Piaui, Maranhão,\nPernambuco, Ceará")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=14))

sexo_Dim13_S

### Escolaridade

escola_Dim13_S = 
  ggplot(Dados_RH_Dim13_S,aes(x=Nivel_escolaridade,
                              fill=Nivel_escolaridade))+
  geom_bar(aes(y = (..count..)/sum(..count..)),
           colour="black")+
  
  scale_y_continuous(limits=c(0,0.5),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("Porcentagem") +
  ggtitle("Bahia, Piaui, Maranhão, Pernambuco, Ceará")+
  scale_fill_brewer(palette="Paired")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15))+
  coord_flip()

escola_Dim13_S


### Cargo

cargo_Dim13_S = 
  ggplot(Dados_RH_Dim13_S,aes(x=Cargo,
                              fill=Cargo))+
  geom_bar(aes(y = (..count..)/sum(..count..)),
           colour="black")+
  
  scale_y_continuous(limits=c(0,0.25),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("Porcentagem") +
  ggtitle("Bahia, Piaui, Maranhão, Pernambuco, Ceará")+
  scale_fill_brewer(palette="Paired")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15))+
  coord_flip()

cargo_Dim13_S


### Profissoes

table(Dados_RH_Dim13_S$Profissao)

freq_words_Dim13_S = as.numeric(table(Dados_RH_Dim13_S$Profissao))
name_words_Dim13_S = names(table(Dados_RH_Dim13_S$Profissao))
data_words_Dim13_S = 
  data.frame(freq_words_Dim13_S,name_words_Dim13_S)


data_words_Dim13_S <- data_words_Dim13_S %>% 
  mutate(rank = dense_rank(desc(freq_words_Dim13_S)))

graf_names_freq_Dim13_S = 
  ggplot(data= data_words_Dim13_S[which(data_words_Dim13_S$rank <= 10),], 
         aes(x = reorder(name_words_Dim13_S, 
                         freq_words_Dim13_S), 
             y =  freq_words_Dim13_S,
             fill = reorder(name_words_Dim13_S, 
                            -freq_words_Dim13_S))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=7),
                     limits=c(0,5000))+
  geom_bar(stat="identity",width=0.6,
           color="black") +
  coord_flip(clip = "off")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  theme(axis.text.y = element_text(face="bold", color="black", 
                                   size=15)) +
  labs(title = "Bahia, Piaui, Maranhão, Pernambuco, Ceará", 
       x = "", y = "Frequência absoluta")

graf_names_freq_Dim13_S

### Idade


summary(Dados_RH_Dim13_S$Idade[Dados_RH_Dim13_S$Idade<100])














#####
# "Minas gerais, Tocantins, Paraná, Santa Catarina" --> Crianca Feliz Nao
#####

table(Dados_RH$UF)

Dados_RH_Dim13_N = Dados_RH %>%
  filter(UF == "Minas gerais" | UF == "Tocantins"
         |UF == "Paraná"| UF == "Santa Catarina")


### Sexo


sexo_Dim13_N = 
  ggplot(manipula_dados_bar(Dados_RH_Dim13_N$Sexo),
         aes(x=Nome,y=Perc,fill=Nome))+
  geom_bar(stat = "identity", color = "black")+
  
  geom_text(label = with(manipula_dados_bar(Dados_RH_Dim13_N$Sexo), 
                         paste(Porc)), 
            vjust=-1)+
  scale_y_continuous(limits=c(0,1.05),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("") +
  ggtitle("Minas gerais, Tocantins,\nParaná, Santa Catarina")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=14))


sexo_Dim13_N

### Escolaridade

escola_Dim13_N = 
  ggplot(Dados_RH_Dim13_N,aes(x=Nivel_escolaridade,
                              fill=Nivel_escolaridade))+
  geom_bar(aes(y = (..count..)/sum(..count..)),
           colour="black")+
  
  scale_y_continuous(limits=c(0,0.5),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("Porcentagem") +
  ggtitle("Minas gerais, Tocantins, Paraná, Santa Catarina")+
  scale_fill_brewer(palette="Paired")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15))+
  coord_flip()

escola_Dim13_N


### Cargo

cargo_Dim13_N = 
  ggplot(Dados_RH_Dim13_N,aes(x=Cargo,
                              fill=Cargo))+
  geom_bar(aes(y = (..count..)/sum(..count..)),
           colour="black")+
  
  scale_y_continuous(limits=c(0,0.25),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("") + 
  ylab("Porcentagem") +
  ggtitle("Minas gerais, Tocantins, Paraná, Santa Catarina")+
  scale_fill_brewer(palette="Paired")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15))+
  coord_flip()

cargo_Dim13_N


### Profissoes

table(Dados_RH_Dim13_N$Profissao)

freq_words_Dim13_N = as.numeric(table(Dados_RH_Dim13_N$Profissao))
name_words_Dim13_N = names(table(Dados_RH_Dim13_N$Profissao))
data_words_Dim13_N = 
  data.frame(freq_words_Dim13_N,name_words_Dim13_N)

data_words_Dim13_N <- data_words_Dim13_N %>% 
  mutate(rank = dense_rank(desc(freq_words_Dim13_N)))

graf_names_freq_Dim13_N = 
  ggplot(data= data_words_Dim13_N[which(data_words_Dim13_N$rank <= 10),], 
         aes(x = reorder(name_words_Dim13_N, 
                         freq_words_Dim13_N), 
             y =  freq_words_Dim13_N,
             fill = reorder(name_words_Dim13_N, 
                            -freq_words_Dim13_N))) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=7),
                     limits = c(0,4500))+
  geom_bar(stat="identity",width=0.6,
           color="black") +
  coord_flip(clip = "off")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  theme(axis.text.y = element_text(face="bold", color="black", 
                                   size=15)) +
  labs(title = "Minas gerais, Tocantins, Paraná, Santa Catarina", 
       x = "", y = "Frequência absoluta")

graf_names_freq_Dim13_N

### Idade


summary(Dados_RH_Dim13_S$Idade[Dados_RH_Dim13_S$Idade<100&Dados_RH_Dim13_S$Idade>16])
summary(Dados_RH_Dim13_N$Idade[Dados_RH_Dim13_N$Idade<100&Dados_RH_Dim13_N$Idade>16])

xtable(
  matrix(c(names(fun_MedidasDesc(Dados_RH_Dim13_S$Idade[Dados_RH_Dim13_S$Idade<100&Dados_RH_Dim13_S$Idade>16])),
           round(fun_MedidasDesc(Dados_RH_Dim13_S$Idade[Dados_RH_Dim13_S$Idade<100&Dados_RH_Dim13_S$Idade>16]),
                 digits=3),
           round(fun_MedidasDesc(Dados_RH_Dim13_N$Idade[Dados_RH_Dim13_N$Idade<100&Dados_RH_Dim13_N$Idade>16]),
                 digits=3)),
         byrow=T,ncol=8))


#####
# Salvando os graficos
#####

ggarrange(sexo_Dim13_S,
          sexo_Dim13_N)

ggsave(filename = "Desc_RH_Dim13_Sexo.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")




ggarrange(escola_Dim13_S,
          escola_Dim13_N)

ggsave(filename = "Desc_RH_Dim13_Escola.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 45.75, 
       height = 12.5, 
       units = "cm")




ggarrange(cargo_Dim13_S,
          cargo_Dim13_N)

ggsave(filename = "Desc_RH_Dim13_Cargo.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 45.75, 
       height = 12.5,  
       units = "cm")




ggarrange(graf_names_freq_Dim13_S,
          graf_names_freq_Dim13_N)

ggsave(filename = "Desc_RH_Dim13_Profissoes.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 45.75, 
       height = 12.5, 
       units = "cm")


