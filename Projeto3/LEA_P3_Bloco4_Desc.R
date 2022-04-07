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
if(!require(GDAtools)){install.packages("GDAtools")}

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


#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(Dados_servicos,2,pMiss)



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
# Tratamento de Dados
#############################################

Dados_servicos$Região =
  gsub("Região ","",Dados_servicos$Região)


#############################################
# Univariada dos Quali
#############################################



#####
## Selecionando os indices das variavis
#e dando o nome dos graficos
#####

indice_col_quali =
  c(match("Região",names(Dados_servicos)),
    
    match("q15",names(Dados_servicos)),
    match("q21",names(Dados_servicos)),
    match("q23",names(Dados_servicos)),
    match("q24",names(Dados_servicos)),
    
    match("q32",names(Dados_servicos)),
    match("q34",names(Dados_servicos)),
    match("q37",names(Dados_servicos)),
    match("q42",names(Dados_servicos)),
    
    match("q41_1_1",names(Dados_servicos)),
    match("q41_2_1",names(Dados_servicos)),
    match("q41_3_1",names(Dados_servicos)),
    match("q41_4_1",names(Dados_servicos)))


nomes_col_quali = 
  c("Região",
    
    "Serviço de Proteção Social para\nPessoas com Deficiência e Idosas",
    "Serviço Especializado\nem Abordagem Social",
    "Serviço de Acolhimento em Família\nAcolhedora para Criança e Adolescente",
    "Programa de apoio à família\nguardiã na família extensa",
    
    
    "Abrigou pessoas atingidas por situações\nde emergência ou de calamidade pública",
    "Proteção a pessoas e\nfamílias imigrantes/refugiados",
    "Executou o programa\nACESSUAS Trabalho",
    "Programa Criança\nFeliz no Município",
    
    "Benefício Eventual\npor Situação de morte",
    "Benefício Eventual\npor Situação de Natalidade",
    "Benefício Eventual\npara situação de calamidade pública",
    "Benefícios eventuais para famílias em\nsituação de vulnerabilidade temporária")




#####
## Funcao para fazer as porncetagens e
# coloca sobre a barra
#####

manipula_dados_bar =function(variavel){
  data.frame(Nome=names(table(variavel)),
             Freq=paste(table(variavel)),
             Porc=paste0(round(prop.table(table(variavel)),
                               digits=3)*100,"%"),
             Perc=as.numeric(round(prop.table(table(variavel)),
                                   digits=3)))
}



#####
## For para fazer as barras
#####

lista_gg_uni_quali = list()

for(i in 1:length(indice_col_quali)){
  
  base_trans =  
    manipula_dados_bar(Dados_servicos[,indice_col_quali[i]])
  
  lista_gg_uni_quali[[i]]=
    ggplot(base_trans,
           aes(x=Nome,y=Perc,fill=Nome))+
    geom_bar(stat = "identity", color = "black")+
    
    geom_text(label = with(base_trans, paste(Freq, paste0('(', Porc, ')'))), vjust=-1)+
    scale_y_continuous(limits=c(0,1.05),
                       breaks = scales::pretty_breaks(n = 5),
                       labels = scales::percent)+
    
    xlab("") + 
    ylab("") +
    ggtitle(nomes_col_quali[i])+
    scale_fill_brewer(palette="Dark2")+
    theme_bw()+
    theme(text = element_text(size = 14, 
                              family ="serif"),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(size=14))
  
}


#####
## Salvando os graficos
#####

### Regicao

lista_gg_uni_quali[[1]]+
  scale_y_continuous(limits=c(0,0.4),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)

ggsave(filename = "Desc_Quali_Uni_Regiao.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75,
       height = 12.5,
       units = "cm")


### q15, q21, q23, q24

ggarrange(lista_gg_uni_quali[[2]],
          lista_gg_uni_quali[[3]],
          lista_gg_uni_quali[[4]],
          lista_gg_uni_quali[[5]],
          labels = LETTERS[1:4])

# width = 22.75,
# height = 17.5,

ggsave(filename = "Desc_Quali_Uni_15_21_23_24.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 22.75,
       height = 17.5,
       units = "cm")



### q32, q34, q37, q42

ggarrange(lista_gg_uni_quali[[6]],
          lista_gg_uni_quali[[7]],
          lista_gg_uni_quali[[8]],
          lista_gg_uni_quali[[9]],
          labels = LETTERS[1:4])


ggsave(filename = "Desc_Quali_Uni_32_34_37_42.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 22.75,
       height = 17.5,
       units = "cm")


### q41_1a4

ggarrange(lista_gg_uni_quali[[10]],
          lista_gg_uni_quali[[11]],
          lista_gg_uni_quali[[12]],
          lista_gg_uni_quali[[13]],
          labels = LETTERS[1:4])


ggsave(filename = "Desc_Quali_Uni_41_1_a_4.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 22.75,
       height = 17.5,
       units = "cm")


#############################################
# Bivariada dos Quali - Regiao e Quali
#############################################

#GDAtools::burt(as.data.frame(Dados_servicos[,-2]))
#xtable(GDAtools::burt(as.data.frame(Dados_servicos[,-2])))

lista_gg_bi_quali = list()

for(i in 2:length(indice_col_quali)){
  
  dados_guarda = Dados_servicos[,c(1,indice_col_quali[i])]
  names(dados_guarda) = c("Região","Vari")
  
  lista_gg_bi_quali[[i]]=
  ggplot(dados_guarda, aes(x= Vari,  group=Região)) + 
    geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
    geom_text(aes( label = scales::percent(..prop..),
                   y= ..prop.. ), stat= "count", vjust = -.5) +
    ylab("") +
    ggtitle("")+
    xlab(nomes_col_quali[i]) + 
    scale_fill_brewer(palette="Dark2")+
    theme_bw()+
    theme(text = element_text(size = 14, 
                              family ="serif"),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "none")+
    
    facet_grid(~Região) +
    scale_y_continuous(labels = scales::percent,
                       limits=c(0,1.1),
                       breaks = seq(0,1,0.2))
  

  
}

#####
## Salvando os graficos
#####

### q15, q21, q23, q24

annotate_figure(ggarrange(lista_gg_bi_quali[[2]],
                          lista_gg_bi_quali[[3]],
                          lista_gg_bi_quali[[4]],
                          lista_gg_bi_quali[[5]]),
                top = text_grob("Região",
                                family = "serif",
                                size = 20),
                left = text_grob("Porcentagem",
                                 family = "serif",
                                 size = 20,
                                 rot = 90))



ggsave(filename = "Desc_Quali_Bi_15_21_23_24.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 35.75,
       height = 17.5,
       units = "cm")



### q32, q34, q37, q42


annotate_figure(ggarrange(lista_gg_bi_quali[[6]],
                          lista_gg_bi_quali[[7]],
                          lista_gg_bi_quali[[8]],
                          lista_gg_bi_quali[[9]]),
                top = text_grob("Região",
                                family = "serif",
                                size = 20),
                left = text_grob("Porcentagem",
                                 family = "serif",
                                 size = 20,
                                 rot = 90))

ggsave(filename = "Desc_Quali_Bi_32_34_37_42.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 35.75,
       height = 17.5,
       units = "cm")


### q41_1a4

annotate_figure(ggarrange(lista_gg_bi_quali[[10]],
                          lista_gg_bi_quali[[11]],
                          lista_gg_bi_quali[[12]],
                          lista_gg_bi_quali[[13]]),
                top = text_grob("Região",
                                family = "serif",
                                size = 20),
                left = text_grob("Porcentagem",
                                 family = "serif",
                                 size = 20,
                                 rot = 90))

ggsave(filename = "Desc_Quali_Bi_41_1_a_4.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 35.75,
       height = 17.5,
       units = "cm")



ggplot(Dados_servicos, aes(x= Região,  group=q15)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")+
  
  facet_grid(~q15) +
  scale_y_continuous(labels = scales::percent,
                     limits=c(0,1))


#############################################
# Univariada das Quanti
#############################################

Dados_servicos_quanti = 
  Dados %>%
  select(Região,
         UF,
         IBGE7,
         q17,q33,q35,q43)


#=============================================
##### Faz o percentual do numero de NA
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(Dados_servicos_quanti,2,pMiss)


######
# Tratamento de Dados
######

Dados_servicos_quanti$Região =
  gsub("Região ","",Dados_servicos_quanti$Região)

#names(Dados_servicos_quanti)


indice_col_quanti =
  c(match("q17",names(Dados_servicos_quanti)),
    match("q33",names(Dados_servicos_quanti)),
    match("q35",names(Dados_servicos_quanti)),
    match("q43",names(Dados_servicos_quanti)))


legenda_quanti=
  c("Número de Pessoas atendidas pelo Serviço em\nDominício para Deficiêntes e Idosos \n",
    "Número de Desabrigados \n \n",
    "Número de imigrantes e/ou\nrefugiados \n",
    "Número de locais realizam\natendimento do Cadastro Único \n")

cores_quanti = 
  c(brewer.pal(n = 4, name = "Dark2"))

lista_fig_quanti=
  list()


for(i in 1:length(indice_col_quanti)){
  
  vetor = 
    na.omit(unlist(Dados_servicos_quanti[,indice_col_quanti[i]]))
  
  bw <- (2 * IQR(vetor))/length(vetor)^(1/3)
  
  box <- 
    ggplot(data=data.frame(vetor),
           aes(x="", y = vetor)) +
    geom_boxplot(fill = cores_quanti[i], 
                 color = "black") + 
    coord_flip() +
    scale_y_continuous(limits = c(min(vetor),
                                  max(vetor)))+
    theme_classic() +
    xlab("") +
    labs(y = legenda_quanti[i], x = "")+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14, 
                              family ="serif"))
  
  
  hist <- 
    ggplot(data=data.frame(vetor)) +
    geom_histogram(aes(x = vetor, y = (..count..)/sum(..count..)),
                   position = "identity", 
                   fill = cores_quanti[i], 
                   color = "black") +
    labs(x = "", y = "")+
    
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14, 
                              family ="serif"))
  
  lista_fig_quanti[[i]]=
    cowplot::plot_grid(hist, 
                       box, 
                       ncol = 1, rel_heights = c(2, 1),
                       align = 'v', axis = 'lr')  
  
}



annotate_figure(ggarrange(lista_fig_quanti[[1]],
                          lista_fig_quanti[[2]],
                          lista_fig_quanti[[3]],
                          lista_fig_quanti[[4]],
                          labels = LETTERS[1:4]),
                left = text_grob("Frequência relativa\n",
                                 family = "serif",
                                 size = 20,
                                 rot = 90),
                right = text_grob(" ",
                                 family = "serif",
                                 size = 20,
                                 rot = 90))

ggsave(filename = "Desc_Quanti_Uni.png", 
       path ="D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 30.75, 
       height = 25.5, 
       units = "cm")

c(match("q17",names(Dados_servicos_quanti)),
  match("q33",names(Dados_servicos_quanti)),
  match("q35",names(Dados_servicos_quanti)),
  match("q43",names(Dados_servicos_quanti)))

xtable(
  matrix(c(names(fun_MedidasDesc(Dados_servicos_quanti$q17)),
           round(fun_MedidasDesc(Dados_servicos_quanti$q17),
                 digits=3),
           round(fun_MedidasDesc(Dados_servicos_quanti$q33),
                 digits=3),
           round(fun_MedidasDesc(Dados_servicos_quanti$q35),
                 digits=3),
           round(fun_MedidasDesc(Dados_servicos_quanti$q43),
                 digits=3)),
         byrow=T,ncol=8))


#############################################
# Bivariada das Quanti
#############################################

### Pegando os codigos das UFs
Dados_servicos_quanti2 = Dados_servicos_quanti

Dados_servicos_quanti2$IBGE7 =
  as.character(Dados_servicos_quanti2$IBGE7) %>%
  substr(1,2)


### Somando dentro de cada UFs  
Dados_servicos_quanti2 =
  Dados_servicos_quanti2 %>%
  group_by(IBGE7) %>%
  summarise(q17_sum  = sum(na.omit(q17)),
            q33_sum  = sum(na.omit(q33)),
            q35_sum  = sum(na.omit(q35)),
            q43_sum  = sum(na.omit(q43)))


### Pegando os estados do brasil 
state_map <- get_brmap(geo = "State",class = "sf")

### Vendo com fica
plot_brmap(state_map)

### Transformando para o mapa
state_map$State = as.character(state_map$State)
state_map_sf <- st_as_sf(state_map) |>
  st_transform(4326) 


state_map_sf <- state_map_sf |>
  filter(State %in% Dados_servicos_quanti2$IBGE7)

state_map_sf <- left_join(state_map_sf,
                          Dados_servicos_quanti2,
                          by=c("State"="IBGE7"))


#####
# Gerando os mapas
#####


ggplot(state_map_sf) + 
  geom_sf(aes(fill=q17_sum))+
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("")+
  labs(fill="Contagem")+
  theme_minimal()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient(low="palegoldenrod", 
                      high="palegreen4",
                      breaks = scales::pretty_breaks(n=5))



ggsave(filename = "Desc_Quanti_Bi_q17.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")




ggplot(state_map_sf) + 
  geom_sf(aes(fill=q33_sum))+
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("")+
  labs(fill="Contagem")+
  theme_minimal()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient(low="palegoldenrod", 
                      high="palegreen4",
                      breaks = scales::pretty_breaks(n=5))



ggsave(filename = "Desc_Quanti_Bi_q33.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")



ggplot(state_map_sf) + 
  geom_sf(aes(fill=q35_sum))+
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("")+
  labs(fill="Contagem")+
  theme_minimal()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient(low="palegoldenrod", 
                      high="palegreen4")

ggsave(filename = "Desc_Quanti_Bi_q35.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")



ggplot(state_map_sf) + 
  geom_sf(aes(fill=q43_sum))+
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("")+
  labs(fill="Contagem")+
  theme_minimal()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient(low="palegoldenrod", 
                      high="palegreen4")


ggsave(filename = "Desc_Quanti_Bi_q43.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto3/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")

