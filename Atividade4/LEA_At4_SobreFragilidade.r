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
    match("educacao",names(Dados_telecom)))

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
# FUncoes
#############################################

somas <-function(grupo, tempo, h.bt, vz, cens, k){

  vz = as.matrix(vz)
  
  ### vz eh matriz das covariaveis
  
  ### k eh numero de variaveis em vz
  
  
  grp <- names(table(grupo))
  g <- dim(table(grupo))
  d.i <- numeric(g)
  T.i <- numeric(g)
  S.i <- numeric(g)
  
  for (i in 1:k) {
    assign(paste0('z', i), numeric(g))
  }
  
  for(i in 1:g){
    linhas <- which(grupo==grp[i])
    d.i[i] <- sum(cens[linhas])
    z1[i] <- mean(unlist(vz[linhas,1]))
    z2[i] <- mean(unlist(vz[linhas,2]))
    z3[i] <- mean(unlist(vz[linhas,3]))
    T.i[i] <- exp(h.bt%*%t(vz[linhas,1:k]))%*%tempo[linhas]
    S.i[i] <- mean(exp(h.bt%*%t(vz[linhas,1:k])))
  }
  list(z1=z1, z2=z2, z3=z3, T.i=T.i, S.i=S.i, d.i=d.i, g=g)
}




#############################################
# Modelando
#############################################

modelo1 = parfm(Surv(clienduracao,churn01)~
                  idade+residduracao+renda, 
                cluster="educacao",
                data=dados, dist="exponential", 
                frailty="gamma")

modelo1

xtable(modelo1,digits=3)

#############################################
# Escrita das fun de risco, sobrevivenca e 
# fragilidade
#############################################

### Escrever no latex a formula

### fazer aqui com a media

## Parâmetros ajustados
h.alf <- 1/modelo1[1]
h.lam <- modelo1[2]
h.bt <- c(modelo1[3], modelo1[4], modelo1[5])

sm <- somas(dados$educacao, 
            dados$clienduracao, 
            h.bt, 
            dados[,c(3,4,5)], 
            dados$churn01, 
            k=3)

ex <- exp(cbind(sm$z1,sm$z2,sm$z3)%*%h.bt)
wi <- (h.alf+sm$d.i)/(h.alf+h.lam*sm$T.i)
round(wi,3)

wi[2]

#####
# Funcao de sobrevivencia
#####

table(dados$educacao)


# grupo == 1 se Inc_Medio
# grupo == 2 se Inc_Superior
# grupo == 3 se Medio
# grupo == 4 se Superior+ 


fun_sobre = function(grupo,t){
  st = exp(-wi[grupo]*h.lam*t*ex[grupo])
  return(st)
}

names(table(dados$educacao))

grafico_surv_estimada = 
  ggplot(data.frame(t = c(0, max(dados$clienduracao) )), 
         aes(x = t)) +
  stat_function(fun = fun_sobre, args = list(grupo=1),
                aes(colour = names(table(dados$educacao))[1]), 
                size = 1.5) +
  stat_function(fun = fun_sobre, args = list(grupo=2),
                aes(colour = names(table(dados$educacao))[2]), 
                size = 1.5) +
  stat_function(fun = fun_sobre, args = list(grupo=3),
                aes(colour = names(table(dados$educacao))[3]), 
                size = 1.5)+
  stat_function(fun = fun_sobre, args = list(grupo=4),
                aes(colour = names(table(dados$educacao))[4]), 
                size = 1.5)+
  scale_x_continuous(name = "Valores do tempo t",
                     breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(name = bquote(hat(S) ~"(t |x)" ),
                     breaks = scales::pretty_breaks(n = 10),
                     limits = c(0,1)) +
  theme_bw()+
  ggtitle("Sobrevivência estimada por grupos") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        legend.position = c(.3,.3))+
  scale_colour_brewer(palette="Dark2") +
  labs(colour = "Educação")

grafico_surv_estimada



# ggsave(filename = "Graf_Sobre_Est_CovMedia.png", 
#        path =
#          "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Atividade4_SobrevivenciaFragilidade/Figuras",  
#        width = 19.75, 
#        height = 12.5, 
#        units = "cm")

#####
# Funcao de risco
#####

fun_risco = function(grupo,t){
  ### lembre que o risco eh constante 
  ## pois a distribuicao eh exponential
  ht = wi[grupo]*h.lam*ex[grupo]
  return(ht)
}

# grupo == 1 se Inc_Medio
# grupo == 2 se Inc_Superior
# grupo == 3 se Medio
# grupo == 4 se Superior+ 


grafico_risco_estimado = 
  ggplot(data.frame(t = c(0, max(dados$clienduracao) )), 
         aes(x = t)) +
  stat_function(fun = fun_risco, args = list(grupo=1),
                aes(colour = names(table(dados$educacao))[1]), 
                size = 2) +
  stat_function(fun = fun_risco, args = list(grupo=2),
                aes(colour = names(table(dados$educacao))[2]), 
                size = 2) +
  stat_function(fun = fun_risco, args = list(grupo=3),
                aes(colour = names(table(dados$educacao))[3]), 
                size = 2)+
  stat_function(fun = fun_risco, args = list(grupo=4),
                aes(colour = names(table(dados$educacao))[4]), 
                size = 2)+
  scale_x_continuous(name = "Valores do tempo t",
                     breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(name = bquote(hat(h) ~"(t |x)" ),
                     breaks = scales::pretty_breaks(n = 10)) +
  theme_bw()+
  ggtitle("Risco estimado por grupos") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        legend.position = "none")+
  scale_colour_brewer(palette="Dark2") +
  labs(colour = "Educação")

grafico_risco_estimado

# ggsave(filename = "Graf_Risco_Est_CovMedia.png", 
#        path =
#          "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Atividade4_SobrevivenciaFragilidade/Figuras",  
#        width = 19.75, 
#        height = 12.5, 
#        units = "cm")



xtable(matrix(c(names(table(dados$educacao)),
                round(c(fun_risco(grupo=1,3),
                        fun_risco(grupo=2,3),
                        fun_risco(grupo=3,3),
                        fun_risco(grupo=4,3)),
                      digits=3)),
              ncol=4,nrow=2,byrow=T),
       digits=3)


ggarrange(grafico_surv_estimada,
          grafico_risco_estimado,
          labels = c("A","B"))

ggsave(filename = "Graf_Risco_e_Sobre_Est_CovMedia.png",
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Atividade4_SobrevivenciaFragilidade/Figuras",
       width = 19.75,
       height = 12.5,
       units = "cm")


#####
# Fragilidade
#####

xtable(matrix(c(names(table(dados$educacao)),
                round(wi,
                      digits=3)),
              ncol=4,nrow=2,byrow=T),
       digits=3)

#############################################
# AIC e BIC
#############################################

### 
AIC(modelo1)
BIC(modelo1)

#############################################
# Fazer km e comparar com o ajustado St
#############################################

#####
# Calculando os KM
#####


nomes_grupos = names(table(dados$educacao))

for(i in 1:length(unique(dados$educacao))){
  
  assign(paste0('ekm1_',i),
         
         survfit(Surv(clienduracao,
                      churn01)~1,
                 data = 
                   dados[dados$educacao==nomes_grupos[i],] ))
  
}


#==================
### Juntando tudo em um data.frame

dados_km1 = data.frame(ekm1_1$surv,
                       ekm1_1$time)


dados_km2 = data.frame(ekm1_2$surv,
                       ekm1_2$time)


dados_km3 = data.frame(ekm1_3$surv,
                       ekm1_3$time)


dados_km4 = data.frame(ekm1_4$surv,
                       ekm1_4$time)

names(dados_km1) = c("Sobre","Tempo")
names(dados_km2) = c("Sobre","Tempo")
names(dados_km3) = c("Sobre","Tempo")
names(dados_km4) = c("Sobre","Tempo")

grafico_surv_KM = 
  
  ggplot(data.frame(t = c(0, max(dados$clienduracao) )), 
         aes(x = t)) +
  
  geom_step(data=dados_km1, 
            mapping=aes(x=Tempo, y=Sobre,
                        colour = nomes_grupos[1]),
            size = 1.2)+
  geom_step(data=dados_km2, 
            mapping=aes(x=Tempo, y=Sobre,
                        colour = nomes_grupos[2]),
            size = 1.2)+
  geom_step(data=dados_km3, 
            mapping=aes(x=Tempo, y=Sobre,
                        colour = nomes_grupos[3]),
            size = 1.2)+
  geom_step(data=dados_km4, 
            mapping=aes(x=Tempo, y=Sobre,
                        colour = nomes_grupos[4]),
            size = 1.2)+
  
  scale_x_continuous(name = "Valores do tempo t",
                     breaks = scales::pretty_breaks(n = 7))+
  scale_y_continuous(name = "KM",
                     breaks = scales::pretty_breaks(n = 10),
                     limits = c(0,1)) +
  
  theme_bw()+
  ggtitle("Kaplan-Meyer\npor grupos") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        legend.position = "none")+
  scale_colour_brewer(palette="Dark2") +
  labs(colour = "Educação")

#grafico_surv_KM




#####
# Calculando as St 
#####


fun_sobre2 = function(grupo,t){
  St <- exp(-wi[grupo]*h.lam*sm$S.i[i]*t)
  return(St)
}


grafico_surv2_estimada = 
  ggplot(data.frame(t = c(0, max(dados$clienduracao) )), 
         aes(x = t)) +
  stat_function(fun = fun_sobre, args = list(grupo=1),
                aes(colour = names(table(dados$educacao))[1]), 
                size = 1.2) +
  stat_function(fun = fun_sobre, args = list(grupo=2),
                aes(colour = names(table(dados$educacao))[2]), 
                size = 1.2) +
  stat_function(fun = fun_sobre, args = list(grupo=3),
                aes(colour = names(table(dados$educacao))[3]), 
                size = 1.2)+
  stat_function(fun = fun_sobre, args = list(grupo=4),
                aes(colour = names(table(dados$educacao))[4]), 
                size = 1.2)+
  scale_x_continuous(name = "Valores do tempo t",
                     breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(name = bquote(hat(S) ~"(t |x)" ),
                     breaks = scales::pretty_breaks(n = 10),
                     limits = c(0,1)) +
  theme_bw()+
  ggtitle("Sobrevivência estimada\npor grupos") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        legend.position = c(.25,.3))+
  scale_colour_brewer(palette="Dark2") +
  labs(colour = "Educação")

#grafico_surv2_estimada


ggarrange(grafico_surv2_estimada,
          grafico_surv_KM,
          labels = c("A","B"))

ggsave(filename = "Graf_Sobre_Surv_e_KM.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Atividade4_SobrevivenciaFragilidade/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")








#####
# Individual
#####

grafico_surv2_km_estimado1 = 
  ggplot(data.frame(t = c(0, max(dados$clienduracao) )), 
         aes(x = t)) +
  
  geom_step(data=dados_km1, 
            mapping=aes(x=Tempo, y=Sobre,
                        colour = "KM"),
            size = 1.2)+
  
  stat_function(fun = fun_sobre, args = list(grupo=1),
                aes(colour = "Curva ajustada"), 
                size = 1.2,
                alpha=0.7) +
  
  scale_x_continuous(name = "Valores do tempo t",
                     breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(name = bquote(hat(S) ~"(t |x)" ),
                     breaks = scales::pretty_breaks(n = 5),
                     limits = c(0,1)) +
  theme_bw()+
  ggtitle("Sobrevivência estimada\n e KM no Ensino Médio Incompleto") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        legend.position = "none")+
  scale_colour_brewer(palette="Dark2") +
  labs(colour = "Método")

  



grafico_surv2_km_estimado2 = 
  ggplot(data.frame(t = c(0, max(dados$clienduracao) )), 
         aes(x = t)) +
  
  geom_step(data=dados_km2, 
            mapping=aes(x=Tempo, y=Sobre,
                        colour = "KM"),
            size = 1.2)+
  
  stat_function(fun = fun_sobre, args = list(grupo=2),
                aes(colour = "Curva ajustada"), 
                size = 1.2,
                alpha=0.7) +
  
  scale_x_continuous(name = "Valores do tempo t",
                     breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(name = bquote(hat(S) ~"(t |x)" ),
                     breaks = scales::pretty_breaks(n = 5),
                     limits = c(0,1)) +
  theme_bw()+
  ggtitle("Sobrevivência estimada\n e KM no Ensino Superior Incompleto") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        legend.position = "none")+
  scale_colour_brewer(palette="Dark2") +
  labs(colour = "Método")






grafico_surv2_km_estimado3 = 
  ggplot(data.frame(t = c(0, max(dados$clienduracao) )), 
         aes(x = t)) +
  
  geom_step(data=dados_km3, 
            mapping=aes(x=Tempo, y=Sobre,
                        colour = "KM"),
            size = 1.2)+
  
  stat_function(fun = fun_sobre, args = list(grupo=3),
                aes(colour = "Curva ajustada"), 
                size = 1.2,
                alpha=0.7) +
  
  scale_x_continuous(name = "Valores do tempo t",
                     breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(name = bquote(hat(S) ~"(t |x)" ),
                     breaks = scales::pretty_breaks(n = 5),
                     limits = c(0,1)) +
  theme_bw()+
  ggtitle("Sobrevivência estimada\n e KM no Ensino Médio Completo") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        legend.position = "none")+
  scale_colour_brewer(palette="Dark2") +
  labs(colour = "Método")





grafico_surv2_km_estimado4 = 
  ggplot(data.frame(t = c(0, max(dados$clienduracao) )), 
         aes(x = t)) +
  
  geom_step(data=dados_km4, 
            mapping=aes(x=Tempo, y=Sobre,
                        colour = "KM"),
            size = 1.2)+
  
  stat_function(fun = fun_sobre, args = list(grupo=4),
                aes(colour = "Curva ajustada"), 
                size = 1.2,
                alpha=0.7) +
  
  scale_x_continuous(name = "Valores do tempo t",
                     breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(name = bquote(hat(S) ~"(t |x)" ),
                     breaks = scales::pretty_breaks(n = 5),
                     limits = c(0,1)) +
  theme_bw()+
  ggtitle("Sobrevivência estimada\n e KM no Ensino Supeior Completo e +") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14, 
                            family ="serif"),
        legend.position = "none")+
  scale_colour_brewer(palette="Dark2") +
  labs(colour = "Método")




ggarrange(grafico_surv2_km_estimado1,
          grafico_surv2_km_estimado2,
          grafico_surv2_km_estimado3,
          grafico_surv2_km_estimado4,
          labels = LETTERS[1:4])



ggsave(filename = "Graf_Sobre_Surv_e_KM_por_grupo.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Atividade4_SobrevivenciaFragilidade/Figuras",  
       width = 22.75, 
       height = 15.5,
       units = "cm")



#####
# Individual com zomm
#####

ggarrange(grafico_surv2_km_estimado1+
            coord_cartesian(ylim=c(0.5, 1)),
          grafico_surv2_km_estimado2+
            coord_cartesian(ylim=c(0.5, 1)),
          grafico_surv2_km_estimado3+
            coord_cartesian(ylim=c(0.5, 1)),
          grafico_surv2_km_estimado4+
            coord_cartesian(ylim=c(0.5, 1)),
          labels = LETTERS[1:4])

ggsave(filename = "Graf_Sobre_Surv_e_KM_por_grupo_com_zoom.png", 
       path =
         "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Atividade4_SobrevivenciaFragilidade/Figuras",  
       width = 22.75, 
       height = 15.5, 
       units = "cm")
