#############################################
# Pacores
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
if(!require(GDAtools)){install.packages("GDAtools")}

### Modelos
if(!require(MASS)){install.packages("MASS")}
library(MASS)
if(!require(ordinal)){install.packages("ordinal")}
library(ordinal)
if(!require(effects)){install.packages("effects")}
library("effects")
if(!require(sure)){install.packages("sure")}
library(sure)
if(!require(ggiraphExtra)){install.packages("ggiraphExtra")}
library(ggiraphExtra)
if(!require(ggeffects)){install.packages("ggeffects")}
library(ggeffects)


### Teste
if(!require(corrplot)){install.packages("corrplot")}
library(corrplot)


### To Latex
if(!require(xtable)){install.packages("xtable")}
library(xtable)



#############################################
# Tratamento dos dados
#############################################

library(readxl)
D21 <- 
  read_excel("D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/D21.xlsx")

dados = D21
dados_m = melt(dados,
               id=c("Pol","Polt","Tab"))

dados_geral = data.frame()

for(i in 1:nrow(dados_m)){
  for(j in 1:dados_m[i,5]){
    dados_geral = rbind(dados_geral,
                        dados_m[i,1:4])
    
  }
}

names(dados_geral) = 
  c("PoluicaoAr",
    "PoluicaoTrab",
    "Fuma",
    "Sintoma")

dados_geral$PoluicaoAr <-
  factor(dados_geral$PoluicaoAr, 
         levels=c("B", "A"),
         labels=c("Baixa","Alta"))

dados_geral$PoluicaoTrab <-
  factor(dados_geral$PoluicaoTrab, 
         levels=c("N", "S"),
         labels=c("Nao","Sim"))

dados_geral$Fuma <-
  factor(dados_geral$Fuma, 
         levels=c("E", "N", "S"),
         labels=c("Ex","Nao","Sim"))

dados_geral$Sintoma <-
  factor(dados_geral$Sintoma,
         ordered = T)


#############################################
# Modelos completos e AIC
#############################################

lista_mod1 = list()

ligacoes = c("logit", "probit", "cloglog", "cauchit", 
             "Aranda-Ordaz", "log-gamma")

### Ligacao Aranda-Ordaz e Log-gamma nao convergiram

for(i in 1:(length(ligacoes))){
  lista_mod1[[i]]=clm(Sintoma ~ (PoluicaoAr + PoluicaoTrab + Fuma)**3,
                      data=dados_geral,
                      link=ligacoes[i],
                      control = list( maxIter = 500L, gradTol = 1e-06))
}

clm(Sintoma ~ (PoluicaoAr + PoluicaoTrab + Fuma)**3,
    data=dados_geral,
    link=ligacoes[6])


vetor_AIC_1 = c()
for(i in 1:(length(ligacoes))){
  vetor_AIC_1[i] = round(AIC(lista_mod1[[i]]),digits=2)
}

matrix(c(ligacoes[1:6],vetor_AIC_1),ncol=2,byrow=F)

xtable(matrix(c(ligacoes[1:6],vetor_AIC_1),ncol=2,byrow=F))

xtable(matrix(c(ligacoes[-4],vetor_AIC_1[-4]),ncol=2,byrow=F))

#####
# Analisando modelo probito
#####

summary(lista_mod1[[1]])

anova(lista_mod1[[2]],type=3)
anova(lista_mod1[[2]],type=2)
anova(lista_mod1[[2]],type=1)

#####
# Analisando modelo cauchit
#####

summary(lista_mod1[[4]])

anova(lista_mod1[[5]],type=3)
anova(lista_mod1[[4]],type=2)
anova(lista_mod1[[4]],type=1)

coef(summary(lista_mod1[[4]]))

### probit e cauchit dando igual

#####
# Analisando modelo logistico
#####

summary(lista_mod1[[1]])

anova(lista_mod1[[1]],type=3)
anova(lista_mod1[[1]],type=2)
anova(lista_mod1[[1]],type=1)

coef(summary(lista_mod1[[1]]))


#####
# Para o latex
#####

coef_geral = 
  round(cbind(coef(summary(lista_mod1[[1]]))[,c(4)],
              coef(summary(lista_mod1[[2]]))[,c(4)],
              coef(summary(lista_mod1[[4]]))[,c(4)],
              coef(summary(lista_mod1[[5]]))[,c(4)],
              coef(summary(lista_mod1[[6]]))[,c(4)]),digits=3)

coef_sig = formatC(cbind(coef(summary(lista_mod1[[1]]))[c(1:3,5,7),c(4)],
                         coef(summary(lista_mod1[[2]]))[c(1:3,5,7),c(4)],
                         coef(summary(lista_mod1[[4]]))[c(1:3,5,7),c(4)],
                         coef(summary(lista_mod1[[5]]))[c(1:3,5,7),c(4)],
                         coef(summary(lista_mod1[[6]]))[c(1:3,5,7),c(4)]),
                   format='e',digits = 2)

coef_geral[c(1:3,5,7),] = coef_sig

coef_geral

xtable(coef_geral)

#############################################
# Modelos reduzidos e AIC
#############################################

lista_mod1.1 = list()

for(i in 1:(length(ligacoes))){
  lista_mod1.1[[i]] = stepAIC(lista_mod1[[i]])
}


vetor_AIC_1.1 = c()
for(i in 1:(length(ligacoes))){
  vetor_AIC_1.1[i] = round(AIC(lista_mod1.1[[i]]),digits=2)
}


matrix(c(ligacoes,vetor_AIC_1,
         vetor_AIC_1.1),ncol=3,byrow=F)

xtable(matrix(c(ligacoes[-4],vetor_AIC_1[-4],
                vetor_AIC_1.1[-4]),ncol=3,byrow=F),ncol=2,byrow=F)




#####
# Analisando modelo probito
#####

summary(lista_mod1.1[[6]])

anova(lista_mod1.1[[3]],type=3)
anova(lista_mod1.1[[3]],type=2)
anova(lista_mod1.1[[3]],type=1)

coef(summary(lista_mod1.1[[3]]))


#####
# Analisando modelo logistico
#####

summary(lista_mod1.1[[1]])

anova(lista_mod1.1[[1]],type=3)
anova(lista_mod1.1[[1]],type=2)
anova(lista_mod1.1[[1]],type=1)

coef(summary(lista_mod1.1[[6]]))

#####
# Para o latex
####

coef_geral1.1 = 
  formatC(cbind(coef(summary(lista_mod1.1[[1]]))[,c(4)],
              coef(summary(lista_mod1.1[[2]]))[,c(4)],
              coef(summary(lista_mod1.1[[4]]))[,c(4)],
              coef(summary(lista_mod1.1[[5]]))[,c(4)],
              coef(summary(lista_mod1.1[[6]]))[,c(4)]),
          format='e',digits = 2)


xtable(coef_geral1.1)

#####
# Para o latex
####


#############################################
# Teste de Brant
#############################################

nominal_test(lista_mod1.1[[1]])
nominal_test(lista_mod1.1[[2]])
rownames(nominal_test(lista_mod1.1[[2]]))[-1]

cbind(rownames(nominal_test(lista_mod1.1[[2]]))[-1],
  round(cbind(nominal_test(lista_mod1.1[[1]])[-1,5],
              nominal_test(lista_mod1.1[[2]])[-1,5]),digits=2))

xtable(cbind(rownames(nominal_test(lista_mod1.1[[2]]))[-1],
             round(cbind(nominal_test(lista_mod1.1[[1]])[-1,5],
                         nominal_test(lista_mod1.1[[2]])[-1,5]),digits=2)))

scale_test(lista_mod1.1[[1]])
scale_test(lista_mod1.1[[2]])

xtable(cbind(rownames(scale_test(lista_mod1.1[[2]]))[-1],
             round(cbind(scale_test(lista_mod1.1[[1]])[-1,5],
                         scale_test(lista_mod1.1[[2]])[-1,5]),digits=2)))

mod_1.1.1.1 = clm(Sintoma~PoluicaoTrab,
                  nominal=~Fuma,
                  data=dados_geral,
                  link="logit")

mod_1.1.2 = lista_mod1.1[[2]]














#############################################
# Residuos
#############################################



#####
# Logito
#####

set.seed(1735,
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion",
         sample.kind = "Rounding")


fig_res_1.1.1.1_fuma=
  sure::autoplot.clm(mod_1.1.1.1, 
                     nsim = 10, 
                     what = "covariate",
                     x=dados_geral$Fuma)+
  xlab("Fuma") + 
  ylab("Resíduos substitutos") +
  theme_bw()+
  ggtitle("")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14))


set.seed(1735,
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion",
         sample.kind = "Rounding")
fig_res_1.1.1.1_trab=
  sure::autoplot.clm(mod_1.1.1.1, 
                     nsim = 10, 
                     what = "covariate",
                     x=dados_geral$PoluicaoTrab)+
  xlab("Poluição no Trabalho") + 
  ylab("Resíduos substitutos") +
  theme_bw()+
  ggtitle("")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14))


set.seed(1735,
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion",
         sample.kind = "Rounding")


fig_res_1.1.1.1_qq=
  ggplot(data=data.frame(residuo=sure::resids(mod_1.1.1.1)),
         aes(sample=residuo))+
  stat_qq_point(size=3,shape=1)+
  stat_qq_line(color="red",linetype="dashed")+
  theme_bw()+
  labs(title = "",
       x = "Quantis teóricos",
       y = "Quantis amostrais") +
  theme( plot.title = element_text(hjust = 0.5),
         text = element_text(size = 14, 
                             family ="serif")) 


set.seed(1735,
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion",
         sample.kind = "Rounding")
fig_res_1.1.1.1_fitted=
  sure::autoplot.clm(mod_1.1.1.1, 
                     nsim = 10, 
                     what = "fitted")+
  xlab("Predito") + 
  ylab("Resíduos substitutos") +
  theme_bw()+
  ggtitle("")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



ggarrange(fig_res_1.1.1.1_trab,
          fig_res_1.1.1.1_fuma,
          fig_res_1.1.1.1_qq,
          labels = c("A","B","C"))


ggsave(filename = "Mod_1.1.1.1_Res.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")






#####
# Probito
#####

set.seed(1735,
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion",
         sample.kind = "Rounding")


fig_res_1.1.2_fuma=
sure::autoplot.clm(mod_1.1.2, 
                   nsim = 10, 
                   what = "covariate",
                   x=dados_geral$Fuma)+
  xlab("Fuma") + 
  ylab("Resíduos substitutos") +
  theme_bw()+
  ggtitle("")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14))


set.seed(1735,
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion",
         sample.kind = "Rounding")
fig_res_1.1.2_trab=
  sure::autoplot.clm(mod_1.1.2, 
                     nsim = 10, 
                     what = "covariate",
                     x=dados_geral$PoluicaoTrab)+
  xlab("Poluição no Trabalho") + 
  ylab("Resíduos substitutos") +
  theme_bw()+
  ggtitle("")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14))


set.seed(1735,
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion",
         sample.kind = "Rounding")


fig_res_1.1.2_qq=
ggplot(data=data.frame(residuo=sure::resids(mod_1.1.2)),
       aes(sample=residuo))+
  stat_qq_point(size=3,shape=1)+
  stat_qq_line(color="red",linetype="dashed")+
  theme_bw()+
  labs(title = "",
       x = "Quantis teóricos",
       y = "Quantis amostrais") +
  theme( plot.title = element_text(hjust = 0.5),
         text = element_text(size = 14, 
                             family ="serif")) 


set.seed(1735,
         kind = "Mersenne-Twister", 
         normal.kind = "Inversion",
         sample.kind = "Rounding")
fig_res_1.1.2_fitted=
  sure::autoplot.clm(mod_1.1.2, 
                     nsim = 10, 
                     what = "fitted")+
  xlab("Predito") + 
  ylab("Resíduos substitutos") +
  theme_bw()+
  ggtitle("")+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


ggarrange(fig_res_1.1.2_trab,
          fig_res_1.1.2_fuma,
          fig_res_1.1.2_qq,
          labels = c("A","B","C"))

ggsave(filename = "Mod_1.1.2_Res.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


#############################################
# Analise dos modelos escolhidos
#############################################



#####
# Logit
#####

xtable(coef(summary(mod_1.1.1.1))[,-3])

coef(summary(mod_1.1.1.1))[4,-3]

predito_1.1.1.1=
  ggpredict(mod_1.1.1.1,
            terms=c("PoluicaoTrab","Fuma"))


# dados_novos =
#   data.frame(PoluicaoTrab=
#                c(rep("Nao",3),
#                  rep("Sim",3)),
#              Fuma=c("Ex","Nao","Sim",
#                     "Ex","Nao","Sim")) %>%
#   mutate(PoluicaoTrab = as.factor(PoluicaoTrab),
#          Fuma = as.factor(Fuma))
# 
# dados_novos
# 
# predict(mod_1.1.1.1,type="p",
#         newdata = dados_novos,
#         interval = T)



colnames(predito_1.1.1.1)[c(1,5,6)]=
  c("PoluicaoTrab","Sintoma","Fuma")


predito_1.1.1.1$PoluicaoTrab=
  factor(predito_1.1.1.1$PoluicaoTrab)

predito_1.1.1.1$Fuma=
  factor(predito_1.1.1.1$Fuma)

predito_1.1.1.1$Sintoma=
  factor(predito_1.1.1.1$Sintoma,
         levels=c(1:4),
         labels = c("S1","S2","S3","S4"))

ggplot(predito_1.1.1.1, 
       aes(x = Sintoma, y = predicted)) + 
  geom_point(aes(color = PoluicaoTrab), 
             position = position_dodge(width = 0.5),
             size=2) + 
  geom_errorbar(aes(ymin = conf.low, 
                    ymax = conf.high, 
                    color = PoluicaoTrab), 
                position = position_dodge(width = 0.5), 
                width = 0.5,
                size=0.8) +
  scale_y_continuous(limits=c(0,1),
                     breaks = scales::pretty_breaks(n = 10))+
  facet_wrap(~Fuma) +
  xlab("Sintoma") + 
  ylab("Probabilidade") +
  ggtitle("Fuma")+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))


ggsave(filename = "Predito_1.1.1.1.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


xtable(dcast(PoluicaoTrab+Fuma~Sintoma,
             data = data.frame(predito_1.1.1.1)[,-c(3,4)],
             value.var="predicted"))








#####
# Probit
#####

xtable(coef(summary(mod_1.1.2))[,-3])





# dados_novos = 
#   data.frame(PoluicaoTrab=
#                c(rep("Nao",3),
#                  rep("Sim",3)),
#              Fuma=c("Ex","Nao","Sim",
#                     "Ex","Nao","Sim")) %>%
#   mutate(PoluicaoTrab = as.factor(PoluicaoTrab),
#          Fuma = as.factor(Fuma))
# 
# dados_novos
# 
# predict(mod_1.1.2,type="p",
#         newdata = dados_novos,
#         interval = T)




predito_1.1.2=
ggpredict(mod_1.1.2,
          terms=c("PoluicaoTrab","Fuma"))

colnames(predito_1.1.2)[c(1,5,6)]=
  c("PoluicaoTrab","Sintoma","Fuma")


predito_1.1.2$PoluicaoTrab=
  factor(predito_1.1.2$PoluicaoTrab)

predito_1.1.2$Fuma=
  factor(predito_1.1.2$Fuma)

predito_1.1.2$Sintoma=
  factor(predito_1.1.2$Sintoma,
         levels=c(1:4),
         labels = c("S1","S2","S3","S4"))

ggplot(predito_1.1.2, 
       aes(x = Sintoma, y = predicted)) + 
  geom_point(aes(color = PoluicaoTrab), 
             position = position_dodge(width = 0.5),
             size=2) + 
  geom_errorbar(aes(ymin = conf.low, 
                    ymax = conf.high, 
                    color = PoluicaoTrab), 
                position = position_dodge(width = 0.5), 
                width = 0.5,
                size=0.8) +
  scale_y_continuous(limits=c(0,1),
                     breaks = scales::pretty_breaks(n = 10))+
  facet_wrap(~Fuma) +
  xlab("Sintoma") + 
  ylab("Probabilidade") +
  ggtitle("Fuma")+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))


ggsave(filename = "Predito_1.1.2.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


xtable(dcast(PoluicaoTrab+Fuma~Sintoma,
             data = data.frame(predito_1.1.2)[,-c(3,4)],
             value.var="predicted"))





#####
# Colocando os dois lado a lado
#####



rbind(dcast(PoluicaoTrab+Fuma~Sintoma,
            data = data.frame(predito_1.1.1.1)[,-c(3,4)],
            value.var="predicted"),
      
      dcast(PoluicaoTrab+Fuma~Sintoma,
            data = data.frame(predito_1.1.2)[,-c(3,4)],
            value.var="predicted"))




xtable(rbind(dcast(PoluicaoTrab+Fuma~Sintoma,
                   data = data.frame(predito_1.1.1.1)[,-c(3,4)],
                   value.var="predicted"),
             
             dcast(PoluicaoTrab+Fuma~Sintoma,
                   data = data.frame(predito_1.1.2)[,-c(3,4)],
                   value.var="predicted")),
       digits=3)



