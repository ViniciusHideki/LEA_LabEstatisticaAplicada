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

### trat dados
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(reshape2)){install.packages("reshape2")}
library(reshape2)
if(!require(GDAtools)){install.packages("GDAtools")}

### Teste
if(!require(coin)){install.packages("coin")}
library(coin)
if(!require(FactoMineR)){install.packages("FactoMineR")}
library(FactoMineR)
if(!require(factoextra)){install.packages("factoextra")}
library(factoextra)
if(!require(corrplot)){install.packages("corrplot")}
library(corrplot)

### To Latex
if(!require(xtable)){install.packages("xtable")}
library(xtable)

#############################################
# Tratamento dos dados
#############################################


library(readxl)
D21 <- read_excel("D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/D21.xlsx")

dados = D21
dados_m = melt(dados,
               id=c("Pol","Polt","Tab"))

dados_geral = data.frame()

for(i in 1:nrow(dados_m)){
  if(dados_m[i,5]!=0){
    for(j in 1:dados_m[i,5]){
      dados_geral = rbind(dados_geral,
                          dados_m[i,1:4])
      
    }
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
  factor(dados_geral$Sintoma)


#library(readr)
#write_csv(dados_geral,
#          file="D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/LEA_Dados_P2_novo.csv")

#############################################
# Descritiva Univariada
#############################################

head(dados_geral)

manipula_dados_bar =function(variavel){
  data.frame(Nome=names(table(variavel)),
             Freq=paste(table(variavel)),
             Porc=paste0(round(prop.table(table(variavel)),
                               digits=3)*100,"%"),
             Perc=as.numeric(round(prop.table(table(variavel)),
                                   digits=3)))
}

### PoluicaoAr

dados_bar = manipula_dados_bar(dados_geral$PoluicaoAr)
  
g_bar_PopAr=
ggplot(dados_bar,aes(x=Nome,y=Perc,fill=Nome))+
  geom_bar(stat = "identity", color = "black")+
  
  geom_text(label = with(dados_bar, paste(Freq, paste0('(', Porc, ')'))), vjust=-1)+
  scale_y_continuous(limits=c(0,1),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("Poluição no Ar \n") + 
  ylab("") +
  ggtitle("")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")

g_bar_PopAr



### PoluicaoTrab

dados_bar = manipula_dados_bar(dados_geral$PoluicaoTrab)

g_bar_PopTrab=
  ggplot(dados_bar,aes(x=Nome,y=Perc,fill=Nome))+
  geom_bar(stat = "identity", color = "black")+
  
  geom_text(label = with(dados_bar, paste(Freq, paste0('(', Porc, ')'))), vjust=-1)+
  scale_y_continuous(limits=c(0,1),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("Poluição no Trabalho \n") + 
  ylab("") +
  ggtitle("")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")


### Fuma

dados_bar = manipula_dados_bar(dados_geral$Fuma)

g_bar_Fuma=
  ggplot(dados_bar,aes(x=Nome,y=Perc,fill=Nome))+
  geom_bar(stat = "identity", color = "black")+
  
  geom_text(label = with(dados_bar, paste(Freq, paste0('(', Porc, ')'))), vjust=-1)+
  scale_y_continuous(limits=c(0,1),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("Fumante \n") + 
  ylab("") +
  ggtitle("")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")


### Sintomas

dados_bar = manipula_dados_bar(dados_geral$Sintoma)

g_bar_Sintoma=
  ggplot(dados_bar,aes(x=Nome,y=Perc,fill=Nome))+
  geom_bar(stat = "identity", color = "black")+
  
  geom_text(label = with(dados_bar, paste(paste0('(', Porc, ')'))), vjust=-3)+
  geom_text(label = with(dados_bar, paste(paste0( Freq ))), 
            vjust=-1)+
  
  scale_y_continuous(limits=c(0,1),
                     breaks = scales::pretty_breaks(n = 5),
                     labels = scales::percent)+
  
  xlab("Sintomas \n") + 
  ylab("") +
  ggtitle("")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")


g_bar_Sintoma


### Juntando tudo

annotate_figure(ggarrange(g_bar_PopAr,g_bar_PopTrab,
                          g_bar_Fuma,g_bar_Sintoma)+ 
                  theme(legend.position = "none",
                        text = element_text(size = 14, 
                                            family ="serif"),
                        panel.background = element_rect(fill = 'white',colour = 'white')),
                left = text_grob("Frequência relativa",
                                 family = "serif",
                                 size = 15,
                                 rot = 90),
                fig.lab.face = "bold")


ggsave(filename = "Desc_Barplots.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")



#############################################
# Descritiva Bivariada
#############################################

#####
# Burt geral
#####

GDAtools::burt(dados_geral)
xtable(GDAtools::burt(dados_geral))

#####
# Chi quadrado e Fisher entre covariaves
#####

valores_p_Ar_Trab=
  c(chisq.test(dados_geral$PoluicaoAr,
               dados_geral$PoluicaoTrab,
               correct=F)$p.value,
    
    fisher.test(dados_geral$PoluicaoAr,
                dados_geral$PoluicaoTrab)$p.value)

valores_p_Ar_Fuma = 
  c(chisq.test(dados_geral$PoluicaoAr,
               dados_geral$Fuma,
               correct=F)$p.value,
    
    fisher.test(dados_geral$PoluicaoAr,
                dados_geral$Fuma)$p.value)

valores_p_Trab_Fuma=
  c(chisq.test(dados_geral$PoluicaoTrab,
               dados_geral$Fuma,
               correct=F)$p.value,
    
    fisher.test(dados_geral$PoluicaoTrab,
                dados_geral$Fuma)$p.value)


valores_p_Ar_Trab
valores_p_Ar_Fuma
valores_p_Trab_Fuma

valores_p_Ar_Trab=formatC(valores_p_Ar_Trab,format = 'e',digits=2)
valores_p_Ar_Fuma=format(round(valores_p_Ar_Fuma,digits=3),scientific = F)
valores_p_Trab_Fuma= formatC(valores_p_Trab_Fuma,format = 'e',digits=2)

mat_valores_p=
  matrix(c(valores_p_Ar_Trab,
           valores_p_Ar_Fuma,
           valores_p_Trab_Fuma),
         byrow=T,ncol=2)

rownames(mat_valores_p) = 
  c("Poluição no Ar e Poluição no Trabalho",
    "Poluição no Ar e Fumante",
    "Poluição no Trabalho e Fumante")

colnames(mat_valores_p)=
  c("Chi-Quadrado","Exato de Fisher")

mat_valores_p
xtable(mat_valores_p)

### Coeficiente de 

calc_coef_corr = function(tabela){
  mat = as.matrix(tabela)
  
  q=min(ncol(mat),nrow(mat))
  estat_chi2 = chisq.test(mat)$statistic
  n = sum(mat)
  
  R1 = estat_chi2/(n*(q-1))
  R2 = sqrt(estat_chi2/(estat_chi2+n))
  R3 = estat_chi2/n
  
  max_R2 = sqrt((q-1)/q)
  max_R3 = q-1

  matrix(c(R1,R2,R3,1,max_R2,max_R3),
         ncol=2,byrow=F)
}

calc_coef_corr(table(dados_geral$PoluicaoAr,
                     dados_geral$PoluicaoTrab))


mat_coef_covariaveis=
  cbind(calc_coef_corr(table(dados_geral$PoluicaoAr,
                             dados_geral$PoluicaoTrab)),
        calc_coef_corr(table(dados_geral$PoluicaoAr,
                             dados_geral$Fuma)),
        calc_coef_corr(table(dados_geral$PoluicaoTrab,
                             dados_geral$Fuma)))


mat_coef_covariaveis2 = mat_coef_covariaveis[c(1,2),-c(2,4,6)]
mat_coef_covariaveis2
row.names(mat_coef_covariaveis2)=c("Cramér",
                                   "Pearson")
colnames(mat_coef_covariaveis2)=  c("Poluição no Ar e Poluição no Trabalho",
                                    "Poluição no Ar e Fumante",
                                    "Poluição no Trabalho e Fumante")


t(mat_coef_covariaveis2)

xtable(t(mat_coef_covariaveis2),digits=3)




#####
# ChiQuadrado Ordinal da resposta com covariaveis
#####

str(dados_geral$Sintoma)

aa=chisq_test(Sintoma~PoluicaoAr,data=dados_geral,
              scores=list("Sintoma"=c(0,1,2,3)))

pvalue(aa)

coin::chisq_test()


valores_p_Sintoma=
  c(pvalue(chisq_test(Sintoma~PoluicaoAr,data=dados_geral,
                      scores=list("Sintoma"=c(1,2,3,4)))),
    pvalue(chisq_test(Sintoma~PoluicaoTrab,data=dados_geral,
                      scores=list("Sintoma"=c(1,2,3,4)))),
    pvalue(chisq_test(Sintoma~Fuma,data=dados_geral,
                      scores=list("Sintoma"=c(1,2,3,4)))) )

valores_p_Sintoma

matrix(c(format(round(valores_p_Sintoma[1],digits=3),scientific = F),
         formatC(valores_p_Sintoma[2],format = 'e',digits=2),
         formatC(valores_p_Sintoma[3],format = 'e',digits=2)))

xtable(matrix(c(format(round(valores_p_Sintoma[1],digits=3),scientific = F),
                formatC(valores_p_Sintoma[2],format = 'e',digits=2),
                "2.2e-16"),
              byrow=T,ncol=1,
              dimnames = 
                list("Valor-p"=c("Sintoma e Poluição no Ar",
                                 "Sintoma e Poluição no Trabalho",
                                 "Sintoma e Fumar") )))










#####
# AF Multipla de Correspondencia
#####

dados_geral2 = dados_geral

dados_geral2$PoluicaoAr =
  factor(dados_geral2$PoluicaoAr, 
         levels=c("Baixa", "Alta"),
         labels=c("PoluicaoAr_Baixa","PoluicaoAr_Alta"))
  

res.mca <- MCA(dados_geral2, graph = FALSE,quali.sup = 4)

# Autovalores
eig.val <- get_eigenvalue(res.mca);
eig.val

eig.val2 = 
  data.frame("Autovalor"=round(get_eigenvalue(res.mca)[,1],digits=2),
             "Dimensao"=paste(c(1:4)) )
eig.val2

ggplot(eig.val2,aes(x=Dimensao,y=Autovalor))+
  geom_bar(stat = "identity", color = "white",fill="steelblue")+
  
  geom_text(label = with(eig.val2, Autovalor), vjust=-1.5,size=4.5)+
  scale_y_continuous(limits=c(0,0.45),
                     breaks = scales::pretty_breaks(n = 5))+
  geom_line(group=1,size=1.1,alpha=0.5) +
  geom_point(size=4)+
  xlab("Dimensão") + 
  ylab("Autorvalor") +
  ggtitle("")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw()+
  theme(text = element_text(size = 14, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none")


ggsave(filename = "Desc_MCA_ScreeplotEigen.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


# Resultados para as variÃ¡veis
var <- get_mca_var(res.mca)

#===============================================
# Qualidade de representacao
#===============================================
var <- get_mca_var(res.mca)

ggplot(data = melt(var$cos2), aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  labs(x = "", y = "", fill = "cos2") +
  geom_text(aes(x = Var2, y = Var1, label = round(value, 2)), color = "black", 
            fontface = "bold", size = 5)+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(9,"Blues")[-c(1,7:9)])+
  theme(text = element_text(size = 16, 
                            family ="serif"),
        plot.title = element_text(hjust = 0.5))

ggsave(filename = "Desc_MCA_Co2_Dims.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")



#===============================================
# Plano 1-2
#===============================================

### Correlcao

set.seed(10)
fviz_mca_var(res.mca, choice = "mca.cor", repel = TRUE,
             axes = c(1,2),
             ggtheme = theme_bw(),
             col.quanti.sup = "firebrick3",
             col.var = "dodgerblue3")+
  labs(title="Plano 1-2")+
  theme(plot.title=element_text(hjust=0.5, 
                                size = 15, 
                                family ="serif"),
        text = element_text(size=13, family ="serif"))


ggsave(filename = "Desc_MCA_Corr_12.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


### Plano

set.seed(10)
fviz_mca_var(res.mca, repel = TRUE,
             axes = 1:2,
             ggtheme = theme_bw(),
             col.quanti.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="Plano 1-2")


ggsave(filename = "Desc_MCA_Plano_12.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")


set.seed(12)
set.seed(10)
fviz_mca_var(res.mca, repel = TRUE,
             axes = c(1,3),
             ggtheme = theme_bw(),
             col.quanti.sup = "firebrick3",
             col.var = "dodgerblue3",
             labelsize = 4)+
  theme(plot.title=element_text(hjust=0.5, family ="serif"),
        text = element_text(size = 14,family ="serif"))+
  labs(title="Plano 1-3")


ggsave(filename = "Desc_MCA_Plano_13.png", 
       path = "D:/Mega/ufscar_mega/Ufscar/8° ENPE 4/LEA/Projeto2/Figuras",  
       width = 19.75, 
       height = 12.5, 
       units = "cm")

