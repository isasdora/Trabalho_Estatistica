#carregando os dados
rm(list=ls(all=T))

library(readxl)
library(tidyverse)
library(ggplot2)

filme <- read_excel("Campoes_Bilheteria.xlsx")
attach(filme)

#################################DESCRITIVA#####################################

#medidas
(medias <- tapply(`Audiencia %`, Animação, mean)) #média para cada filme
(desvio <- tapply(`Audiencia %`, Animação, sd)) #desvio padrão para cada filme
(cv <- desvio/medias*100) #coeficiente de variação para cada filme


  #Tabela de frequencia
ni <- table(filme$Estudio)   #frequen. absoluta
ni

fi <- 100*prop.table(ni)  #frequen. relativa
fi

Fi <- cumsum(fi)   #frequen. acumulada
Fi

  #Adicionando linhas
ni <- c(ni, sum(ni))#ni recebe os valores de ni + adicional
fi <- c(fi, sum(fi))#fi recebe os valores de fi + adicional
Fi <- c(Fi, NA)#Fi recebe os valores de fi + adicional nulo
names(ni)[16]<- "Warner Bros. Pictures"
names(ni)[17]<- "Total" #adiciona a String total ao 3º elemen. de ni

#criando a tabela de frequen.
tabela <- cbind(ni, fi, Fi)
tabela

###############################grafico de barras#################################
  #CONTAGEM DE FILMES POR ESTÚDIO
filme%>%
  group_by(Estudio)%>%
  summarise(
    cont = n()
  )%>%
  ggplot(aes(x = Estudio, y = cont)) +
  geom_bar(stat = "identity", fill = "#84BEDF", color = "#3F4D86") +
  coord_flip()

  #CONTAGEM POR ESTÚDIO E FILME DE ANIMAÇÃO
filme%>%
  filter(Estudio%in% c("Walt Disney Studios Motion Pictures", "Universal Pictures", "Paramount Pictures", "20th Century Fox")) %>%
  group_by(Estudio, Animação)%>%
  summarise(
    cont = n()
  )%>%
  ggplot(aes(x = Estudio, y = cont, fill = Animação)) +
geom_bar(stat = "identity",position = "dodge") +
  scale_fill_manual(values = c("#B1060F", "#84BEDF"))


###############################boxplot##########################################
ggplot(data = filme, aes(y = `Rotten Tomatoes %`, x = Animação)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(aes(fill = Animação ), width = 0.6,
               outlier.shape = 1, outlier.size = 2,
               show.legend = FALSE) +
  geom_point(stat = "summary", fun = "mean", shape = 4,
             size = 2, color = "blue") +
  labs(y = "Avaliação (%)", x = "Animação") +
  theme_classic()

##############################dispersão########################################
filme$`Bilheteria (US$)` <- as.numeric(filme$`Bilheteria (US$)`)

ggplot(filme, aes(x = `Bilheteria (US$)`, y = Ano)) +
  geom_point(col = "#E20812") +
  scale_x_continuous(labels = scales::dollar)

################################INFERENCIA#####################################

###########################teste de hipotese###################################
  #variancia
#H0: var = var
#H1: var != var
var.test(`Rotten Tomatoes %`[Animação=='sim'], `Audiencia %`[Animação=='não'])
  #média
#H0: media = media
#H1: media != media
#como o teste anterior indicou H1 verdadeiro então var.equal = F
t.test(`Rotten Tomatoes %`[Animação=='sim'], `Audiencia %`[Animação=='não'], var.equal=F)

###############################teste de proporção##############################
#H0: prop. = prop.
#H1: prop. != prop.
prop.test(c(80, 20), c(100, 100), correct = F)