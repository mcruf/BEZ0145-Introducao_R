#########################################
#                                       #
#      Visualização de dados no R       #
#                                       #
#########################################


# O presente script abordará as principais funções que o R dispõe para fazer gráficos.
# O foco será nas funções base, as quais permitem uma visualização rápida dos dados. 
# Em termos visuais, os gráficos resultantes são mais simples e estesticamente menos atrativos comparados ao ggplot2.
# Para fins didáticos, será usado o banco de dados dos pinguins de palmer


# A escolha de cores e palhetas de cores pode ser auxiliada mediante as 
# seguintes fontes:
# https://r-charts.com/colors/
# http://nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
# https://coolors.co/


# https://vanderleidebastiani.github.io/tutoriais/Graficos_com_R.html



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr) #manipulação dos dados
library(tidyr) #manipulação dos dados
#install.packages("tidyr")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define o diretório de trabalho, i.e., o local
# aonde são guardados os dados, scripts, etc. 

setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/BEZ0145-Introducao_R/")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando o banco de dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dados baixados de: https://www.kaggle.com/datasets/samybaladram/palmers-penguin-dataset-extended?resource=download

dados <- read.csv("palmerpenguins.csv")




#><><><><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1 - Explorando visualmente os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1.1) Histograma
#~~~~~~~~~~~~~~~~~
# Na análise de dados, é fundamental avaliar a distribuição
# de frequência da variável de intersse, uma vez que permite 
# ver como os dados estão distribuídos - e assim detectar padrões ou anomalias.

## Conferindo a função
?hist

## Histograma padrão
hist(dados$body_mass_g) #Frequência absoluta
hist(dados$body_mass_g, freq=F) #Frequência relativa


## Alterando o número de classes
hist(dados$body_mass_g, breaks = 10) #10, 30, 100, ...



# 1.2) Boxplot
#~~~~~~~~~~~~~~~~~
# Exibe a distribuição da variável numérica, destacando os principais valores estatísticos: 
# @ mínimo
# @ quartil inferior (Q1)
# @ mediana (Q2)
# @ quartil superior (Q3)
# Complemento ao histograma,e útil para identificar outliers e/ou realizar comparações entre grupos.

#conferindo a função
?boxplot

## Visualizando a distribuição da variável de interesse
boxplot(dados$body_mass_g)


## Comparando a variável de interesse com diferentes grupos
boxplot(dados$body_mass_g ~ dados$species)


## Complemento
dotchart(dados$body_mass_g)


# 1.3) Gráfico de barra
#~~~~~~~~~~~~~~~~~~~~~~~~
# Ideal para comparar categorias ou grupos discretos, 
# Permite  a visualização rápida dos maiores ou menores valores e
# a compreensão de tendências, comparando a altura ou o comprimento das barras.

# Dado precisa estar no formato MATRIX (e não DATA.FRAME!)


## Calculando a média de peso de cada espécie de pinguim
tmp <- dados %>%
       group_by(species) %>% #agrupando por especie
       summarize(Peso_medio = mean(body_mass_g)) %>% #calculando a media dentro de cada grupo (mean)
       pivot_wider(names_from = species,
                   values_from = Peso_medio) %>% #transformando o dado do formato longo (long) para largo (wide)
       as.matrix() #Transformando para matrix 

#tmp2 <- tapply(dados$body_mass_g, dados$species, mean) #Usando função base

barplot(tmp)


## Podemos também verificar a média por sexo dentro de cada espécie
dados %>%
  group_by(species, sex) %>% #agrupando por especie e sexo
  summarize(Peso_medio = mean(body_mass_g)) %>% #calculando a media dentro de cada grupo (mean)
  pivot_wider(names_from = species,
              values_from = Peso_medio) %>% #transformando o dado do formato longo (long) para largo (wide)
  tibble::column_to_rownames('sex') %>% #renomeando as linhas
  as.matrix() %>% #Transformando para matrix 
  barplot(beside = TRUE)



# 1.4) Gráfico de pizza
#~~~~~~~~~~~~~~~~~~~~~~~~
# Alternativa ao gráfico de barras.
# No entanto, esses gráficos idealmente não deveriam ser usados,
# porque o olho humano não consegue comparar áreas e ângulos com precisão, 
# o que dificulta a comparação de fatias, especialmente aquelas com tamanhos semelhantes ou muitas categorias.

#conferindo a função
?pie

pie(tmp, 
    labels = colnames(tmp))




# 1.5) Gráfico de linha
#~~~~~~~~~~~~~~~~~~~~~~~~
# Ideal para mostrar tendências de dados numéricos, especialmente ao longo do tempo.


## Como se deu a variação de peso médio da espécie Adelie ao longo dos anos?

tmp <- dados %>%
       filter(species == "Adelie") %>% #filtrando para a espécie desejada
       group_by(year) %>% #agrupando os dados por ano
       summarize(Peso_medio = mean(body_mass_g)) #calculando as médias anuais



plot(tmp$Peso_medio ~ tmp$year, 
     type = "b") #outras opções: l, o, s,...



## Como se deu essa variação para as demais espécies?
tmp <- dados %>%
  group_by(year, species) %>% #agrupando os dados por ano
  summarize(Peso_medio = mean(body_mass_g)) #calculando as médias anuais por espécie


### Vamos começar a plotar....

#### Filtrando os dados por espécies
adelie <- filter(tmp, species == "Adelie")
chinstrap <- filter(tmp, species == "Chinstrap")
gentoo <- filter(tmp, species == "Gentoo")


#### Criando o gráfico base
plot(NA, xlim = range(tmp$year), ylim = range(tmp$Peso_medio))


#### Adicionando as tendências individuais
lines(adelie$year, adelie$Peso_medio, 
      type = "b",
      col = 'cyan4')


lines(chinstrap$year, chinstrap$Peso_medio, 
      type = "b",
      col = 'darkorange')


lines(gentoo$year, gentoo$Peso_medio, 
      type = "b",
      col = 'gray40')


#### Podemos otimizar a sequência anterior conforme

## criando um vetor com as espécies
lista_especies <- unique(tmp$species)

## criando um vetor com as cores desejadas
cores <- c("cyan4", "darkorange", "gray40")


#### Criando o gráfico base
plot(NA, xlim = range(tmp$year), ylim = range(tmp$Peso_medio))


### Adicionando as tendências individuais de forma automatizada

for(i in seq_along(lista_especies)){
  
  spp <- filter(tmp, species == lista_especies[i]) #filtrando por espécie
  
  lines(spp$year, spp$Peso_medio, #adicionando as linhas individuais
        col = cores[i], 
        type = "b")
}




# 1.6) Gráfico de dispersão
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Usado para visualizar a relação entre duas variáveis numéricas, sendo ideal para:
# (i) identificar correlações potenciais, como tendências positivas ou negativas 
# (ii) detectar padrões como relações lineares ou não lineares
# (iii) detectar anormalidades

#conferindo a função
?plot

# Qual a relação entre a massa corporal e o tamanho do bico dos pinguins?

plot(dados$body_mass_g ~ dados$bill_length_mm)


plot(dados$body_mass_g ~ dados$bill_length_mm,
     pch = 19) # modificando o tipo de ponto; digite ?pch




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Customização de gráficos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Modificando a legenda dos eixos
plot(dados$body_mass_g ~ dados$bill_length_mm,
     xlab = "Comprimento do bico (mm)",
     ylab = "Massa corporal (g)")


## Adicionando um título
plot(dados$body_mass_g ~ dados$bill_length_mm,
     xlab = "Comprimento do bico (mm)",
     ylab = "Massa corporal (g)",
     main = "Relação peso-comprimento")


## Rotacionando os eixos
plot(dados$body_mass_g ~ dados$bill_length_mm,
     xlab = "Comprimento do bico (mm)",
     ylab = "Massa corporal (g)",
     main = "Relação peso-comprimento",
     las = 1) #1,2,3


## Modificando as bordas
plot(dados$body_mass_g ~ dados$bill_length_mm,
     xlab = "Comprimento do bico (mm)",
     ylab = "Massa corporal (g)",
     main = "Relação peso-comprimento",
     las = 1,
     bty = "l") #o,n,u,l,c,7


## Modificando e colorindo os pontos de acordo com a especie
library(scales)
plot(dados$body_mass_g ~ dados$bill_length_mm,
     xlab = "Comprimento do bico (mm)",
     ylab = "Massa corporal (g)",
     main = "Relação peso-comprimento",
     las = 1,
     bty = "l",
     cex = 1.5,
     pch = 19,
     #pch =  15:17,
     #col = as.factor(dados$species)
     col = alpha(cores, 0.3)) 


## Adicionando uma legenda
legend("bottomright",
       legend = lista_especies,
       col = cores,
       pch = 19,
       bty = "n")



## Adicionando uma linha de tendência média
abline(lm(dados$body_mass_g~dados$bill_length_mm),
       col = 'black',
       lwd = 4)


## salvando a figura
jpeg("~/Downloads/meu_grafico.jpg", #png,pdf
     res = 300, #definindo a resolução
     units = "in", #unidade da dimensão
     width = 6, #largura do grafico
     height = 5) #altura do grafico


plot(dados$body_mass_g ~ dados$bill_length_mm,
     xlab = "Comprimento do bico (mm)",
     ylab = "Massa corporal (g)",
     main = "Relação peso-comprimento",
     las = 1,
     bty = "l",
     cex = 1.5,
     pch = 19,
     col = alpha(cores, 0.3)) 

legend("bottomright",
       legend = lista_especies,
       col = cores,
       pch = 19,
       bty = "n")

abline(lm(dados$body_mass_g~dados$bill_length_mm),
       col = 'black',
       lwd = 4)

dev.off()



### Plotando vários gráficos em uma única figura
jpeg("~/Downloads/meu_grafico2.jpg", #png,pdf
     res = 300, 
     units = "in",
     width = 10, 
     height = 4)


par(mfrow = c(1,3))

#### barplot ####
tmp2 <- tapply(dados$body_mass_g, dados$species, mean) #Usando função base

barplot(tmp2, 
        col = cores,
        border = 'black',
        width = 0.7,
        ylab = "Massa corporal (g)",
        main = "Massa corporal média por espécie",
        las = 1)



#### boxplot ####
boxplot(dados$body_mass_g ~ dados$species, 
        col = cores,
        lwd = 1.5,
        xlab = "",
        ylab = "Massa corporal (g)",
        main = "Variação de massa corporal por espécie",
        las = 1)


#### gráfico de dispersão ####
plot(dados$body_mass_g ~ dados$bill_length_mm,
     xlab = "Comprimento do bico (mm)",
     ylab = "Massa corporal (g)",
     main = "Relação peso-comprimento",
     las = 1,
     cex = 1.5,
     pch = 19,
     col = alpha(cores, 0.3)) 

legend("bottomright",
       legend = lista_especies,
       col = cores,
       pch = 19,
       bty = "n")

abline(lm(dados$body_mass_g~dados$bill_length_mm),
       col = 'black',
       lwd = 4)



dev.off()



