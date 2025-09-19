#########################################
#                                       #
#    Manipulando os dados com dplyr     #
#                                       #
#########################################


# Este script visa introduzir as principais
# funções que fazem parte do pacote dplyr.



#~~~~~~~~~~~~~~~~~~~~~~~~
# Carregando os pacotes
#~~~~~~~~~~~~~~~~~~~~~~~~
#install.packages("dplyr")
library(dplyr)

?dplyr



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Definindo o diretório base
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define o diretório de trabalho, i.e., o local
# onde são guardados os dados, scripts, do projeto em questão.

setwd("~/OneDrive/Arbeit/Lectures_and_Talks/UFRN/Lectures/BEZ0145-Introducao_R/")


#><><><><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Carregando o banco de dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Como exemplo, usaremos o banco de dados dos pinguins de palmer.
# Dados baixados de: https://www.kaggle.com/datasets/samybaladram/palmers-penguin-dataset-extended?resource=download

# Os dados foram coletados no Arquipélago Palmer, próximo à Antártida, ao longo dos anos 2021-2025. 
# Esses dados fornecem informações sobre três espécies de pinguins e inclui variáveis biológicas,
# como dimensões do bico e massa corporal. Além disso, o banco de dados
# conta com variáveis adicionais, como dieta, ano de observação, estágio de vida e 
# indicadores de saúde. Esse conjunto de informação permitem uma compreensão 
# mais detalhada da biologia e ecologia dos pinguins.

dados <- read.csv("palmerpenguins.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Explorando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~


# 2.1) Estrutura geral
#~~~~~~~~~~~~~~~~~~~~~~

### Visualizador dos dados tipo excel ###
View(dados)


## Primeiras 6 linhas 
head(dados)


## Últimas 6 linhas 
tail(dados)


## Estrutura dos dados
str(dados)


## Resumo geral dos dados
summary(dados)
summary(dados$bill_length_mm)



# 2.2) Acessando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Via funções base do R

## Colunas
dados$species           # Acessa a coluna com $
dados[["species"]]      # Acessa o conteúdo da coluna  [[]]
dados[, "species"]      # Idem
dados[, 1]        # Acessa a coluna pelo seu índice numérico



## Linhas
dados[1, ]            # Primeira linha
dados[1:3, ]          # Primeiras 3 linhas
dados[c(1:3, 15), ]   # Acessa as linas 1:3 & 15


dados[1:3, 5] # Acessa as 3 primeiras linhas da 5a coluna

colnames(dados)[5]

dados[ , "island"]

dados[, colnames(dados)[5]]

COLUNAS <- colnames(dados)
COLUNAS

obj <- colnames(dados)[5]
obj

dados[,obj]
dados[, colnames(dados)[5]]


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Manipulando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Filtrando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~


### Baseado em variáveis numéricas ###
######################################

summary(dados$body_mass_g)

## Valores maiores ou menores que...
dados_f <- dados %>%
           filter(body_mass_g <= 4835)



#dados_f <- dados[dados$body_mass_g <= 4835, ] #Função base


summary(dados_f$body_mass_g)



## Valores em um intervalo
dados_f <- dados %>%
           filter(body_mass_g >= 3884 & body_mass_g <= 5622)


#dados_f <- dados[dados$body_mass_g >= 3884 & dados$body_mass_g <= 5622, ] #Função base


summary(dados_f$body_mass_g)



### Baseado em variáveis categóricas ###
########################################
table(dados$species)


## Categorias especificas
dados_f <- dados %>%
           filter(species == "Adelie")

#dados_f <- dados[dados$species == "Adelie", ] #Função base


dados_f <- dados %>%
  filter(species %in% c("Adelie", "Chinstrap"))

#dados_f <- dados[dados$species %in% c("Adelie", "Chinstrap"), ] #Função base




## Todas as categorias, menos a que está sendo especificada
dados_f <- dados %>%
           filter(!(species == "Adelie"))

table(dados_f$species)

#dados_f <- dados[!(dados$species == "Adelie"), ] #Função base



## Filtrando várias colunas (de variáveis diferentes)
dados_f <- dados %>%
           filter(species == "Adelie", body_mass_g >= 4835, sex == "female")


#dados_f <- dados[dados$species == "Adelie" & dados$body_mass_g >= 4835 & dados$sex == "female", ] #Função base




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##################     DESAFIO     ##################

# Filtre os dados de modo que o dado final
# contenha apenas pinguins juvenis (life_stage)
# das ilhas Biosce e Torgensen (island) e com
# pesos maiores que 4000 g.

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



# 3.2) Selecionando as colunas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Colunas específicas
dados_s <- dados %>%
           select(species, life_stage, body_mass_g, health_metrics, year)

#dados_s2 <- dados[, c("species", "life_stage")] #Função base


## Colunas sequenciais
dados_s <- dados %>%
           select(species:diet)

#  dados_s2 <- dados[, 1:8] #Função base



## Todas as colunas, menos a que está sendo especificada
dados_s <- dados %>%
  select(-c(bill_length_mm, bill_depth_mm))



COLNAMES <- colnames(dados)
EXCLUDE <- c("bill_length_mm", "bill_depth_mm")
KEEP <- setdiff(COLNAMES, EXCLUDE)
dados[, KEEP]



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##################     DESAFIO     ##################

# Selecione apenas as colunas 'sex', 'health_metrics' & 'body_mass_g'.
# Na sequência, filtre apenas pinguins do sexo feminino
# (female) que estejam acima do peso (overweight).
# Salve esse banco de dados em um novo objeto.

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


dados %>%
  select(sex, health_metrics, body_mass_g) %>%
  filter(sex == 'female' , health_metrics == 'overweight')


# 3.3) Ordenando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~

## Ordenando por uma coluna específica
dados_o <- dados %>%
           #arrange(body_mass_g) #Em ordem crescente
           arrange(desc(body_mass_g)) #Em ordem decrescente

head(dados$body_mass_g)
head(dados_o$body_mass_g)


dados_o2 <- dados[order(dados$body_mass_g),] # Função base



## Ordenando por mais de uma coluna
dados_o <- dados %>%
           arrange(body_mass_g, bill_length_mm) 
           #arrange(body_mass_g, desc(bill_length_mm)) 




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
##################     DESAFIO     ##################

# Use os dados do último desafio e ordene a coluna
# 'body_mass_g' por ordem decrescente de peso.
# Qual o peso da fêmea mais pesada?

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<




# 3.4) Criando/modificando colunas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Criando uma nova coluna com base em coluna existente
dados_m <- dados %>% 
           mutate(body_mass_kg = body_mass_g / 1000)

cbind(dados_m$body_mass_g, dados_m$body_mass_kg)

dados_m$body_mass_kg <- dados$body_mass_g / 1000 #Função base



## Modificando uma coluna

table(dados$life_stage)

dados_m <- dlife_stagedados_m <- dados %>% 
           mutate(life_stage2 = ifelse(life_stage == 'adult', 'adult', 'sub-adult'))

table(dados_m$life_stage2)


# dados$life_stage <- ifelse(dados$life_stage  == 'adult', 'adult', 'sub-adult') #Função base

# dados$life_stage2 <- if(dadosdados$life_stage  == 'adult', "adulto")
#                      else("subadult")




## Trocando o tipo de variável
str(dados)


dados <- dados %>%
         mutate(across(fatores, as.factor))


### alternativa
dados <- dados %>%
         mutate(across(where(is.character), as.factor))

str(dados)


dados$species <- as.factor(dados$species)
class(dados$species)

levels(dados$species)
nlevels(dados$species)


#### Usando função base
#fatores <- c("species","island", "sex", "diet", "life_stage", "health_metrics")
#dados[ , fatores] <- lapply(dados[, fatores], as.factor)



## Modificando os níveis de uma variável fator
dados_m <- dados %>%
           mutate(sexo = ifelse(sex == 'female', "femea", "macho"),
                  dieta = ifelse(diet == "fish", "peixe",
                          ifelse(diet == "krill", "krill",
                          ifelse(diet == "parental", "parental", "lula"))))
           

### Renomando todos os níveis       
levels(dados_m$diet) <- c("peixe", "krill", "parental", "lula") #Função base
levels(dados_m$diet) 

### Renomando alguns níveis
levels(dados_m$diet)[levels(dados_m$diet)=="squid"] <- "lula" #Função base





# 3.5) Renomeando as colunas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Trocando para português
dados_r <- dados %>%
           rename(especie = species,
                  ilha = island,
                  comp_bico_mm = bill_length_mm,
                  prof_bico_mm = bill_depth_mm,
                  comp_nadadeira_mm = flipper_length_mm,
                  massa_corporal_g = body_mass_g,
                  sexo = sex,
                  dieta = diet,
                  estagio = life_stage,
                  estado_peso = health_metrics,
                  ano = year)

head(dados_r)

#colnames(dados)[1:3] <- c("especie", "ilha", "comp_bico_mm") #Funcao base





# 3.6) Sumarizando os dados
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Calculando o peso médio por espécie de pinguim
dados %>%
  group_by(species) %>%
  summarise(Peso_medio = mean(body_mass_g, na.rm=T))



## Quantos pinguins há em cada categoria de peso?
dados %>%
  group_by(health_metrics) %>%
  #group_by(health_metrics, sex) %>%
  count()


### Proporção por categoria
dados %>%
  group_by(health_metrics) %>%
  summarise(Contagem = n()) %>%
  mutate(Proporcao = Contagem/sum(Contagem))






