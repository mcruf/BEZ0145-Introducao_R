######################################################
#                                                    #
#      Introdução a algoritmos & programação         #
#                   (parte 02)                       #
#                                                    #
######################################################


# O presente script abordará aspectos introdutórios sobre programação funcional (i.e., construção de funções no R).
# Serão apresentadas três funções distintas, ao longo das quais se construirá um nível crescente de complexidade.



#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Função para classificar temperatura
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Recapitulando a aula anterior 

## Criando uma sequência de temperaturas
temperaturas <- sample(-10:50, 10) 

## Criando a função
class_temp <- function(temperatura){
  
  #print(paste("Temperatura avaliada:", temperatura))

  #Classificando as temperaturas
  if( temperatura < 20){
    classificacao <- "Frio"
  } else if(temperatura >= 20 & temperatura <= 35){
    classificacao <- "Quente"
  } else{
    classificacao <- "Inferno em terra"
  }
  
  #Retornando o resultado
  return(classificacao)
  
}

class_temp(temperatura = temperaturas) #Oops...a função não é aplicável para vetores
class_temp(temperaturas[1])
sapply(temperaturas, class_temp) #Alternativa...


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Função para calcular o coeficinete de variação (CV)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Medida estatística de dispersão relativa de um conjunto de dados.
# Indica o quanto os dados variam em relação à média, sendo útil para comparar a 
# variabilidade de diferentes conjuntos de dados, mesmo que eles tenham unidades de 
# medida ou médias muito diferentes.

# O R não possui uma função interna para calcular o CV (ao contrário da média, mediana, ...)


## Criando a função
cv <- function(x){
  
  #Calculando o desvio padrão
  desvio_pdr <- sd(x) 
  
  #Calculando a média
  media <- mean(x) 
  
  #Calculando o CV
  cv <- (desvio_pdr / media) * 100 
  
  #Retornando o resultado
  return(cv)
    
}


## Testando a função
valores_1 <- c(1, 2, 3, 4, 5)
cv(x = valores_1)


## Testando a função com NA
valores_2 <- c(1, 2, 3, 4, NA)

cv(valores_2) #oops; as funções 'mean' e 'sd' não aceitam NAs!


## Aprimorando a função: vamos imbutir um mecanismo de validação
cv2 <- function(x){
  
  if(any(is.na(x))) message("Seu dado tem NA - a função não é aplicável!")
  
  desvio_pdr <- sd(x)
  media <- mean(x)
  cv <- (desvio_pdr / media) * 100
  
  return(cv)
  
}

cv2(valores_2) #Temos o aviso, mas o problema persiste.

### Pode-se resolver o problema de 2 formas: 
# 1 - remover manualmente os NAs do vetor antes de aplicar a função cv2
# 2 - remover NAs dentro da função cv2

cv2 <- function(x){
  
  if(any(is.na(x))) message("Seu dado tem NA. Todos serão removidos.")
  
  ## Alternativa 1
  x <- x[!is.na(x)]
  desvio_pdr <- sd(x)
  media <- mean(x)
  
  ## Alternativa 2
  #desvio_pdr <- sd(x, na.rm = T)
  #media <- mean(x, na.rm = T)
  
  cv <- (desvio_pdr / media) * 100
  
  return(cv)
  
}

cv2(x = valores_2) # Verificando...


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Função para padronizar os dados 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# A maioria dos dados possuem colunas numéricas cujos valores estão em escalas ou unidades diferentes.
# Podemos padronizar essas colunas de modo a tornar essas variáveis comparáveis.
# A padronização dos dados, também conhecida por Z-score, transforma os valores de modo que o novo conjunto de dados
# resultante tenha média zero (0) e desvio padrão um (1).
# Muitas análises estatísticas requerem a padronização dos dados, especialmente na estatística multivada

# Z-score = (x_i - media ) / desvio_padrao #onde x_i é o iesmo valor do vetor numérico

# Como exemplo, usaremos os dados 'iris', que já faz parte do ecosistema do R.

data("iris")

head(iris)
summary(iris) #Repare na amplitude dos valores de cada variável


## Criando a função
padronizacao <- function(dado){
  
  #Calculando a média
  media <- mean(dado, na.rm=T) 
  
  #Calculando o desvio padrão
  desvio_pdr <- sd(dado, na.rm=T) 
  
  #Padronizando os dados
  zscore <- (dado - media)/desvio_pdr 
  
  #Retornando o resultado
  return(zscore)
  
}


## Testando a função
padronizacao(dado = iris$Sepal.Length)

### No momento a função só consegue ser aplicada separadamente para cada coluna.
### Podemos usar a função apply para aplicar a função para múltiplas colunas:

?apply 
apply(X = iris, MARGIN = 2, FUN = padronizacao) #MARGIN = 1 -> função aplicada a nível de linha
str(iris)

iris_pdr <- apply(X = iris[, -5], MARGIN = 2, FUN = padronizacao) #Retirando a coluna que não é numérica
iris_pdr <- as.data.frame(iris_pdr)
iris_pdr$Species <- iris$Species #Retornando a coluna 'espécie'
head(iris_pdr)
summary(iris_pdr); apply(iris_pdr[, -5], 2, sd)


### Embora a função já tenha cortado uma parte do trabalho manual, percebe-se que ela ainda é limitada no seu escopo.
### No momento, precisamos indicar manualmente as colunas numéricas para poder aplicar a função, além de ter que adicionar
### manualmente que não são numéricas.
### Podemos otimizar a função de forma que ambos os processos sejam realizados internamente na função.
### Para tal, vamos salvar nossa função


padronizacao <- function(dado){
  
  #Identificando colunas numéricas
  col_numerica <- sapply(X = dado, FUN = is.numeric)
  
  #Filtrando os dados
  dado_f <- dado[ , col_numerica]
  
  #Padronizando os dados (maneira menos eficiente)
  media <- sapply(dado_f, mean, na.rm=T)
  desvio_pdr <- sapply(dado_f, sd, na.rm=T)
  
  for(coluna in 1:ncol(dado_f)){
    for(linha in 1:nrow(dado_f)){
      dado_f[linha, coluna] <- (dado_f[linha, coluna] - media[coluna]) / desvio_pdr[coluna]
    }
  }
  
  
  #Padronizando os dados (maneira mais eficiente)
  # dado_f <- sapply(dado_f, function(x) {
  #   (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  # })
  
  # Revando as demais colunas (todas )
  dado <- cbind(dado_f, dado[ , !col_numerica])
  
  
  #Retornando o resultado 
  return(dado)
  
}


## testando a função
padronizacao(dado = iris)

### Quantificando a otimização da função em relação ao loop/sapply
start <- Sys.time()
test <- padronizacao(iris)
end <- Sys.time()
diff <- end-start


### Os exemplos anteriores foram todos construídos com funções usando apenas um único argumento.
### Podemos constuir funções com diversos argumentos - isso confere maior flexibilidade às funções.
### Vamos expandir nossa função de modo a permitir que o usuário possa escolher o método de transformação
### das variáveis numéricas: padronização ou normalização (minmax)

### A normalização garante que todos as variáveis numéricas estejam na mesma escala e com amplitude contida em um valor específico.
### Normalizar pelos valores minimos e máximos, por exemplo, garante que os valores transformados estejam em uma escala de (0,1)


padronizacao <- function(dado, metodo = 'zscore'){
  
  #Identificando colunas numéricas
  col_numerica <- sapply(X = dado, FUN = is.numeric)
  
  #Filtrando os dados
  dado_f <- dado[ , col_numerica]
  
  
  if(metodo == 'zscore'){ #Padronizando os dados
  dado_f <- sapply(dado_f, function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    })
  
  } else if(metodo == 'minmax'){ #Normalizando os dados
    dado_f <- sapply(dado_f, function(x) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    })
    
  } else{
    
    stop("Método inválido. Escolha 'zscore' ou 'minmax'.")
  }
  

  # Reavendo as demais colunas
  dado <- cbind(dado_f, dado[ , !col_numerica, drop=F])
  
  
  #Retornando o resultado 
  return(dado)
  
}


test <- padronizacao(dado = iris, metodo = "minmax")
head(test)
summary(test[,1:4])