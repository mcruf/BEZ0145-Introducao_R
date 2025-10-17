######################################################
#                                                    #
#      Introdução a algoritmos & programação         #
#                   (parte 01)                       #
#                                                    #
######################################################


# O script abordará os controles de fluxo mais utilizados dentro do programa R.
# @ Condicionantes - if, if-else, ifelse
# @ Iteradores - for, while (break, next)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Controle de fluxo: condicionantes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1.1) if
#~~~~~~~~~
# A ideia geral é:
#if (expressão lógica) { # se expressão lógica for verdadeira
  # faça isso
#} 


## Ex. 1
y <- -0.5

if(as.integer(y)){
  print("Y é um número inteiro") # oops, o que tá errado?
}


## Ex. 2
x <- sample(6, 1) #simula o lançamento de um dado não viciado

if ( (x %% 2) == 0) {
  print(paste(x, "é par"))
} 


## Ex. 3
x <- c(1, 3, 10, 15)
x2 <- c("a", "b", "c")

if(class(x) == 'numeric'){
  sum(x)
}


if(class(x2) == 'numeric'){
  sum(x2)
}



# 1.2) if-else
#~~~~~~~~~~~~~~~
# Se as condições não forem cumpridas? Vamos testar via 'if-else'

# A ideia geral é:
# if (condição) {
#   # código executado se a condição for verdadeira (TRUE)
# } else {
#   # código executado se a condição for falsa (FALSE)
# }


## Ex. 1 
temperatura <- 28

if (temperatura < 20) {
  print("Frio")
} else  {
  print("Quente")
}

### + de 2 condições
if (temperatura < 20) {
  print("Frio")
} else if (temperatura < 30) {
  print("Agradável")
} else {
  print("Quente")
}



## Ex. 2 
y <- c("Programação","Algoritmos","Funções")

if("programação" %in% y) {
  print("Palavra encontrada")
} else {
  print("Palavra não encontrada")
}



## Ex. 3
### função ifelse: versão vetorizada do if-else -> avalia um vetor (ao invés de um único valor)

temperaturas <- sample(-10:30, 10)

ifelse(temperaturas <= 0, "frio", "quente") #2 condições

ifelse(temperaturas <= 0, "Frio", 
       ifelse(temperaturas <= 18, "Agradável", "Quente")) #+2 condições




#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#######################      DESAFIO       #########################

## Classifique as seguintes notas nas seguintes categorias:
# Nota >= 9: Excelente
# Nota <= 7 & Nota > 9: Bom
# Nota <= 5 & Nota > 7: Regular
# Nota < 5: Insuficiente

NOTAS <- c(3.5, 9, 6.5, 4, 10, 8.2)

####################################################################
#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Controle de fluxo: iteradores
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 2.1) for
#~~~~~~~~~~
# A ideia geral é:
# for (elemento in vetor) {
#   executar ação 
# } 


## Ex. 1
x <- 100:200 #vetor numérico

for(j in 1:10){
  print(x[j])
}


## Ex. 2
NOMES <- c("Morpheus", "Neo", "Trinity", "Agente Smith") #vetor character

for(nome in NOMES){
  print(nome)
}



## Ex. 3
for(i in c(2, 9, 4, 6)){
  print(i^2)
}


## Ex. 4
for (ano in 2010:2015){
  print(paste("Ano:", ano))
}


## Ex. 5
df <- data.frame("Peixe" = c("sp1","sp2","sp3","sp4","sp5","sp6"), 
                 "Peso1" = sample(10:30, 6, replace = TRUE), 
                 "Peso2" = sample(25:35, 6, replace = TRUE), 
                 "Peso3" = sample(40:50, 6, replace = TRUE))

for(i in 1:nrow(df)){
  print(df[i, c("Peso1","Peso2","Peso3")])
}



# 2.2) while
#~~~~~~~~~~~~
# A ideia geral é:
# while ( expressão lógica ) { # se expressão lógica for verdadeira
# sequência de comandos }


## Ex. 1
contagem_regressiva <- 5

while(contagem_regressiva >= 1){
  print(paste("Contagem regressiva:", contagem_regressiva))
  contagem_regressiva <- contagem_regressiva - 1
} 


## Ex. 2
x <- c(7:10)
n <- length(x) # n recebe o valor do comprimento do vetor x, no caso n = 4
i <- 1
soma_acumulada <- 0

while (i <= n) { # executa os comandos entre chaves 3 vezes, com i
  # assumindo os valores de 1 a 3 sucessivamente
  soma_acumulada = soma_acumulada + x[i]
  print(paste("x[", i, "] = ", x[i], " ", 
              ", soma = ", soma_acumulada, sep=''))
  i <- i + 1
}



# 2.3) break/next
#~~~~~~~~~~~~~~~~~~~

## Ex. 1 (for com next)
letras <- LETTERS[1:6]

for ( j in letras) {
  if (j == "C") {
    next
  }
  print(j)
}


## Ex. 2 (for com break)
for ( j in letras) {
  if (j == "C") {
    break
  }
  print(j)
}




#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#######################      DESAFIO       #########################

## O professor recebeu a lista de notas de uma turma e gostaria
## de realizar as seguintes tarefas:
## 1. Adicionar meio ponto na nota (0.5)
## 2. Contar quantos alunos foram aprovados na discipina após o
## acréscimo de notas
## Dica: precisa usar as funções 'for' e 'if'


NOTAS <- c(6.5, 4.0, 8.2, 5.9, 7.0, 3.5, 9.5)



####################################################################
#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Exemplos mais complexos
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Os exemplos na sequência visam introduzir um maior nível de 
# complexidade. A maioria dos algoritmos são compostos por um conjunto
# de controles de fluxo - i.e., usa-se mais de um tipo de controle de fluxo
# no mesmo algoritmo.


# 3.1) Loop aninhado
#~~~~~~~~~~~~~~~~~~~~~~~
# Loops aninhados são muito convenientes, porém crescem
# rapidamente em complexidade quanto mais camadas iterativas se adicionar.
# Quanto mais complexo o loop, maior suscetibilidade a erros.


for(linha in 1:nrow(df)) {
  
  for(coluna in 1:ncol(df)) {
    
    print(df[linha, coluna])
    
  }   
}


# 3.2) Inserindo nova coluna & calculando médias
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- data.frame("Peixe" = c("sp1","sp2","sp3","sp4","sp5","sp6"), 
                 "Peso1" = sample(10:30, 6, replace = TRUE), 
                 "Peso2" = sample(25:35, 6, replace = TRUE), 
                 "Peso3" = sample(40:50, 6, replace = TRUE))


df$media <- NA #Inicializando nova coluna para guardar os valores de peso médio

for(i in 1:6){
  df$media[i] <- sum(exemplo1[i, c("Peso1", "Peso2", "Peso3")]) / 3 #Cálculo manual
}

head(df)


## O código anterior ainda é passível a erros, porque a média é calculada manualmente.
## Podemos tornar o código menos susceptível a erros da seguinte forma

nlinhas <- nrow(df) #Armazena o número de linhas no dataframe
Pesos <- c("Peso1", "Peso2", "Peso3") #Identifica colunas de interesse
ncol_pesos <- length(Pesos) #Sabendo o número de colunas com peso, fica mais fácil dividir pelo total no cálculo da média
df$media2 <- NA #Inicializando nova coluna para guardar os valores a título de comparação

for(i in 1:nlinhas){
  df$media2[i] <- sum(df[i, Pesos])/ncol_pesos
}

head(df)


## Ainda podemos melhorar o código fazendo-se valer da função mean()

df$media3 <- 0 #Nova coluna para comparação

for(i in 1:nlinhas){
  df$media3[i] <- mean(as.numeric(df[i, Pesos]))
}

head(df)


## Definindo a condição de peso

df$condicao <- NA #Inicializando uma nova coluna

for(i in 1:nlinhas){
  
  if(df$media[i] >= 32){
    df$condicao[i] <- "excesso de peso"
  } else {
    df$condicao[i] <- "pouco peso"
    }
}

df



# 3.3) Crescimento de bactérias
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Variáveis iniciais
populacao_inicial <- 50 # N0: População inicial de bactérias
capacidade_suporte <- 500 # K: Capacidade suporte
ciclo <- 1 # Contador de ciclos de replicação
populacao_atual <- populacao_inicial


# O loop continua ENQUANTO a população atual for MENOR que a capacidade de carga
while (populacao_atual < capacidade_carga) {
  
  ## Simula o crescimento (aumento aleatório entre 20 e 80 bactérias por ciclo)
  aumento_populacional <- sample(20:80, 1) 
  
  populacao_total <- populacao_atual + aumento_populacional
  
  if (populacao_total  > capacidade_carga) {
    
    # Calcula o crescimento máximo possível
    crescimento_maximo <- capacidade_suporte - populacao_atual
    populacao_atual <- capacidade_suporte
    
    print(paste("Ciclo", ciclo, ": O crescimento de", aumento_populacional, 
                "excederia o limite. A população cresce apenas", crescimento_maximo, 
                "e atinge K (Capacidade de Suporte)."))
    break # Usa 'break' para finalizar a simulação assim que K é atingido
  }
  
  
  # 3. Atualiza a população
  populacao_atual <- populacao_atual + aumento_populacional
  
  print(paste("Ciclo:", ciclo, ", Crescimento de", aumento_populacional, "-> Tamanho populacional =", populacao_atual))
  
  ciclo <- ciclo + 1 # Passa para o próximo ciclo
}
