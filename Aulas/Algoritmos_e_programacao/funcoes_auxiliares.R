##################################
#       Funções auxiliares       #
##################################

# Carregando pacotes

# Lista de pacotes necessários
pacotes <- c("ggplot2", "tidyr", "dplyr", "viridis")

# Loop para verificar e instalar/carregar cada pacote
for (pacote in pacotes) {
  if (!requireNamespace(pacote, quietly = TRUE)) {
    install.packages(pacote)
  }
  library(pacote, character.only = TRUE)
}

rm(list = c("pacote", "pacotes"))


# Mensagem de aviso
message("Carregando as funções auxiliares")



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><



## Padronização / Normalização dos dados ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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



## Estatística descritiva por grupo ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# media, mediana, desvio padrão, min, max

estatisticas_por_grupo <- function(dado, variavel_agrupamento) {
  
  grupo_sym <- rlang::sym(variavel_agrupamento)
  
  # Agrupar, selecionar numéricas e calcular as estatísticas
  estatisticas <- dado %>%
                  group_by(!!grupo_sym) %>% #Agrupa pelo nome da coluna fornecida (ex: 'Species')
                  select(where(is.numeric)) %>% #Seleciona as colunas numéricas para o resumo
                  summarise( #Calcula as estatísticas de resumo para todas as colunas numéricas restantes
                    across(
                      .cols = everything(), # Aplica a todas as colunas numéricas restantes
                      .fns = list(
                        Media = ~ mean(., na.rm = TRUE),
                        Mediana = ~ median(., na.rm = TRUE),
                        DP = ~ sd(., na.rm = TRUE),
                        Min = ~ min(., na.rm = TRUE),
                        Max = ~ max(., na.rm = TRUE)
                      ),
                      .names = "{.col}_{.fn}" #Nomeia as novas colunas como "NomeDaVariavel_Estatistica"
                    )
                  ) %>%
                  ungroup() %>% #Remove o agrupamento
                  data.frame() #Retorna data frame
  
  return(estatisticas)
}



## Gráfico de densidade ##
#~~~~~~~~~~~~~~~~~~~~~~~~~
grafico_densidade <- function(dado, titulo = "Distribuição das Variáveis Numéricas") {
  
  #Identificar e selecionar colunas numéricas
  dado_num <- dado %>%
              select(where(is.numeric))
  
  #Transformar os dados para o formato "longo" (necessario para ggplot2)
  dado_longo <- dado_num %>%
                pivot_longer(
                  cols = everything(), #Seleciona todas as colunas
                  names_to = "Variavel",
                  values_to = "Valor")
  
  #Gerar o gráfico de densidade
  p <- ggplot(dado_longo, aes(x = Valor, fill = Variavel)) +
        geom_density(alpha = 0.6) + # Gera o gráfico de densidade com transparência
        facet_wrap(~ Variavel, scales = "free") + # Separa um gráfico por variável
        scale_fill_viridis_d() +
        labs(title = titulo,
             x = "Valor",
             y = "Densidade") +
        theme_minimal() +
        theme(legend.position = "none") # Remove a legenda já que as cores são usadas no facet
      
  return(p)

}




