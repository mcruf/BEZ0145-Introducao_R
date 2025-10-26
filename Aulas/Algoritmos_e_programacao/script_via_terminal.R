#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                             #
#   Programação funcional     #
#                             #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Exemplo de script que pode ser executado diretamente pelo terminal via comando: Rscript script_via_terminal.R
# O script abaixo consiste em um conjunto de instruções, as quais serão executadas na sequência em que aparecem, e salvas em um novo diretório chamado /Resultados.

# NOTE: para garantir o funcionamento do script, o diretório de trabalho tem que ser definido em relação à pasta da aula:
# ~/BEZ0145-Introducao_R/Aulas/Algoritmos_e_prorgramação



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


# Lendo as funções auxiliares & banco de dados
source("funcoes_auxiliares.R")
data("iris")


# Criando diretório de saída (armazenamento dos resultados)
wd <- getwd() #O diretório TEM que ser em relação à pasta da aula (Algoritmos_e_funcoes/)
OUTDIR <- paste(wd, "Resultados", sep="/")

if(!dir.exists(OUTDIR)){
  dir.create(OUTDIR)
}



## Calculando as estatisticas
estatisticas <- estatisticas_por_grupo(iris, "Species")

if(!is.null(estatisticas)){
  
  print("Salvando o sumário estatístico...")
  
  OUTDIR2 <- paste(OUTDIR, "sumario_estatistico.csv", sep = "/")
  write.csv(estatisticas, OUTDIR2, row.names = F)
  
}


## Padronizando os dados
dados_padronizados <- padronizacao(iris, metodo = 'zscore')
dados_normalizados <- padronizacao(iris, metodo = 'minmax')


if(!is.null(dados_padronizados)){
  
  print("Salvando os dados padronizados...")
  print("--------------------------------------------")
  
  
  OUTDIR2 <- paste(OUTDIR, "dados_padronizados.csv", sep = "/")
  write.csv(dados_padronizados, OUTDIR2, row.names = F)
} 

if(!is.null(dados_normalizados)){
  
  print("Salvando os dados normalizados...")
  print("--------------------------------------------")
  
  
  OUTDIR2 <- paste(OUTDIR, "dados_normalizados.csv", sep = "/")
  write.csv(dados_normalizados, OUTDIR2, row.names = F)
}



## Plotando os resultados
gpdr <- grafico_densidade(dados_padronizados, "Dados padronizados")
gn <- grafico_densidade(dados_padronizados, "Dados normalizados")



if(!is.null(gpdr)){
  
  print("Salvando o gráfico de dados padronizados...")
  print("--------------------------------------------")
  
  OUTDIR2 <- paste(OUTDIR, "grafico_padronizado.jpeg", sep = "/")
  ggsave(filename = OUTDIR2, plot = gpdr)
}

if(!is.null(gn)){
  print("Salvando o gráfico de dados normalizados...")
  print("--------------------------------------------")
  
  
  OUTDIR2 <- paste(OUTDIR, "grafico_normalizado.jpeg", sep = "/")
  ggsave(filename = OUTDIR2, plot = gn)
}

