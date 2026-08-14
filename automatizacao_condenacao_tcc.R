# Instalar e carregar pacotes necessários 
install.packages("readxl") 
install.packages("dplyr") 
install.packages("openxlsx") 
library(readxl) 
library(dplyr) 
library(openxlsx) 

# Carregar os dados 
dados<- read_excel("C:/Users/Usuario/Documents/dados tcc/Base tabular - Jurisprudência TSE.xlsx")

# Verificar se os dados foram carregados corretamente 
print(dim(dados))  # Mostra o número de linhas e colunas 
print(names(dados))  # Lista os nomes das colunas 

# Garantir que as colunas existam e estejam no formato correto 
dados$textoEmenta <- as.character(dados$textoEmenta) 
dados$textoDecisao <- as.character(dados$textoDecisao) 
dados$textoEmenta[is.na(dados$textoEmenta)] <- "" 
dados$textoDecisao[is.na(dados$textoDecisao)] <- "" 

# Definir palavras-chave indicativas de fraude 
palavras_chave <- c("fraude", "candidatura fictícia", "desvio de cota", "candidata laranja", 
                    "irregularidade", "simulação") 

# Criar função para verificar a presença de palavras-chave 
classificar_fraude <- function(ementa, decisao) { 
  texto_completo <- paste(ementa, decisao, collapse = " ") 
  if (any(grepl(paste(palavras_chave, collapse = "|"), texto_completo, ignore.case = TRUE))) { 
    return("CONDENAÇÂO") 
  } else { 
    return("ABSOLVIÇÂO") 
  } 
} 
# Aplicar a função para classificar os casos 
dados <- dados %>%  
  mutate(resultado_automatizado = mapply(classificar_fraude, textoEmenta, textoDecisao)) 
# Visualizar os primeiros resultados 
head(dados[, c("textoEmenta", "textoDecisao", "resultado_automatizado")]) 

# Salvar os dados com a nova classificação 
write.xlsx(dados, "Base de dados - tcc_classificada.xlsx") 
getwd() 

#revisão manual
set.seed(123)  # para conseguir reproduzir a mesma amostra

amostra_revisao <- dados %>%
  group_by(resultado_automatizado) %>%
  slice_sample(n = 30) %>%
  ungroup()
amostra_revisao <- amostra_revisao %>%
  mutate(resultado_manual = "")
write.xlsx(
  amostra_revisao,
  "amostra_para_revisao_manual.xlsx"
)