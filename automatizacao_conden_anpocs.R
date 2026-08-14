# Instalar e carregar pacotes necessários 
if(!require(readxl)) install.packages("readxl")
if(!require(dplyr)) install.packages("dplyr")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(stringr)) install.packages("stringr")

library(readxl)
library(dplyr)
library(openxlsx)
library(stringr)

# 1. Carregar os dados 
caminho_arquivo <- "C:/Users/Usuario/Downloads/Base tabular - Jurisprudência TSE.xlsx"
dados <- read_excel(caminho_arquivo)

# 2. Tratamento inicial das colunas de texto
dados <- dados %>%
  mutate(
    textoEmenta = coalesce(as.character(textoEmenta), ""),
    textoDecisao = coalesce(as.character(textoDecisao), ""),
    # Criamos um texto combinado em minúsculo linha por linha
    texto_analise = tolower(paste(textoEmenta, textoDecisao))
  )

# 3. Definição das regras de classificação da DECISÃO (Condenação vs Não Condenação)
classificar_decisao <- function(texto) {
  # Termos que indicam manutenção de condenação ou reconhecimento da fraude
  termos_condenacao <- c(
    "julgar procedente", "julgou procedente", "procedência da investigação",
    "reconhecida a fraude", "caracterizada a fraude", "configurada a fraude",
    "cassação", "declarar a inelegibilidade", "provimento ao recurso",
    "negou provimento ao agravo", "reconhecimento da fraude"  # Atenção: se o recurso era dos réus contra a condenação, negar provimento mantém a condenação!
  )
  
  # Termos que indicam absolvição / improcedência / não reconhecimento de fraude
  termos_absolvicao <- c(
    "julgar improcedente", "julgou improcedente", "improcedência",
    "não configurada a fraude", "não caracterizada a fraude", "ausência de prova",
    "provas insuficientes", "provimento para julgar improcedente",
    "insuficiência probatória"
  )
  
  # Lógica de verificação
  tem_condenacao <- any(sapply(termos_condenacao, function(p) str_detect(texto, p)))
  tem_absolvicao <- any(sapply(termos_absolvicao, function(p) str_detect(texto, p)))
  
  if (tem_condenacao & !tem_absolvicao) {
    return("CONDENAÇÃO / FRAUDE RECONHECIDA")
  } else if (tem_absolvicao & !tem_condenacao) {
    return("ABSOLVIÇÃO / IMPROCEDENTE")
  } else if (tem_condenacao & tem_absolvicao) {
    return("DÚVIDA / AMBÍGUO (REVISAR MANUAMENTE)")
  } else {
    return("OUTRO / INDETERMINADO")
  }
}

# 4. Aplicar a classificação linha a linha (usando Vectorize ou rowwise)
dados <- dados %>%
  rowwise() %>%
  mutate(
    decisao_classificada = classificar_decisao(texto_analise)
  ) %>%
  ungroup()

# 5. Remover a coluna auxiliar de análise de texto
dados$texto_analise <- NULL

# 6. Visualizar a distribuição dos resultados
print(table(dados$decisao_classificada))

# 7. Salvar o arquivo final
write.xlsx(dados, "Base_de_dados_tcc_classificada_2.xlsx")

dados$decisao_classificada
