# 1. Instalar e carregar pacotes
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")

library(readxl)
library(writexl)
library(dplyr)
library(stringr)

# 2. Carregar a base de dados
df <- read_excel("C:/Users/Usuario/Documents/dados tcc/Base tabular - Jurisprudência TSE.xlsx")

# 3. Função de classificação jurídica automática
classificar_juridico <- function(decisao, ementa) {
  txt <- str_to_lower(paste(coalesce(as.character(decisao), ""), coalesce(as.character(ementa), "")))
  dec <- str_to_lower(coalesce(as.character(decisao), ""))
  
  # A. Termos de CONDENAÇÃO / PROCEDÊNCIA
  condenacao <- c(
    "julgar procedente", "julgou procedente", "procedência da aije", "procedência da aime",
    "cassar", "cassação", "declarar a inelegibilidade", "nulidade dos votos", 
    "desconstituir o drap", "desconstituição dos diplomas", "reconhecer a fraude", "procedência da investigação", "reconhecimento da fraude"
  )
  
  # B. Termos de ABSOLVIÇÃO / IMPROCEDÊNCIA
  absolvicao <- c(
    "julgar improcedente", "julgou improcedente", "improcedência", "não configurada a fraude",
    "não caracterizada a fraude", "inocorrência", "ausência de prova", "insuficiência probatória",
    "afastar a fraude", "restabelecer a sentença"
  )
  
  # Checagem lógica
  tem_condenacao <- any(sapply(condenacao, function(t) str_detect(txt, t)))
  tem_absolvicao <- any(sapply(absolvicao, function(t) str_detect(txt, t)))
  
  if (tem_condenacao && !str_detect(dec, "negou provimento ao recurso de")) {
    return("CONDENAÇÃO / FRAUDE RECONHECIDA")
  } else if (tem_absolvicao) {
    return("ABSOLVIÇÃO / IMPROCEDENTE")
  } else {
    return("PROCESSUAL / NÃO CONHECIDO / OUTRO")
  }
}

# 4. Aplicar a reclassificação automática
df <- df %>%
  rowwise() %>%
  mutate(
    decisao_classificada_final = classificar_juridico(textoDecisao, textoEmenta)
  ) %>%
  ungroup()

# 5. Exibir resultado na tela
cat("=== RESULTADO DA CLASSIFICAÇÃO AUTOMÁTICA ===\n")
print(table(df$decisao_classificada_final))

# 6. Salvar novo arquivo
write_xlsx(df, "Base_de_dados_tcc_RECLASSIFICADA_FINAL.xlsx")
cat("\n✅ Planilha salva com sucesso sem pendências manuais!\n")   
getwd()

## revisão manual
# 5. Criar amostra aleatória para validação manual

set.seed(123)  # garante que a mesma amostra possa ser reproduzida

amostra_validacao <- df %>%
  group_by(decisao_classificada_final) %>%
  slice_sample(n = min(30, n())) %>%
  ungroup() %>%
  mutate(
    classificacao_manual = NA_character_
  )

# 6. Verificar quantos casos foram selecionados por categoria
cat("=== AMOSTRA PARA VALIDAÇÃO MANUAL ===\n")
print(table(amostra_validacao$decisao_classificada_final))

# 7. Salvar a amostra em um arquivo separado
write_xlsx(
  amostra_validacao,
  "Amostra_para_validacao_manual.xlsx"
)

cat("\n✅ Amostra criada e salva com sucesso!\n")

amostra_validacao <- read_excel(
  "C:/Users/Usuario/Documents/amostra_para_revisao_manual.xlsx"
)

# 8. Calcular acurácia da classificação automática

amostra_validacao <- amostra_validacao %>%
  mutate(
    acertou = decisao_classificada_final == resultado_manual
  )

# Acurácia geral
acuracia <- mean(amostra_validacao$resultado_manual, na.rm = TRUE)

cat(
  "Acurácia nos casos revisados:",
  round(acuracia * 100, 2),
  "%\n"
)

revisados <- sum(!is.na(amostra_validacao$resultado_manual))

acertos <- sum(amostra_validacao$resultado_manual == 1, na.rm = TRUE)

erros <- sum(amostra_validacao$resultado_manual == 0, na.rm = TRUE)

cat("Casos revisados:", revisados, "\n")
cat("Acertos:", acertos, "\n")
cat("Erros:", erros, "\n")
cat("Acurácia:", round(acuracia * 100, 2), "%\n")