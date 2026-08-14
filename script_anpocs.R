library(readxl)    
library(dplyr) 
library(lmtest)    # para testes estatísticos 
library(sandwich)  # para erros robustos 
library(broom)     # para organizar resultados 
library(fixest)    # para modelos com efeitos fixos (opcional) 

#importando a base de dados
base_de_dados <- read_excel("C:/Users/Usuario/Documents/dados tcc/base de dados - regressão e médias.xlsx")
View(base_de_dados) 
names(base_de_dados) 


############CALCULANDO AS CONDENAÇÕES#############
# Pacotes necessários 
library(readxl) 
library(dplyr) 
library(ggplot2) 

# Calcular aS condenações por ano e por partido 

############ por ideologia  
condenacoes_ideologia <- base_de_dados %>% 
  group_by(ano, ideologia) %>% 
  summarise(
    condenacoes = sum(condenação),
    .groups = "drop"
  )

ggplot(condenacoes_ideologia, aes(x = ano, y = condenacoes, color = ideologia)) + 
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Número de condenações por ano e ideologia",
    x = "Ano",
    y = "Número de condenações",
    color = "Ideologia"
  ) +
  scale_x_continuous(
    breaks = condenacoes_ideologia$ano
  ) +
  theme_minimal()

############ numero de condenações por ano e partido 
condenacoes_filtradas <- dados_filtrados %>% 
  group_by(ano, partido) %>% 
  summarise(
    condenacoes = sum(condenação),
    .groups = "drop"
  )

ggplot(condenacoes_filtradas, aes(x = ano, y = condenacoes, color = partido)) + 
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Número de condenações por ano",
    x = "Ano",
    y = "Número de condenações",
    color = "Partido"
  ) +
  scale_x_continuous(
    limits = c(min(condenacoes_filtradas$ano),
               max(condenacoes_filtradas$ano)),
      breaks = condenacoes_ideologia$ano
    ) +
  theme_minimal() +
  theme(legend.position = "bottom")

##########CALCULANDO A REGRESSÃO
##primeiro, eu tive que agrupar a base de dados a soma de condenações
dados_modelo <- base_de_dados %>%
  group_by(partido, ano) %>%
  summarise(
    condenacoes = sum(condenação),
    aije = max(aije),
    aime = max(aime),
    .groups = "drop"
  )
modelo_fixest_7 <- feols(
  condenacoes ~ ano + aije + aime | partido,
  data = dados_modelo
)

library(modelsummary)
library(flextable)

modelsummary(
  modelo_fixest_7,
  output = "flextable",
  statistic = "p = {p.value}",
  gof_omit = "AIC|BIC|RMSE|FE"
)

table(base_de_dados$condenação)

### testando a multicolineariedade

cor(base_de_dados$aije, base_de_dados$aime)

###gráfico mostrando o coeficiente estimado do aime e aije

coef_df <- tidy(modelo7, conf.int = TRUE) %>%
  filter(term %in% c("aime", "aije"))
ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Coeficiente estimado",
    y = "",
    title = "Coeficientes estimados para AIJE e AIME",
    subtitle = "Modelo com efeitos fixos por partido"
  )

####### numero de processos por fraude em cada ano
processos_ano <- base_de_dados %>%
  group_by(ano) %>%
  summarise(
    processos = n(),
    .groups = "drop"
  )

ggplot(processos_ano, aes(x = ano, y = processos)) +
  geom_line(linewidth = 1.2, color = "pink") +
  geom_point(size = 2, color = "pink") +
  labs(
    title = "Número de processos sobre cota de gênero por ano",
    x = "Ano",
    y = "Número de processos"
  ) +
  scale_x_continuous(
    breaks = processos_ano$ano
  ) +
  theme_minimal()

