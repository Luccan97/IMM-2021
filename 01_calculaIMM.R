# Carregando as bibliotecas necessárias
pacman::p_load(tidyverse)

# Instalando e carregando a biblioteca microdatasus
remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)

# Extrair dados do Sistema de Informação de Mortalidade do DATASUS
obitos <- fetch_datasus(year_start = 2010, 
                        year_end = 2021,
                        information_system = "SIM-DO", 
                        vars = c('CODMUNRES', # Código do município de residência
                                 'DTOBITO',  # Data do óbito
                                 'IDADE'))  # Idade (variável não processada)

# Processamento dos dados
dados <- process_sim(obitos, municipality_data = FALSE)

# Salvando os dados brutos
saveRDS(dados, "dados_bruto_2010_2021.RDS")

# Realizando o cálculo da idade média ao morrer (IMM) por município e ano
dados_final <- dados %>% 
  # Criando uma nova coluna com o ano de cada registro
  mutate(ano = year(DTOBITO)) %>% 
  # Convertendo as idades para anos e criando uma nova coluna
  mutate(idade_anos_final = case_when(
    !is.na(IDADEanos) ~ as.numeric(IDADEanos), # converte anos para anos
    !is.na(IDADEmeses) ~ as.numeric(IDADEmeses)/12, # converte meses para anos
    !is.na(IDADEhoras) ~ as.numeric(IDADEhoras)/8760, # converte horas para anos
    !is.na(IDADEminutos) ~ as.numeric(IDADEminutos)/525600, # converte minutos para anos
    TRUE ~ as.numeric(IDADEanos) # mantém o valor original da idade em anos 
  )) %>%
  # Agrupando os dados por município e ano
  group_by(CODMUNRES, ano) %>%
  # Calculando a idade média ao morrer e o total de óbitos por município e ano
  summarise(IMM = mean(idade_anos_final, na.rm = TRUE),
            total_obitos = n())

# Salvando os dados finais
saveRDS(dados_final, "IMM_final_2010_2021.RDS")
