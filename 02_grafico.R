# Descrição: Script elabora gráfico para visualizar distribuição regional 
# do indicador Idade média ao morrer dos municípios brasileiros.
# Autor: Lucca Nielsen
# Contato: lnielsen.rwe@gmail.com

# Instalar pacote geobr
devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

# Carregar pacotes necessários
pacman::p_load(tidyverse, geobr)

# Ler arquivo com dados
df <- readRDS("IMM_final_2010_2021.RDS")

# Selecionar ano de interesse
IMM2021 <- df %>% 
  filter(ano == 2021)

# Obter dados geográficos dos municípios
mun <- geobr::read_municipal_seat() %>% 
  mutate(CODMUNRES = as.character(str_sub(code_muni,1, 6)))

# Juntar dados de indicador com dados geográficos
munIMM2021 <- left_join(mun, IMM2021)

# Definir ordem das regiões
munIMM2021$name_region <- factor(munIMM2021$name_region, 
                                 levels = c("Norte","Nordeste","Centro Oeste", "Sudeste", "Sul"))

# Criar histograma


# Criar o histograma com as seguintes características:
g <- ggplot(munIMM2021) +
  
  # Geometria de histograma, definindo que a variável de interesse é a IMM
  # Além disso, definir a cor e preenchimento das barras de acordo com a média da variável IMM
  geom_histogram(aes(x = IMM, 
                     color = ifelse(IMM <=  mean(IMM, na.rm = T),"Abaixo da média","Acima da média"),
                     fill = ifelse(IMM <=  mean(IMM, na.rm = T),"Abaixo da média","Acima da média")),
                 binwidth = 1, 
                 alpha = 0.8) +
  
  # Adicionar uma linha vertical para indicar a média da variável IMM
  geom_vline(xintercept = mean(munIMM2021$IMM, na.rm = T), 
             color = "black", 
             linewidth = 1.5) +
  
  # Definir as cores de preenchimento e borda das barras do histograma manualmente
  scale_fill_manual(values = c("coral2","seagreen"))+
  scale_color_manual(values = c("coral2","seagreen"))+
  
  # Expandir os limites do eixo y para 0, de modo a não haver espaço entre a borda e as barras do histograma
  scale_y_continuous(expand = c(0,0)) +

  
  scale_x_continuous(breaks = round(seq(min(munIMM2021$IMM, na.rm = T), 
                                        max(munIMM2021$IMM, na.rm = T),
                                        by = 5),0),
                    expand = c(0,0)) + 
  
  labs(title = "Idade média ao morrer -  Municípios brasileiros, 2021 ", 
       subtitle = paste0("A média desse indicador para o Brasil em 2021 foi de ",round(mean(munIMM2021$IMM, na.rm = T),0), " anos representado pela linha preta"),
       x = "Idade em anos", 
       fill = "",
       color = "",
       caption = "Fonte: SIM (Sistema de Informação sobre Mortalidade)",
       y = "Número de cidades") +
  
  
  facet_wrap(~name_region, 
             scales = "free_y")+
  
  theme_grey() +
  
  theme(legend.position = "top",
        legend.text = element_text(size = 13),
        plot.title = element_text(hjust = .5, size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = .5),
        strip.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(face = "bold"))


print(g)

# save the ggplot object as a high-quality PNG file
ggsave("grafico.png", g, dpi = 300)

# save the ggplot object as a high-quality JPG file
ggsave("grafico.jpg", g, dpi = 300)
