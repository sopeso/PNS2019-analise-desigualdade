
# Autora: Sophia Araújo de Moraes
# Data: 08/2022

# Instalando e carregando pacotes necessários
loadpackages <- function(packages){
  for(pkg in packages){
    if(!require(pkg, character.only = TRUE)){
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Pacotes necessários para análise e visualização
loadpackages(c("ggcorrplot", "haven", "tinytex", "grid", "tidyverse", "ggpubr", "readxl", "shiny", "readr", 
               "scales", "dplyr", "corrplot", "devtools", "kableExtra", "knitr", "modelsummary", "forcats", 
               "sp", "ggplot2", "rgdal", "ineq", "convey", "pacman", "GISTools", "cowplot", "egg", 
               "classInt", "RColorBrewer", "ggmap", "mapproj", "plyr", "ggrepel", "gridExtra", "IC2"))

# Instala e carrega o pacote 'brmap' do GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("italocegatta/brmap")
library(tibble)
library(brmap)

# Importação e pré-processamento dos dados--------------------------------------

# Dados de sexo
sexo <- read.csv("dados/tab_sexo_unidade.csv", sep = ";")
sexo <- sexo[, !names(sexo) %in% c("X", "X.1", "X.2", "X.3", "X.4")]
ufs <- c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", "Maranhão", "Piauí", 
         "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", 
         "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", 
         "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal", "Total")
sexo[, 1] <- ufs
ordem <- c(5, 1, 3, 6, 4, 2, 7, 11, 14, 10, 15, 12, 13, 8, 16, 9, 18, 17, 19, 20, 21, 22, 23, 26, 25, 24, 27, 28)
sexo[, 4] <- ordem
tabsexo <- arrange(sexo, ordem)
tabsexo <- tabsexo[, c(1, 3)]
tabsexo[, 2] <- tabsexo[, 2] * 100

# Dados de idade
idade <- read.csv("dados/tab_media_idade_unidade.csv", sep = ";")
idade <- idade[, !names(idade) %in% c("X", "X.1")]
idade[, 2] <- as.numeric(idade[, 2])
idade <- idade[, c(1, 2)]
media <- mean(idade[, 2])
idade[28, ] <- c("Total", 69.71876)
idade[, 1] <- ufs
idade[, 3] <- ordem
tabidade <- arrange(idade, ordem)
tabidade <- tabidade[, c(1, 2)]

# Dados de raça
raca <- read.csv("dados/tab_raca_unidade.csv", sep = ";")
raca[, 1] <- ufs
raca[, 5] <- ordem
tabraca <- arrange(raca, ordem)
tabraca <- tabraca[, c(1, 2, 3, 4)]
tabraca[, 2:4] <- tabraca[, 2:4] * 100

# Dados de escolaridade
escolaridade <- read.csv("dados/tab_instrucao_unidade.csv", sep = ";")
escolaridade <- escolaridade[, !names(escolaridade) %in% "X"]
escolaridade[, 1] <- ufs
escolaridade[, 5] <- ordem
tabescolaridade <- arrange(escolaridade, ordem)
tabescolaridade <- tabescolaridade[, c(1, 2, 3, 4)]
tabescolaridade[, 2:4] <- tabescolaridade[, 2:4] * 100

# Junta os dados de sexo, idade, raça e escolaridade
oficial1 <- tabsexo %>% inner_join(tabidade) %>% inner_join(tabraca) %>% inner_join(tabescolaridade)

# Dados de autoavaliação de saúde
autoavaliacaosaude <- read.csv("dados/tab_saude_unidade.csv", sep = ";")
autoavaliacaosaude <- autoavaliacaosaude[, !names(autoavaliacaosaude) %in% c("X", "X.1", "X.2")]
autoavaliacaosaude[, 1] <- ufs
autoavaliacaosaude[, 4] <- ordem
tabautoavaliacaosaude <- arrange(autoavaliacaosaude, ordem)
tabautoavaliacaosaude <- tabautoavaliacaosaude[, c(1, 2)]
tabautoavaliacaosaude <- tabautoavaliacaosaude[,-3] # Mantém apenas dados de autoavaliação classificados como 0 ("Regular, ruim e muito ruim")
tabautoavaliacaosaude[, 2] <- tabautoavaliacaosaude[, 2] * 100

# Dados de multimorbidade
tabmultimorb <- read.csv("dados/tab_mult_unidade.csv", sep = ";")
tabmultimorb <- tabmultimorb[, !names(tabmultimorb) %in% c("X", "X.1", "X.2")]
tabmultimorb[, 1] <- ufs
tabmultimorb[, 4] <- ordem
tabmultimorb <- arrange(tabmultimorb, ordem)
tabmultimorb <- tabmultimorb[, c(1, 2)]
tabmultimorb <- tabmultimorb[,-3] # Mantém apenas dados de multimorbidade classificados como SIM ("HÁ MULTIMORBIDADE")
tabmultimorb[, 2] <- tabmultimorb[, 2] * 100

# Junta os dados de autoavaliação de saúde e multimorbidade
oficial2 <- tabautoavaliacaosaude %>% inner_join(tabmultimorb)

# Resultados Preliminares-------------------------------------------------------

## Caracterização sociodemográfica da amostra

# Abaixo estão as características sociodemográficas da amostra analisada.
# Observa-se que 56,73% são do sexo feminino, 51,45% são brancos e 26,97% possuem ensino médio completo.
# A idade média dos participantes é de 69,72 anos.
# A categoria mais prevalente na escolaridade é o Ensino Fundamental Incompleto, com 63,49%.
# A Tabela 1 apresenta a caracterização da amostra por Unidade Federativa, de acordo com Sexo, Idade Média, Raça/Cor (Branca, Preta, Parda) e Escolaridade (Ensino Fundamental Incompleto, Ensino Médio Incompleto, Ensino Médio Completo).

# Ajustar tipos de dados para numéricos
oficial1$Mulher <- as.numeric(oficial1$Mulher)
oficial1$idade <- as.numeric(oficial1$idade)
oficial1$Branca <- as.numeric(oficial1$Branca)
oficial1$Preta <- as.numeric(oficial1$Preta)
oficial1$Parda <- as.numeric(oficial1$Parda)
oficial1$Fund_inc <- as.numeric(oficial1$Fund_inc)
oficial1$Medio_inc <- as.numeric(oficial1$Medio_inc)
oficial1$Medio_completo <- as.numeric(oficial1$Medio_completo)

# Arredondar valores para 2 casas decimais
oficial1[,-1] <- round(oficial1[,-1], 2)

# Preparar os dados para exibição
off1 <- oficial1
colnames(off1) <- c("UF", "Feminino (%)", "Média Idade", "Branca", "Preta", "Parda", "EF Inc", "EM Inc", "EM Completo")

# Definir legendas para a tabela
fn1 <- "Média Idade = Média das idades por UF"
fn <- "Feminino (%) = Percentual da amostra do sexo feminino"
fn3 <- "EF Inc = Ensino Fundamental Incompleto, EM Inc = Ensino Médio Incompleto, EM Completo = Ensino Médio Completo"

# Gerar e exibir a tabela
library(kableExtra)
off1 %>%
  kbl(caption = "Caracterização sociodemográfica (PNS 2019)") %>%
  add_header_above(c(" " = 3, "Raça/Cor (%)" = 3, "Escolaridade (%)" = 3)) %>%
  pack_rows(index = c("Região Norte" = 7, "Região Nordeste" = 9, "Região Sudeste" = 4, "Região Sul" = 3, "Região Centro-Oeste" = 4, "Brasil" = 1)) %>%
  kable_classic(full_width = FALSE, font_size = 9) %>%
  kable_styling(latex_options = "HOLD_position") %>%
  row_spec(0, bold = TRUE, italic = TRUE, hline_after = TRUE, font_size = 10, align = 'c') %>%
  footnote(c(fn1, fn, fn3))


## Caracterização da população de estudo em relação aos 2 desfechos de saúde: Auto-avaliação de saúde e multimorbidade (2 ou mais doenças crônicas)

# Ajustar tipos de dados para numéricos
oficial2$Regular <- as.numeric(oficial2$Regular)
oficial2$Sim <- as.numeric(oficial2$Sim)

# Arredondar valores para 2 casas decimais
oficial2[,-1] <- round(oficial2[,-1], 2)

# Preparar os dados para exibição
off2 <- oficial2
colnames(off2) <- c("UF", "Auto-avaliação de Saúde Ruim (%)", "Presença de Multimorbidade (%)")

fn2 <- 'Auto-avaliação de Saúde: Regular, Ruim e Muito Ruim (%)'

# Gerar e exibir a tabela
off2 %>%
  kbl(caption = "Caracterização da amostra em relação aos desfechos de saúde: Auto-avaliação de Saúde e Multimorbidade (PNS 2019)") %>%
  pack_rows(index = c("Região Norte" = 7, "Região Nordeste" = 9, "Região Sudeste" = 4, "Região Sul" = 3, "Região Centro-Oeste" = 4, "Brasil" = 1)) %>%
  kable_classic(full_width = FALSE, font_size = 10) %>%
  kable_styling(latex_options = "HOLD_position") %>%
  row_spec(0, bold = TRUE, italic = TRUE, hline_after = TRUE, font_size = 11, align = 'c') %>%
  footnote(fn2)

# Observação dos desfechos de saúde
# A análise dos desfechos de saúde revela que 33,01% da amostra autoavaliou sua saúde como regular, ruim ou muito ruim, enquanto 23,42% da amostra possui multimorbidade (ou seja, apresenta 2 ou mais doenças crônicas).

# Mapas temático: Desfechos de saúde por Unidade Federativa---------------------

# Visando a postulação de uma análise comparativa, foram desenvolvidos mapas temáticos dos desfechos de saúde, por Unidade Federativa.
# Figura 2: Região Nordeste apresenta uma maior porcentagem de idosos que auto-avaliaram sua saúde como regular, ruim ou muito ruim.
# Figura 3: Rio Grande do Sul é o estado que apresenta a maior porcentagem de idosos com multimorbidade; Sul, Sudeste e Nordeste têm altos percentuais de idosos com multimorbidade.

dados <- oficial2[1:27, ]
dadossauderuim <- dados[, 1:2]

# Para autoavaliação de saúde
estados <- left_join(brmap_estado, dadossauderuim, by = c("estado_nome" = "UF"))

g1 <- ggplot(estados) +
  geom_sf(aes(fill = Regular)) +
  scale_fill_distiller(type = "seq", palette = "R3", direction = 1) +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_void() +
  guides(fill = guide_legend(title = "Percentual (%)"))

# Para multimorbidade
dadoscommult <- dados[, -2]
estados2 <- left_join(brmap_estado, dadoscommult, by = c("estado_nome" = "UF"))

g2 <- ggplot(estados2) +
  geom_sf(aes(fill = Sim)) +
  scale_fill_distiller(type = "seq", palette = "R3", direction = 1) +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  labs(caption = "Fonte: autoria própria", fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_void() +
  guides(fill = guide_legend(title = "Percentual (%)", size = 8))

plot1 <- ggarrange(
  g1 + ggtitle("Figura 2\nAuto-avaliação de saúde:\nRegular, ruim e muito ruim (%)\n") +
    theme(plot.title = element_text(hjust = 0.5, size = 9)),
  g2 + ggtitle("Figura 3\nPresença de multimorbidade (%)\n") +
    theme(plot.title = element_text(hjust = 0.5, size = 9)),
  nrow = 1,
  ncol = 2
)

annotate_figure(plot1, top = text_grob("Mapa temático para desfechos de saúde", size = 12))

# Índices de concentração e Fatores Contextuais ------------------------------

# Construção dos índices de concentração usando escolaridade como indicador socioeconômico e autoavaliação de saúde e multimorbidade como indicadores de saúde.
# Os cálculos foram realizados no software Stata.

# Leitura dos dados
indicadores <- read.csv("dados/indicadores.csv", sep = ";")
indicadores1 <- indicadores[1:27, ]

colnames(indicadores) <- c("UF", "CIX Saúde", "CIX Doença", "Renda per capita", "Gini", "IDHM")

fn7 <- "CIX Saúde = Índice de concentração para auto-avaliação de estado de saúde"
fn8 <- "CIX Doença = Índice de concentração para número de doenças crônicas"
fn9 <- "Gini = Índice de Gini"

indicadores %>%
  kbl(caption = "Índices de concentração e variáveis de caracterização do contexto (PNS 2019 e PNAD 2017)") %>%
  pack_rows(index = c("Região Norte" = 7, "Região Nordeste" = 9, "Região Sudeste" = 4, "Região Sul" = 3, "Região Centro-Oeste" = 4, "Brasil" = 1)) %>%
  add_header_above(c(" " = 1, "Índices de concentração" = 2, "Índices de caracterização" = 3)) %>%
  kable_classic(full_width = FALSE, font_size = 9) %>%
  kable_styling(latex_options = "HOLD_position") %>%
  row_spec(0, bold = TRUE, italic = TRUE, hline_after = TRUE, font_size = 10, align = 'c') %>%
  footnote(c(fn7, fn8, fn9))


# Mapas temático: Índices de concentração (CIX)--------------------------------

# Para a interpretação da medida de desigualdade relativa: Índice de concentração, foram utilizados os seguintes critérios:
# 1. Distância do coeficiente com relação ao valor zero. Quanto mais afastados do zero forem os valores, maior será a desigualdade relativa.
# 2. Análise do sinal do coeficiente (negativo ou positivo): um valor positivo do CIX indica que a desigualdade em saúde está mais concentrada entre os grupos socioeconômicos mais altos; um valor negativo sugere que a desigualdade está mais concentrada entre os grupos socioeconômicos mais baixos.

indicadores_saude <- indicadores1[, 1:2]
indicadores_saude$CIX_saude <- as.numeric(indicadores_saude$CIX_saude)
estados3 <- left_join(brmap_estado, indicadores_saude, by = c("estado_sigla" = "UF"))

g3 <- ggplot(estados3) +
  geom_sf(aes(fill = CIX_saude)) +
  scale_fill_distiller(type = "seq", palette = "R3", direction = 1) +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  theme_void() +
  guides(fill = guide_legend(title = "CIX", size = 8))

# Para CIX multimorbidade
indicadores_multimorbidade <- indicadores1[, c(1, 3)]
indicadores_multimorbidade$CIX_multimorbidade <- as.numeric(indicadores_multimorbidade$CIX_multimorbidade)
estados4 <- left_join(brmap_estado, indicadores_multimorbidade, by = c("estado_sigla" = "UF"))

g4 <- ggplot(estados4) +
  geom_sf(aes(fill = CIX_multimorbidade)) +
  scale_fill_distiller(type = "seq", palette = "R3", direction = 1) +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  labs(caption = "Fonte: autoria própria", fill = NULL) +
  theme_void() +
  guides(fill = guide_legend(title = "CIX"))

plot2 <- ggarrange(
  g3 + ggtitle("Figura 4 \nCIX Auto-Avaliação de Saúde\n") + theme(plot.title = element_text(hjust = 0.5, size = 9)),
  g4 + ggtitle("Figura 5 \nCIX Multimorbidade\n") + theme(plot.title = element_text(hjust = 0.5, size = 9)),
  nrow = 1,
  ncol = 2
)

annotate_figure(plot2, top = text_grob("Mapa temático para índices de concentração", size = 12))

## Correlações

### Matrizes de correlação

# Objetivo: Correlacionar as duas medidas de desigualdade social em saúde com as 3 variáveis de caracterização do contexto

# Para analisar a presença ou não de associação entre as variáveis, utilizou-se matrizes de correlação. A primeira matriz relaciona o Índice de Concentração para autoavaliação do estado de saúde e as variáveis de contexto: Índice de Desenvolvimento Humano Municipal (IDHM), Índice de Gini e Renda Per Capita. A segunda matriz relaciona o Índice de Concentração para número de doenças crônicas e as mesmas variáveis de contexto.


attach(indicadores1)

# Matriz de correlação para autoavaliação de saúde
cor_saude <- data.frame(
  'CIX saúde' = as.numeric(unlist(CIX_saude)),
  'Renda' = as.numeric(unlist(Rendapercapita2017)),
  'Gini' = as.numeric(unlist(ÍndicedeGini2017)),
  'IDHM' = as.numeric(unlist(IDHM2017))
)

cor_saude <- as_tibble(cor_saude)
cor1 <- cor(cor_saude, method = "pearson")

col <- colorRampPalette(c('#7aa120', '#bde0a6', "#00300c"))
pval1 <- cor.mtest(cor_saude)$p

corrplot(cor1, method = "color", col = col(200),  
         type = "upper", order = "original", 
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 45, 
         p.mat = pval1, sig.level = 0.05, 
         diag = FALSE, 
         title = "Matriz de Correlação entre: Auto-avaliação \nde saúde e variáveis de contexto",
         tl.cex = 1, titlefont = font(12, "Arial"),
         mar = c(0, 0, 2.3, 0), cl.cex = 0.5
)

# Matriz de correlação para número de doenças crônicas
cor_multimorbidade <- data.frame(
  'CIX multimorbidade' = as.numeric(unlist(CIX_multimorbidade)),
  'Renda' = as.numeric(unlist(Rendapercapita2017)),
  'Gini' = as.numeric(unlist(ÍndicedeGini2017)),
  'IDHM' = as.numeric(unlist(IDHM2017))
)

cor_multimorbidade <- as_tibble(cor_multimorbidade)
cor2 <- cor(cor_multimorbidade, method = "pearson")
pval2 <- cor.mtest(cor_multimorbidade)$p

corrplot(cor2, method = "color", col = col(200),  
         type = "upper", order = "original", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         p.mat = pval2, sig.level = 0.05, # Combine with significance
         diag = FALSE, # Hide correlation coefficient on the principal diagonal
         title = "Matriz de Correlação entre: Número de doença \nde crônicas e variáveis de contexto",
         tl.cex = 1, titlefont = font(12, "Arial"),
         mar = c(0, 0, 2.3, 0), cl.cex = 0.5
)

# Teste de independência
c0 <- cor.test(indicadores1$CIX_saude, indicadores1$Rendapercapita2017, method = "pearson") 
c1 <- cor.test(indicadores1$CIX_saude, indicadores1$ÍndicedeGini2017, method = "pearson") 
c2 <- cor.test(indicadores1$CIX_saude, indicadores1$IDHM2017, method = "pearson") 

c3 <- cor.test(indicadores1$CIX_multimorbidade, indicadores1$Rendapercapita2017, method = "pearson") 
c4 <- cor.test(indicadores1$CIX_multimorbidade, indicadores1$ÍndicedeGini2017, method = "pearson") 
c5 <- cor.test(indicadores1$CIX_multimorbidade, indicadores1$IDHM2017, method = "pearson") 

# Coeficientes de Pearson
c0c <- c0$estimate
c1c <- c1$estimate
c2c <- c2$estimate

c3c <- c3$estimate
c4c <- c4$estimate
c5c <- c5$estimate

# P-valores
p0 <- c0$p.value
p1 <- c1$p.value
p2 <- c2$p.value

p3 <- c3$p.value
p4 <- c4$p.value
p5 <- c5$p.value

# Tabela 1 - Autoavaliação de saúde
variaveis1 <- c("Renda per capita", "Índice de Gini", "IDHM")
correlacoes1 <- c(c0c, c1c, c2c)
pvalores1 <- c(p0, p1, p2)

d <- data.frame(variaveis1, correlacoes1, pvalores1)
names(d) <- c("Variáveis", "Coeficiente de correlação", "P-Valores")
d %>% 
  dplyr::mutate_if(is.numeric, funs(as.character(signif(., 3)))) %>%
  kbl(caption = "Coeficientes de correlação e p-valores de Pearson para: índice de concentração da autoavaliação e variáveis contextuais") %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "HOLD_position") %>%
  row_spec(0, bold = TRUE, italic = TRUE, hline_after = TRUE, font_size = 13, align = 'c')

# Tabela 2 - Multimorbidade
variaveis2 <- c("Renda per capita", "Índice de Gini", "IDHM")
correlacoes2 <- c(c3c, c4c, c5c)
pvalores2 <- c(p3, p4, p5)

d2 <- data.frame(variaveis2, correlacoes2, pvalores2)
names(d2) <- c("Variáveis", "Coeficiente de correlação", "P-Valores")
d2 %>% 
  dplyr::mutate_if(is.numeric, funs(as.character(signif(., 3)))) %>%
  kbl(caption = "Coeficientes de correlação e p-valores de Pearson para: índice de concentração de multimorbidade e variáveis contextuais") %>%
  kable_classic(full_width = F) %>%
  kable_styling(latex_options = "HOLD_position") %>%
  row_spec(0, bold = TRUE, italic = TRUE, hline_after = TRUE, font_size = 14, align = 'c')

# Gráficos de dispersão---------------------------------------------------------

# Construindo gráficos de dispersão para ilustrar as correlações significativas

## Índice de concentração para autoavaliação do estado de saúde e variáveis contextuais correlacionadas

### Renda per capita
disp2 <- ggplot(indicadores1, aes(x = Rendapercapita2017, y = CIX_saude)) + 
  geom_point(color = "#32572b", size = 3) + 
  xlab("Renda per capita (PNAD 2017)") + 
  ylab("CIX saúde") + 
  labs(title = "Gráfico de dispersão",
       subtitle = "CIX auto-avaliação de saúde e Renda per Capita",
       caption = "Fonte: autoria própria") +
  theme(axis.text = element_text(size = 9, colour = "black"), 
        axis.title = element_text(size = 12, colour = "black"))

disp2 + 
  geom_label_repel(aes(label = UF), size = 2,
                   box.padding = 0.25, 
                   point.padding = 0.35,
                   segment.color = 'grey50') +
  theme_classic()

### Índice de Desenvolvimento Humano Municipal (IDHM)

disp3 <- ggplot(indicadores1, aes(x= IDHM2017, y= CIX_saude)) + 
  geom_point(color = "#32572b", size = 3) + xlab("IDHM (PNAD 2017)") + ylab("CIX saúde")+ theme(axis.text = element_text(size = 9, colour = "black"),axis.title = element_text(size = 12, colour = "black"))+ labs(title = "Gráfico de dispersão",
                                                                                                                                                                                                                   subtitle = "CIX para auto-avaliação do estado de saúde e IDHM",
                                                                                                                                                                                                                   caption = "Fonte: autoria própria")
disp3 + 
  geom_label_repel(aes(label = UF),size = 2,
                   box.padding   = 0.25, 
                   point.padding = 0.35,
                   segment.color = 'grey50') +
  theme_classic()

## Índice de concentração para multimorbidade e variáveis contextuais correlacionadas

### Índice de Gini
disp4 <- ggplot(indicadores1, aes(x= ÍndicedeGini2017, y= CIX_multimorbidade)) + 
  geom_point(color = "#32572b", size = 3) + theme(axis.text = element_text(size = 9, colour ="black"),axis.title = element_text(size = 12, colour = "black")) + xlab("Índice de Gini (PNAD 2017)") + ylab("CIX multimorbidade") + labs(title = "Gráfico de dispersão",
                                                                                                                                                                                                                                       subtitle = "CIX multimorbidade e índice de Gini",
                                                                                                                                                                                                                                       caption = "Fonte: autoria própria")

disp4 + 
  geom_label_repel(aes(label = UF),size = 2,
                   box.padding   = 0.25, 
                   point.padding = 0.35,
                   segment.color = 'grey50') +
  theme_classic()
