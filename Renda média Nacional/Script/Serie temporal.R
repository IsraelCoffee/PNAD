library(PNADcIBGE)
library(survey)
library(tidyverse)
library(dplyr)
library(ggplot2)

# =============================================================================
# Série Temporal da Renda Média Nacional - PNAD Contínua
# Fonte: IBGE | Pacote: PNADcIBGE
# =============================================================================

# --- 1. Instalação e carregamento de pacotes ----------------------------------

pacotes <- c("PNADcIBGE", "survey", "dplyr", "ggplot2", "lubridate",
             "scales", "tidyr", "stringr")

instalar_se_necessario <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

invisible(lapply(pacotes, instalar_se_necessario))
invisible(lapply(pacotes, library, character.only = TRUE))


# --- 2. Definição do período de análise --------------------------------------

ano_atual  <- as.integer(format(Sys.Date(), "%Y"))
ano_inicio <- ano_atual - 5          # últimos 5 anos
trimestres <- 1:4

# Combina ano × trimestre (grade completa)
periodos <- expand.grid(ano = ano_inicio:ano_atual, trimestre = trimestres) |>
  arrange(ano, trimestre)

cat(sprintf("Coletando dados de %d T1 a %d T4...\n\n", ano_inicio, ano_atual))


# --- 3. Função para baixar e calcular renda média de um trimestre ------------

calcular_renda_trimestre <- function(ano, trimestre) {
  
  cat(sprintf("  Processando %d T%d... ", ano, trimestre))
  
  tryCatch({
    
    # Variáveis necessárias: VD4020 = renda domiciliar per capita habitual
    #                        V2001  = número de moradores (peso de expansão)
    dados_brutos <- get_pnadc(
      year      = ano,
      quarter   = trimestre,
      vars      = c("VD4020", "V2001", "UF"),
      deflator  = TRUE   # aplica deflator oficial do IBGE
    )
    
    # Plano amostral complexo
    plano <- pnadc_design(dados_brutos)
    
    # Estimativa da renda média (apenas ocupados com renda positiva)
    est <- svymean(
      ~VD4020,
      design = subset(plano, !is.na(VD4020) & VD4020 > 0),
      na.rm  = TRUE
    )
    
    resultado <- tibble(
      ano        = ano,
      trimestre  = trimestre,
      periodo    = sprintf("%d-T%d", ano, trimestre),
      data       = as.Date(sprintf("%d-%02d-01", ano, trimestre * 3 - 2)),
      renda_media = as.numeric(coef(est)),
      erro_padrao = as.numeric(SE(est)),
      ic_inf      = as.numeric(coef(est)) - 1.96 * as.numeric(SE(est)),
      ic_sup      = as.numeric(coef(est)) + 1.96 * as.numeric(SE(est))
    )
    
    cat("OK\n")
    return(resultado)
    
  }, error = function(e) {
    cat(sprintf("ERRO: %s\n", conditionMessage(e)))
    return(NULL)
  })
}


# --- 4. Loop de coleta -------------------------------------------------------

resultados_lista <- Map(
  calcular_renda_trimestre,
  periodos$ano,
  periodos$trimestre
)

df_renda <- bind_rows(Filter(Negate(is.null), resultados_lista)) |>
  arrange(data)

cat(sprintf("\n%d trimestres coletados com sucesso.\n\n", nrow(df_renda)))


# --- 5. Visualização: Série Temporal -----------------------------------------

grafico <- ggplot(df_renda, aes(x = data, y = renda_media)) +
  
  # Faixa de intervalo de confiança (95%)
  geom_ribbon(
    aes(ymin = ic_inf, ymax = ic_sup),
    fill  = "#2166ac",
    alpha = 0.15
  ) +
  
  # Linha principal
  geom_line(
    color     = "#2166ac",
    linewidth = 1.2
  ) +
  
  # Pontos trimestrais
  geom_point(
    color = "#2166ac",
    size  = 2.8,
    shape = 21,
    fill  = "white",
    stroke = 1.5
  ) +
  
  # Rótulos nos pontos
  geom_text(
    aes(label = scales::number(renda_media,
                               big.mark = ".",
                               decimal.mark = ",",
                               prefix = "R$ ",
                               accuracy = 1)),
    vjust  = -1.2,
    size   = 2.8,
    color  = "#333333"
  ) +
  
  # Eixos e escalas
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    expand      = expansion(mult = 0.03)
  ) +
  
  scale_y_continuous(
    labels = scales::number_format(
      big.mark    = ".",
      decimal.mark = ",",
      prefix      = "R$ "
    ),
    expand = expansion(mult = c(0.05, 0.12))
  ) +
  
  # Títulos e notas
  labs(
    title    = "Renda Média Nacional — PNAD Contínua",
    subtitle = sprintf(
      "Renda habitual do trabalho principal (deflacionada) | %d T1 a %d T4",
      ano_inicio, ano_atual
    ),
    caption  = paste0(
      "Fonte: IBGE — PNAD Contínua | Variável: VD4020 (com deflator IBGE)\n",
      "Faixa: intervalo de confiança de 95%"
    ),
    x = NULL,
    y = "Renda média mensal (R$)"
  ) +
  
  # Tema limpo
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle    = element_text(color = "#555555", size = 10, hjust = 0),
    plot.caption     = element_text(color = "#777777", size = 8, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#eeeeee"),
    axis.text.x      = element_text(size = 8),
    plot.margin      = margin(15, 20, 10, 15)
  )

print(grafico)

# --- 6. Resumo estatístico ---------------------------------------------------

cat("\n========== RESUMO ==========\n")
cat(sprintf("Período analisado : %s a %s\n",
            format(min(df_renda$data), "%b/%Y"),
            format(max(df_renda$data), "%b/%Y")))
cat(sprintf("Renda média mínima: R$ %s\n",
            scales::number(min(df_renda$renda_media),
                           big.mark = ".", decimal.mark = ",")))
cat(sprintf("Renda média máxima: R$ %s\n",
            scales::number(max(df_renda$renda_media),
                           big.mark = ".", decimal.mark = ",")))
cat(sprintf("Variação total    : %.1f%%\n",
            (last(df_renda$renda_media) / first(df_renda$renda_media) - 1) * 100))
cat("============================\n")