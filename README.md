# PNAD

Análise da série temporal da renda média nacional a partir da PNAD Contínua (IBGE), usando dados extraídos diretamente pela API do pacote `PNADcIBGE`.

## Conteúdo

**Renda média Nacional**: script em R que baixa os microdados trimestrais dos últimos 5 anos, aplica os pesos amostrais da pesquisa (pacote `survey`) e constrói a série temporal da renda média nacional, com gráfico gerado em ggplot2.

**Pacotes usados:** `PNADcIBGE`, `survey`, `tidyverse`, `dplyr`, `ggplot2`, `lubridate`, `scales`, `tidyr`, `stringr`

## Fonte dos dados

PNAD Contínua (Pesquisa Nacional por Amostra de Domicílios Contínua), IBGE, acessada via pacote `PNADcIBGE`.

## Autor

Israel Lisbôa — Ciências Econômicas, UDF.

[LinkedIn](https://www.linkedin.com/in/israel-lisb%C3%B4a-b02497351/) · [GitHub](https://github.com/IsraelCoffee)
