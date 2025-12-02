# Carrega pacotes e objetos compartilhados pela aplicação Shiny
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(lubridate)
library(purrr)
library(yfR)

`%||%` <- function(x, y) if (!is.null(x)) x else y

# Questionário de perfil do investidor
questions <- list(
  list(
    id = "knowledge",
    title = "Qual é o seu conhecimento sobre investimentos?",
    options = c(
      "Sou iniciante e ainda estou aprendendo" = 1,
      "Tenho algum conhecimento e já fiz poucas aplicações" = 2,
      "Tenho amplo conhecimento e acompanho o mercado com frequência" = 3
    )
  ),
  list(
    id = "objective",
    title = "Qual é o seu principal objetivo ao investir?",
    options = c(
      "Preservar o patrimônio com segurança" = 1,
      "Equilibrar segurança e crescimento" = 2,
      "Maximizar o crescimento, aceitando oscilações" = 3
    )
  ),
  list(
    id = "horizon",
    title = "Qual o seu horizonte de investimento?",
    options = c(
      "Menos de 2 anos" = 1,
      "Entre 2 e 5 anos" = 2,
      "Mais de 5 anos" = 3
    )
  ),
  list(
    id = "tolerance",
    title = "Como você reage a oscilações negativas na carteira?",
    options = c(
      "Fico desconfortável e prefiro sair da posição" = 1,
      "Suporto quedas moderadas esperando recuperação" = 2,
      "Aceito quedas relevantes se houver perspectiva de retorno" = 3
    )
  ),
  list(
    id = "experience",
    title = "Qual a sua experiência com produtos de risco?",
    options = c(
      "Só utilizo renda fixa tradicional" = 1,
      "Já investi em fundos ou multimercados" = 2,
      "Invisto em ações, FIIs ou derivativos" = 3
    )
  )
)

profile_label <- function(score) {
  if (is.na(score)) return("Perfil não avaliado")
  if (score <= 7) {
    "Conservador"
  } else if (score <= 12) {
    "Moderado"
  } else {
    "Arrojado"
  }
}

# Universo de setores (nomes completos B3) e tickers ampliados
sector_tickers <- list(
  `Energia Elétrica` = c("ELET3", "ELET6", "EQTL3", "TAEE11", "NEOE3", "ENBR3", "ENGI11", "CPLE6", "CMIG4", "TRPL4", "COCE5"),
  `Financeiro e Outros` = c("ITUB4", "BBDC4", "BBAS3", "SANB11", "ITSA4", "BPAC11", "BRSR6", "BRBI11"),
  `Consumo Cíclico` = c("MGLU3", "LREN3", "AMER3", "CVCB3", "LAME4", "GUAR3", "ARZZ3"),
  `Consumo não Cíclico` = c("CRFB3", "PCAR3", "ASAI3", "MDIA3", "BRFS3", "JBSS3", "MRFG3"),
  `Materiais Básicos` = c("VALE3", "GGBR4", "CSNA3", "USIM5", "BRKM5", "SUZB3", "KLBN11", "GOAU4"),
  `Petróleo, Gás e Biocombustíveis` = c("PETR4", "PETR3", "PRIO3", "RAIZ4", "RECV3", "UGPA3"),
  `Saúde` = c("HAPV3", "RDOR3", "PARD3", "FLRY3", "DASA3", "QUAL3"),
  `Tecnologia da Informação` = c("TOTS3", "LWSA3", "POSI3", "WEGE3", "BMOB3", "NGRD3"),
  `Telecomunicações` = c("VIVT3", "TIMS3", "OIBR3", "OIBR4", "BRIT3"),
  `Utilidades Públicas` = c("SBSP3", "CSMG3", "SAPR11", "SAPR4", "SAPR3", "EGIE3", "CMIG3")
)

# Seleciona tickers a partir de setores escolhidos
select_tickers_by_sector <- function(chosen_sectors) {
  chosen <- sector_tickers[names(sector_tickers) %in% chosen_sectors]
  unique(unlist(chosen, use.names = FALSE))
}

# Download seguro com tratamento mínimo para datas
safe_yf_get <- function(tickers, first_date, last_date) {
  if (length(tickers) == 0) return(tibble::tibble())
  out <- tryCatch(
    yf_get(
      tickers = paste0(tickers, ".SA"),
      first_date = as.Date(first_date),
      last_date = as.Date(last_date),
      do_cache = FALSE,
      thresh_bad_data = 0.60
    ),
    error = function(e) {
      message("Falha no download de preços: ", conditionMessage(e))
      tibble::tibble()
    }
  )
  if (!inherits(out$ref_date, "Date")) {
    out$ref_date <- as.Date(out$ref_date)
  }
  out %>%
    filter(!is.na(ref_date), !is.na(price_adjusted)) %>%
    mutate(ticker = gsub("\\.SA$", "", ticker)) %>%
    arrange(ticker, ref_date)
}

# Métricas de risco-retorno por ativo
compute_asset_metrics <- function(prices_df) {
  if (nrow(prices_df) == 0) return(tibble())

  prices_df %>%
    group_by(ticker) %>%
    arrange(ref_date, .by_group = TRUE) %>%
    mutate(daily_ret = price_adjusted / lag(price_adjusted) - 1) %>%
    summarise(
      n_obs = sum(!is.na(daily_ret)),
      annual_return = (prod(1 + daily_ret, na.rm = TRUE)^(252 / n_obs) - 1) %>% coalesce(NA_real_),
      annual_volatility = (sd(daily_ret, na.rm = TRUE) * sqrt(252)) %>% coalesce(NA_real_),
      sharpe = ifelse(annual_volatility > 0, annual_return / annual_volatility, NA_real_),
      .groups = "drop"
    ) %>%
    filter(is.finite(annual_return), is.finite(annual_volatility))
}

# Mapeia perfil em pesos de decisão
profile_weights <- function(profile) {
  switch(profile,
         "Conservador" = list(return = 0.30, sharpe = 0.50, vol = -0.20, preferred_cluster = "Baixo risco"),
         "Moderado"    = list(return = 0.40, sharpe = 0.40, vol = -0.20, preferred_cluster = "Risco intermediário"),
         list(return = 0.55, sharpe = 0.30, vol = -0.15, preferred_cluster = "Alto risco"))
}

# Rankeia ativos a partir do perfil e devolve top 5
rank_assets_by_profile <- function(tickers, years, profile) {
  years <- years %||% 3
  profile <- profile %||% "Moderado"

  last_date <- Sys.Date()
  first_date <- last_date - years * 365

  prices <- safe_yf_get(tickers, first_date, last_date)
  metrics <- compute_asset_metrics(prices)
  if (nrow(metrics) == 0) {
    return(list(ranking = tibble(), prices = tibble()))
  }

  # Clusterização em risco-retorno
  features <- metrics %>% select(annual_volatility, annual_return)
  km <- kmeans(scale(features), centers = 3, nstart = 20)
  metrics <- metrics %>% mutate(cluster = km$cluster)

  # Classifica clusters pelo risco (volatilidade média)
  cluster_order <- metrics %>%
    group_by(cluster) %>%
    summarise(avg_vol = mean(annual_volatility), .groups = "drop") %>%
    arrange(avg_vol) %>%
    mutate(risk_label = c("Baixo risco", "Risco intermediário", "Alto risco"))

  metrics <- metrics %>%
    left_join(cluster_order, by = "cluster")

  w <- profile_weights(profile)

  metrics <- metrics %>%
    mutate(
      scaled_return = rescale(annual_return, to = c(0, 1)),
      scaled_sharpe = rescale(sharpe %||% 0, to = c(0, 1)),
      scaled_vol = rescale(annual_volatility, to = c(0, 1)),
      cluster_bonus = case_when(
        risk_label == w$preferred_cluster ~ 0.10,
        TRUE ~ 0
      ),
      score = w$return * scaled_return + w$sharpe * scaled_sharpe + w$vol * (1 - scaled_vol) + cluster_bonus
    ) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number(), top5 = rank <= 5)

  list(ranking = metrics, prices = prices)
}

# Resumo agregado para um portfólio selecionado (para o gráfico padrão)
portfolio_metrics <- function(selected_tickers, prices_df) {
  if (length(selected_tickers) == 0 || nrow(prices_df) == 0) return(tibble())
  prices_df %>%
    filter(ticker %in% selected_tickers) %>%
    group_by(ref_date) %>%
    summarise(portfolio_price = mean(price_adjusted, na.rm = TRUE), .groups = "drop") %>%
    arrange(ref_date) %>%
    mutate(daily_ret = portfolio_price / lag(portfolio_price) - 1) %>%
    summarise(
      annual_return = (prod(1 + daily_ret, na.rm = TRUE)^(252 / sum(!is.na(daily_ret))) - 1),
      annual_volatility = sd(daily_ret, na.rm = TRUE) * sqrt(252),
      sharpe = ifelse(annual_volatility > 0, annual_return / annual_volatility, NA_real_)
    )
}

# Histórico de preços normalizado para o gráfico
normalised_history <- function(prices_df, tickers) {
  if (length(tickers) == 0 || nrow(prices_df) == 0) return(tibble())
  prices_df %>%
    filter(ticker %in% tickers) %>%
    group_by(ticker) %>%
    arrange(ref_date, .by_group = TRUE) %>%
    mutate(px_index = price_adjusted / first(price_adjusted) * 100) %>%
    ungroup()
}
