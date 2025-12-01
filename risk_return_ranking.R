# Funções utilitárias para seleção de pool de ativos e rankeamento risco-retorno
# O código usa uma clusterização k-means em métricas de risco e retorno
# para apoiar a recomendação de acordo com o perfil do investidor.

library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(scales)
library(yfR)

`%||%` <- function(x, y) if (!is.null(x)) x else y

# universo simplificado de setores e tickers (B3) usado pela aplicação
sector_tickers <- list(
  `Energia` = c("ELET3", "ELET6", "EQTL3", "TAEE11", "NEOE3", "ENBR3", "ENGI11"),
  `Financeiro` = c("ITUB4", "BBDC4", "BBAS3", "SANB11", "ITSA4"),
  `Consumo` = c("VVAR3", "MGLU3", "LREN3", "AMER3", "BTOW3"),
  `Materiais` = c("VALE3", "GGBR4", "CSNA3", "USIM5", "BRKM5"),
  `Saude` = c("HAPV3", "GNDI3", "RDOR3", "PARD3", "FHER3"),
  `Tecnologia` = c("TOTS3", "WEGE3", "POSI3", "LINX3", "LWSA3"),
  `Saneamento` = c("SBSP3", "CSMG3", "SAPR11", "SAPR4", "SAPR3"),
  `Transportes` = c("RAIL3", "CCRO3", "STBP3", "LOGN3", "AZUL4")
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
rank_assets_by_profile <- function(tickers, years = 3, profile = "Moderado") {
  last_date <- Sys.Date()
  first_date <- last_date - years * 365

  prices <- safe_yf_get(tickers, first_date, last_date)
  metrics <- compute_asset_metrics(prices)
  if (nrow(metrics) == 0) {
    return(list(ranking = tibble(), prices = tibble()))
  }

  # clusterização em risco-retorno
  features <- metrics %>% select(annual_volatility, annual_return)
  km <- kmeans(scale(features), centers = 3, nstart = 20)
  metrics <- metrics %>% mutate(cluster = km$cluster)

  # classifica clusters pelo risco (volatilidade média)
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

# histórico de preços normalizado para o gráfico
normalised_history <- function(prices_df, tickers) {
  if (length(tickers) == 0 || nrow(prices_df) == 0) return(tibble())
  prices_df %>%
    filter(ticker %in% tickers) %>%
    group_by(ticker) %>%
    arrange(ref_date, .by_group = TRUE) %>%
    mutate(px_index = price_adjusted / first(price_adjusted) * 100) %>%
    ungroup()
}

