# Utilitários para seleção de setores e formação do pool de ativos da B3

library(tidyverse)
library(lubridate)
library(yfR)
library(glue)

# Catálogo consolidado com os principais setores negociados na B3
b3_sector_catalog <- tibble::tibble(
  sector = c(
    "Energia e Utilidades",
    "Petróleo, Gás e Combustíveis",
    "Financeiro e Seguros",
    "Saúde e Farmacêutico",
    "Consumo e Varejo",
    "Agronegócio e Alimentos",
    "Materiais Básicos e Mineração",
    "Construção e Imobiliário",
    "Tecnologia e Telecom",
    "Transporte e Logística",
    "Mídia, Educação e Serviços"
  ),
  description = c(
    "Geração, transmissão, distribuição e saneamento básico",
    "Exploração, refino, distribuição e combustíveis renováveis",
    "Bancos, seguradoras e serviços financeiros",
    "Hospitais, diagnósticos, medicamentos e planos de saúde",
    "Varejo físico e digital, vestuário e alimentos",
    "Produção agrícola, proteínas e processamento de alimentos",
    "Siderurgia, mineração, papel & celulose e químicos",
    "Construtoras, incorporadoras e gestão de propriedades",
    "Software, serviços digitais, telecom e data centers",
    "Rodovias, portos, ferrovias e transporte aéreo",
    "Comunicação, educação privada e serviços adjacentes"
  ),
  tickers = list(
    c("ELET3", "ELET6", "EQTL3", "TAEE11", "NEOE3", "ENBR3", "CMIG4", "CPFE3"),
    c("PETR3", "PETR4", "PRIO3", "RRRP3", "RECV3"),
    c("ITUB4", "BBDC4", "BBAS3", "SANB11", "BPAC11", "B3SA3", "IRBR3"),
    c("HAPV3", "RDOR3", "FLRY3", "PARD3"),
    c("MGLU3", "LREN3", "VIIA3", "PCAR3", "ARZZ3"),
    c("SLCE3", "SMTO3", "BEEF3", "JBSS3", "MRFG3", "BRFS3"),
    c("VALE3", "GGBR4", "CSNA3", "USIM5", "CMIN3", "KLBN11"),
    c("CYRE3", "MRVE3", "EZTC3", "TEND3", "DIRR3", "MULT3"),
    c("TOTS3", "LWSA3", "POSI3", "VIVT3", "TIMS3"),
    c("RAIL3", "CCRO3", "ECOR3", "AZUL4", "GOLL4", "LOGG3"),
    c("YDUQ3", "COGN3", "SEER3", "SOMA3", "ALPA4", "CASH3")
  )
)

sector_ticker_map <- b3_sector_catalog %>%
  tidyr::unnest_longer(tickers, keep_empty = FALSE) %>%
  dplyr::rename(ticker = tickers)

list_b3_sectors <- function() {
  b3_sector_catalog %>% dplyr::select(sector, description)
}

sector_candidates <- function(selected_sectors) {
  if (length(selected_sectors) == 0) {
    return(character())
  }
  sector_ticker_map %>%
    dplyr::filter(.data$sector %in% selected_sectors) %>%
    dplyr::pull(.data$ticker) %>%
    unique()
}

safe_yf_get <- function(tks, first_date, last_date) {
  out <- yfR::yf_get(
    tickers = tks,
    first_date = as.Date(first_date),
    last_date  = as.Date(last_date),
    do_cache = FALSE,
    thresh_bad_data = 0.60
  )
  if (!inherits(out$ref_date, "Date")) {
    suppressWarnings({
      d1 <- as.Date(out$ref_date)
      d2 <- ifelse(is.na(d1), as.Date(out$ref_date, "%d/%m/%Y"), d1)
      d3 <- ifelse(is.na(d2), as.Date(out$ref_date, "%m/%d/%Y"), d2)
      out$ref_date <- as.Date(d3, origin = "1970-01-01")
    })
  }
  out %>%
    dplyr::filter(!is.na(.data$ref_date), !is.na(.data$price_adjusted)) %>%
    dplyr::arrange(.data$ticker, .data$ref_date)
}

fetch_sector_pool <- function(selected_sectors,
                              years_back = 2,
                              min_obs = 35,
                              verbose = interactive()) {
  if (length(selected_sectors) == 0) {
    stop("Selecione pelo menos um setor para formar o pool de ativos.")
  }

  tickers_raw <- sector_candidates(selected_sectors)
  if (length(tickers_raw) == 0) {
    stop("Nenhum ticker encontrado para os setores informados.")
  }

  Sys.setlocale("LC_TIME", "C")
  Sys.setenv(TZ = "UTC")

  first_date <- as.Date(Sys.Date() - 365 * years_back)
  last_date  <- as.Date(Sys.Date())

  tickers_yahoo <- paste0(tickers_raw, ".SA")

  raw_prices <- tryCatch(
    safe_yf_get(tickers_yahoo, first_date, last_date),
    error = function(e) {
      message("Falha no yf_get: ", conditionMessage(e))
      tibble::tibble()
    }
  )

  if (nrow(raw_prices) == 0) {
    warning("Nenhum dado retornado pela API para os tickers selecionados.")
    return(list(
      prices = tibble::tibble(),
      tickers_valid = character(),
      tickers_requested = tickers_raw,
      sector_summary = tibble::tibble()
    ))
  }

  valid_prices <- raw_prices %>%
    dplyr::group_by(.data$ticker) %>%
    dplyr::filter(dplyr::n() >= min_obs) %>%
    dplyr::ungroup()

  valid_info <- valid_prices %>%
    dplyr::group_by(.data$ticker) %>%
    dplyr::summarise(
      n = dplyr::n(),
      first = min(.data$ref_date, na.rm = TRUE),
      last  = max(.data$ref_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$n))

  valid_all <- valid_info$ticker
  tickers_final <- gsub("\\.SA$", "", valid_all)

  prices <- valid_prices %>%
    dplyr::filter(.data$ticker %in% valid_all) %>%
    dplyr::select(.data$ref_date, .data$ticker, .data$price_adjusted) %>%
    dplyr::arrange(.data$ticker, .data$ref_date)

  sector_summary <- sector_ticker_map %>%
    dplyr::filter(.data$ticker %in% tickers_final) %>%
    dplyr::left_join(valid_info %>% dplyr::mutate(ticker = gsub("\\.SA$", "", .data$ticker)),
                     by = "ticker") %>%
    dplyr::select(.data$sector, .data$ticker, .data$n, .data$first, .data$last)

  if (isTRUE(verbose)) {
    message(glue::glue("Selecionados {length(tickers_final)} tickers válidos: {paste(tickers_final, collapse = ', ')}"))
  }

  list(
    prices = prices,
    tickers_valid = tickers_final,
    tickers_requested = tickers_raw,
    sector_summary = sector_summary
  )
}

if (interactive()) {
  print(list_b3_sectors())
}
