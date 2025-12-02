# Lógica do servidor para o aplicativo Shiny. As funções auxiliares estão em global.R

server <- function(input, output, session) {
  profile_score <- reactive({
    vals <- vapply(questions, function(q) as.numeric(input[[q$id]] %||% NA_real_), numeric(1))
    if (any(is.na(vals))) return(NA_real_)
    sum(vals)
  })

  profile <- reactive({ profile_label(profile_score()) })

  horizon_years <- reactive({
    case_when(
      input$horizon == 1 ~ 1,
      input$horizon == 2 ~ 3,
      input$horizon == 3 ~ 5,
      TRUE ~ 3
    )
  })

  selected_years <- reactive({
    input$years %||% horizon_years()
  })

  observeEvent(input$submit_profile, {
    if (is.na(profile_score())) {
      showNotification("Responda todas as perguntas para calcular o perfil.", type = "error")
      return(NULL)
    }
    updateSliderInput(session, "years", value = horizon_years())
    updateNavbarPage(session, "main_nav", selected = "Setores")
  })

  observeEvent(input$go_to_sectors, {
    updateNavbarPage(session, "main_nav", selected = "Setores")
  })

  observeEvent(input$go_to_portfolio, {
    updateNavbarPage(session, "main_nav", selected = "Portfólio")
  })

  output$profile_text <- renderText({
    req(input$submit_profile)
    profile()
  })

  output$pool_list <- renderUI({
    sectors <- input$sectors %||% character(0)
    tickers <- select_tickers_by_sector(sectors)
    lapply(tickers, function(tk) tags$li(class = "list-inline-item badge bg-light text-dark", tk))
  })

  output$sector_hint <- renderText({
    sectors <- input$sectors %||% character(0)
    paste0("Setores escolhidos: ", paste(sectors, collapse = ", "))
  })

  ranking_data <- eventReactive(input$run_ranking, {
    if (is.na(profile_score())) {
      showNotification("Responda ao questionário para calcular o perfil antes de gerar o ranking.", type = "error")
      return(list(ranking = tibble(), prices = tibble()))
    }

    if (is.null(input$sectors) || length(input$sectors) == 0) {
      showNotification("Selecione ao menos um setor para formar o pool de ativos.", type = "error")
      return(list(ranking = tibble(), prices = tibble()))
    }

    pool <- select_tickers_by_sector(input$sectors)
    rank_assets_by_profile(pool, years = selected_years(), profile = profile())
  })

  output$ranking_table <- renderTable({
    data <- ranking_data()$ranking
    req(nrow(data) > 0)
    data %>%
      transmute(
        Posicao = rank,
        Ticker = ticker,
        `Retorno anualizado` = percent(annual_return, accuracy = 0.1),
        `Volatilidade anualizada` = percent(annual_volatility, accuracy = 0.1),
        `Índice de Sharpe` = round(sharpe, 2),
        `Cluster de risco` = risk_label,
        Destaque = if_else(top5, "⭐ Top 5", "")
      )
  })

  observeEvent(ranking_data(), {
    data <- ranking_data()$ranking
    if (nrow(data) == 0) return(NULL)
    updateCheckboxGroupInput(session, "custom_assets", choices = data$ticker, selected = data$ticker[data$top5])
    updateSelectInput(session, "focus_asset", choices = c("Nenhum" = "", data$ticker))
  })

  active_selection <- reactive({
    data <- ranking_data()$ranking
    req(nrow(data) > 0)
    if (!is.null(input$focus_asset) && nchar(input$focus_asset) > 0) {
      return(input$focus_asset)
    }
    selected <- input$custom_assets %||% character(0)
    if (length(selected) > 0) return(selected)
    data$ticker[data$top5]
  })

  output$price_chart <- renderPlot({
    hist <- normalised_history(ranking_data()$prices, active_selection())
    req(nrow(hist) > 0)
    ggplot(hist, aes(x = ref_date, y = px_index, colour = ticker)) +
      geom_line(size = 1) +
      labs(x = "Data", y = "Preço normalizado (100 = início)", colour = "Ticker") +
      theme_minimal()
  })

  output$metrics_panel <- renderUI({
    data <- ranking_data()$ranking
    req(nrow(data) > 0)
    focus <- active_selection()
    metrics <- data %>% filter(ticker %in% focus)
    if (length(focus) > 1) {
      agg <- portfolio_metrics(focus, ranking_data()$prices)
      return(tagList(
        h5("Portfólio selecionado"),
        p(paste("Ativos:", paste(focus, collapse = ", "))),
        p(paste("Retorno anualizado:", percent(agg$annual_return))),
        p(paste("Volatilidade anualizada:", percent(agg$annual_volatility))),
        p(paste("Índice de Sharpe:", round(agg$sharpe, 2)))
      ))
    }

    row <- metrics[1, ]
    tagList(
      h5(paste("Ativo", row$ticker)),
      p(paste("Cluster de risco:", row$risk_label)),
      p(paste("Retorno anualizado:", percent(row$annual_return))),
      p(paste("Volatilidade anualizada:", percent(row$annual_volatility))),
      p(paste("Índice de Sharpe:", round(row$sharpe, 2))),
      p(paste("Score no ranking:", round(row$score, 3)))
    )
  })

  output$portfolio_note <- renderUI({
    if (!is.null(input$focus_asset) && nchar(input$focus_asset) > 0) return(NULL)
    data <- ranking_data()$ranking
    req(nrow(data) > 0)
    top <- data$ticker[data$top5]
    tagList(
      hr(),
      p("Visualização padrão usando o portfólio Top 5 enquanto nenhum ativo específico é selecionado."),
      p(paste("Ativos:", paste(top, collapse = ", ")))
    )
  })
}
