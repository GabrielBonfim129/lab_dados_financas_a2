# Aplicação Shiny unificada com três telas: questionário de perfil,
# seleção de setores e visualização do portfólio ranqueado.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

source("risk_return_ranking.R")

`%||%` <- function(x, y) if (!is.null(x)) x else y

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

theme <- bs_theme(bootswatch = "minty")

ui <- page_navbar(
  id = "main_nav",
  title = "Recomendação de Ativos",
  theme = theme,
  fillable = TRUE,
  nav_panel(
    "Perfil do Investidor",
    tags$head(
      tags$style(HTML(".question-block strong{display:block;font-size:16px;white-space:normal;}\n",
                      ".question-block .form-check-label{white-space:normal;width:100%;}"))
    ),
    layout_columns(
      col_widths = c(9, 3),
      card(
        card_header("Questionário"),
        card_body(
          lapply(questions, function(q) {
            div(
              class = "mb-3 question-block",
              tags$strong(q$title),
              radioButtons(q$id, label = NULL, choices = q$options, selected = character(0), width = "100%")
            )
          })
        ),
        card_footer(
          actionButton("submit_profile", "Calcular perfil", class = "btn-success w-100")
        )
      ),
      card(
        class = "h-100",
        card_header("Resultado"),
        card_body(
          h4(textOutput("profile_text")),
          p("Após calcular, avance para selecionar os setores.")
        ),
        card_footer(
          actionButton("go_to_sectors", "Ir para seleção de setores", class = "btn-primary w-100")
        )
      )
    )
  ),
  nav_panel(
    "Setores",
    layout_sidebar(
      sidebar = sidebar(
        title = "Seleção de setores",
        width = 380,
        checkboxGroupInput(
          "sectors",
          "Escolha os setores que comporão o pool de ativos:",
          choices = names(sector_tickers),
          selected = names(sector_tickers)[1:2],
          width = "100%"
        ),
        sliderInput("years", "Período (anos)", min = 1, max = 5, value = 3, step = 1),
        actionButton("go_to_portfolio", "Avançar para portfólio", class = "btn-primary w-100")
      ),
      card(
        class = "mt-3",
        card_header("Pool selecionado"),
        card_body(
          tags$ul(class = "list-inline", uiOutput("pool_list")),
          helpText("Amostra usada para a clusterização de risco-retorno."),
          verbatimTextOutput("sector_hint")
        )
      )
    )
  ),
  nav_panel(
    "Portfólio",
    layout_sidebar(
      sidebar = sidebar(
        title = "Controles",
        actionButton("run_ranking", "Gerar ranking", class = "btn-success w-100 mb-3"),
        checkboxGroupInput("custom_assets", "Selecione múltiplos ativos para compor o gráfico:", choices = NULL),
        selectInput("focus_asset", "Visualizar um ativo específico:", choices = c("Nenhum" = ""))
      ),
      tagList(
        fluidRow(
          column(
            width = 8,
            card(
              card_header("Histórico de preços normalizado"),
              plotOutput("price_chart", height = 300)
            )
          ),
          column(
            width = 4,
            card(
              card_header("Métricas"),
              uiOutput("metrics_panel"),
              uiOutput("portfolio_note")
            )
          )
        ),
        card(
          card_header("Ranking completo"),
          card_body(tableOutput("ranking_table"))
        )
      )
    )
  )
)

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

shinyApp(ui, server)

