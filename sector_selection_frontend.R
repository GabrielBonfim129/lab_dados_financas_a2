# Página de seleção de setores e formação do pool de ativos

library(shiny)
library(bslib)
library(dplyr)
library(glue)

if (!exists("b3_sector_catalog")) {
  source("sector_pool_selection.R")
}

sector_choices <- setNames(b3_sector_catalog$sector, b3_sector_catalog$sector)

two_col_badges <- function(sectors) {
  if (length(sectors) == 0) return(NULL)
  tagList(lapply(sectors, function(x) {
    div(class = "badge bg-success text-wrap me-2 mb-2", x)
  }))
}

ui <- page_fillable(
  theme = bs_theme(bootswatch = "minty"),
  tags$head(
    tags$style(
      HTML(paste0(
        ".card-outline {box-shadow: 0 6px 16px rgba(0,0,0,0.08);}\n",
        ".section-title {font-weight: 700; color: #1b4332;}\n",
        ".table-sm td, .table-sm th {padding: 0.35rem;}\n"
      ))
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      title = "Seleção de setores",
      selectInput(
        "sectors",
        label = "Escolha um ou mais setores (B3)",
        choices = sector_choices,
        multiple = TRUE,
        selected = c("Energia e Utilidades", "Financeiro e Seguros")
      ),
      sliderInput(
        "years",
        "Horizonte histórico (anos)",
        min = 1, max = 5, value = 2, step = 1
      ),
      numericInput(
        "min_obs",
        "Observações mínimas por ativo",
        value = 35, min = 10, max = 252, step = 5
      ),
      actionButton("load_pool", "Buscar pool de ativos", class = "btn-success w-100 mt-2")
    ),
    card(
      class = "card-outline",
      card_header(div(class = "section-title", "Seleção do pool de ativos por setor")),
      card_body(
        p("Selecione os grandes setores econômicos para formar um universo inicial de ativos listados na B3. " ,
          "Os dados são buscados via API do Yahoo Finance (pacote yfR) e filtrados por volume de observações."),
        uiOutput("profile_info"),
        hr(),
        h5("Setores escolhidos"),
        uiOutput("sector_badges"),
        hr(),
        h5("Pool de ativos encontrado"),
        verbatimTextOutput("pool_summary"),
        tableOutput("ticker_table")
      )
    )
  )
)

server <- function(input, output, session) {
  pool_data <- eventReactive(input$load_pool, {
    req(input$sectors)
    withProgress(message = "Consultando API", value = 0, {
      incProgress(0.2, detail = "Preparando parâmetros")
      res <- fetch_sector_pool(
        selected_sectors = input$sectors,
        years_back = input$years,
        min_obs = input$min_obs,
        verbose = FALSE
      )
      incProgress(0.7, detail = "Montando tabela")
      res
    })
  }, ignoreNULL = FALSE)

  output$profile_info <- renderUI({
    if (exists("investor_profile_result", envir = .GlobalEnv)) {
      prof <- get("investor_profile_result", envir = .GlobalEnv)
      div(class = "alert alert-info", glue("Perfil investidor selecionado: {prof}"))
    }
  })

  output$sector_badges <- renderUI({
    two_col_badges(input$sectors)
  })

  output$pool_summary <- renderText({
    req(pool_data())
    data <- pool_data()
    glue(
      "Tickers solicitados: {length(data$tickers_requested)} | ",
      "Tickers válidos (com dados): {length(data$tickers_valid)}"
    )
  })

  output$ticker_table <- renderTable({
    req(pool_data())
    df <- pool_data()$sector_summary
    if (nrow(df) == 0) return(NULL)
    df %>%
      dplyr::mutate(
        first = format(first, "%d/%m/%Y"),
        last  = format(last, "%d/%m/%Y")
      ) %>%
      dplyr::arrange(.data$sector, .data$ticker)
  }, striped = TRUE, hover = TRUE, width = "100%", spacing = "s")
}

sector_selection_app <- shinyApp(ui, server)

if (interactive()) {
  shiny::runApp(sector_selection_app)
}
