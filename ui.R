# Aplicação Shiny unificada com três telas: questionário de perfil,
# seleção de setores e visualização do portfólio ranqueado.

# Objetos compartilhados (perguntas, funções e pacotes) estão em global.R

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
