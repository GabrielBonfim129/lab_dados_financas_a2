# Interface simples de avaliação de perfil do investidor
# Inspirada em questionários padronizados utilizados em plataformas brasileiras

library(shiny)
library(bslib)

`%||%` <- function(x, y) if (!is.null(x)) x else y

# variável global para reutilização em outros scripts
investor_profile_result <- "Não avaliado"

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

ui <- page_fillable(
  theme = bs_theme(bootswatch = "minty"),
  tags$head(
    tags$style(
      HTML(paste0(
        ".page-header {margin-bottom: 30px; text-align: center;}\n",
        ".question-card {background: #ffffff; border-radius: 12px; padding: 18px; margin-bottom: 14px;",
        "box-shadow: 0 6px 18px rgba(0,0,0,0.08);}\n",
        ".question-title {font-weight: 700; color: #184d47; margin-bottom: 8px;}\n",
        ".profile-box {background: linear-gradient(135deg, #e8f5e9, #e3f2fd);",
        "border-radius: 14px; padding: 18px; text-align: center;",
        "box-shadow: inset 0 0 0 1px #c5e1a5; margin-top: 8px;}\n",
        ".profile-label {font-size: 28px; font-weight: 800; color: #2e7d32;}\n",
        ".footer-note {color: #52796f; margin-top: 8px; font-size: 12px;}\n"
      ))
    )
  ),
  layout_sidebar(
    sidebar = sidebar(
      title = "Questionário de Perfil",
      lapply(questions, function(q) {
        div(class = "question-card",
            div(class = "question-title", q$title),
            radioButtons(q$id, label = NULL, choices = q$options, selected = character(0))
        )
      }),
      actionButton("submit", "Calcular perfil", class = "btn-success w-100 mt-2")
    ),
    card(
      class = "mt-3",
      card_header(div(class = "page-header",
                       h2("Avaliação de Perfil do Investidor"),
                       p("Modelo simplificado inspirado em questionários de suitability utilizados no mercado brasileiro."))),
      card_body(
        div(class = "profile-box",
            h4("Perfil sugerido"),
            div(class = "profile-label", textOutput("profile_text", container = span)),
            p("Pontuação agregada a partir das respostas selecionadas.")
        ),
        uiOutput("score_details"),
        div(class = "footer-note",
            "O resultado fica armazenado na variável global `investor_profile_result` para uso em outros scripts.")
      )
    )
  )
)

server <- function(input, output, session) {
  score <- reactive({
    vals <- vapply(questions, function(q) as.numeric(input[[q$id]] %||% NA_real_), numeric(1))
    if (any(is.na(vals))) return(NA_real_)
    sum(vals)
  })

  profile <- reactive({ profile_label(score()) })

  observeEvent(input$submit, {
    if (!is.na(score())) {
      assign("investor_profile_result", profile(), envir = .GlobalEnv)
    }
  })

  output$profile_text <- renderText({
    req(input$submit)
    profile()
  })

  output$score_details <- renderUI({
    req(input$submit)
    if (is.na(score())) {
      return(div(class = "text-danger mt-3", "Responda todas as perguntas para calcular o perfil."))
    }
    profile_class <- profile()
    badge_class <- switch(profile_class,
                         "Conservador" = "bg-primary",
                         "Moderado" = "bg-warning text-dark",
                         "Arrojado" = "bg-danger",
                         "bg-secondary")
    tagList(
      h5("Detalhamento da pontuação"),
      p("Soma dos pontos (1 = conservador, 3 = arrojado) em cada questão."),
      div(class = paste("badge", badge_class), profile_class),
      p(class = "mt-2", paste0("Pontuação final: ", score(), " pontos."))
    )
  })
}

investor_profile_app <- shinyApp(ui, server)

if (interactive()) {
  shiny::runApp(investor_profile_app)
}

