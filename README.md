# lab_dados_financas_a2
Final Project. Data lab applied to finances - Gabriel Bonfim
This project creates a program that recommends assets for the user to buy based on a questionnaire filled out in a interface.

New test line

## Questionário de perfil do investidor

O script `investor_profile_frontend.R` adiciona uma aba simples em Shiny para avaliar o perfil do investidor (conservador, moderado ou arrojado) seguindo perguntas inspiradas em questionários de suitability brasileiros. Para executar:

1. Certifique-se de ter `shiny` e `bslib` instalados.
2. Rode `Rscript -e "shiny::runApp('investor_profile_frontend.R')"`.
3. Após responder e clicar em **Calcular perfil**, o resultado fica disponível na variável global `investor_profile_result`, que pode ser reutilizada por outros scripts (ex.: avaliação de ativos).
