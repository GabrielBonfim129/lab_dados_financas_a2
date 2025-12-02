# lab_dados_financas_a2

Projeto final do Lab de Dados Aplicado a Finanças (Gabriel Bonfim). A solução hoje é composta por scripts R que calculam o perfil do investidor, constroem um ranking risco-retorno via clusterização k-means e exibem tudo em uma interface Shiny unificada.

## Visão geral dos scripts
- **risk_return_ranking.R**: funções utilitárias para montar o pool de ativos por setor, baixar preços via `yfR`, calcular métricas anualizadas de retorno/volatilidade/Sharpe e rankear os ativos com pesos dependentes do perfil (conservador, moderado ou arrojado). Também gera séries normalizadas e métricas agregadas de portfólio para a visualização final.
- **final_portfolio_frontend.R**: aplicativo Shiny completo com três etapas: (1) questionário de perfil, (2) seleção de setores e horizonte, (3) ranking de ativos com tabela, destaques do Top 5, checkboxes para montar portfólios customizados e gráficos/métricas interativos. Utiliza `updateNavbarPage` para navegar entre as abas e valida entradas antes de rodar o ranking.

## Requisitos
- R com os pacotes `shiny`, `bslib`, `dplyr`, `ggplot2`, `tidyr`, `scales`, `lubridate`, `purrr` e `yfR` instalados.
- Acesso à internet para baixar cotações com `yfR` ao executar o ranking.

## Como executar
1. Instale os pacotes necessários (ex.: `install.packages(c('shiny','bslib','dplyr','ggplot2','tidyr','scales','lubridate','purrr','yfR'))`).
2. Rode o frontend unificado: `Rscript -e "shiny::runApp('final_portfolio_frontend.R')"`.
3. Alternativamente, para testar apenas o questionário isolado: `Rscript -e "shiny::runApp('investor_profile_frontend.R')"`.

