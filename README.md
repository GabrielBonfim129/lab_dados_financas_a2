# lab_dados_financas_a2

Aplicação Shiny do projeto final do Lab de Dados Aplicado a Finanças (Gabriel Bonfim). Ela calcula o perfil do investidor, gera um ranking risco-retorno via clusterização k-means e exibe os resultados em uma interface única.

## Novidades mais recentes
- Interface reorganizada em três etapas (perfil, setores e portfólio) para guiar o usuário pelo fluxo completo.
- Controles revisados para seleção de setores, horizonte de análise (anos) e filtros de ativos focados/customizados.
- Métricas e gráficos consolidados na aba de portfólio, incluindo ranking completo e histórico de preços normalizado.

## Estrutura do projeto
- **global.R**: carrega pacotes, perguntas do questionário, universo de setores e funções de cálculo.
- **ui.R**: define a navegação entre abas, layout dos cards e inputs utilizados no app.
- **server.R**: implementa a lógica reativa do questionário, seleção de setores, ranking e visualizações.
- **rsconnect/**: metadados de publicação no shinyapps.io.

## Demonstração online
A interface publicada está disponível em: https://cd5oc7-gabriel-bonfim.shinyapps.io/lab_dados_a2/

## Como executar localmente
1. Instale os pacotes necessários: `install.packages(c('shiny','bslib','dplyr','ggplot2','tidyr','scales','lubridate','purrr','yfR'))`.
2. Execute a aplicação a partir do diretório do projeto:
   ```
   Rscript -e "shiny::runApp('.')"
   ```
3. Abra o navegador no endereço informado pelo console do R para interagir com as abas de Perfil do Investidor, Setores e Portfólio.
