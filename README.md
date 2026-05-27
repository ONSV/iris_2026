# Projeto IRIS

## Objetivo

O objetivo do Projeto IRIS - Indicadores Rodoviários Integrados de Segurança - é estabelecer indicadores de desempenho para diagnosticar o nível de segurança viária nas unidades da federação brasileira.

## Estrutura

- `R`: Pasta com todos os scripts em R.

- `_targets`: Pasta que armazena todos os objetos gerados pelo `{targets}`. Não editar os arquivos dessa pasta.

- `_targets.R`: Script que controla a execução com o `{targets}`.

- `app`: Pasta que inclui os arquivos do dashboard.

- `data-raw`: Pasta com os dados brutos de entrada do projeto.

- `renv`: Pasta com os pacotes e controle do `{renv}`.

- `renv.lock`: Arquivo do `{renv}` que controla as versões do R e dos pacotes.

- `report`: Pasta com os projetos do Quarto Markdown e arquivos `*.qmd` que geram os relatórios em PDF e DOCX

## Execução

Todo o controle de fluxo desse projeto é controlado pela biblioteca `{targets}`, cuja configuração está descrita em `_targets.R`. Os pacotes utilizados no projeto são controlados pelo `{renv}`. O resultado final da execução é a exportação do relatório nos formatos PDF e DOCX.

Requisitos:
- R > 4.4
- `{renv}`

1. Instalação do `{renv}` e das dependências do projeto: 

```r
install.packages("renv")
renv::init()
renv::restore()
```

2. Alteração dos caminhos dos pacotes

Os reports em `.qmd` carregam os pacotes necessários que estão armazenados pelo `{renv}`. Para isso, o usuário deve indicar o local de armazenamento do `{renv}`. O caminho `renv/library/macos/R-4.4/aarch64-apple-darwin20/` deve ser alterado nos documentos `.qmd` antes de executar o pipeline com o `{targets}`, de acordo com o sistema de quem estiver rodando o projeto.

3. Execução do pipeline com `{targets}`

```r
targets::tar_make()
```

4. Compilação do dashboard

O `{targets}` não compila o dashboard. Para isso, deve-se executar o seguinte comando do pacote `{shiny}`: 

```r
shiny::runApp("app")
```