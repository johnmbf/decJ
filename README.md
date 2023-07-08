
<!-- README.md is generated from README.Rmd. Please edit that file -->

# decJ

<!-- badges: start -->
<!-- badges: end -->

O pacote decJ tem como objetivo auxiliar as pesquisas envolvendo dados
de processos no Supremo Tribunal Federal.

## Instalação

Você pode baixar a versão de desenvolvedor em
[GitHub](https://github.com/johnmbf/decJ) com:

``` r
# install.packages("devtools")
devtools::install_github("johnmbf/decJ")
```

## Exemplos

A função `extrairSTF.partes( )` retorna uma lista com dados das partes
do processo que for requerido. O exemplo abaixo busca as partes da ADI
6201 e retorna um `data frame` com os dados da ADI 6201.

``` r
library(decJ)

listaPartes <- list()
listaPartes <- extrairSTF.partes(listaPartes, 'ADI', 6201, UA)
listaPartes[[6201]] %>% as_tibble()
```

A função `extrairSTF.info( )` retorna uma lista com dados de protocolo e
assunto que for requerido. O exemplo abaixo busca a data de protocolo e
assunto da ADC 26 e retorna em um `data frame`.

``` r
library(decJ)

listaInfo <- list()
listaInfo <- extrairSTF.info(listaInfo, 'ADC', 26, UA)
listaInfo[[26]] %>% as_tibble()
```
