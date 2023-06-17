
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
6201 a 6205 e retorna um `data frame` com os dados da ADI 6201.

``` r
library(decJ)
#> Carregando pacotes exigidos: tidyr

listaPartes <- list()
listaPartes <- extrairSTF.partes(listaPartes, 'ADI', 6201:6205, UA)
listaPartes[[6201]]
#>    ADI        Tipo
#> 1 6201   REQTE.(S)
#> 2 6201   ADV.(A/S)
#> 3 6201 INTDO.(A/S)
#> 4 6201   ADV.(A/S)
#>                                                            Parte
#> 1 ASSOCIACAO NACIONAL DOS DELEGADOS DE POLICIA JUDICIARIA - ADPJ
#> 2                   HILTON ULISSES FIALHO ROCHA JUNIOR (5967/PI)
#> 3                                  GOVERNADOR DO ESTADO DO PIAUÍ
#> 4                            PROCURADOR-GERAL DO ESTADO DO PIAUÍ
```

A função `extrairSTF.info( )` retorna uma lista com dados de protocolo e
assunto que for requerido. O exemplo abaixo busca a data de protocolo e
assunto da ADC 26 e retorna em um `data frame`.

``` r
library(decJ)

listaInfo <- list()
listaInfo <- extrairSTF.info(listaInfo, 'ADC', 26, UA)
listaInfo[[26]]
#>   ADC Ajuizamento
#> 1  26  17/02/2010
#>                                                                                                                                                                                                                                                                                           Assunto
#> 1 DIREITO ADMINISTRATIVO E OUTRAS MATÉRIAS DE DIREITO PÚBLICO | Serviços | Concessão / Permissão / Autorização | Energia Elétrica\r\n                        \r\n                            DIREITO DO TRABALHO | Responsabilidade Solidária / Subsidiária | Terceirização / Tomador de Serviços
```
