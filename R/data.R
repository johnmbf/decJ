#' Dicionários de expressões com 6 palavras
#'
#' Conjunto de expressões frequentes em decisões judiciais para serem aplicadas sobre tokens para facilitar na análise textual de decisões judiciais.
#'
#' @format ## `dic_6grams`
#' Um data frame com 1 coluna e 9 linhas:
#' \describe{
#'  \item{V1}{Expressões}
#' }
#'
#' @source Elaborado por Jonathan Ferreira
"dic_6grams"

#' Dicionários de expressões com 5 palavras
#'
#' Conjunto de expressões frequentes em decisões judiciais para serem aplicadas sobre tokens para facilitar na análise textual de decisões judiciais.
#'
#' @format ## `dic_5grams`
#' Um data frame com 1 coluna e 1 linha:
#' \describe{
#'  \item{V1}{Expressões}
#' }
#'
#' @source Elaborado por Jonathan Ferreira
"dic_5grams"

#' Dicionários de expressões com 4 palavras
#'
#' Conjunto de expressões frequentes em decisões judiciais para serem aplicadas sobre tokens para facilitar na análise textual de decisões judiciais.
#'
#' @format ## `dic_4grams`
#' Um data frame com 1 coluna e 14 linhas:
#' \describe{
#'  \item{V1}{Expressões}
#' }
#'
#' @source Elaborado por Jonathan Ferreira
"dic_4grams"

#' Dicionários de expressões com 3 palavras
#'
#' Conjunto de expressões frequentes em decisões judiciais para serem aplicadas sobre tokens para facilitar na análise textual de decisões judiciais.
#'
#' @format ## `dic_3grams`
#' Um data frame com 1 coluna e 15 linhas:
#' \describe{
#'  \item{V1}{Expressões}
#' }
#'
#' @source Elaborado por Jonathan Ferreira
"dic_3grams"

#' Dicionários de expressões com 2 palavras
#'
#' Conjunto de expressões frequentes em decisões judiciais para serem aplicadas sobre tokens para facilitar na análise textual de decisões judiciais.
#'
#' @format ## `dic_2grams`
#' Um data frame com 1 coluna e 41 linhas:
#' \describe{
#'  \item{V1}{Expressões}
#' }
#'
#' @source Elaborado por Jonathan Ferreira
"dic_2grams"

#' Lista para uniformização do nome das partes
#'
#' Objeto do tipo `characeter` que contém nome das partes de processos de controle concentrado para serem uniformizadas. Para uniformizar é só utilizar a função da seguinte forma: `[COLUNA/TEXTO QUE SERÁ ALTERADO] |>stringr::str_replace_all(decJ::lista_partes)`
#'
#' @format ## `lista_partes`
#' Um character com 842 elementos
#'
#' @source Elaborado por Jonathan Ferreira
"lista_partes"

#' Lista para uniformização do nome das partes
#'
#' Objeto do tipo `characeter` que contém nome das partes de processos de controle concentrado para serem uniformizadas. Para uniformizar é só utilizar a função da seguinte forma: `[COLUNA/TEXTO QUE SERÁ ALTERADO] |>stringr::str_replace_all(decJ::lista_partes)`
#'
#' @format ## `Busca Juisprudencia com base em palavras chaves`
#' Um character com 842 elementos
#'
#' @source Elaborado por Jonathan Ferreira
"busca_jurisprudencia"

#' Lista para uniformização do nome das partes
#'
#' Objeto do tipo `characeter` que contém nome das partes de processos de controle concentrado para serem uniformizadas. Para uniformizar é só utilizar a função da seguinte forma: `[COLUNA/TEXTO QUE SERÁ ALTERADO] |>stringr::str_replace_all(decJ::lista_partes)`
#'
#' @format ## `Busca Classe`
#' Um character com 842 elementos
#'
#' @source Elaborado por Jonathan Ferreira
"busca_classe"
