#' Cria um dicionário de expressões a partir de ngrams
#'
#' @description
#' `dicionario.Criar()` cria um dicionário de expressões usando ngrams do pacote `quanteda`
#' Esse dicionário pode ser aplicado depois com a função [quanteda::tokens_compound()], permitindo unir palavras que possuem um únicos significado quando em conjunto.
#'
#'
#' @param token Objeto do tipo token. Criado com [quanteda::tokens()]
#' @param n Número de ngrams
#' @param arquivo Local para salvar o arquivo. Deve terminar com `/`
#'
#' @return Um arquivo csv com as 500 expressões mais frequentes
#' @export
#'
#' @examples
dicionario.Criar = function(token, n = 2, arquivo){

  ngrams <- quanteda::tokens_ngrams(
     token,
     n = n,
    concatenator = ' '
  )

  ngrams.dfm <- quanteda::dfm(ngrams)
  ngrams.freq <- quanteda::topfeatures(ngrams.dfm, n = 500)

  write.csv2(ngrams.freq,
              file = paste(arquivo, n, 'gram.csv', sep = ''),
              quote = F,
              row.names = T,
              fileEncoding = 'UTF-8')
}

#' Aplica o dicionário de expressões jurídicas
#'
#'@description
#'`dicionario.Aplicar()` aplica aos tokens o dicionário de expressões criados para facilitar as análises textuais de decisões judiciais.
#'O dicionário consta da base de dados do pacote e as atualizações serão informadas no Github
#'
#'
#' @param token Objeto do tipo token. Criado com [quanteda::tokens()]
#'
#' @return Os tokens com as expressões unidas
#' @export
#'
#' @examples
dicionario.Aplicar = function(token) {

  # Aplica o dicionario 6grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = phrase(dic_6grams$V1)
    )
  # Aplica o dicionario 5grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = phrase(dic_5grams$V1)
    )
  # Aplica o dicionario 4grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = phrase(dic_4grams$V1)
    )
  # Aplica o dicionario 3grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = phrase(dic_3grams$V1)
    )
  # Aplica o dicionario 2grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = phrase(dic_2grams$V1)
    )
}
