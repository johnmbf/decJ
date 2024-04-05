#' Cria um dicionário de termos a partir de tokens
#'
#' Esta função cria um dicionário de termos a partir de tokens, onde os termos são
#' gerados a partir de n-gramas, e as frequências dos termos são calculadas a partir
#' de uma matriz de documentos-frequência.
#'
#' @param token Um objeto de tokens, normalmente produzido por funções da biblioteca quanteda.
#' @param n Um número inteiro especificando o tamanho dos n-gramas. O padrão é 2.
#' @param arquivo O caminho do arquivo onde o dicionário será salvo.
#'
#' @return Esta função não retorna um valor diretamente, mas salva um arquivo CSV contendo
#' o dicionário de termos.
#'
#' @import quanteda
#' @importFrom utils write.csv2
#' @export
#'
#' @examples
#' token <- quanteda::tokens(c("bigram", "analysis", "example"))
#' dicionario_criar(token, arquivo = "meu_dicionario.csv")
dicionario_criar = function(token, n = 2, arquivo){

  quanteda::tokens_ngrams(
     token,
     n = n,
    concatenator = ' '
  ) |>
    quanteda::dfm() |>
    quanteda::topfeatures(n = 500) |>
    utils::write.csv2(file = paste(arquivo, n, 'gram.csv', sep = ''),
              quote = F,
              row.names = T,
              fileEncoding = 'UTF-8')
}

#' Aplica dicionários de n-gramas a tokens
#'
#' Esta função aplica uma série de dicionários de n-gramas a um objeto de tokens,
#' substituindo sequências de tokens que correspondem aos n-gramas nos dicionários
#' por tokens compostos.
#'
#' @param token Um objeto de tokens, geralmente produzido por funções da biblioteca quanteda.
#'
#' @return Um objeto de tokens modificado após a aplicação dos dicionários de n-gramas.
#'
#' @import quanteda
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' token <- quanteda::tokens(c("bigram", "analysis", "example"))
#' token_modificado <- dicionario_aplicar(token)
dicionario_aplicar = function(token) {

  # Aplica o dicionario 6grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = quanteda::phrase(dic_6grams$V1)
    )
  # Aplica o dicionario 5grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = quanteda::phrase(dic_5grams$V1)
    )
  # Aplica o dicionario 4grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = quanteda::phrase(dic_4grams$V1)
    )
  # Aplica o dicionario 3grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = quanteda::phrase(dic_3grams$V1)
    )
  # Aplica o dicionario 2grams
  token <-
    quanteda::tokens_compound(
      token,
      pattern = quanteda::phrase(dic_2grams$V1)
    )

  return(token)

}
