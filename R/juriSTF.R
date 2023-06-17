#' Pegar o conteúdo da página de jurisprudências do STF
#'
#'`juriSTF.conteudo` permite extrair o conteúdo do site de [jurisprudência.stf.jus.br](jurisprudência.stf.jus.br).
#'
#' @param busca Arquivo `.json` com os parâmetros de busca
#' @param quantidade Número de decisões que devem retornar
#' @param UA User-Agent
#'
#' @return Um objeto `html` com o conteúdo
#' @export
#'
#' @examples
juriSTF.conteudo = function(busca, quantidade, UA){

  # Arquivo de busca json
  stfBusca <- jsonlite::read_json(busca)

  # Quantidade de registros que vão ser buscados
  stfBusca$size <- quantidade

  # Extração dos dados
  htmlSTF <- httr::POST(
    'https://jurisprudencia.stf.jus.br/api/search/search',
    body = stfBusca,
    encode = 'json',
    httr::add_headers(
      'User-Agent' = UA
    )
  )

  # Retorna o conteúdo HTML
  return(httr::content(htmlSTF))

}

#' Tabelar o conteúdo da página de jurisprudências do STF
#'
#'`juriSTF.tabela` permite extrair o conteúdo do site de [jurisprudência.stf.jus.br](jurisprudência.stf.jus.br) e devolver em uma tabela.
#'
#' @param busca Arquivo `.json` com os parâmetros de busca
#' @param quantidade Número de decisões que devem retornar
#' @param UA User-Agent
#'
#' @return Uma tabela com o conteúdo
#' @export
#'
#' @examples
juriSTF.tabela = function(busca, quantidade, UA){

  # Arquivo de busca json
  stfBusca <- jsonlite::read_json(busca)

  # Quantidade de registros que vão ser buscados
  stfBusca$size <- quantidade

  # Extração dos dados
  htmlSTF <- httr::POST(
    'https://jurisprudencia.stf.jus.br/api/search/search',
    body = stfBusca,
    encode = 'json',
    httr::add_headers(
      'User-Agent' = UA
    )
  )

  # Coleta o conteúdo
  getContent <- jsonlite::fromJSON(httr::content(htmlSTF, 'text'))

  # Retorna uma tabela com o conteúdo
  return(getContent$result$hits$hits$`_source`)

}
