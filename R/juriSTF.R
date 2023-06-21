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

#' Tabelar o conteúdo grande (+250) da página de jurisprudências do STF
#'
#' @param lista
#' @param busca
#' @param quantidade
#' @param UA
#'
#' @return
#' @export
#'
#' @examples
juriSTF.tabela250 = function(lista, busca, quantidade, UA){

  x <- trunc(quantidade / 250)

  # Arquivo de busca json
  stfBusca <- jsonlite::read_json(busca)

  # Quantidade de registros que vão ser buscados
  if(quantidade > 250){
    quantidade <- 250
  } else {
    quantidade
  }
  stfBusca$size <- quantidade

  for(i in 1:x) {

    stfBusca$from <- (i - 1) * 250
    # Extração dos dados
    htmlSTF <- httr::POST(
      'https://jurisprudencia.stf.jus.br/api/search/search',
      body = stfBusca,
      encode = 'json',
      httr::add_headers(
        'User-Agent' = UA
      )
    )

    # Coleta o conteudo
    getContent <- jsonlite::fromJSON(httr::content(htmlSTF, 'text'))
    getTable <- getContent$result$hits$hits$`_source`

    lista[[i]] <- getTable

  }

  # Retorna uma tabela com o conteuo
  return(bind_rows(lista))

}

#' Baixa as decisões extraídas da página de jurisprudências do STF
#'
#'`juriSTF.download` permite baixar as decisões do site de site de [jurisprudência.stf.jus.br](jurisprudência.stf.jus.br).
#'
#' @param conteudo Objeto `html` que pode ser obtido com a função [juriSTF.conteudo()]
#' @param UA User-Agent
#' @param arquivo Local onde será salvo as decisões
#' @param quantidade Quantidade de decisões que se pretende baixar \(não maior do que as buscadas com a função `juriSTF.conteudo`\). Pode ser colocado um valor referente a posição do conteúdo.
#'
#' @return Decisões em `.pdf`
#' @export
#'
#' @examples
juriSTF.download = function(conteudo, UA, arquivo, quantidade){
  for(i in 1:quantidade){
    getConteudo <- content$result$hits$hits[[i]]$`_source`
    doc <- stringr::str_split_i(
      content$result$hits$hits[[i]]$`_source`$inteiro_teor_url,
      pattern = '=',
      -1
    )
    httr::POST(
      paste(
        'https://redir.stf.jus.br/paginadorpub/paginador.jsp?docTP=TP&docID=',
        doc,
        sep = ''
      ),
      httr::add_headers(
        'User-Agent' = UA
      ),
      httr::write_disk(paste(arquivo,getConteudo$titulo, '.pdf', sep = ''), T)
    )
  }
}
