# Extrair conteúdo ----

#' Pegar o conteúdo da página de jurisprudências do STF
#'
#' @description
#' <img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-deprecated.svg" alt="Deprecated"/>
#'
#' Utilize a função [decJ::jurisprudencia_stf()] no lugar.
#'
#'
#' @param busca Arquivo `.json` com os parâmetros de busca
#' @param quantidade Número de decisões que devem retornar
#'
#' @return Um objeto `html` com o conteúdo
#' @export
#'
#' @examples
juriSTF.conteudo <- function(busca, quantidade) {
  lifecycle::deprecate_stop(
    "19/12/2023",
    "juriSTF.conteudo()",
    "jurisprudencia_stF()"
  )
  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  # Arquivo de busca json
  stfBusca <- jsonlite::read_json(busca)
  stfBusca$size <- quantidade

  # Extração dos dados
  htmlSTF <- httr::POST(
    "https://jurisprudencia.stf.jus.br/api/search/search",
    body = stfBusca,
    encode = "json",
    httr::add_headers(
      "User-Agent" = UA
    )
  )

  # Retorna o conteúdo HTML
  return(httr::content(htmlSTF))
}

# Extrair tabela ----

#' Tabelar o conteúdo da página de jurisprudências do STF
#'
#' @description
#' <img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-deprecated.svg" alt="Deprecated"/>
#'
#' Utilize a função [decJ::jurisprudencia_stf()] no lugar.
#'
#'
#' @param busca Arquivo `.json` com os parâmetros de busca
#' @param quantidade Número de decisões que devem retornar
#'
#' @return Uma tabela com o conteúdo
#' @export
#'
#' @examples
juriSTF.tabela <- function(busca, quantidade) {
  lifecycle::deprecate_stop(
    "19/12/2023",
    "juriSTF.tabela()",
    "jurisprudencia_stf()"
  )
  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  # Arquivo de busca json
  stfBusca <- jsonlite::read_json(busca)
  stfBusca$size <- quantidade

  # Quantidade de registros que vão ser buscados
  stfBusca$size <- quantidade

  # Extração dos dados
  htmlSTF <- httr::POST(
    "https://jurisprudencia.stf.jus.br/api/search/search",
    body = stfBusca,
    encode = "json",
    httr::add_headers(
      "User-Agent" = UA
    )
  )

  # Coleta o conteúdo
  getContent <- jsonlite::fromJSON(httr::content(htmlSTF, "text"))

  # Retorna uma tabela com o conteúdo
  return(getContent$result$hits$hits$`_source`)
}

# Extrair conteúdo grande ----

#' Tabelar o conteúdo grande (+250) da página de jurisprudências do STF
#' @description
#' <img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-deprecated.svg" alt="Deprecated"/>
#'
#' Utilize a função [decJ::jurisprudencia_stf()] no lugar.
#' @param busca
#' @param quantidade
#'
#' @return
#' @export
#'
#' @examples
juriSTF.tabela250 <- function(busca, quantidade) {
  lifecycle::deprecate_stop(
    "19/12/2023",
    "juriSTF.tabela250()",
    "jurisprudencia_stf()"
  )
  lista <- list()
  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  x <- trunc(quantidade / 250)

  # Arquivo de busca json
  stfBusca <- jsonlite::read_json(busca)

  # Quantidade de registros que vão ser buscados
  if (quantidade > 250) {
    quantidade <- 250
  } else {
    quantidade
  }
  stfBusca$size <- quantidade

  for (i in 1:x) {
    stfBusca$from <- (i - 1) * 250
    # Extração dos dados
    htmlSTF <- httr::POST(
      "https://jurisprudencia.stf.jus.br/api/search/search",
      body = stfBusca,
      encode = "json",
      httr::add_headers(
        "User-Agent" = UA
      )
    )

    # Coleta o conteudo
    getContent <- jsonlite::fromJSON(httr::content(htmlSTF, "text"))
    getTable <- getContent$result$hits$hits$`_source`

    lista[[i]] <- getTable
  }

  # Retorna uma tabela com o conteudo
  return(dplyr::bind_rows(lista))
}

# Baixar decisões ----

#' Baixa as decisões extraídas da página de jurisprudências do STF
#'
#' `juriSTF.download` permite baixar as decisões do site de site de [jurisprudência.stf.jus.br](jurisprudência.stf.jus.br).
#'
#' @param conteudo Objeto `html` que pode ser obtido com a função [juriSTF.conteudo()]
#' @param arquivo Local onde será salvo as decisões
#' @param quantidade Quantidade de decisões que se pretende baixar \(não maior do que as buscadas com a função `juriSTF.conteudo`\). Pode ser colocado um valor referente a posição do conteúdo.
#'
#' @return Decisões em `.pdf`
#' @export
#'
#' @examples
juriSTF.download <- function(conteudo, arquivo, quantidade) {
  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  for (i in 1:quantidade) {
    getConteudo <- conteudo$result$hits$hits[[i]]$`_source`
    doc <- stringr::str_split_i(
      conteudo$result$hits$hits[[i]]$`_source`$inteiro_teor_url,
      pattern = "=",
      -1
    )
    httr::POST(
      paste(
        "https://redir.stf.jus.br/paginadorpub/paginador.jsp?docTP=TP&docID=",
        doc,
        sep = ""
      ),
      httr::add_headers(
        "User-Agent" = UA
      ),
      httr::write_disk(paste(arquivo, getConteudo$titulo, ".pdf", sep = ""), T)
    )
  }
}
