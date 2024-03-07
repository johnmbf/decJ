#' Extrair dados do portal de jurisprudência do STF
#'
#' @description
#' A função permite extrair dados de jurisprudência direto do portal do Supremo Tribunal Federal.
#'
#' Atualmente a busca pode ser feita em duas bases: decisões monocráticas e de acórdãos.
#'
#' Devido a limitações a quantidade não retorna necessariamente um número exato de decisões. Isto é, se você quer 300 acórdãos e estabelecer que a quantidade é 300, não necessariamente irá retornar 300 acórdãos. Se houver, por exemplo, 800 acórdãos, irá retornar 500 acórdãos, pois a busca sempre retorna a página inteira (com até 250 decisões cada página).
#'
#' O critério busca pode ser construído utilizando dos operadores `AND`,  `OR`, `NO`, `""`, `""~`, `~`, `$`,`?` e `()`. Por exemplo, você quer os acórdãos sobre direito ao esquecimento, portanto, deverá pesquisar `jurisprudencia_stf('"direito ao esquecimento"', 'acordaos')`. Mas digamos que você queira acórdãos sobre direito à vida e direito à dignidade humana: `jurisprudencia_stf('"direito à vida" AND "direito à dignidade humana"', 'acordaos')`
#'
#'
#' @param busca
#' @param base
#' @param quantidade
#'
#' @return Uma tabela com os dados
#'
#'
#' @examples
#' dados <- jurisprudencia_stf('"direito ao esquecimento"', 'acordaos', 5)
#' dados
#'
#' @export
jurisprudencia_stf = function(busca = NULL, classe = NULL, base = c("acordaos", "decisoes"), quantidade = 25){
  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")

  if (!is.null(busca) & is.null(classe)) {
    body <- decJ::busca_jurisprudencia
    body$query$bool$filter[[1]]$query_string$query <- busca
    body$post_filter$bool$must[[1]]$term$base <- base
  } else if (is.null(busca) & !is.null(classe)) {
    body <- decJ::busca_classe
    body$query$bool$filter$query_string$query <- classe
    body$post_filter$bool$must$term$base <- base
  } else if ((!is.null(busca) & !is.null(classe))) {
    stop("Essa função só funciona com busca por palavras chaves OU por classe. Ainda estamos desenvolvendo uma forma de trabalhar com as duas buscas juntas.")
  }

  num_iteracoes <- ceiling(quantidade / 250)

  if (quantidade > 250) {
    body$size <- 250
  } else {
    body$size <- quantidade
  }

  purrr::map_dfr(1:num_iteracoes, ~{
    body$from <- (.x - 1) * 250
    htmlSTF <- httr::POST(
      "https://jurisprudencia.stf.jus.br/api/search/search",
      body = body,
      encode = "json", header
    )
    getContent <- jsonlite::fromJSON(httr::content(htmlSTF, "text"))
    dados <- getContent$result$hits$hits$`_source`
  })
}

#' Title
#'
#' @param busca
#' @param base
#' @param quantidade
#' @param arquivo
#'
#' @return
#' @export
#'
#' @examples
jurisprudencia_stf_download = function(busca = " ", base = c("acordaos", "decisoes"), quantidade = 25, arquivo = "."){
  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")
  body <- decJ::busca_jurisprudencia
  body$query$bool$filter[[1]]$query_string$query <- busca
  body$post_filter$bool$must[[1]]$term$base <- base

  num_iteracoes <- ceiling(quantidade / 250)

  if (quantidade > 250) {
    body$size <- 250
  } else {
    body$size <- quantidade
  }

  purrr::walk(1:num_iteracoes, ~{
    body$from <- (.x - 1) * 250
    htmlSTF <- httr::POST("https://jurisprudencia.stf.jus.br/api/search/search", body = body, encode = "json", header)
    getContent <- jsonlite::fromJSON(httr::content(htmlSTF, "text"))
    conteudo <- getContent$result$hits$hits$`_source`

    purrr::walk(1:quantidade, purrr::slowly(~{
      doc <- stringr::str_split_i(conteudo$inteiro_teor_url[.x],pattern = "=", -1)
      httr::POST(paste0("https://redir.stf.jus.br/paginadorpub/paginador.jsp?docTP=TP&docID=",doc), header,httr::write_disk(paste0(arquivo, conteudo$id[.x], ".pdf"), T))
    }, rate = purrr::rate_delay(5)), .progress = list(type = 'tasks'))
    })
}
