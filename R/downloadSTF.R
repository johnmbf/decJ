#' Baixa as decisões extraídas da página de jurisprudências do STF
#'
#' `downloadSTF.juris` permite baixar as decisões do site de site de [jurisprudência.stf.jus.br](jurisprudência.stf.jus.br).
#'
#' Igual a função [juriSTF.download()]
#'
#' @param conteudo Objeto `html` que pode ser obtido com a função [juriSTF.conteudo()]
#' @param arquivo Local onde será salvo as decisões
#' @param quantidade Quantidade de decisões que se pretende baixar \(não maior do que as buscadas com a função `juriSTF.conteudo`\). Pode ser colocado um valor referente a posição do conteúdo.
#'
#' @return Decisões em `.pdf`
#' @export
#'
#' @examples
downloadSTF.juris <- function(conteudo, arquivo, quantidade) {

  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  for (i in 1:quantidade) {
    doc <- stringr::str_split_i(
      conteudo$inteiro_teor_url[i],
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
      httr::write_disk(paste(arquivo, conteudo$id[i], ".pdf", sep = ""), T)
    )

    # Aguarde 5 segundos
    date_time <- Sys.time()
    while ((as.numeric(Sys.time()) - as.numeric(date_time)) < 5) {}
  }
}

#' Baixa a petição inicial do processo de controle concentrado
#'
#' @param classe
#' @param n
#' @param arquivo
#'
#' @return
#' @export
#'
#' @examples
downloadSTF.inicial <- function(classe, n, arquivo) {

  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  for (i in n) { ## faça isso em cada processo ##

    # Busque no site o processo
    getProcesso <- httr::GET(
      paste(
        "https://portal.stf.jus.br/processos/listarProcessos.asp?classe=",
        classe,
        "&numeroProcesso=",
        i,
        sep = ""
      ),
      httr::add_headers(
        "User-Agent" = UA
      )
    )

    # Salve o incidente
    getIncidente <- stringr::str_split_i(
      getProcesso$url,
      pattern = "=",
      -1
    )

    # Consulta o processo eletrônico
    getProcesso <- httr::GET(
      paste("https://redir.stf.jus.br/estfvisualizadorpub/jsp/consultarprocessoeletronico/ConsultarProcessoEletronico.jsf?seqobjetoincidente=",
        getIncidente,
        sep = ""
      ),
      httr::add_headers(
        "User-Agent" = UA
      )
    )

    # Leia o conteúdo do processo
    getConteudo <- httr::content(getProcesso, encoding = "UTF-8")

    # Busca a petição inicial
    getInicial <- getConteudo |>
      rvest::html_element("a")

    getInicial <- xml2::xml_attr(getInicial, "href")

    # Salva o arquivo
    httr::GET(
      getInicial,
      httr::add_headers(
        "User-Agent" = UA
      ),
      httr::write_disk(paste(arquivo, i, "_inicial", ".pdf", sep = ""), T)
    )

    # Aguarde 5 segundos
    date_time <- Sys.time()
    while ((as.numeric(Sys.time()) - as.numeric(date_time)) < 5) {}
  }
}
