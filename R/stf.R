#' Extrair dados de processos do Supremo Tribunal Federal
#'
#'@description
#' Quatro funções que permitem extrair diferentes dados de processos no Supremo Tribunal Federal.
#'
#' [stf_partes()]: Extrair as partes dos processos
#'
#' [stf_info()]: Extrair data de ajuizamento e assuntos do processo
#'
#' [stf_relator()]: Extrair o relator do processo
#'
#' [stf_decisoes()]: Extrair as decisões tomadas no processo
#'
#' @param classe Classe processual. Por exemplo: "ADPF" ou "ADI". Usar abreviatura da classe. Se tiver em dúvida, consultar o site do Supremo.
#' @param processo Número dos processos que pretende solicitar. Por exemplo: 500:600 (do 500 ao 600), ou list(500,600,700) que vai buscar o processo 500, 600 e 700
#'
#' @return Data frame
#'
#' @examples
#' # Retorna 1 processo
#' tabela_partes <- stf_partes("ADPF", 500)
#'
#' # Retorna uma sequência de processos
#' tabela_partes <- stf_partes("ADI", 1500:1505)
#'
#' # Retorna um conjunto de processos
#' tabela_partes <- stf_partes("ADO", c(10,15,20))
#'
#' @export
stf_ = function(){
  message("Conjunto de funções para extrair dados de processos do Supremo Tribunal Federal")
  help(stf_)
}

#' @rdname stf_
stf_partes = function(classe, processo){
  classe <- base::toupper(classe)
  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")

  purrr::map_dfr(processo, purrr::slowly(purrr::possibly(~{
    getProcesso <- httr::GET(
      paste0("https://portal.stf.jus.br/processos/listarProcessos.asp?classe=", classe, "&numeroProcesso=", .x), header)

    getIncidente <- stringr::str_split_i(getProcesso$url, pattern = "=", -1)

    getPartes <- httr::GET(
      paste0("https://portal.stf.jus.br/processos/abapartes.asp?incidente=", getIncidente), header)

    getConteudo <- httr::content(getPartes, encoding = "UTF-8")

    getTipo <- xml2::xml_find_all(
      getConteudo,
      "//div[@class='detalhe-parte']"
    ) |> xml2::xml_text()

    getNome <- xml2::xml_find_all(
      getConteudo,
      "//div[@class='nome-parte']"
    ) |> xml2::xml_text()

    dados <- data.frame(
      Classe = .x,
      Tipo = getTipo,
      Parte = getNome
    )

  }, NULL), rate = purrr::rate_delay(5)), .progress = list(type = 'tasks'))
}

#' @rdname stf_
stf_info = function(classe, processo){
  classe <- base::toupper(classe)
  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")

  purrr::map_dfr(processo, purrr::slowly(purrr::possibly(~{
    getProcesso <- httr::GET(
      paste0( "https://portal.stf.jus.br/processos/listarProcessos.asp?classe=", classe, "&numeroProcesso=", .x), header)

    # Salve o incidente
    getIncidente <- stringr::str_split_i(getProcesso$url, pattern = "=", -1)

    # Buscar na aba informações
    getInfo <- httr::GET(
      paste0("https://portal.stf.jus.br/processos/abaInformacoes.asp?incidente=", getIncidente), header)

    # Leia o conteúdo da aba informações
    getConteudo <- httr::content(getInfo, encoding = "UTF-8")

    # Salve o conteúdo da aba informações
    ## Data do protocolo (ajuizamento)
    getProtocolo <- xml2::xml_find_all(
      getConteudo,
      "//div[@class='col-md-5 processo-detalhes-bold m-l-0']"
    ) |> xml2::xml_text(trim = T)

    ## Assunto do processo
    getAssunto <- xml2::xml_find_all(
      getConteudo,
      "//div[@class='col-md-10 processo-detalhes']"
    ) |> xml2::xml_text(trim = T)

    # Coloque os dados em uma tabela
    data <- data.frame(
      Classe = .x,
      Ajuizamento = getProtocolo,
      Assunto = getAssunto
    )
  }, NULL), rate = purrr::rate_delay(5)), .progress = list(type = 'tasks'))

}

#' @rdname stf_
stf_relator = function(classe, processo){
  classe <- base::toupper(classe)
  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")

  purrr::map_dfr(processo, purrr::slowly(purrr::possibly(~{
    getProcesso <- httr::GET(paste0("https://portal.stf.jus.br/processos/listarProcessos.asp?classe=", classe,"&numeroProcesso=",.x), header)

    # Salve o incidente
    getIncidente <- stringr::str_split_i(getProcesso$url, pattern = "=", -1)

    # Buscar nos detalhes do processo
    getDetalhe <- httr::GET(paste0("https://portal.stf.jus.br/processos/detalhe.asp?incidente=", getIncidente), header)

    # Leia o conteúdo dos detalhes do processo
    getConteudo <- httr::content(getDetalhe, encoding = "UTF-8")

    # Salve o relator  do processo
    getRelator <- xml2::xml_find_all(
      getConteudo,
      "//div[@class='processo-dados p-l-16']"
    ) |> xml2::xml_text(trim = T)

    # Limpe a string
    getRelator <- getRelator[2]
    getRelator <- getRelator |> stringr::str_split_i("MIN\\.", -1)
    getRelator <- stringr::str_trim(getRelator, side = "both")

    # Coloque os dados em uma tabela
    data <- data.frame(
      Classe = .x,
      Relator = getRelator
    )
  }, NULL), rate = purrr::rate_delay(5)), .progress = list(type = 'tasks'))

}

#' @rdname stf_
stf_decisoes = function(classe, processo){
  classe <- base::toupper(classe)
  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")

  purrr::map_dfr(processo, purrr::slowly(purrr::possibly(~{

    getProcesso <- httr::GET(paste0("https://portal.stf.jus.br/processos/listarProcessos.asp?classe=",classe,"&numeroProcesso=",.x), header)

    # Salve o incidente
    getIncidente <- stringr::str_split_i(getProcesso$url, pattern = "=", -1)

    # Buscar decisões do processo
    getDecisao <- httr::GET(paste0("https://portal.stf.jus.br/processos/abaDecisoes.asp?incidente=",getIncidente), header)

    # Leia o conteúdo das decisões do processo
    getConteudo <- httr::content(getDecisao, encoding = "UTF-8")

    contentTeste <-
      xml2::xml_find_all(getConteudo, "//div[@class='andamento-data']") |>
      xml2::xml_text(trim = T)

    if (is.na(contentTeste[1])) {
      getData <- NA
      getNome_Decisao <- NA
      getJulgador <- NA
      getConteudo_Decisao <- NA
    } else {
      # Salve as decisões do processo
      getData <- xml2::xml_find_all(
        getConteudo,
        "//div[@class='andamento-data']"
      ) |> xml2::xml_text(trim = T)

      getNome_Decisao <- xml2::xml_find_all(
        getConteudo,
        "//div[@class='col-md-5 p-l-0']"
      ) |> xml2::xml_text(trim = T)

      getJulgador <- xml2::xml_find_all(
        getConteudo,
        "//div[@class='col-md-3 p-0']"
      ) |> xml2::xml_text(trim = T)

      getConteudo_Decisao <- xml2::xml_find_all(
        getConteudo,
        "//div[@class='col-md-9 p-0']"
      ) |> xml2::xml_text(trim = T)
    }

    # Coloque os dados em uma tabela
    data <- data.frame(
      Classe = .x,
      Data = getData,
      Nome = getNome_Decisao,
      Julgador = getJulgador,
      Decisao = getConteudo_Decisao
    )

  }, NULL), rate = purrr::rate_delay(5)), .progress = list(type = 'tasks'))

}
