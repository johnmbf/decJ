#' Extrair partes do processo
#'
#' @description
#' `extrairSTF.partes()` permite extrair do site do Supremo Tribunal Federal dados das partes dos processos.
#'
#' @param lista Uma lista
#' @param classe Um caracter que indica a classe processual
#' @param n Um número que indica o número do processo
#' @param UA User-Agent
#'
#' @return Uma lista com os dados das partes requeridos.
#' @export
#'
#' @examples
#' # Extrair os dados da ADPF 800
#' \dontrun{
#' extrairSTF.partes(lista, "ADPF", 800, UA)
#'}
#'
#' # Extrair os dados da ADI 500 a 600
#' \dontrun{
#' extrairSTF.partes(lista, "ADI", 500:600, UA)
#' }
extrairSTF.partes = function(lista, classe, n, UA){

  for(i in n){  ## faça isso em cada processo ##

    # Busque no site o processo
    getProcesso <- httr::GET(
      paste(
        'https://portal.stf.jus.br/processos/listarProcessos.asp?classe=',
        classe,
        '&numeroProcesso=',
        i,
        sep = ''
      ),
      httr::add_headers(
        'User-Agent' = UA
      )
    )

    # Salve o incidente
    getIncidente <- stringr::str_split_i(
      getProcesso$url,
      pattern = '=',
      -1
    )

    # Busque na aba partes
    getPartes <- httr::GET(
      paste('https://portal.stf.jus.br/processos/abapartes.asp?incidente=',
            getIncidente,
            sep = ''),
      httr::add_headers(
        'User-Agent' = UA
      )
    )

    # Leia o conteúdo da aba partes
    getConteundo <- httr::content(getPartes, encoding = 'UTF-8')

    # Salve o tipo e o nome da parte
    getTipo <- xml2::xml_find_all(
      getConteundo,
      "//div[@class='detalhe-parte']"
    ) %>% xml2::xml_text()

    getNome <- xml2::xml_find_all(
      getConteundo,
      "//div[@class='nome-parte']"
    ) %>% xml2::xml_text()

    # Coloque os dados em uma tabela
    data <- data.frame(
      Classe = i,
      Tipo = getTipo,
      Parte = getNome
    )

    # Coloque os dados em uma lista
    names(data)[names(data) == 'Classe'] <- classe
    lista[[i]] <- data

    # Aguarde 15 segundos
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<15){}
  }

  # Devolva a lista com os processos
  return(lista)

}


#' Extrair data do protocolo e assunto do processo
#'
#' @description
#' `extrairSTF.info()` permite extrair do site do Supremo Tribunal Federal dados de ajuizamento e assunto dos processos.
#'
#' @param lista Uma lista
#' @param classe Um caracter que indica a classe processual
#' @param n Um número que indica o número do processo
#' @param UA User-Agent
#'
#' @return Uma lista com as datas de ajuizamento e assuntos requeridos.
#' @export
#'
#' @examples
#' # Extrair os dados da ADPF 800
#' \dontrun{
#' extrairSTF.info(lista, "ADPF", 800, UA)
#'}
#'
#' # Extrair os dados da ADI 500 a 600
#' \dontrun{
#' extrairSTF.info(lista, "ADI", 500:600, UA)
#' }
extrairSTF.info = function(lista, classe, n, UA){

  for(i in n){ ## faça isso em cada processo ##

    # Busque no site o processo
    getProcesso <- httr::GET(
      paste(
        'https://portal.stf.jus.br/processos/listarProcessos.asp?classe=',
        classe,
        '&numeroProcesso=',
        i,
        sep = ''
      ),
      httr::add_headers(
        'User-Agent' = UA
      )
    )

    # Salve o incidente
    getIncidente <- stringr::str_split_i(
      getProcesso$url,
      pattern = '=',
      -1
    )

    # Buscar na aba informações
    getInfo <- httr::GET(
      paste('https://portal.stf.jus.br/processos/abaInformacoes.asp?incidente=',
            getIncidente,
            sep = ''),
      httr::add_headers(
        'User-Agent' = UA
      )
    )

    # Leia o conteúdo da aba informações
    getConteudo <- httr::content(getInfo, encoding = 'UTF-8')

    # Salve o conteúdo da aba informações
    ## Data do protocolo (ajuizamento)
    getProtocolo <- xml2::xml_find_all(
      getConteudo,
      "//div[@class='col-md-5 processo-detalhes-bold m-l-0']"
    ) %>% xml2::xml_text(trim = T)

    ## Assunto do processo
    getAssunto <- xml2::xml_find_all(
      getConteudo,
      "//div[@class='col-md-10 processo-detalhes']"
    ) %>% xml2::xml_text(trim = T)

    # Coloque os dados em uma tabela
    data <- data.frame(
      Classe = i,
      Ajuizamento = getProtocolo,
      Assunto = getAssunto
    )

    # Coloque os dados em uma lista
    names(data)[names(data) == 'Classe'] <- classe
    lista[[i]] <- data

    # Aguarde 15 segundos
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<15){}
  }

  # Devolva a lista com os processos
  return(lista)

}
