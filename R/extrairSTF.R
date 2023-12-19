# Extrair partes ----

#' Extrair partes do processo
#'
#' @description
#' <img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-deprecated.svg" alt="Deprecated"/>
#'
#' Utilize a função [decJ::stf_partes()] no lugar.
#'
#' @param lista Uma lista
#' @param classe Um caracter que indica a classe processual
#' @param n Um número que indica o número do processo
#'
#' @return Uma lista com os dados das partes requeridos.
#' @export
#'
#' @examples
#' # Extrair os dados da ADPF 800
#' \dontrun{
#' extrairSTF.partes(lista, "ADPF", 800, UA)
#' }
#'
#' # Extrair os dados da ADI 500 a 600
#' \dontrun{
#' extrairSTF.partes(lista, "ADI", 500:600, UA)
#' }
extrairSTF.partes <- function(lista, classe, n) {
  lifecycle::deprecate_stop("18/12/2023", "extrairSTF.partes()", "stf_partes()")

  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  print("Extraindo ADPF:")

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

    # Busque na aba partes
    getPartes <- httr::GET(
      paste("https://portal.stf.jus.br/processos/abapartes.asp?incidente=",
        getIncidente,
        sep = ""
      ),
      httr::add_headers(
        "User-Agent" = UA
      )
    )

    # Leia o conteúdo da aba partes
    getConteundo <- httr::content(getPartes, encoding = "UTF-8")

    # Salve o tipo e o nome da parte
    getTipo <- xml2::xml_find_all(
      getConteundo,
      "//div[@class='detalhe-parte']"
    ) |> xml2::xml_text()

    getNome <- xml2::xml_find_all(
      getConteundo,
      "//div[@class='nome-parte']"
    ) |> xml2::xml_text()

    # Coloque os dados em uma tabela
    data <- data.frame(
      Classe = i,
      Tipo = getTipo,
      Parte = getNome
    )

    # Coloque os dados em uma lista
    names(data)[names(data) == "Classe"] <- classe
    lista[[i]] <- data

    # Aguarde 15 segundos
    date_time <- Sys.time()
    while ((as.numeric(Sys.time()) - as.numeric(date_time)) < 5) {}

    print(i)
  }

  beepr::beep(1)

  # Devolva a lista com os processos
  return(lista)
}

# Extrair info ----

#' Extrair data do protocolo e assunto do processo
#'
#' @description
#'
#' <img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-deprecated.svg" alt="Deprecated"/>
#'
#' Utilize a função [decJ::stf_info()] no lugar.
#'
#' @param lista Uma lista
#' @param classe Um caracter que indica a classe processual
#' @param n Um número que indica o número do processo
#'
#' @return Uma lista com as datas de ajuizamento e assuntos requeridos.
#' @export
#'
#' @examples
#' # Extrair os dados da ADPF 800
#' \dontrun{
#' extrairSTF.info(lista, "ADPF", 800, UA)
#' }
#'
#' # Extrair os dados da ADI 500 a 600
#' \dontrun{
#' extrairSTF.info(lista, "ADI", 500:600, UA)
#' }
extrairSTF.info <- function(lista, classe, n) {
  lifecycle::deprecate_stop("19/12/2023", "extrairSTF.info()", "stf_info()")

  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  print("Extraindo ADPF:")

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

    # Buscar na aba informações
    getInfo <- httr::GET(
      paste("https://portal.stf.jus.br/processos/abaInformacoes.asp?incidente=",
        getIncidente,
        sep = ""
      ),
      httr::add_headers(
        "User-Agent" = UA
      )
    )

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
      Classe = i,
      Ajuizamento = getProtocolo,
      Assunto = getAssunto
    )

    # Coloque os dados em uma lista
    names(data)[names(data) == "Classe"] <- classe
    lista[[i]] <- data

    # Aguarde 5 segundos
    date_time <- Sys.time()
    while ((as.numeric(Sys.time()) - as.numeric(date_time)) < 5) {}

    print(i)
  }

  beepr::beep(1)

  # Devolva a lista com os processos
  return(lista)
}

# Extrair relator ----

#' Extrair o relator do processo
#'
#' @description
#' <img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-deprecated.svg" alt="Deprecated"/>
#'
#' Utilize a função [decJ::stf_relator()] no lugar.
#'
#' @param lista Uma lista
#' @param classe Um caracter que indica a classe processual
#' @param n Um número que indica o número do processo
#'
#' @return Uma lista com os relatores dos processos requeridos.
#' @export
#'
#' @author Essa função teve a colaboração de Gabriel Delias de Sousa Simões
#'
#' @examples
#' # Extrair os dados da ADPF 800
#' \dontrun{
#' extrairSTF.relator(lista, "ADPF", 800, UA)
#' }
#'
#' # Extrair os dados da ADI 500 a 600
#' \dontrun{
#' extrairSTF.relator(lista, "ADI", 500:600, UA)
#' }
extrairSTF.relator <- function(lista, classe, n) {
  lifecycle::deprecate_stop("19/12/2023", "extrairSTF.relator()", "stf_relator()")

  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  print("Extraindo ADPF:")

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

    # Buscar nos detalhes do processo
    getDetalhe <- httr::GET(
      paste("https://portal.stf.jus.br/processos/detalhe.asp?incidente=",
        getIncidente,
        sep = ""
      ),
      httr::add_headers(
        "User-Agent" = UA
      )
    )

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
      Classe = i,
      Relator = getRelator
    )

    # Coloque os dados em uma lista
    names(data)[names(data) == "Classe"] <- classe
    lista[[i]] <- data

    # Aguarde 15 segundos
    date_time <- Sys.time()
    while ((as.numeric(Sys.time()) - as.numeric(date_time)) < 5) {}

    print(i)
  }

  beepr::beep(1)

  # Devolva a lista com os relatores dos processos
  return(lista)
}

# Extrair decisões ----

#' Extrair as decisões do processo
#'
#' @description
#' <img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-deprecated.svg" alt="Deprecated"/>
#'
#' Utilize a função [decJ::stf_decisoes()] no lugar.
#'
#' @param lista Uma lista
#' @param classe Um caracter que indica a classe processual
#' @param n Um número que indica o número do processo
#'
#' @return Uma lista com as decisões dos processos requeridos.
#' @export
#'
#' @examples
#' # Extrair os dados da ADPF 800
#' \dontrun{
#' extrairSTF.decisao(lista, "ADPF", 800, UA)
#' }
#'
#' # Extrair os dados da ADI 500 a 600
#' \dontrun{
#' extrairSTF.decisao(lista, "ADI", 500:600, UA)
#' }
extrairSTF.decisao <- function(lista, classe, n) {
  lifecycle::deprecate_stop(
    "19/12/2023",
    "extrair.STF(decisao)",
    "stf_decisoes()"
  )

  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51"

  print("Extraindo ADPF:")

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

    # Buscar decisões do processo
    getDecisao <- httr::GET(
      paste("https://portal.stf.jus.br/processos/abaDecisoes.asp?incidente=",
        getIncidente,
        sep = ""
      ),
      httr::add_headers(
        "User-Agent" = UA
      )
    )

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
      Classe = i,
      Data = getData,
      Nome = getNome_Decisao,
      Julgador = getJulgador,
      Decisao = getConteudo_Decisao
    )

    # Coloque os dados em uma lista
    names(data)[names(data) == "Classe"] <- classe
    lista[[i]] <- data

    # Aguarde 5 segundos
    date_time <- Sys.time()
    while ((as.numeric(Sys.time()) - as.numeric(date_time)) < 5) {}

    print(i)
  }

  beepr::beep(1)
  # Devolva a lista com as decisões dos processos
  return(lista)
}
