#' Extrai informações sobre as partes de processos judiciais no STF
#'
#' Esta função extrai informações sobre as partes de processos judiciais no Supremo Tribunal Federal (STF)
#' utilizando a API do portal STF. Ela recebe como entrada a classe do processo e o número do processo
#' e retorna um \code{data.frame} contendo informações sobre as partes envolvidas no processo.
#'
#'
#' @param classe A classe do processo, em maiúsculas. Por exemplo, "RE", "HC", "ADI", etc.
#' @param processo O número do processo ou uma lista de números de processo.
#' @return Um \code{data.frame} contendo informações sobre as partes envolvidas no processo.
#'   O \code{data.frame} contém as seguintes colunas:
#'   \describe{
#'     \item{Classe}{A classe do processo.}
#'     \item{Tipo}{O tipo da parte (por exemplo, "Autor", "Réu", "Interessado", etc.).}
#'     \item{Parte}{O nome da parte envolvida no processo.}
#'   }
#' @import httr
#' @importFrom xml2 xml_find_all xml_text
#' @importFrom purrr map_dfr slowly possibly rate_delay
#' @importFrom stringr str_split_i
#' @export
#' @examples
#' \dontrun{
#' stf_partes("ADPF", "800")
#' stf_partes("ADPF", c("800", "900"))
#'}
#' @family stf
#'
#' @details
#' Para mais detalhes, acesse:
#' \code{vignette("familia_stf", package = "decJ")}
stf_partes <- function(classe, processo){
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

#' Extrai informações sobre um processo judicial no STF
#'
#' Esta função extrai informações sobre um processo judicial no Supremo Tribunal Federal (STF)
#' utilizando a API do portal STF. Ela recebe como entrada a classe do processo e o número do processo
#' e retorna um \code{data.frame} contendo informações sobre o processo, incluindo a data do protocolo
#' (ajuizamento) e o assunto do processo.
#'
#' @param classe A classe do processo, em maiúsculas. Por exemplo, "RE", "HC", "ADI", etc.
#' @param processo O número do processo ou uma lista de números de processo.
#' @return Um \code{data.frame} contendo informações sobre o processo, incluindo as seguintes colunas:
#'   \describe{
#'     \item{Classe}{A classe do processo.}
#'     \item{Ajuizamento}{A data do protocolo (ajuizamento) do processo.}
#'     \item{Assunto}{O assunto do processo.}
#'   }
#' @import httr
#' @importFrom xml2 xml_find_all xml_text
#' @importFrom purrr map_dfr slowly possibly rate_delay
#' @importFrom stringr str_split_i
#' @export
#' @examples
#' \dontrun{
#' stf_info("ADPF", "800")
#' stf_info("ADPF", c("900", "800"))
#'}
#' @family stf
#'
#' @details
#' Para mais detalhes, acesse:
#' \code{vignette("familia_stf", package = "decJ")}
stf_info <- function(classe, processo){
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

#' Extrai informações sobre o relator de um processo judicial no STF
#'
#' Esta função extrai informações sobre o relator de um processo judicial no Supremo Tribunal Federal (STF)
#' utilizando a API do portal STF. Ela recebe como entrada a classe do processo e o número do processo
#' e retorna um \code{data.frame} contendo informações sobre o relator do processo.
#'
#' @param classe A classe do processo, em maiúsculas. Por exemplo, "RE", "HC", "ADI", etc.
#' @param processo O número do processo ou uma lista de números de processo.
#' @return Um \code{data.frame} contendo informações sobre o relator do processo, incluindo as seguintes colunas:
#'   \describe{
#'     \item{Classe}{A classe do processo.}
#'     \item{Relator}{O nome do relator do processo.}
#'   }
#' @import httr
#' @importFrom xml2 xml_find_all xml_text
#' @importFrom purrr map_dfr slowly possibly rate_delay
#' @importFrom stringr str_split_i str_trim
#' @export
#' @examples
#' \dontrun{
#' stf_relator("ADPF", "800")
#' stf_relator("ADPF", c("800", "900"))
#'}
#' @family stf
#'
#' @details
#' Para mais detalhes, acesse:
#' \code{vignette("familia_stf", package = "decJ")}
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

#' Extrai informações sobre as decisões de um processo judicial no STF
#'
#' Esta função extrai informações sobre as decisões de um processo judicial no Supremo Tribunal Federal (STF)
#' utilizando a API do portal STF. Ela recebe como entrada a classe do processo e o número do processo
#' e retorna um \code{data.frame} contendo informações sobre as decisões do processo.
#'
#' @param classe A classe do processo, em maiúsculas. Por exemplo, "RE", "HC", "ADI", etc.
#' @param processo O número do processo ou uma lista de números de processo.
#' @return Um \code{data.frame} contendo informações sobre as decisões do processo, incluindo as seguintes colunas:
#'   \describe{
#'     \item{Classe}{A classe do processo.}
#'     \item{Data}{A data da decisão.}
#'     \item{Nome}{O nome da decisão.}
#'     \item{Julgador}{O julgador responsável pela decisão.}
#'     \item{Decisao}{O conteúdo da decisão.}
#'   }
#' @import httr
#' @importFrom xml2 xml_find_all xml_text
#' @importFrom purrr map_dfr slowly possibly rate_delay
#' @importFrom stringr str_split_i
#' @export
#' @examples
#' \dontrun{
#' stf_decisoes("ADPF", "800")
#' stf_decisoes("ADPF", c("800", "900"))
#'}
#' @family stf
#'
#' @details
#' Para mais detalhes, acesse:
#' \code{vignette("familia_stf", package = "decJ")}
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

#' Baixa a petição inicial de processos do STF
#'
#' Esta função permite baixar a petição inicial de processos do Supremo Tribunal Federal (STF) com base nos parâmetros fornecidos.
#'
#' @param classe A classe do processo.
#' @param n Um vetor contendo os números dos processos.
#' @param arquivo O diretório onde os arquivos serão salvos.
#' @return Esta função não retorna nada explicitamente. Ela salva os arquivos PDF da petição inicial dos processos no diretório especificado.
#' @import httr
#' @importFrom stringr str_split_i
#' @importFrom rvest html_element
#' @importFrom xml2 xml_attr
#' @export
#' @examples
#' \dontrun{
#' # Baixar a petição inicial do processo com número 12345 da classe "RE"
#' stf_inicial(classe = "ADPF", n = 800, arquivo = ".")
#'}
#' @family stf
#'
#' @details
#' Para mais detalhes, acesse:
#' \code{vignette("familia_stf", package = "decJ")}
stf_inicial <- function(classe, n, arquivo) {

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

#' Extrai jurisprudência do Supremo Tribunal Federal (STF)
#'
#' Esta função extrai jurisprudência do Supremo Tribunal Federal (STF) utilizando a API do portal de jurisprudência STF.
#' Você pode pesquisar por palavras-chave ou por classe de processo.
#'
#' @param busca Uma string contendo palavras-chave para a busca de jurisprudência. Se não especificado, a busca será realizada por classe.
#' @param classe Uma string contendo a classe de processo para a busca de jurisprudência. Se não especificado, a busca será realizada por palavras-chave.
#' @param base Um vetor contendo os tipos de documentos a serem buscados. Pode incluir "acordaos" ou "decisoes".
#' @param quantidade O número de resultados desejados. O padrão é 25.
#' @return Um \code{data.frame} contendo as informações da jurisprudência encontrada.
#' @import httr
#' @importFrom jsonlite fromJSON
#' @export
#' @examples
#' \dontrun{
#' # Buscar jurisprudência por palavras-chave
#' stf_jurisprudencia(busca = "direitos humanos", base = 'decisoes', quantidade = 10)
#'
#' # Buscar jurisprudência por classe de processo
#' stf_jurisprudencia(classe = "ADPF", base = 'acordaos', quantidade = 10)
#'}
#' @family stf
#'
#' @details
#' Para mais detalhes, acesse:
#' \code{vignette("familia_stf", package = "decJ")}
stf_jurisprudencia = function(busca = NULL, classe = NULL, base = c("acordaos", "decisoes"), quantidade = 25){
  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")

  if (!is.null(busca) & is.null(classe)) {
    body <- busca_jurisprudencia
    body$query$bool$filter[[1]]$query_string$query <- busca
    body$post_filter$bool$must[[1]]$term$base <- base
  } else if (is.null(busca) & !is.null(classe)) {
    body <- busca_classe
    body$query$bool$filter$query_string$query <- classe
    body$post_filter$bool$must$term$base <- base
  } else if ((!is.null(busca) & !is.null(classe))) {
    cat("Essa funcao so funciona com busca por palavras chaves OU por classe. Ainda estamos desenvolvendo uma forma de trabalhar com as duas buscas juntas.")
    return(NULL)
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

#' Baixa jurisprudência do Supremo Tribunal Federal (STF)
#'
#' Esta função baixa jurisprudência do Supremo Tribunal Federal (STF) utilizando a API do portal de jurisprudência STF.
#' Você pode pesquisar por palavras-chave ou por classe de processo e especificar a quantidade de documentos a serem baixados.
#' Os documentos são salvos como arquivos PDF.
#'
#' @param busca Uma string contendo palavras-chave para a busca de jurisprudência. Se não especificado, a busca será realizada por classe.
#' @param classe Uma string contendo a classe de processo para a busca de jurisprudência. Se não especificado, a busca será realizada por palavras-chave.
#' @param base Um vetor contendo os tipos de documentos a serem buscados. Pode incluir "acordaos" ou "decisoes".
#' @param quantidade O número de resultados desejados. O padrão é 25.
#' @param arquivo O diretório onde os arquivos serão salvos. O padrão é o diretório atual.
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr walk slowly rate_delay
#' @importFrom stringr str_split_i
#' @export
#' @examples
#' \dontrun{
#' # Baixar jurisprudência por palavras-chave
#' stf_jurisprudencia_download(busca = "direitos humanos", base = "acordaos", quantidade = 10, arquivo = ".")
#'
#' # Baixar jurisprudência por classe de processo
#' stf_jurisprudencia_download(classe = "ADPF", base = 'acordaos', quantidade = 10, arquivo = ".")
#'}
#' @family stf
#'
#' @details
#' Para mais detalhes, acesse:
#' \code{vignette("familia_stf", package = "decJ")}
stf_jurisprudencia_download = function(busca = NULL, classe = NULL, base = c("acordaos", "decisoes"), quantidade = 25, arquivo = "."){

  if (!is.null(busca) & is.null(classe)) {
    body <- busca_jurisprudencia
    body$query$bool$filter[[1]]$query_string$query <- busca
    body$post_filter$bool$must[[1]]$term$base <- base
  } else if (is.null(busca) & !is.null(classe)) {
    body <- busca_classe
    body$query$bool$filter$query_string$query <- classe
    body$post_filter$bool$must$term$base <- base
  } else if ((!is.null(busca) & !is.null(classe))) {
    cat("Essa funcao so funciona com busca por palavras chaves OU por classe. Ainda estamos desenvolvendo uma forma de trabalhar com as duas buscas juntas.")
    return(NULL)
  }

  if (length(base) != 1) {
    cat("Voce deve selecionar apenenas uma base, de acordaos OU de decisoes (monocraticas)")
    return(NULL)
  }

  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")

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
