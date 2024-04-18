#' Extrai informações sobre as partes de processos judiciais no STF
#'
#' Esta função extrai informações sobre as partes de processos judiciais no Supremo Tribunal Federal (STF)
#' utilizando a API do portal STF. Ela recebe como entrada a classe do processo e o número do processo
#' e retorna um \code{data.frame} contendo informações sobre as partes envolvidas no processo.
#'
#'@usage
#'Genérica:
#'stf_partes(obj, ...)
#'@usage
#'Classe data.frame
#'stf_partes(obj, classe, numero)
#'@usage
#'Classe numeric
#'stf_partes(obj, classe)
#'
#' @param obj Pode ser um vetor de números, um data frame ou uma lista.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Outros argumentos que devem ser preenchidos conforme a classe do objeto:
#' * numeric: você deve informar a classe referente aos números que indicam o processo: Ex: `stf_partes(800, "ADPF")` retornará os dados da ADPF 800.
#' * data.frame: você deve informar a coluna em que estão os números dos processos e a coluna em que estão as classes dos processos. Ex: `stf_partes(tabela, "classes", "numeros")` retornará os dados das ações de numero correspondente aos números da coluna numeros da tabela e a classe da coluna classes da tabela.
#' * list: (em desenvolvimento)
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
stf_partes <- function(obj, ...){

  if (!is.data.frame(obj) & !is.numeric(obj)){
    cli::cli_alert_danger("Erro: Obj não é um data.frame ou um vetor numérico")
    return(invisible())
  }

  UseMethod('stf_partes')
}

#'@export
#'@method stf_partes data.frame
stf_partes.data.frame <- function(x, classe, numero){

  x[[classe]] <- x[[classe]] |> toupper()

  dados <- data.frame(
    x = x[[classe]],
    y = x[[numero]]
  )

  cli::cli_alert_info("Serão extraídos dados de {nrow(dados)} processos")

  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")

  dados <- purrr::map2_dfr(dados$x, dados$y, purrr::slowly(purrr::possibly(~{
    getProcesso <- httr::GET(
      paste0("https://portal.stf.jus.br/processos/listarProcessos.asp?classe=",
             .x, "&numeroProcesso=", .y), header)

    if (getProcesso$status_code != 200){
      cli::cli_alert_danger("Não foi possível extrair os dados da {.x} {.y}")
      next()
    }

    getIncidente <- stringr::str_split_i(getProcesso$url, pattern = "=", -1)

    getPartes <- httr::GET(
      paste0("https://portal.stf.jus.br/processos/abapartes.asp?incidente=", getIncidente), header)

    if (getPartes$status_code != 200){
      cli::cli_alert_danger("Não foi possível extrair os dados da {.x} {.y}")
      next()
    }

    getConteudo <- httr::content(getPartes, encoding = "UTF-8")

    getTipo <- xml2::xml_find_all(
      getConteudo,
      "//div[@class='detalhe-parte']"
    ) |> xml2::xml_text()

    getNome <- xml2::xml_find_all(
      getConteudo,
      "//div[@class='nome-parte']"
    ) |> xml2::xml_text()

    if (rlang::is_empty(getTipo) | rlang::is_empty(getNome)){
      cli::cli_alert_danger("Não foi possível extrair os dados da {.x} {.y}")
      next()
    }

    dados <- data.frame(
      Processo = paste(.x, .y),
      Classe = .x,
      Numero = .y,
      Tipo = getTipo,
      Parte = getNome
    )
  }, NULL), rate = purrr::rate_delay(5)), .progress = list(type = 'tasks'))

  cli::cli_alert_success("Dados extraídos")
  if (nrow(dados) != nrow(x)){
    cli::cli_alert_warning("O número de dados extraídos foi diferente dos dados solicitados. Verificar nos alertas os processos em que a função não funcionou")
  }

  dados
}

#'@export
#'@method stf_partes numeric
stf_partes.numeric <- function(x, classe){

  classe <- base::toupper(classe)
  processo <- x
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









