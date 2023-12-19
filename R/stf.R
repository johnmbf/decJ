#' Extrair partes de um processo no Supremo Tribunal Federal
#'
#'@description
#' Essa função permite extrair as partes de um processo no Supremo Tribunal Federal, retornando uma tabela com o número do processo, o tipo da parte e o nome da parte.
#'
#' @param classe Classe processual. Por exemplo: "ADPF" ou "ADI". Usar abreviatura da classe. Se tiver em dúvida, consultar o site do Supremo.
#' @param processo Número dos processos que pretende solicitar. Por exemplo: 500:600 (do 500 ao 600), ou list(500,600,700) que vai buscar o processo 500, 600 e 700
#'
#' @return
#' @export
#'
#' @examples
stf_partes = function(classe, processo){
  classe <- base::toupper(classe)
  header <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36 Edg/114.0.1823.51")

  purrr::map_dfr(processo, purrr::slowly(~{
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

    data <- data.frame(
      Classe = .x,
      Tipo = getTipo,
      Parte = getNome
    )

  }, rate = purrr::rate_delay(5)), .progress = list(type = 'tasks'))
}
