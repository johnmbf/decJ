#' Buscar informações sobre proposição legislativa na Assembleia Legislativa do RS
#'
#' Esta função busca informações sobre uma proposição legislativa na Assembleia Legislativa do Rio Grande do Sul (ALERS),
#' dado o número da norma e o ano de sua proposição.
#'
#' @param norma Número da norma.
#' @param ano Ano da proposição.
#' @param .reportar Se TRUE, exibe uma mensagem informando sobre a proposição buscada. O padrão é TRUE.
#' @return Um data frame contendo informações sobre a proposição legislativa, incluindo o número da norma, a proposição,
#' o proponente, o assunto, a ementa e a URL da proposição.
#' @export
#' @examples
#' # Buscar informações sobre uma proposição legislativa
#' alers_projeto(norma = 10098, ano = 1994)
alers_projeto <- function(norma, ano, .reportar = TRUE){

  header <- c('user_agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0')

  parametros <- list(
    "Hid_IdNorma" = "",
    "txthNRO_PROPOSICAO" = "",
    "txthAdin" = "",
    "txthQualquerPalavra" = "",
    "cboTipoNorma" = "",
    "TxtNumero_Norma" = 9820,
    "TxtAno" =  1993,
    'txtData' = "",
    'txtDataInicial' = '',
    'txtDataFinal' = '',
    'txtPalavraChave' = '',
    'TxtQualquerPalavra' = '',
    'CmbPROPOSICAO' = '',
    'txtProcAdin' = '',
    'cmbNumero_Docs' = 5,
    'txtOrdenacao' = '',
    'txtOperacaoFormulario' = 'Pesquisar')

  parametros$TxtNumero_Norma <- norma
  parametros$TxtAno <- ano

  url <- 'https://ww3.al.rs.gov.br/legis/M010/M0100008.asp'

  r <- httr::GET(url, httr::add_headers(header), query = parametros)
  conteudo <- r |> httr::content(encoding = 'ISO-8859-1')

  id <- conteudo |> xml2::xml_find_all('//input[@name="Hid_IdNorma1"]') |> xml2::xml_attr('value')

  id

  url <- 'https://ww3.al.rs.gov.br/legis/M010/M0100018.asp?'

  parametros <- list(
    "Hid_IdNorma" = "",
    "Text" = "",
    "Origem" = "1"
  )

  parametros$Hid_IdNorma = id

  r <- httr::GET(url, httr::add_headers(header), query = parametros)
  conteudo <- r |> httr::content(encoding = 'ISO-8859-1')
  id <- conteudo |> xml2::xml_find_all('//a[@href="javascript:;"]') |> xml2::xml_attr('onclick')

  id <- id |>
    stringr::str_split_i("\\(", -1) |>
    stringr::str_split(",", simplify = T) |>
    stringr::str_remove_all("[:punct:]") |>
    stringr::str_trim()

  url <- glue::glue('https://ww3.al.rs.gov.br/legislativo/ExibeProposicao/tabid/325/SiglaTipo/{id[1]}/NroProposicao/{id[2]}/AnoProposicao/{id[3]}/Default.aspx')

  r <- httr::GET(url, httr::add_headers(header))
  conteudo <- httr::content(r)

  proposicao <- conteudo |> xml2::xml_find_all('//div[@class="dvproposicao"]') |> xml2::xml_text() |> stringr::str_split(':', simplify = T) |> stringr::str_trim()

  proponente <- conteudo |> xml2::xml_find_all('//div[@class="dvNomeProponente"]') |> xml2::xml_text() |> stringr::str_split(':', simplify = T) |> stringr::str_trim()

  assunto <- conteudo |> xml2::xml_find_all('//div[@class="dvAssunto"]') |> xml2::xml_text() |> stringr::str_split(':', simplify = T) |> stringr::str_trim()

  ementa <- conteudo |> xml2::xml_find_all('//div[@class="dvEmenta"]') |> xml2::xml_text() |> stringr::str_split(':', simplify = T) |> stringr::str_trim()

  df <- data.frame(
    "norma" = paste(norma, ano, sep = '/'),
    "Proposição" = proposicao[2],
    "Proponente" = proponente[2],
    "Assunto" = assunto[2],
    "Ementa" = ementa[2],
    "URL" = url
  )

  if (.reportar == TRUE) {

    tipo <- ifelse(proposicao[2] |> stringr::str_detect("PR"), "Resolução", ifelse(proposicao[2] |> stringr::str_detect("PR"), "Lei", "norma"))

    glue::glue(
      "Essa função buscou no site da Assembleia Legislativa do Rio Grande do Sul a proposição referente à {tipo} nº {norma} de {ano}.",
      "A norma foi proposta pelo {proponente[2]} e autuada na Assembleia como {proposicao[2]}.",
      "Você pode conferir a proposta em: {url}",
      "\t", .sep = '\n\n') |> print()
  }

  return(df)

}
