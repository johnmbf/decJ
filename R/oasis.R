#' Função de busca no OASIS
#'
oasis.interno <- function(lookfor, type="AllFields", sort="year", facets = TRUE){

  url <- "http://oasisbr.ibict.br/vufind/api/v1/search?"

  facet_parameters <- "&facet[]=author_facet&facet[]=dc.subject.por.fl_str_mv&facet[]=eu_rights_str_mv&facet[]=dc.publisher.program.fl_str_mv&facet[]=dc.subject.cnpq.fl_str_mv&facet[]=publishDate&facet[]=language&facet[]=format&facet[]=institution&facet[]=dc.contributor.advisor1.fl_str_mv&facet[]=network_name_str"

  query <- paste(url,"lookfor=",URLencode(lookfor),"&type=",type,"&sort=",sort,ifelse(facets == TRUE, facet_parameters, NA),sep="")
  x <- jsonlite::fromJSON(query)

  return(x)
}


#' Realiza uma pesquisa no OASIS e retorna os resultados
#'
#' Esta função realiza uma pesquisa no OASIS utilizando a API e retorna os resultados encontrados.
#' Ela extrai os metadados dos registros encontrados, como título, autores, resumo, palavras-chave, data de publicação, notas e URL.
#'
#' @param busca Uma string contendo os termos de busca desejados.
#' @param tipo O tipo de campo onde a busca será realizada. Pode ser 'AllFields' (padrão), 'title', 'author', 'abstract', entre outros.
#' @param ordem A ordem de classificação dos resultados. Pode ser 'year' (padrão), 'title', 'author', 'relevance', entre outros.
#' @param metricas Um valor lógico indicando se devem ser retornadas apenas métricas (TRUE) ou registros completos (FALSE). O padrão é FALSE.
#' @param rayyan Um valor lógico indicando se os dados devem ser formatados para importação no Rayyan (TRUE) ou não (FALSE). O padrão é FALSE.
#' @return Um data frame contendo os metadados dos registros encontrados.
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_flatten
#' @importFrom dplyr select rename
#' @importFrom tibble rownames_to_column
#' @importFrom glue glue
#'
#' @references
#' Ativando métricas (metricas = TRUE) você vai ter acesso a indicadores sobre o assunto pesquisado. Esse argumento chama a função busca_oasisbr() que retorna um data frame com as métricas. A função foi adaptada do repositório [Indicadores-oasisbr](https://github.com/projetos-codic-ibict/Indicadores-oasisbr).
#'
#' Informações sobre o OASIS: https://oasisbr.ibict.br/vufind/
#'
#' @export
#' @examples
#' \dontrun{
#' # Realizar pesquisa no OASIS e obter os resultados
#' oasis('"perfilamento racial"')
#'
#' # Realizar pesquisa no OASIS e formatar os resultados para uso com o Rayyan
#' oasis('"perfilamento racial"', rayyan = TRUE)
#' }
oasis <- function(busca, tipo = 'AllFields', ordem = 'year', metricas = FALSE, rayyan = FALSE){

  parametros <- list(
    `lookfor` = busca,
    `type` = tipo,
    `field[]` = "formats",
    `field[]` = "id",
    `field[]` = 'publicationDates',
    `field[]` = "languages",
    `field[]` = "subjects",
    `field[]` = "title",
    `field[]` = "urls",
    `field[]` = "institutions",
    `field[]` = "primaryAuthors",
    `field[]` = "secondaryAuthors",
    `field[]` = "summary",
    `sort` = ordem,
    `page` = "1",
    `limit` = "100",
    `prettyPrint` = "false",
    `lng` = "pt-br"
  )

  if (metricas == TRUE){
    tabela_registros <-oasis.interno(busca, tipo, ordem, metricas)
    tabela_registros <- tabela_registros$facets
    return(tabela_registros)
  }

  dados <- oasis.interno(busca, tipo, ordem, metricas)

  if (dados[["resultCount"]] == 0){
    cat('A busca n\u00e3o retornou nenhum registro. Verifique os par\u00e2metros de consulta.')
    return(invisible())
  } else {
    glue::glue("{dados$resultCount} registros encontrados") |> cat()
  }

  conteudo_total <- list()

  for (i in 1:ceiling(dados$resultCount / 100)) {
    parametros$page <- i

    # Primeiro vamos buscar os ID
    resposta <- httr::GET(
      url = "https://oasisbr.ibict.br/vufind/api/v1/search",
      query = parametros
    )

    # Extrai o conteúdo do site
    conteudo <- jsonlite::fromJSON(httr::content(resposta, "text"))

    # salvo o ID em uma lista (isso permite salvar os 1056 registros)

    conteudo_total[[i]] <- conteudo
  }

  for (p in 1:length(conteudo_total)){

    data <- conteudo_total[[p]]

    for (i in 1:nrow(data$records)){
      resumo <- data$records$summary[[i]]
      formato <- data$records$formats[[i]]
      id <- data$records$id[[i]]
      lang <- data$records$languages[[i]]
      assunto <- data$records$subjects[[i]]
      title <- data$records$title[[i]]
      instituicao <- data$records$institutions[[i]]
      url <- ifelse(length(data$records$urls[[i]]) == 0, NA, data$records$urls[[i]][,1])
      primeiro_autor <- data$records$primaryAuthors[[i]]
      ano <- data$records$publicationDates[[i]]
      autor <- stringr::str_flatten(
        c(data$records$primaryAuthors[[i]], data$records$secondaryAuthors[[i]]),
        collapse = '; '
      )

      df <- data.frame(
        resumo = ifelse(length(resumo) == 0, NA, resumo),
        ano = ifelse(length(ano) == 0, NA, ano),
        formato = ifelse(length(formato) == 0, NA, formato),
        id = ifelse(length(id) == 0, NA, id),
        lang = ifelse(length(lang) == 0, NA, lang),
        assunto = ifelse(length(assunto) == 0, NA, assunto),
        title = ifelse(length(title) == 0, NA, title),
        instituicao = ifelse(length(instituicao) == 0, NA, instituicao),
        url = ifelse(length(url) == 0, NA, url),
        primeiro_autor = ifelse(length(primeiro_autor) == 0, NA, primeiro_autor),
        autor = ifelse(length(autor) == 0, NA, autor)
      )

      if (i == 1) {
        dados_parte <- df
      } else {
        dados_parte <- rbind(dados_parte, df)
      }

    }

    if (p == 1){
      tabela <- dados_parte
    } else {
      tabela <- rbind(tabela, dados_parte)
    }

  }

  if (rayyan == TRUE) {
    tabela <- tabela |>
      dplyr::select(
        title, autor, resumo,
        assunto, ano, formato, url, id
      ) |>
      dplyr::rename(
        authors = autor, abstract = resumo,
        keywords = assunto, notes = id, year = ano,
        format = formato
      )  |>
      tibble::rownames_to_column() |>
      dplyr::rename(key = rowname)
  }

  return(tabela)

}
