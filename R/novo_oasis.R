oasis.interno <- function(lookfor, type="AllFields", sort="year", facets = TRUE){

  url <- "http://oasisbr.ibict.br/vufind/api/v1/search?"

  facet_parameters <- "&facet[]=author_facet&facet[]=dc.subject.por.fl_str_mv&facet[]=eu_rights_str_mv&facet[]=dc.publisher.program.fl_str_mv&facet[]=dc.subject.cnpq.fl_str_mv&facet[]=publishDate&facet[]=language&facet[]=format&facet[]=institution&facet[]=dc.contributor.advisor1.fl_str_mv&facet[]=network_name_str"

  query <- paste(url,"lookfor=",URLencode(lookfor),"&type=",type,"&sort=",sort,ifelse(facets == TRUE, facet_parameters, NA),sep="")
  x <- jsonlite::fromJSON(query)

  return(x)
}

# Melhorar a função de busca no site OASISBR -----

novo_oasis <- function(busca, tipo = 'AllFields', ordem = 'year', metricas = FALSE){

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

  data <- conteudo_total[[1]]

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
      tabela <- df
    } else {
      tabela <- rbind(tabela, df)
    }

  }

  return(tabela)

}
