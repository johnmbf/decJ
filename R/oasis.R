#' Oasis to Rayyan
#'
#' Extrai dados do repositório Oasisbr e retorna um objeto compatível com o Rayyan (mas pode ser utilizado para outras coisas também rs)
#'
#' @param pesquisa `String` - os descritores da pesquisa. Ver exemplos.
#'
#' @return Um `tibble`
#' @export
#'
#' @details
#' Por enquanto o único parâmetro que pode ser alteardo é a busca. Estou estudando a alteração dos demais campos de pesquisa.
#'
#'
#' @examples
#' # Pesquisa com apenas uma palavra-chave:
#' df <- oasis_to_rayyan('judicialização')
#'
#' # Pesquisa com mais de uma palavra mas que a ordem importa:
#' df <- oasis_to_rayyan('"judicialização da política"')
#'
#' # Pesquisa com conjunto(s) de palavras:
#' df <- oasis_to_rayyan('"judicialização da política" AND "supremo tribunal federal"')
#' df <- oasis_to_rayyan('("judicialização da política" AND "supremo tribunal federal") AND ("partidos políticos" OR "presidente da república")')
oasis_to_rayyan = function(pesquisa) {

  headers <- c("accept" = "application/json")

  parametros <- list(
    `lookfor` = pesquisa,
    `type` = "AllFields",
    `field[]` = "id",
    `sort` = "year",
    `page` = "1",
    `limit` = "100"
  )

  lista_id <- list()

  n_page <- httr::GET(url = "https://oasisbr.ibict.br/vufind/api/v1/search",
                      httr::add_headers(.headers = headers),
                      query = parametros) |>
    httr::content('text') |>
    jsonlite::fromJSON()

  for (i in 1:ceiling(n_page$resultCount/100)) {
    parametros$page <- i

    # Primeiro vamos buscar os ID
    resposta <- httr::GET(
      url = "https://oasisbr.ibict.br/vufind/api/v1/search",
      httr::add_headers(.headers = headers),
      query = parametros
    )

    # Extrai o conteúdo do site
    conteudo <- jsonlite::fromJSON(httr::content(resposta, "text"))

    # salvo o ID em uma lista (isso permite salvar os 1056 registros)
    lista_id[[i]] <- conteudo$records$id
  }

  id <- unlist(lista_id)

  lista_registros <- list()

  for (i in 1:length(id)) {

    # Busca o registro (pesquisa)
    registro <- httr::GET(
      url = paste("https://oasisbr.ibict.br/vufind/Record/", id[i], sep = "") |>
        stringr::str_replace_all('\\s', '%20') # precisei substituir os espaços (\\s) por %20 (que é um código de espaço)
    )

    # Conteúdo do registro
    dados_registro <- httr::content(registro)

    # Busca as tabelas
    xml2::xml_find_all(dados_registro, "//table")

    # Nos interessa a tabela 2 - possui todos os metadados do registro
    ## O pivot_wider serve para transformar a coluna X1 (variável) em que cada variável será uma nova coluna e o X2 serão os valores desses variáveis
    dados_tabela <-
      rvest::html_table(dados_registro)[[2]] |>
      tidyr::pivot_wider(names_from = X1, values_from = X2)

    # salva tudo em uma lista (permite salvar os 1056 registros)
    lista_registros[[i]] <- dados_tabela

    # aqui é só um penduricário para eu acompanhar o progresso
    ## Ele retorna no console qual registro foi concluído do total de registros
    print(paste('Registro: ', i, ' de ', length(id), sep = ''))
  }

  tabela_registros <- dplyr::bind_rows(lista_registros)

  tabela_registros <- tabela_registros |>
    dplyr::select(title, author_facet, description,
                  topic, publishDate, spelling, url) |>
    dplyr::rename(authors = author_facet, abstract = description,
                  keywords = topic, notes = spelling, year = publishDate) |>
    dplyr::mutate(authors = stringr::str_replace_all(authors, '\n', '; ') |>
                    stringr::str_squish(),
                  keywords = keywords |> stringr::str_squish()) |>
    tibble::rownames_to_column() |>
    dplyr::rename(key = rowname)

  return(tabela_registros)

}

#' Oasis
#'
#' Extrai dados do repositório Oasisbr e retorna um tibble
#'
#' @param pesquisa `String` - os descritores da pesquisa. Ver exemplos.
#'
#' @return Um `tibble`
#' @export
#'
#' @details
#' Por enquanto o único parâmetro que pode ser alteardo é a busca. Estou estudando a alteração dos demais campos de pesquisa.
#'
#'
#' @examples
#' # Pesquisa com apenas uma palavra-chave:
#' df <- oasis_to_rayyan('judicialização')
#'
#' # Pesquisa com mais de uma palavra mas que a ordem importa:
#' df <- oasis_to_rayyan('"judicialização da política"')
#'
#' # Pesquisa com conjunto(s) de palavras:
#' df <- oasis_to_rayyan('"judicialização da política" AND "supremo tribunal federal"')
#' df <- oasis_to_rayyan('("judicialização da política" AND "supremo tribunal federal") AND ("partidos políticos" OR "presidente da república")')
oasis = function(pesquisa) {

  headers <- c("accept" = "application/json")

  parametros <- list(
    `lookfor` = pesquisa,
    `type` = "AllFields",
    `field[]` = "id",
    `sort` = "year",
    `page` = "1",
    `limit` = "100"
  )

  lista_id <- list()

  n_page <- httr::GET(url = "https://oasisbr.ibict.br/vufind/api/v1/search",
                      httr::add_headers(.headers = headers),
                      query = parametros) |>
    httr::content('text') |>
    jsonlite::fromJSON()

  for (i in 1:ceiling(n_page$resultCount/100)) {
    parametros$page <- i

    # Primeiro vamos buscar os ID
    resposta <- httr::GET(
      url = "https://oasisbr.ibict.br/vufind/api/v1/search",
      httr::add_headers(.headers = headers),
      query = parametros
    )

    # Extrai o conteúdo do site
    conteudo <- jsonlite::fromJSON(httr::content(resposta, "text"))

    # salvo o ID em uma lista (isso permite salvar os 1056 registros)
    lista_id[[i]] <- conteudo$records$id
  }

  id <- unlist(lista_id)

  lista_registros <- list()

  for (i in 1:length(id)) {

    # Busca o registro (pesquisa)
    registro <- httr::GET(
      url = paste("https://oasisbr.ibict.br/vufind/Record/", id[i], sep = "") |>
        stringr::str_replace_all('\\s', '%20') # precisei substituir os espaços (\\s) por %20 (que é um código de espaço)
    )

    # Conteúdo do registro
    dados_registro <- httr::content(registro)

    # Busca as tabelas
    xml2::xml_find_all(dados_registro, "//table")

    # Nos interessa a tabela 2 - possui todos os metadados do registro
    ## O pivot_wider serve para transformar a coluna X1 (variável) em que cada variável será uma nova coluna e o X2 serão os valores desses variáveis
    dados_tabela <-
      rvest::html_table(dados_registro)[[2]] |>
      tidyr::pivot_wider(names_from = X1, values_from = X2)

    # salva tudo em uma lista (permite salvar os 1056 registros)
    lista_registros[[i]] <- dados_tabela

    # aqui é só um penduricário para eu acompanhar o progresso
    ## Ele retorna no console qual registro foi concluído do total de registros
    print(paste('Registro: ', i, ' de ', length(id), sep = ''))
  }

  tabela_registros <- dplyr::bind_rows(lista_registros)

  # tabela_registros <- tabela_registros |>
  #   dplyr::select(title, author_facet, description,
  #                 topic, publishDate, spelling, url) |>
  #   dplyr::rename(authors = author_facet, abstract = description,
  #                 keywords = topic, notes = spelling, year = publishDate) |>
  #   dplyr::mutate(authors = stringr::str_replace_all(authors, '\n', '; ') |>
  #                   stringr::str_squish(),
  #                 keywords = keywords |> stringr::str_squish()) |>
  #   tibble::rownames_to_column() |>
  #   dplyr::rename(key = rowname)

  return(tabela_registros)

}
