#' Consulta projetos legislativos no site da Assembleia Legislativa do Estado do Paraná
#'
#' Esta função realiza uma consulta aos projetos legislativos no site da Assembleia
#' Legislativa do Estado do Paraná, com base nos parâmetros fornecidos. Os parâmetros
#' podem incluir o tipo de proposição, autores, número, ano, conclusão, assuntos, chave
#' e sumula.
#'
#' @param prop_tipo Tipo da proposição. Pode ser vazio (todos os tipos) ou um número de 1 a 11,
#' onde:
#'   - 1: Lei
#'   - 2: Lei Complementar
#'   - 3: Resolução
#'   - 4: Decreto Legislativo
#'   - 5: Emenda Constitucional
#'   - 6: Indicação Legislativa
#'   - 7: Ato
#'   - 10: Ofício
#'   - 11: Documento de Designação
#'   - 12: Portaria
#' @param prop_autores Autores da proposição.
#' @param prop_numero Número da proposição.
#' @param prop_ano Ano da proposição.
#' @param prop_conclusao Status de conclusão da proposição. Pode ser vazio (todos os tipos) ou um
#' número de 0 a 5, onde:
#'   - 0: Sancionada
#'   - 1: Promulgada
#'   - 2: Sancionada/Republicada
#'   - 3: Promulgada/Republicada
#'   - 4: Revogada
#'   - 5: Aprovada
#' @param prop_assuntos Assuntos relacionados à proposição.
#' @param prop_chave Chave da proposição.
#' @param prop_sumula Sumula da proposição.
#' @param .reportar Valor lógico indicando se deve reportar a ação realizada. Padrão é TRUE.
#'
#' @return Retorna uma tabela com os projetos legislativos correspondentes aos parâmetros fornecidos.
#' Se nenhum projeto for encontrado, retorna NULL.
#'
#' @export
#'
#' @importFrom httr POST user_agent
#' @importFrom rvest html_table
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @import glue
#'
#' @examples
#' \dontrun{
#' alepr_projeto(prop_tipo = 1, prop_numero = 21890, prop_ano = 2024)
#'
#' alepr_projeto(prop_tipo = 4, prop_numero = 29, prop_ano = 2020)
#' }
#' @family legislativo
alepr_projeto <- function(prop_tipo = "", prop_autores = "", prop_numero = "", prop_ano = "", prop_conclusao = "", prop_assuntos = "", prop_chave = "", prop_sumula = "", .reportar = TRUE) {

  # Verificar se o prop_tipo é válido (deve ser entre 1 e 11 ou vazio)
  ## Vazio - Todos os tipos
  ## 1 - Lei
  ## 2 - Lei Complementar
  ## 3 - Resolução
  ## 4 - Decreto Legislativo
  ## 5 - Emenda Constitucional
  ## 6 - Indicação Legislativa
  ## 7 - Ato
  ## 10 - Ofício
  ## 11 - Documento de Designação
  ## 12 - Portaria
  if (prop_tipo != "" & !prop_tipo %in% 1:11) {
    glue::glue("Prop_tipo {prop_tipo}. \n Prop_tipo deve ser vazio ou entre 1 e 11") |> print()
    return(NULL)
  }
  # Verificar se o prop_conclusao é válido (deve ser vazio ou entre 0 e 5)
  ## Vazio - Todos os tipos
  ## 0 - Sancionada
  ## 1 - Promulgada
  ## 2 - Sancionada/Republicada
  ## 3 - Promulgada/Republicada
  ## 4 - Revogada
  ## 5 - Aprovada
  if (prop_conclusao != "" & !prop_conclusao %in% 0:5) {
    glue::glue("Prop_conclusao {prop_conclusao}. \n Prop_conclusao deve ser vazio ou entre 0 e 5") |> print()
    return(NULL)
  }

  # URL da requisição
  url <- "http://portal.assembleia.pr.leg.br/index.php/pesquisa-legislativa/legislacao-estadual"

  # Parâmetros da requisição
  parametros <- list(
    enviado = 1,
    prop_tipo = prop_tipo,
    prop_autores = prop_autores,
    prop_numero = prop_numero,
    prop_ano = prop_ano,
    prop_conclusao = prop_conclusao,
    prop_assuntos = prop_assuntos,
    prop_chave = prop_chave,
    prop_sumula = prop_sumula
  )

  # Requisição
  res <- httr::POST( url, body = parametros, httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0"))

  # Verifica erro
  if (res$status_code != 200) {
    print("Erro. Requisicao retornou erro. Verifique os parametros")
    return(NULL)
  }

  # Extrai o conteúdo
  conteudo <- httr::content(res)

  # Extrai a tabela com os dados
  tabela <- rvest::html_table(conteudo)

  if (length(tabela) == 0) {
    cat("A funcao nao retornou nenhum projeto. Verifique os parametros informados")
    return(NULL)
  }

  tabela <- tabela[[1]]
  tabela <-
    tabela |>
    dplyr::select(1:2)

  # Primeira linha da primeira coluna sempre sai vazio, atribui alguma informação
  tabela[1, 1] <- "Norma"

  # Pivota a tabela para deixar larga
  tabela <-
    tabela |>
    tidyr::pivot_wider(
      names_from = 1,
      values_from = 2
    ) |>
    dplyr::select(1:7)

  # Retorna a tabela

  if (.reportar == TRUE) {
    glue::glue("Extrai do site da Assembleia Legislativa do Estado do Parana as informacoes sobre a proposicao que deu origem a norma n {prop_numero} de {prop_ano}") |> cat()
  }

  tabela
}
