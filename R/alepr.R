#' Função principal alepr_projeto
#'
#' Esta função extrai dados da API da Assembleia Legislativa do Estado do Paraná.
#'
#' @param ... Argumentos adicionais que serão passados para métodos específicos.
#' @param .report Se TRUE, mostra informações sobre a extração de dados.
#' @details
#' ## Método data.frame
#' Argumento obrigatório `obj` que deve ser um data.frame. E os argumentos opcionais: `numero`, `ano` e `codigoTipoNormaLegal`, que devem conter as colunas do data frame com os respectivos valores. Se você não quiser um resumo ao final, use `.report = FALSE`.
#' ## Método numeric
#' @export
#' @rdname alepr_projeto
#' @usage
#' alepr_projeto(...)
#' @usage
#' ## s3 method for class 'data.frame'
#' alepr_projeto(
#'  obj,
#'  numero = NULL,
#'  ano = NULL,
#'  codigoTipoNormaLegal = NULL,
#'  .report = TRUE)
#' @examples
#' \dontrun{
#' alepr_projeto()
#' }
#' @family legislativo
alepr_projeto <- function(..., .report = TRUE) {
  UseMethod('alepr_projeto')
}

#' Método para extrair dados de um data frame
#'
#' Este método extrai dados da API da Assembleia Legislativa do Estado do Paraná com base em um data frame.
#'
#' @param obj Um data frame contendo os dados de entrada.
#' @param numero Coluna do data frame contendo números de norma legal.
#' @param ano Coluna do data frame contendo anos de norma legal.
#' @param codigoTipoNormaLegal Coluna do data frame contendo códigos de tipo de norma legal.
#' @param .report Se TRUE, mostra informações sobre a extração de dados.
#' @return Um data frame contendo os dados extraídos da API.
#' @method alepr_projeto data.frame
#' @export
#' @examples
#' dados <- data.frame(numero = c(1, 2, 3), ano = c(2020, 2021, 2022), codigoTipoNormaLegal = c("A", "B", "C"))
#' alepr_projeto.data.frame(dados)
alepr_projeto.data.frame <- function(obj, numero = NULL, ano = NULL, codigoTipoNormaLegal = NULL, .report = TRUE){
  url <- 'https://consultas.assembleia.pr.leg.br/api/public/norma-legal/filtrar'

  cli::cli_progress_bar(
    format = "Extraindo {cli::pb_bar} {cli::pb_porcent} ",
    total = nrow(obj)
  )
  for (i in 1:nrow(obj)){
    if (!is.null(numero)) {
      numero <- obj[[numero]][i]
    }
    if (!is.null(ano)) {
      ano <- obj[[ano]][i]
    }
    if (!is.null(codigoTipoNormaLegal)) {
      codigoTipoNormaLegal <- obj[[codigoTipoNormaLegal]][i]
    }

    parametros <- list(
      ano = ano,
      codigoTipoNormaLegal = codigoTipoNormaLegal,
      numero = numero,
      numeroMaximoRegistro = 200
    )

    a <- httr::POST(url, body = parametros, encode = 'json')

    if (a$status_code != 0) {
      cat("Erro ao acessar à API da ALEPR")
      return(invisible())
    }

    a <- a |> httr::content(as = 'text') |> jsonlite::fromJSON()
    a <- a |> purrr::pluck('lista')

    if (i == 1) {
      dados <- a
    } else {
      dados <- dplyr::bind_rows(dados, a)
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  if (.report == TRUE) {
    cli::cli_alert_info("A função extraiu da Assembleia Legislativa do Estado do Paraná os dados solicitados.")
    cli::cli_text("Os projetos foram propostos por {length(unique(dados$autores))} autor{?es} distinto{?s} sobre {length(unique(dados$assunto))} matéria{?s}")
  }

  dados
}
