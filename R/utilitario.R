#' Retorna uma paleta de cores baseada na capa dos álbuns da Taylor Swift.
#'
#' Esta função retorna uma paleta de cores baseada em nos álbuns da Taylor Swift. Os álbuns suportados são "debut", "fearless_sv", "fearless_tv", "speaknow_sv", "speaknow_tv", "red_sv", "red_tv", "nineteen89_sv" e "nineteen89_tv". A paleta de cores é gerada com base nas cores predominantes na capa do álbum. A função aceita um argumento adicional, `n`, que especifica o número de cores a serem retornadas na paleta. O padrão é 5. Você pode ainda pedir os tons pasteis de todos os álbuns.
#'
#' @param album Uma string indicando o álbum para o qual a paleta de cores deve ser gerada.
#' @param n Opcional. O número de cores a serem retornadas na paleta. O padrão é 5.
#' @return Um vetor de cores.
#' @examples
#' utilitario_tscolor("debut")
#' utilitario_tscolor("nineteen89_tv", 3)
#' @export
#'
#' @family utilitarios
utilitario_tscolor <- function(album, n = 5){
  switch(
    album,
    pastel = rep(c("#A5C9A5", "#EFC180", "#C7A8CB", "#7A2E39", "#B5E5F8", "#746F70", "#F7B0CC", "#CDC9C1", "#C5AC90", "#242E47"), n)[1:n],
    debut = c('#34F2FE', '#6AEBB3', '#97E9C1', '#C6E597', '#00A2AD'),
    fearless_sv = rep(c('#F7ED94', '#DEC477', '#C3B377', '#C49977', '#BB8F68'), n)[1:n],
    fearless_tv = rep(c('#A8762F', '#C19651', '#D9BE7B', '#EDE4D5', '#B59467'), n)[1:n],
    speaknow_sv = rep(c('#53306A', '#E2B6CD', '#923D81', '#622E6E', '#DB7CA4'), n)[1:n],
    speaknow_tv = rep(c('#53215C', '#8E508F', '#733659', '#F6CCAA', '#311744'), n)[1:n],
    red_sv = rep(c('#891D3D', '#C60404', '#C3403F', '#FA8282', '#510303'), n)[1:n],
    red_tv = rep(c('#730A02', '#B21038', '#982B2E', '#C4AE97', '#792300'), n)[1:n],
    nineteen89_sv = rep(c('#CBBC9D', '#CA8D63', '#998292', '#C9A281', '#51576F'), n)[1:n],
    nineteen89_tv = rep(c('#91ABC0', '#D5A57C', '#DADCD0', '#D1C2A0', '#4F799F'), n)[1:n]
  )
}

#' Remove cabeçalhos e rodapés de texto extraído de documentos PDF.
#'
#' Esta função extrai texto de documentos PDF, remove cabeçalhos e rodapés específicos
#' de cada página e retorna uma tabela com o texto tratado para cada documento.
#'
#' @param txt_path O caminho para a pasta que contém os arquivos PDF. O padrão é o diretório atual.
#' @param rodape O número de linhas do rodapé a serem removidas de cada página. O padrão é 4.
#' @param cabecalho O número de linhas do cabeçalho a serem removidas de cada página. O padrão é 2.
#' @return Uma tabela com uma coluna de texto e uma coluna de identificação do documento.
#' @examples
#' \dontrun{
#' utilitario_remover_cr()
#' utilitario_remover_cr(txt_path = 'caminho/para/pasta', rodape = 3, cabecalho = 1)
#' }
#' @import pdftools
#' @import stringr
#' @import purrr
#' @importFrom stringr str_extract
#' @importFrom purrr map map_df
#' @export
#'
#' @family utilitarios
utilitario_remover_cr <- function(txt_path = '.', rodape = 4, cabecalho = 2){

  ## lista os arquivos em pdf de uma determinada pasta
  pdf <- list.files(txt_path, pattern = "*.pdf", full.names = TRUE)

  ## cria uma tabela com a adpf, id documento e o texto sem cabeçalhos e rodapé
  ## aplicar a função ~ {... em cada pdf)
  tabela <- purrr::map_df(pdf, ~ {
    ## extrair o texto de todas as páginas de cada pdf
    # texto <- tabulizer::extract_text(
    #   .x,
    #   1:pdftools::pdf_length((.x))
    # )
    texto <- pdftools::pdf_text(.x)
    ## cria uma lista com o texto sem cabeçalho e rodapé
    ## para cada página do texto aplica a função
    texto_tratado <- purrr::map(texto, function(pagina) {
      ## separa as linhas das páginas
      linhas <- strsplit(pagina, "\n")[[1]]

      ## remove n linhas de cima e n linhas de baixo
      linhas_tratadas <-
        linhas[-c(
          1:min(cabecalho, length(linhas)),
          seq(
            length(linhas),
            length(linhas) - min(rodape, length(linhas) + 1)
          )
        )]

      ## junta as linhas
      texto_novo <- paste(linhas_tratadas, collapse = "\n")
      return(texto_novo)
    })
    ## cria a tabela com a adpf, id documento e o texto sem cabeçalhos e rodapé
    data <- data.frame(
      id = stringr::str_extract(.x, '(\\d+).pdf')[[1]] |> stringr::str_extract('\\d+'),
      text = paste0(texto_tratado, collapse = "\n") # juntar as páginas
    )
  }, .progress = TRUE)
}

#' Remove acentos de um determinado texto
#'
#' Esta função remove os acentos de textos.
#'
#' @param texto Um vetor de strings contendo os textos com acentos.
#'
#' @return Um vetor de strings com os acentos removidos.
#'
#' @examples
#' utilitario_remover_acentos(c("Café", "Maçã", "São Paulo"))
#'
#' @details
#' Esta função substitui os caracteres acentuados por suas
#' versões não acentuadas. Por exemplo, "Café" é transformado em "Cafe".
#'
#' @details
#' Esta função é semelhante à função \code{txt4cs::remove_accent()} do pacote \code{txt4cs},
#' mas não depende de outros pacotes externos.
#'
#' @export
utilitario_remover_acentos <- function(texto) {
  substituicao <- c("\u00E1" = "a", "\u00E9" = "e", "\u00ED" = "i", "\u00F3" = "o", "\u00FA" = "u",
                    "\u00E0" = "a", "\u00E8" = "e", "\u00EC" = "i", "\u00F2" = "o", "\u00F9" = "u",
                    "\u00E3" = "a", "\u00F5" = "o", "\u00E2" = "a", "\u00EA" = "e", "\u00EE" = "i",
                    "\u00F4" = "o", "\u00FB" = "u", "\u00E7" = "c", "\u00C1" = "A", "\u00C9" = "E",
                    "\u00CD" = "I", "\u00D3" = "O", "\u00DA" = "U", "\u00C0" = "A", "\u00C8" = "E",
                    "\u00CC" = "I", "\u00D2" = "O", "\u00D9" = "U", "\u00C3" = "A", "\u00D5" = "O",
                    "\u00C2" = "A", "\u00CA" = "E", "\u00CE" = "I", "\u00D4" = "O", "\u00DB" = "U",
                    "\u00C7" = "C")

  texto_sacento <- chartr(paste(names(substituicao), collapse = ""),
                              paste(substituicao, collapse = ""), texto)

  return(texto_sacento)
}
