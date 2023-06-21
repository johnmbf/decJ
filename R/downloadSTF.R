#' Baixa as decisões extraídas da página de jurisprudências do STF
#'
#'`downloadSTF.juris` permite baixar as decisões do site de site de [jurisprudência.stf.jus.br](jurisprudência.stf.jus.br).
#'
#'Igual a função [juriSTF.download()]
#'
#' @param conteudo Objeto `html` que pode ser obtido com a função [juriSTF.conteudo()]
#' @param UA User-Agent
#' @param arquivo Local onde será salvo as decisões
#' @param quantidade Quantidade de decisões que se pretende baixar \(não maior do que as buscadas com a função `juriSTF.conteudo`\). Pode ser colocado um valor referente a posição do conteúdo.
#'
#' @return Decisões em `.pdf`
#' @export
#'
#' @examples
downloadSTF.juris = function(conteudo, UA, arquivo, quantidade){

  for(i in 1:quantidade){
    doc <- stringr::str_split_i(
      conteudo$inteiro_teor_url[i],
      pattern = '=',
      -1
    )
    httr::POST(
      paste(
        'https://redir.stf.jus.br/paginadorpub/paginador.jsp?docTP=TP&docID=',
        doc,
        sep = ''
      ),
      httr::add_headers(
        'User-Agent' = UA
      ),
      httr::write_disk(paste(arquivo,conteudo$id[i], '.pdf', sep = ''), T)
    )

    # Aguarde 5 segundos
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<5){}

  }
}

# downloadSTF.juris250 = function(UA, arquivo, busca, n.pesq){
#
#   x <- trunc(n.pesq / 250)
#
#   # Arquivo de busca json
#   stfBusca <- jsonlite::read_json(busca)
#
#   # Quantidade de registros que vão ser buscados
#   if(n.pesq > 250){
#     n.pesq <- 250
#   } else {
#     n.pesq
#   }
#   stfBusca$size <- n.pesq
#
#   for(i in 1:x) {
#
#     stfBusca$from <- (i - 1) * 250
#     n.dec <- (i - 1) * 250
#     # Extração dos dados
#     htmlSTF <- httr::POST(
#       'https://jurisprudencia.stf.jus.br/api/search/search',
#       body = stfBusca,
#       encode = 'json',
#       httr::add_headers(
#         'User-Agent' = UA
#       )
#     )
#
#     conteudo <- httr::content(htmlSTF)
#
#     for(i in 1:n.dec){
#       getConteudo <- conteudo$result$hits$hits[[i]]$`_source`
#       doc <- stringr::str_split_i(
#         conteudo$result$hits$hits[[i]]$`_source`$inteiro_teor_url,
#         pattern = '=',
#         -1
#       )
#       httr::POST(
#         paste(
#           'https://redir.stf.jus.br/paginadorpub/paginador.jsp?docTP=TP&docID=',
#           doc,
#           sep = ''
#         ),
#         httr::add_headers(
#           'User-Agent' = UA
#         ),
#         httr::write_disk(paste(arquivo,getConteudo$titulo, '.pdf', sep = ''), T)
#       )
#
#       # Aguarde 5 segundos
#       date_time<-Sys.time()
#       while((as.numeric(Sys.time()) - as.numeric(date_time))<5){}
#
#     }
#   }
# }

#' Baixa a petição inicial do processo de controle concentrado
#'
#' @param classe
#' @param n
#' @param UA
#' @param arquivo
#'
#' @return
#' @export
#'
#' @examples
downloadSTF.inicial = function(classe, n, UA, arquivo){

  for(i in n){ ## faça isso em cada processo ##

    # Busque no site o processo
    getProcesso <- httr::GET(
      paste(
        'https://portal.stf.jus.br/processos/listarProcessos.asp?classe=',
        classe,
        '&numeroProcesso=',
        i,
        sep = ''
      ),
      httr::add_headers(
        'User-Agent' = UA
      )
    )

    # Salve o incidente
    getIncidente <- stringr::str_split_i(
      getProcesso$url,
      pattern = '=',
      -1
    )

    # Consulta o processo eletrônico
    getProcesso <- httr::GET(
      paste('https://redir.stf.jus.br/estfvisualizadorpub/jsp/consultarprocessoeletronico/ConsultarProcessoEletronico.jsf?seqobjetoincidente=',
            getIncidente,
            sep = ''),
      httr::add_headers(
        'User-Agent' = UA
      )
    )

    # Leia o conteúdo do processo
    getConteudo <- httr::content(getProcesso, encoding = 'UTF-8')

    # Busca a petição inicial
    getInicial <- getConteudo %>%
      html_element('a')

    getInicial <- xml_attr(t1, 'href')

    # Salva o arquivo
    httr::GET(
      getInicial,
      httr::add_headers(
        'User-Agent' = UA
      ),
      httr::write_disk(paste(arquivo, i, '_inicial', '.pdf', sep = ''), T)
    )

    # Aguarde 5 segundos
    date_time<-Sys.time()
    while((as.numeric(Sys.time()) - as.numeric(date_time))<5){}

  }

}
