% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oasis.R
\name{oasis}
\alias{oasis}
\title{Realiza uma pesquisa no OASIS e retorna os resultados}
\usage{
oasis(
  busca,
  tipo = "AllFields",
  ordem = "year",
  metricas = FALSE,
  rayyan = FALSE
)
}
\arguments{
\item{busca}{Uma string contendo os termos de busca desejados.}

\item{tipo}{O tipo de campo onde a busca será realizada. Pode ser 'AllFields' (padrão), 'title', 'author', 'abstract', entre outros.}

\item{ordem}{A ordem de classificação dos resultados. Pode ser 'year' (padrão), 'title', 'author', 'relevance', entre outros.}

\item{metricas}{Um valor lógico indicando se devem ser retornadas apenas métricas (TRUE) ou registros completos (FALSE). O padrão é FALSE.}

\item{rayyan}{Um valor lógico indicando se os dados devem ser formatados para importação no Rayyan (TRUE) ou não (FALSE). O padrão é FALSE.}
}
\value{
Um data frame contendo os metadados dos registros encontrados.
}
\description{
Esta função realiza uma pesquisa no OASIS utilizando a API e retorna os resultados encontrados.
Ela extrai os metadados dos registros encontrados, como título, autores, resumo, palavras-chave, data de publicação, notas e URL.
}
\examples{
\dontrun{
# Realizar pesquisa no OASIS e obter os resultados
oasis('"perfilamento racial"')

# Realizar pesquisa no OASIS e formatar os resultados para uso com o Rayyan
oasis('"perfilamento racial"', rayyan = TRUE)
}
}
\references{
Ativando métricas (metricas = TRUE) você vai ter acesso a indicadores sobre o assunto pesquisado. Esse argumento chama a função busca_oasisbr() que retorna um data frame com as métricas. A função foi adaptada do repositório \href{https://github.com/projetos-codic-ibict/Indicadores-oasisbr}{Indicadores-oasisbr}.

Informações sobre o OASIS: https://oasisbr.ibict.br/vufind/
}
