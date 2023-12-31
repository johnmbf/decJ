% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extrairSTF.R
\name{extrairSTF.decisao}
\alias{extrairSTF.decisao}
\title{Extrair as decisões do processo}
\usage{
extrairSTF.decisao(lista, classe, n)
}
\arguments{
\item{lista}{Uma lista}

\item{classe}{Um caracter que indica a classe processual}

\item{n}{Um número que indica o número do processo}
}
\value{
Uma lista com as decisões dos processos requeridos.
}
\description{
\if{html}{\out{
<img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-deprecated.svg" alt="Deprecated"/>
}}


Utilize a função \code{\link[=stf_decisoes]{stf_decisoes()}} no lugar.
}
\examples{
# Extrair os dados da ADPF 800
\dontrun{
extrairSTF.decisao(lista, "ADPF", 800, UA)
}

# Extrair os dados da ADI 500 a 600
\dontrun{
extrairSTF.decisao(lista, "ADI", 500:600, UA)
}
}
