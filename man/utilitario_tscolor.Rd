% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilitario.R
\name{utilitario_tscolor}
\alias{utilitario_tscolor}
\title{Retorna uma paleta de cores baseada na capa dos álbuns da Taylor Swift.}
\usage{
utilitario_tscolor(album, n = 5)
}
\arguments{
\item{album}{Uma string indicando o álbum para o qual a paleta de cores deve ser gerada.}

\item{n}{Opcional. O número de cores a serem retornadas na paleta. O padrão é 5.}
}
\value{
Um vetor de cores.
}
\description{
Esta função retorna uma paleta de cores baseada em nos álbuns da Taylor Swift. Os álbuns suportados são "debut", "fearless_sv", "fearless_tv", "speaknow_sv", "speaknow_tv", "red_sv", "red_tv", "nineteen89_sv" e "nineteen89_tv". A paleta de cores é gerada com base nas cores predominantes na capa do álbum. A função aceita um argumento adicional, \code{n}, que especifica o número de cores a serem retornadas na paleta. O padrão é 5. Você pode ainda pedir os tons pasteis de todos os álbuns.
}
\examples{
utilitario_tscolor("debut")
utilitario_tscolor("nineteen89_tv", 3)
}
\seealso{
Other utilitarios: 
\code{\link{utilitario_remover_cr}()}
}
\concept{utilitarios}
