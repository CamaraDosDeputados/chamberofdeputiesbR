% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extrair_partidos_legislatura.R
\name{extrair_partidos_legislatura}
\alias{extrair_partidos_legislatura}
\title{Extrai os dados básicos dos \emph{partidos} de uma \emph{Legislatura}
específica da Câmara dos Deputados.}
\arguments{
\item{url_base}{A URL do JSON que contém os dados básicos de partidos.}

\item{id_legislatura}{O ID da legislatura. Se nada for passado, o padrão é a
legislatura atual.}

\item{ordem}{A ordem de classificação dos resultados.}

\item{criterio_ordenacao}{O critério de classificação dos resultados.}

\item{itens}{O número de itens por página.}
}
\value{
Um dataframe contendo os dados básicos do partido.
}
\description{
Esta função consulta a API de Dados Abertos da Câmara dos Deputados e extrai
os dados básicos dos \emph{partidos políticos} de uma \emph{Legislatura}
específica.
}
\examples{
df_partidos <- extrair_partidos_legislatura()
}
