% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extrair_frentes_id.R
\name{extrair_frentes_id}
\alias{extrair_frentes_id}
\title{Extrai os dados básicos das \emph{frentes parlamentares} da Câmara dos
Deputados por \emph{id}.}
\arguments{
\item{url_base}{A URL do JSON que contém os dados básicos de frentes parlamentares, a partir de seu id.}

\item{id}{Identificador único da frente parlamentar.}
}
\value{
Um dataframe contendo os dados básicos da frente parlamentar.
}
\description{
Esta função consulta a API de Dados Abertos da Câmara dos Deputados e extrai
os dados básicos de \emph{frentes parlamentares} a partir de um \emph{id}
específico.
}
\examples{
df_frentes <- extrair_frentes_id(id = 55640)
}
