% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extrair_frentes_parlamentares.R
\name{extrair_frentes_parlamentares}
\alias{extrair_frentes_parlamentares}
\title{Extrai os dados básicos das \emph{frentes parlamentares} da Câmara dos
Deputados de uma dada \emph{Legislatura}.}
\arguments{
\item{url}{A URL do JSON que contém os dados básicos de frentes parlamentares}

\item{idLegilatura}{Identificador único da legislatura; caso não informado,
retorna os dados das frentes de todas as legislaturas.}

\item{itens}{Número de itens retornados por página; padrão = 100 (mesmo da API).}
}
\value{
Um dataframe contendo os dados básicos da frente parlamentar.
}
\description{
Esta função consulta a API de Dados Abertos da CD e extrai os dados básicos
de \emph{frentes parlamentares} de uma \emph{Legislatura} específica ou de
todas as legislaturas.
}
\examples{
df_frentes_legislatura <- extrair_frentes_parlamentares()
df_frentes_legislatura_57 <- extrair_frentes_legislatura(idLegislatura = 57)
}
