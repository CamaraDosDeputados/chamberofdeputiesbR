# chamberofdeputiesbR

## Sobre o projeto

*chamberofdeputiesbR* é uma biblioteca que encapsula chamdas à API de Dados 
Abertos da Câmara dos Deputados. Foi escrita na linguagem R.

Ela foi desenvolvida pelo Serviço de Ciência de Dados da Câmara dos Deputados
(Secid).

## Público-alvo

O público-alvo da biblioteca são servidores públicos, pesquisadores, jornalistas, 
integrantes de ONGs e cidadãos em geral que tenham necessidade de consultar e
analisar dados da atividade legislativa.

## Não escopo

Essa biblioteca não tem como objetivo recuperar dados sobre cotas, contratos e outras
informações de cunho admnistrativo.

## Licenciamento

É permitida a reprodução ou uso do código em outras aplicações, desde que citada a 
autoria, que o código permaneça aberto e que trechos do código não sejam usados em
serviços de código fechado (licença GPLv3).

Por enquanto, as funções disponíveis são as listadas a seguir. Todas estão documentadas
na pssta *Man*.

## Funções implementadas até o momento

Em ordem alfabética:

1. extrair_blocos_id()
2. extrair_blocos_partidarios()
3. extrair_deputados_legislatura()
4. extrair_frentes_id()
5. extrair_frentes_parlamentares()
6. extrair_legislatura()
7. extrair_legislaturas()
8. extrair_lideres_legislatura()
9. extrair_membros_frentes_parlamentares()
10. extrair_mesa_legislatura()
11. extrair_partidos_legislatura()
12. extrair_partidos_membros()
13. extrair_proposicoes_autores()
14. extrair_proposicoes_id()
15. extrair_proposicoes_relacionadas()
16. extrair_proposicoes_temas()
17. extrair_proposicoes_tramitacoes()
18. extrair_proposicoes_votacoes()
19. extrair_votacoes()
20. extrair_votacoes_id()
21. extrair_votacoes_orientacoes()
22. extrair_votacoes_votos()

## Instalação

A instalação segue o padrão de pacotes R disponíveis no Github:

1. Instale o pacote devtools:

```
install.packages("devtools")
```

2. Instale o pacote a partir do repositório:

```
install_github("CamaraDosDeputados/chamberofdeputiesbR")
```
