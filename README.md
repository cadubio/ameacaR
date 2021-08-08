
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ameacaR

<!-- badges: start -->
<!-- badges: end -->

O objetivo deste aplicativo é servir como ferramente de busca por
espécies ameaçadas de extinção.

A primeira versão (1.0.0) busca por espécies de plantas (Angiospermas)
ameaçadas de extinção em nível nacional e, estadual para cada estado do
sul do Brasil ([Paraná](https://www.sociedadechaua.org/publicacoes),
[Santa
Catarina](https://www.sde.sc.gov.br/index.php/biblioteca/consema/legislacao/resolucoes/325-resolucao-consema-no-512014-1/file)
e [Rio Grande do
Sul](http://www.mcn.fzb.rs.gov.br/conteudo/4816/?Homologada_a_nova_Lista_da_Flora_Ga%C3%BAcha_Amea%C3%A7ada_de_Extin%C3%A7%C3%A3o)).

# Como usar

## Digitando os nomes científicos

![](img/digitando.gif)

## Colando os nomes científicos

![](img/colando.gif)

## Enviando uma planilha com os nomes científicos

![](img/enviar.gif)

# Notas sobres as listas estaduais

A [Lista Vermelha da Flora Ameaçada no Estado do
Paraná](https://www.sociedadechaua.org/publicacoes) foi elabora pela
Secretaria de Estado do Meio Ambiente e Deutsche Gessellschaft
Technische Zusammenarbeit (SEMA/GTZ), e publicada em 1995. Em 2020 as
grafias dos nomes científicos da lista de 1995 foram atualizadas por
técnicos da [Sociedade Chauá](https://www.sociedadechaua.org) em
parceria com o [Mater Natura - Instituto de Estudos
Ambientais](http://maternatura.org.br/). Conforme o sítio da Sociedade
Chauá (visitado em 09/08/2021):

> Todos os nomes das espécies passaram por atualização da grafia, feita
> com base na Lista da Flora do Brasil (FLORA DO BRASIL, 2020) e
> realizada através da ferramenta on-line denominada Plantminer
> (CARVALHO et al, 2010). Para os nomes que não constavam na Lista da
> Flora do Brasil foram buscados sinônimos no Global Biodiversity
> Information Facility (GBIF.org) e <https://www.tropicos.org/home>.
> Alguns nomes não puderam ser devidamente atualizados, porque não
> demonstraram sinônimos aceitos na Lista da Flora do Brasil, foram
> mantidos com o nome original que constava em suas listas. O banco de
> dados foi formatado utilizando como base o sistema de classificação
> vegetal Angiosperm Phylogeny Group versão IV (APG, 2016) e
> relacionados.

Está lista atualizada em 2020 foi utilizada como base de dados inicial
para esta ferramenta.

Foi realizada uma nova atualização nomenclatural da Lista Vermelha da
Flora Ameaçada no Estado do Paraná (2020), com o pacote `R`
[flora](http://www.github.com/gustavobio/flora).

``` r
readr::read_csv("testes/ameacadasPR.csv")
#> # A tibble: 582 × 3
#>    nome.original              nome.BFG                   pr   
#>    <chr>                      <chr>                      <chr>
#>  1 Abuta selloana             Abuta selloana             RR   
#>  2 Acanthocladus brasiliensis Acanthocladus brasiliensis RR   
#>  3 Achatocarpus praecox       Achatocarpus praecox       RR   
#>  4 Achimenes ichtyostoma      <NA>                       EN   
#>  5 Acianthera adiri           Acianthera adiri           EN   
#>  6 Acianthera karlii          Acianthera karlii          EN   
#>  7 Acianthera langeana        Acianthera langeana        RR   
#>  8 Acianthera ophiantha       Acianthera ophiantha       VU   
#>  9 Acianthera tricarinata     Acianthera tricarinata     EN   
#> 10 Adenocalymma paulistarum   Adenocalymma paulistarum   RR   
#> # … with 572 more rows
```

<span style="font-size: small">(BFG ou Brasilian Flora Group, refere-se
à sigla em inglês para Grupo da Flora do Brasil).</span>

Dos 582 nomes científicos constantes na lista de ameaçadas do Paraná
(2020), 23 não foram encontrados em [Flora do Brasil
2020](http://floradobrasil.jbrj.gov.br/)

``` r
readr::read_csv("testes/ameacadasPR.csv") |> 
  dplyr::filter(is.na(nome.BFG))
#> # A tibble: 23 × 3
#>    nome.original                               nome.BFG pr   
#>    <chr>                                       <chr>    <chr>
#>  1 Achimenes ichtyostoma                       <NA>     EN   
#>  2 Aeschynomene montevidensis var. microphylla <NA>     RR   
#>  3 Begonia diaphones                           <NA>     EN   
#>  4 Begonia glabrescens                         <NA>     EN   
#>  5 Begonia klydophylla                         <NA>     EN   
#>  6 Begonia succulenta                          <NA>     EN   
#>  7 Begonia tibagiensis                         <NA>     EN   
#>  8 Byttneria catalpaefoli subsp. sidaefolia    <NA>     EN   
#>  9 Chaetoclamys psammina                       <NA>     EN   
#> 10 Echinodorus rhombifolia                     <NA>     RR   
#> # … with 13 more rows
```

Verificando estes 23 nomes, com a função `TNRS`do pacote
[TNRS](https://github.com/EnquistLab/RTNRS), temos agora 6 nomes
científicos sem correspondência nas bases de dados [Flora do Brasil
2020](http://floradobrasil.jbrj.gov.br/),
[Tropicos](https://www.tropicos.org/home) ou
[TLP](http://www.theplantlist.org).

``` r
readr::read_csv("testes/ameacadasPR.csv") |>
  dplyr::filter(is.na(nome.BFG)) |>
  dplyr::select(nome.original) |>
  purrr::flatten_chr() |>
  TNRS::TNRS() |>
  dplyr::select(Name_submitted, Name_matched, Taxonomic_status, Accepted_name,
    Source, Name_matched_url
  ) |>
  tibble::as_tibble()
#> # A tibble: 23 × 6
#>    Name_submitted     Name_matched      Taxonomic_status Accepted_name    Source
#>    <chr>              <chr>             <chr>            <chr>            <chr> 
#>  1 Achimenes ichtyos… Achimenes ichthy… "Synonym"        "Mandirola icht… "trop…
#>  2 Aeschynomene mont… Aeschynomene mon… "Synonym"        "Aeschynomene m… "trop…
#>  3 Begonia diaphones  Begonia diaphones "No opinion"     ""               "trop…
#>  4 Begonia glabresce… [No match found]  ""               ""               ""    
#>  5 Begonia klydophyl… [No match found]  ""               ""               ""    
#>  6 Begonia succulenta [No match found]  ""               ""               ""    
#>  7 Begonia tibagiens… [No match found]  ""               ""               ""    
#>  8 Byttneria catalpa… Byttneria catalp… "Accepted"       "Byttneria cata… "trop…
#>  9 Chaetoclamys psam… [No match found]  ""               ""               ""    
#> 10 Echinodorus rhomb… [No match found]  ""               ""               ""    
#> # … with 13 more rows, and 1 more variable: Name_matched_url <chr>
```

Os mesmo procedimentos realizados com a lista de espécies ameaçadas do
estado do Paraná foram realizados com as lista do Rio Grande do Sul e
Santa Catarina. Nestas listas todos os nomes foram validados.

# Novas versões

Em versões futuras serão incluídos outros grupos de plantas (briófitas,
samambaias, licófitas e gimnospermas), além das algas e dos fungos.
Pretende-se ainda adicionar as espécies de fauna ameaçadas de extinção.
