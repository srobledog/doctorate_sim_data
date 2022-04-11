library(tidyverse)
library(semTools)
library(ltm)
library(gt)
library(semPlot)
library(Hmisc)
library(bibliometrix)
library(igraph)
library(tidygraph)

sebastian_scopus <-
  convert2df("data/sebastian_scopus.bib",
             dbsource = "scopus",
             format = "bibtex")

data_tidied_scopus_ref <-
  sebastian_scopus |>
  dplyr::select(SR, CR) |>
  separate_rows(CR, sep = "; ") |>
  na.omit() |>
  mutate(PY = str_extract(CR, "\\([0-9]{4}\\)"),
         PY = str_remove_all(PY, "[\\(\\)]")) |>
  na.omit() |>
  mutate(AU = str_extract(CR, ".*\\([0-9]{4}\\)"),
         AU = str_extract(AU, ".*\\.,"),
         AU = gsub("([^,]+,[^,]+),", "\\1;", AU),
         AU = str_sub(AU, 1, nchar(AU)-1),
         AU = str_replace_all(AU,
                              pattern = "; ",
                              replacement = ";"),
         AU = str_remove_all(AU, pattern = "\\."),
         AU = str_remove_all(AU, pattern = ",")) |>
  na.omit()|>
  rename(main_ref = SR,
         id_ref = CR)

dummy <-
  data_tidied_scopus_ref |>
  dplyr::select(AU, PY) |>
  mutate(PY = as.numeric(PY)) |>
  bind_rows(sebastian_scopus |>
              dplyr::select(AU, PY)) |>
  unique()

sebastian_asn <-
  biblioNetwork(M = data.frame(dummy),
                analysis = "collaboration",
                network = "authors") |>
  graph_from_adjacency_matrix(mode = "undirected",
                              weighted = TRUE) |>
  simplify() |>
  as_tbl_graph() |>
  activate(nodes) |>
  mutate(communities = group_components(type = "weak")) |>
  filter(communities == 1)

asn_nodes <-
  sebastian_asn |>
  activate(nodes) |>
  as_tibble() |>
  rename(author = name) |>
  rownames_to_column("name")

asn_edges <-
  sebastian_asn |>
  activate(edges) |>
  as_tibble() |>
  unique()

asn_igraph <-
  graph_from_data_frame(d = asn_edges,
                        directed = FALSE,
                        vertices = asn_nodes)

asn_igraph |> write_graph("sebastian_asn.graphml",
                          "graphml")
