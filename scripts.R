giant.component <- function(graph) {
  cl <- igraph::clusters(graph)
  igraph::induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}


scopus_asn_main_matrix <-
  biblioNetwork(M = sebastian_scopus,
                analysis = "collaboration",
                network = "authors")

scopus_asn_main_graph <-
  scopus_asn_main_matrix |>
  graph_from_adjacency_matrix(mode = "undirected",
                              weighted = TRUE) |>
  simplify() |>
  giant.component()


write_graph(scopus_asn_main_graph,
            "sebastian_graph_bx.graphml",
            "graphml")

