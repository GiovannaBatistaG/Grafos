# -------------------------------------------------------
# 00) Pacotes
# -------------------------------------------------------
if(!require(dplyr)) install.packages("dplyr")
if(!require(igraph)) install.packages("igraph")
if(!require(proxy)) install.packages("proxy")
if(!require(visNetwork)) install.packages("visNetwork")

library(dplyr)
library(igraph)
library(proxy)
library(visNetwork)

# -------------------------------------------------------
# 01) Dataset da pesquisa
# -------------------------------------------------------
dados <- data.frame(
  Nome = c(
    "Giovanna Batista", "Alani da Silva Rodrigues", "Marcelo Amorim",
    "Nathan Nogueira Carrara", "Enzo Vitorino", "Vinicius Tamae",
    "Felipe Januario Alonso", "Vitória", "Alexandre Alonso Pereira", "Cauê Leal"
  ),
  Banda = c(
    "Oasis", "Turma do pagode", "Henrique e Juliano",
    "Iron Maiden", "nx0", "One ok rock",
    "Linkin Park", "Turma do pagode", "Three Days Grace", "Turma do pagode"
  ),
  Genero = c(
    "Rock", "Pagode", "Sertanejo",
    "Rock", "Rock Nacional", "Rock",
    "Rock", "Pagode", "Rock", "Pagode"
  ),
  Preferencia = c(
    "Internacional", "Nacional", "Nacional",
    "Internacional", "Nacional", "Internacional",
    "Internacional", "Nacional", "Internacional", "Nacional"
  )
)

dados

# -------------------------------------------------------
# 02) MATRIZ DE INCIDÊNCIA
# -------------------------------------------------------
cat_vars <- dados %>% select(Banda, Genero, Preferencia)

inc_mat <- model.matrix(~ . - 1, data = cat_vars)
incidencia <- as.data.frame(inc_mat)
rownames(incidencia) <- dados$Nome

print("Matriz de Incidência:")
print(incidencia)

# -------------------------------------------------------
# 03) MATRIZ DE SIMILARIDADE (Jaccard)
# -------------------------------------------------------
similaridade <- as.matrix(simil(as.matrix(incidencia), method = "Jaccard"))

print("Matriz de Similaridade:")
print(round(similaridade, 3))

# -------------------------------------------------------
# 04) MATRIZ DE COOCOORRÊNCIA
# -------------------------------------------------------
coocorrencia <- t(as.matrix(incidencia)) %*% as.matrix(incidencia)

print("Matriz de Coocorrência:")
print(coocorrencia)

# -------------------------------------------------------
# 05) GRAFO DE INCIDÊNCIA
# -------------------------------------------------------
nodes_alunos <- data.frame(
  id = rownames(incidencia),
  label = rownames(incidencia),
  group = "Aluno",
  color = "lightblue"
)

nodes_categorias <- data.frame(
  id = colnames(incidencia),
  label = colnames(incidencia),
  group = "Categoria",
  color = "salmon"
)

node_inc <- rbind(nodes_alunos, nodes_categorias)

links_inc <- data.frame()
for (aluno in rownames(incidencia)) {
  for (cat in colnames(incidencia)) {
    if (incidencia[aluno, cat] == 1) {
      links_inc <- rbind(links_inc, data.frame(from = aluno, to = cat))
    }
  }
}

visNetwork(node_inc, links_inc) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout = "layout_with_fr")

# -------------------------------------------------------
# 06) GRAFO DE SIMILARIDADE
# -------------------------------------------------------
node_sim <- data.frame(
  id = rownames(similaridade),
  label = rownames(similaridade),
  group = "Aluno",
  color = "lightgreen"
)

links_sim <- data.frame()
for (i in 1:nrow(similaridade)) {
  for (j in 1:ncol(similaridade)) {
    if (i < j && similaridade[i, j] > 0) {
      links_sim <- rbind(
        links_sim,
        data.frame(
          from = rownames(similaridade)[i],
          to = rownames(similaridade)[j],
          value = similaridade[i, j]
        )
      )
    }
  }
}

visNetwork(node_sim, links_sim) %>%
  visEdges(scaling = list(min = 1, max = 7)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout = "layout_with_fr")

# -------------------------------------------------------
# 07) GRAFO DE COOCOORRÊNCIA
# -------------------------------------------------------
node_cooc <- data.frame(
  id = rownames(coocorrencia),
  label = rownames(coocorrencia),
  group = "Categoria",
  color = "orange"
)

links_cooc <- data.frame()
for (i in 1:nrow(coocorrencia)) {
  for (j in 1:ncol(coocorrencia)) {
    if (i < j && coocorrencia[i, j] > 0) {
      links_cooc <- rbind(
        links_cooc,
        data.frame(
          from = rownames(coocorrencia)[i],
          to = rownames(coocorrencia)[j],
          value = coocorrencia[i, j]
        )
      )
    }
  }
}

visNetwork(node_cooc, links_cooc) %>%
  visEdges(scaling = list(min = 1, max = 10)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout = "layout_with_fr")

# -------------------------------------------------------
# 08) MÉTRICAS TOPOLOGICAS (Opcional)
# -------------------------------------------------------
grafo_inc <- graph_from_incidence_matrix(as.matrix(incidencia))
grafo_sim <- graph_from_adjacency_matrix(similaridade, mode="undirected", diag=FALSE, weighted=TRUE)
grafo_cooc <- graph_from_adjacency_matrix(coocorrencia, mode="undirected", diag=FALSE, weighted=TRUE)

metricas <- function(g) {
  list(
    Grau = degree(g),
    Centralidade_Proximidade = closeness(g),
    Centralidade_Intermediacao = betweenness(g),
    Densidade = edge_density(g),
    Diametro = diameter(g)
  )
}

print("Métricas - Grafo de Incidência:")
print(metricas(grafo_inc))

print("Métricas - Grafo de Similaridade:")
print(metricas(grafo_sim))

print("Métricas - Grafo de Coocorrência:")
print(metricas(grafo_cooc))


#mostrar matrizes
View(incidencia)
View(similaridade)
View(coocorrencia)

#mostrar grafos
#incidencia
visNetwork(node_inc, links_inc) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout = "layout_with_fr")

#similaridade
visNetwork(node_sim, links_sim) %>%
  visEdges(scaling = list(min = 1, max = 7)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout = "layout_with_fr")

#coocorrencia
visNetwork(node_cooc, links_cooc) %>%
  visEdges(scaling = list(min = 1, max = 10)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visIgraphLayout(layout = "layout_with_fr")



