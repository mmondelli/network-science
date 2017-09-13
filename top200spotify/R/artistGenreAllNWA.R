source('getData.R')

## Genre - Country (Top 200) #####################################################

# Simplificar dataframe market.genre
# Pegar uma ocorrência de determinado gênero por país
market.genre.u <- unique(market.genre)
market.genre.u <- na.omit(market.genre.u); head(market.genre.u)
unique(market.genre.u$genre) ## 367
# Qntd de gênero por market
genre.eachMk <- as.data.frame(table(market.genre.u$market)); colnames(genre.eachMk) <- c('market', 'freq')
# Qtd de cada gênero
eachGenre.total <- as.data.frame(table(market.genre.u$genre))
eachGenre.total <- eachGenre.total[with(eachGenre.total, order(-Freq)), ]
# Gêneros que conectam todo mundo
genre.cntAll <- eachGenre.total[eachGenre.total$Freq>=55,]
market.genre.u <- market.genre.u[!(market.genre.u$genre %in% genre.cntAll$Var1),]

# Matriz de adjacência 57 linhas, 367 colunas
market.genre.am <- xtabs(~ market + genre, market.genre.u)
market.genre.amdf <- as.data.frame(market.genre.am) #market, genre, #freq
# Grafo da matriz de adjacência 
market.genre.g <- graph_from_incidence_matrix(market.genre.am, weighted = TRUE)

# Plot Bipartite #############################################

V(market.genre.g)$size <- 3
V(market.genre.g)$frame.color <- "white"
V(market.genre.g)$color <- colors[4]
V(market.genre.g)$label <- ""
E(market.genre.g)$arrow.mode <- 0
plot.market.genre.g <-plot(market.genre.g,
                           vertex.color=colors[c(1,4)][V(market.genre.g)$type+1],
                           layout = layout.bipartite)

# Projeção (1 - País, 2 - Gênero) ############################
market.genre.projec <- bipartite.projection(market.genre.g)
E(market.genre.projec$proj1)$weight       # 1596
V(market.genre.projec$proj1)       # 57

# Plot heat map - genre similarity between markets ##########
market.genre.edges <- as.data.frame(get.edgelist(market.genre.projec$proj1)); head(market.genre.edges)

# Pegar os qntds de gêneros de cada país 
market.genre.edges <- cbind(market.genre.edges, E(market.genre.projec$proj1)$weight)
colnames(market.genre.edges) <- c('market', 'to', 'w.sim'); head(market.genre.edges)

market.genre.edges <- merge(market.genre.edges, genre.eachMk, by='market')
colnames(market.genre.edges) <- c('from', 'market', 'w.sim'); head(market.genre.edges)

market.genre.edges <- merge(market.genre.edges, genre.eachMk, by='market')
colnames(market.genre.edges) <- c('from', 'to', 'w.sim', 'w.to', 'w.from')
#Calcula porcentagem de similaridade
market.genre.edges['simPercent'] <- market.genre.edges$w.sim/(market.genre.edges$w.to + market.genre.edges$w.from)
# Make a new data.frame, simply reversing A and B
market.genre.edges.oh <- data.frame(to = market.genre.edges$from,
                                    from = market.genre.edges$to,
                                    simPercent = market.genre.edges$simPercent)
# Here's the diagonal
diagonal <- data.frame(to = unique(market.genre.edges$to),
                       from = unique(market.genre.edges$to),
                       simPercent = 1)
# Mash 'em all together
full <- rbind(market.genre.edges[,c(2,1,6)], market.genre.edges.oh); head(full)

# Pegar nome completo dos países
colnames(full) <- c('mk', 'from'); head(full)
full <- merge(full, countries, by='mk')

colnames(full) <- c('t', 'mk', 'sim', 'to'); head(full)
full <- merge(full, countries, by='mk')

colnames(full) <- c('f', 't', 'simPercent', 'from', 'to'); head(full)
full <- full[,c(-1,-2)]

heatMap.mg <- 
  ggplot(data = full, aes(x = to, y = from)) +
  geom_tile(aes(fill = simPercent)) +
  scale_fill_gradient(low = colors[4], high = colors[1], name = "Similarity"
  ) +
  theme_minimal() + 
  labs(title="Similarity between countries based on musical genre",
       y="Countries", 
       x="Countries") +
  theme(axis.text.x=element_text(angle=60, hjust=1, size=10),
        axis.text.y=element_text(size=8))


# Adjacência
market.genre.adj <- get.adjacency(market.genre.projec$proj1,
                                  sparse=TRUE, attr="weight")

# Checking - Comparing markets #################################
# Tá certo!
a <- market.genre.u[market.genre.u$market=='br', ]; nrow(a) # 48
b <- market.genre.u[market.genre.u$market=='us', ]; nrow(b) # 71
c <- a[a$genre %in% b$genre,]; nrow(c) # 37

# Plot Projec 1 ################################################
V(market.genre.projec$proj1)$size <- 8
V(market.genre.projec$proj1)$frame.color <- "white"
V(market.genre.projec$proj1)$color <- colors[4]
V(market.genre.projec$proj1)$label <- ""
E(market.genre.projec$proj1)$arrow.mode <- 0
plot(market.genre.projec$proj1)

# Plot Projec 2 ################################################
V(market.genre.projec$proj2)$size <- 2
V(market.genre.projec$proj2)$frame.color <- "white"
V(market.genre.projec$proj2)$color <- colors[4]
V(market.genre.projec$proj2)$label <- ""
E(market.genre.projec$proj2)$arrow.mode <- 2
plot(market.genre.projec$proj2, layout = layout.circle)

simpleNetwork(as.data.frame(get.edgelist(market.genre.projec$proj1)))

# Grau
# The degree of a vertex is its most basic structural property, 
# the number of its adjacent edges.
market.genre.deg <- degree(market.genre.projec$proj1, mode="all"); market.genre.deg
#plot(market.genre.projec$proj1, vertex.size=market.genre.deg*3)
market.genre.deg.dist <- degree_distribution(market.genre.projec$proj1, cumulative=F, mode="all")
head(market.genre.deg.dist)
plot(x=0:max(market.genre.deg), y=market.genre.deg.dist, pch=19, 
     cex=1.2, col=colors[5], xlab="Grau", ylab="Frequência Acumulada",
     xlim = c(0,57), xaxt='n'); axis(side = 1, at=seq(0,55,5))

# Diameter #######################################################
# The diameter of a graph is the length of the longest geodesic.
market.genre.dmt <- diameter(market.genre.projec$proj1, directed = F); market.genre.dmt

# Density  #######################################################
# The ratio of the number of edges and the number of possible edges.
market.genre.dst <- edge_density(market.genre.projec$proj1, loops = F); market.genre.dst

# Transitivity ###################################################
# Ou coeficiente de clusterização
market.genre.trans <- transitivity(market.genre.projec$proj1, type="global"); market.genre.trans # 0.93

# Distance ########################################################
distances(market.genre.projec$proj1, v = V(market.genre.projec$proj1), 
          to = V(market.genre.projec$proj1), 
          mode = "all", weights = E(market.genre.projec$proj1)$weight, 
          algorithm = "automatic")

mean_distance(market.genre.projec$proj1, directed=F) # 1


# Communities
market.genre.comunities <- cluster_edge_betweenness(market.genre.projec$proj1)
dendPlot(market.genre.comunities, mode="hclust")
plot(market.genre.comunities, market.genre.projec$proj1)

# Strength ####################################################

market.genre.str <- strength(market.genre.projec$proj1, vids = V(market.genre.projec$proj1), 
         mode = "total",
         loops = TRUE, weights = E(market.genre.projec$proj1)$weight)
head(market.genre.str)

hist(market.genre.str,
     main="Histograma de força dos nós",
     xlab="Força", ylab="Frequência", col=colors[4],
     ylim = c(0,40))
