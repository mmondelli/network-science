source('getData.R')
source('~/Dropbox/LNCC/Doutorado/Disciplinas/Ciência de Redes/Project/getData022018.R')

# Genre - Country (Top 5 Genres) #####################################################
# Pegando apenas 5 gêneros para cada pais, de um top 200 artistas
# Excluindo gêneros que aparecem em todos os países

# Simplificar dataframe market.genre
# Pegar uma ocorrência de determinado gênero por país
# Assim é possível verificar em quantos países cada gênero aparece
market.genre.u <- unique(market.genre)
market.genre.u <- na.omit(market.genre.u); head(market.genre.u)
unique(market.genre.u$genre) ## 367
# Quantos países cada gênero alcança
eachGenre.total <- as.data.frame(table(market.genre.u$genre))
eachGenre.total <- eachGenre.total[with(eachGenre.total, order(-Freq)), ]; head(eachGenre.total, 10)

# Excluir gêneros que conectam determinado número de países
genre.cntAll <- eachGenre.total[eachGenre.total$Freq>=51,]; nrow(genre.cntAll)
market.genre.u <- na.omit(market.genre); head(market.genre)
market.genre.u <- market.genre.u[!(market.genre.u$genre %in% genre.cntAll$Var1),]

# Quantas vezes cada gênero aparece por market, ordenado por país e frequência
freqGenre.eachMk <- as.data.frame(table(market.genre.u))
colnames(freqGenre.eachMk) <- c('market', 'genre', 'freq')
freqGenre.eachMk <- freqGenre.eachMk[with(freqGenre.eachMk, order(market, -freq)), ]; head(freqGenre.eachMk)

# Subset pra pegar só os 5 gêneros mais frequêntes de cada market
aux <- as.data.table(freqGenre.eachMk)
top5Genre.eachMk <- aux[, head(.SD, 5), by = 'market']; head(top5Genre.eachMk)
# Excluir países que não possuem gêneros
top5Genre.eachMk <- droplevels(top5Genre.eachMk[top5Genre.eachMk$freq>0,])
length(unique(top5Genre.eachMk$genre)) # 101 gêneros

# Matriz de adjacência 57 linhas, 70 colunas
market.top5genre.am <- xtabs(~ market + genre, top5Genre.eachMk)
market.top5genre.amdf <- as.data.frame(market.top5genre.am) #market, genre, #freq
# Para pegar as diferenças
# market.genre10.amdf$Freq <- ifelse(market.genre10.amdf$Freq=="0","1","0")
# Grafo da matriz de adjacência 
market.top5genre.g <- graph_from_incidence_matrix(market.top5genre.am)
market.top5genre.projec <- bipartite.projection(market.top5genre.g)

# Projections (1 - Market, 2 - Genre) ############################
# Projection 1
edges.projec1 <- E(market.top5genre.projec$proj1)       # 459 
vertices.projec1 <- V(market.top5genre.projec$proj1)     # 56

# Projection 2
edges.projec2 <- E(market.top5genre.projec$proj2)       # 310 
vertices.projec2 <- V(market.top5genre.projec$proj2)    # 101

# Checking - Comparing markets #################################
# Yep!
market.top5genre.edges <- as.data.frame(get.edgelist(market.top5genre.projec$proj1)); head(market.top5genre.edges)
a <- top5Genre.eachMk[top5Genre.eachMk$market=='br', ]; nrow(a) # 5
b <- top5Genre.eachMk[top5Genre.eachMk$market=='pt', ]; nrow(b) # 5
c <- a[a$genre %in% b$genre,]; nrow(c)                          # 1

# Plot bipartite ###############################################
V(market.top5genre.g)$size <- 1
V(market.top5genre.g)$frame.color <- "white"
V(market.top5genre.g)$color <- colors[4]
#V(market.top5genre.g)$label <- NA
E(market.top5genre.g)$arrow.mode <- 0
plot(market.top5genre.g)

# Plot Projection 1 ################################################
V(market.top5genre.projec$proj1)$size <- 5
V(market.top5genre.projec$proj1)$frame.color <- "white"
V(market.top5genre.projec$proj1)$color <- colors[4]
V(market.top5genre.projec$proj1)$label <- NA
E(market.top5genre.projec$proj1)$arrow.mode <- 0
plot(market.top5genre.projec$proj1)

# Plot Projection 2 ################################################
V(market.top5genre.projec$proj2)$size <- 5
V(market.top5genre.projec$proj2)$frame.color <- "white"
V(market.top5genre.projec$proj2)$color <- colors[4]
V(market.top5genre.projec$proj2)$label <- NA
E(market.top5genre.projec$proj2)$arrow.mode <- 0
plot(market.top5genre.projec$proj2)

# Degree #######################################################
# The degree of a vertex is its most basic structural property, 
# the number of its adjacent edges.
# Projection 1
market.top5genre.deg <- degree(market.top5genre.projec$proj1, mode="all"); market.top5genre.deg
max(market.top5genre.deg); min(market.top5genre.deg); mean(market.top5genre.deg)
#plot(market.genre.projec$proj1, vertex.size=market.genre.deg*3)
market.top5genre.deg.dist <- degree_distribution(market.top5genre.projec$proj1, 
                                               cumulative=F, mode="all")

ggplot(as.data.frame(market.top5genre.deg.dist), 
       aes(x = seq(1:length(market.top5genre.deg.dist)), y = market.top5genre.deg.dist)) +
  geom_point(colour=colors[4],size = 2) +
  theme_minimal() +
  theme(panel.background = element_rect(colour = "grey50", size=0.3),
        legend.title=element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12)) +
  labs(
    subtitle=" ",
    y="Probabilidade", 
    x="Grau")

hist(market.top5genre.deg, breaks=1:vcount(market.top5genre.projec$proj1)-1, 
     main="Filtro 2",
     xlab="Nós", ylab="Frequência", col=colors[4],
     ylim=c(0, 35),
     xlim = c(0,57), xaxt='n'); axis(side = 1, at=seq(0,55,5))

# Projection 2
market.top5genre.deg2 <- degree(market.top5genre.projec$proj2, mode="all"); market.top5genre.deg2
max(market.top5genre.deg2); min(market.top5genre.deg2); mean(market.top5genre.deg2)

# Random Network ################################################
# Projection 1
gnp_proj1 <- sample_gnp(length(vertices.projec1), 16.1/(length(vertices.projec1)-1))
min.weight <- min(E(market.top5genre.projec$proj1)$weight)
max.weight <- max(E(market.top5genre.projec$proj1)$weight)
E(gnp_proj1)$weight <- sample(min.weight:max.weight, length(E(gnp_proj1)), replace=T)
degree(gnp_proj1); mean(degree(gnp_proj1)) # 16.21429
degree_distribution(gnp_proj1)

plot(x=0:max(degree(gnp_proj1)), y=degree_distribution(gnp_proj1), pch=19, 
     cex=1.2, col=colors[4], xlab="Grau", ylab="Frequência Acumulada",
     xaxt='n'); axis(side = 1, at=seq(0,25,5))

# Projection 2
gnp_proj2 <- sample_gnp(length(vertices.projec2), 6.1/(length(vertices.projec2)-1))
min.weight <- min(E(market.top5genre.projec$proj2)$weight)
max.weight <- max(E(market.top5genre.projec$proj2)$weight)
E(gnp_proj2)$weight <- sample(min.weight:max.weight, length(E(gnp_proj2)), replace=T)
degree(gnp_proj2); mean(degree(gnp_proj2)) # 6.07

# Diameter #######################################################
# The diameter of a graph is the length of the longest geodesic.
# Projection 1
market.top5genre.dmt <- diameter(market.top5genre.projec$proj1, directed = F, 
                        weights = E(market.top5genre.projec$proj1)$weight); market.top5genre.dmt # 5
gnp.dmt.projec1 <- diameter(gnp_proj1, directed = F, weights = E(gnp_proj1)$weight); gnp.dmt.projec1 # 5

# Projection 2
market.top5genre.dmt2 <- diameter(market.top5genre.projec$proj2, directed = F, 
                         weights = E(market.top5genre.projec$proj2)$weight); market.top5genre.dmt2 # 7
gnp.dmt.projec2 <- diameter(gnp_proj2, directed = F, weights = E(gnp_proj2)$weight); gnp.dmt.projec2 # 41


# Density  #######################################################
# The ratio of the number of edges and the number of possible edges.
# Projection 1
market.top5genre.dst <- edge_density(market.top5genre.projec$proj1); market.top5genre.dst # 0.294
gnp.dst.projec1 <- edge_density(gnp_proj1); gnp.dst.projec1                               # 0.298

# Projection 2
market.top5genre.dst2 <- edge_density(market.top5genre.projec$proj2); market.top5genre.dst2 # 0.06
gnp.dst.projec2 <- edge_density(gnp_proj2); gnp.dst.projec2                                 # 0.06

# Transitivity #########################################
# Projection 1
market.top5genre.trans <- transitivity(market.top5genre.projec$proj1, 
                                       type="global"); market.top5genre.trans # 0.79
gnp.trans.projec1 <- transitivity(gnp_proj1, type="global"); gnp.trans.projec1 # 0.27

# Projection 1
market.top5genre.trans2 <- transitivity(market.top5genre.projec$proj2, 
                                       type="global"); market.top5genre.trans2 # 0.43
gnp.trans.projec2 <- transitivity(gnp_proj2, type="global"); gnp.trans.projec2 # 0.07


# Distance ##############################################
# Projection 1
distances(market.top5genre.projec$proj1, v = V(market.top5genre.projec$proj1), 
          to = V(market.top5genre.projec$proj1), 
          mode = "all", weights = E(market.top5genre.projec$proj1)$weight, 
          algorithm = "automatic")

market.top5genre.mdist <- mean_distance(market.top5genre.projec$proj1, directed=F) # 1.766
gnp.mdist.projec1 <- mean_distance(gnp_proj1, directed = F)                        # 1.70

# Projection 2
market.top5genre.mdist2 <- mean_distance(market.top5genre.projec$proj2, directed=F) # 2.6
gnp.mdist.projec2 <- mean_distance(gnp_proj2, directed = F)                         # 2.7

# Components  ####################################################
# Projection 1
components(market.top5genre.projec$proj1)$no # 6

# Projection 2
components(market.top5genre.projec$proj2)$no # 6

# Communities  ####################################################
new_cols <- c("white", "red", "black")[membership(wc)]

#market.genre.comunities <- cluster_edge_betweenness(market.top5genre.projec$proj1)
market.genre.comunities <- cluster_louvain(market.top5genre.projec$proj1, 
                                           weights = E(market.top5genre.projec$proj1)$weight)
#plot_dendrogram(market.genre.comunities, mode = 'dendrogram')
#plot(market.genre.comunities, market.top5genre.projec$proj1)

colors.c <- brewer.pal(10, "BuGn")[membership(market.genre.comunities)]
V(market.top5genre.projec$proj1)$community <- market.genre.comunities$membership
par(mar=c(0,0,0,0))
plot(market.genre.comunities, 
     market.top5genre.projec$proj1, 
     vertex.size = 6, col = colors[4],
     mark.border = '#143662',
     #edge.color = colors[2],
     mark.col='white',
     #vertex.label = NA,
     vertex.label.family = "sans", 
     vertex.label.cex=.7
     )

# Total of tracks, artists e genres (each country) ######################################

colnames(top200complete)

music.eachMk <- top200complete[,c(2,4)]
music.eachMk <- data.frame(table(music.eachMk$mk))
music.eachMk <- music.eachMk[with(music.eachMk, order(Var1)),]; head(music.eachMk)
music.eachMk['tipo'] <- as.factor('Músicas')

artist.eachMk <- top200complete[,c(2,1)]
artist.eachMk <- unique(artist.eachMk)
artist.eachMk <- data.frame(table(artist.eachMk$mk))
artist.eachMk <- artist.eachMk[with(artist.eachMk, order(Var1)),]; head(artist.eachMk)
artist.eachMk['tipo'] <- as.factor('Artistas')

genre.eachMk <- unique(market.genre)
genre.eachMk <- data.frame(table(genre.eachMk$market))
genre.eachMk <- genre.eachMk[with(genre.eachMk, order(Var1)),]; head(genre.eachMk)
genre.eachMk['tipo'] <- as.factor('Gêneros')

eachMk.merged <- rbind(music.eachMk, artist.eachMk, genre.eachMk)
colnames(eachMk.merged) <- c('mk', 'freq', 'tipo')
eachMk.merged <- merge(eachMk.merged, countries, by='mk')

eachMk.Plot <- 
  ggplot(eachMk.merged, 
         aes(market, freq, fill=tipo, colour=tipo),
         na.rm = TRUE) + 
  geom_bar(stat= 'identity', position = 'stack') +
  theme_minimal() +
  scale_colour_manual(values=colors[c(1,4,3)]) +
  scale_fill_manual(values=colors[c(1,4,3)]) +
  theme(panel.background = element_rect(colour = "grey20", size=0.3),
        legend.title=element_blank(),
        legend.position="top",
        legend.text = element_text(size = 12),
       
        axis.text=element_text(size=11),
        axis.title=element_text(size=12)) + 
  labs(subtitle=" ",
    y="Quantidade", 
    x="Países") + scale_y_continuous(breaks = seq(0, 500, by = 50)) +
  theme(axis.text.x=element_text(angle=60, hjust=1, size=10))

# Ranking Genres ############################################

# Closeness
closeness.genre <- data.frame(closeness(market.top5genre.projec$proj2, 
                             vids = V(market.top5genre.projec$proj2), 
                             mode = 'all',
                             weights = E(market.top5genre.projec$proj2)$weight, 
                             normalized = TRUE))
closeness.genre <- cbind(V(market.top5genre.projec$proj2)$name, closeness.genre)
colnames(closeness.genre) <- c('genre', 'closeness')
closeness.genre <- closeness.genre[with(closeness.genre, order(-closeness)), ]

# Betweeness
betweeness.genre <- data.frame(betweenness(market.top5genre.projec$proj2, 
                                        weights = E(market.top5genre.projec$proj2)$weight, 
                                        normalized = TRUE, directed = FALSE))
betweeness.genre <- cbind(V(market.top5genre.projec$proj2)$name, betweeness.genre)
colnames(betweeness.genre) <- c('genre', 'betweeness')
betweeness.genre <- betweeness.genre[with(betweeness.genre, order(-betweeness)), ]

# Degree
degree.genre <- data.frame(degree(market.top5genre.projec$proj2))
degree.genre <- cbind(V(market.top5genre.projec$proj2)$name, degree.genre)
colnames(degree.genre) <- c('genre', 'degree')
degree.genre <- degree.genre[with(degree.genre, order(-degree)), ]

# Strength
strength.genre <- data.frame(strength(market.top5genre.projec$proj2, 
                                      vids = V(market.top5genre.projec$proj2), 
                                      mode = "all",
                                      loops = TRUE,
                                      weights = E(market.top5genre.projec$proj2)$weight))
strength.genre <- cbind(V(market.top5genre.projec$proj2)$name, strength.genre)
colnames(strength.genre) <- c('genre', 'strength')
strength.genre <- strength.genre[with(strength.genre, order(-strength)), ]

rank.genre <- data.frame(degree = degree.genre$genre[1:10],
                         strength = strength.genre$genre[1:10],
                         closeness = closeness.genre$genre[1:10],
                         betweenness = betweeness.genre$genre[1:10])

layout(matrix(1:4, 2, 2, byrow = TRUE))
par(mar=c(0,2,2,0))
plot(market.top5genre.projec$proj2, 
     main="Degree", vertex.size=V(market.top5genre.projec$proj2)$size*strength.genre/10)

# How many genres are original? #############################

origin_genre <- read.csv(paste0(dir,'origin_genre.csv'))
unique(origin_genre$mk) # 99
unique(droplevels(top5Genre.eachMk$market))
origin_genre <- merge(origin_genre, countries, by='market')
a <- origin_genre[,c(2,3)]
b <- top5Genre.eachMk[,c(2,1)]
colnames(a) <- colnames(b) <- c('genre', 'mk')
equal <- inner_join(a, b)
equal <- aggregate(genre ~ mk, data = equal, FUN = function(x){NROW(x)})
b <- aggregate(genre ~ mk, data = b, FUN = function(x){NROW(x)})
c <- merge(equal, b, by='mk')
c <- rbind(c, data.frame(mk = b$mk[which(!(b$mk %in% c$mk))], genre.x = 0, genre.y = 5))
c['percent'] <- (c$genre.x/c$genre.y)*100
c <- merge(c, countries, by = 'mk')

how.many <- 
  ggplot(c, 
         aes(market, percent),
         na.rm = TRUE) + 
  geom_bar(stat= 'identity', fill = colors[4]) +
  theme_minimal() +
  scale_colour_manual(values=colors[4]) +
  scale_fill_manual(values=colors[4]) +
  theme(panel.background = element_rect(colour = "grey20", size=0.3),
        legend.title=element_blank(),
        legend.position="top",
        legend.text = element_text(size = 12),
        
        axis.text=element_text(size=11),
        axis.title=element_text(size=12)) + 
  labs(subtitle=" ",
       y="Porcentagem de origem", 
       x="Países") + scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  theme(axis.text.x=element_text(angle=60, hjust=1, size=10))

# Heat Map ######################################################

# Plot heat map - genre similarity between markets ##########
market.genre.edges <- as.data.frame(get.edgelist(market.top5genre.projec$proj1)); head(market.genre.edges)

# Pegar os qntds de gêneros de cada país 
market.genre.edges <- cbind(market.genre.edges, E(market.top5genre.projec$proj1)$weight)
colnames(market.genre.edges) <- c('market', 'to', 'w.sim'); head(market.genre.edges)

#market.genre.edges <- merge(market.genre.edges, genre.eachMk, by='market')
#colnames(market.genre.edges) <- c('from', 'market', 'w.sim'); head(market.genre.edges)

#market.genre.edges <- merge(market.genre.edges, genre.eachMk, by='market')
#colnames(market.genre.edges) <- c('from', 'to', 'w.sim', 'w.to', 'w.from')
#Calcula porcentagem de similaridade
#market.genre.edges['simPercent'] <- market.genre.edges$w.sim/(market.genre.edges$w.to + market.genre.edges$w.from)
# Make a new data.frame, simply reversing A and B
market.genre.edges.oh <- data.frame(to = market.genre.edges$market,
                                    from = market.genre.edges$to,
                                    sim = market.genre.edges$w.sim)
# Here's the diagonal
diagonal <- data.frame(to = unique(market.genre.edges$to),
                       from = unique(market.genre.edges$to),
                       sim = 1)
# Mash 'em all together
full <- rbind(market.genre.edges, market.genre.edges.oh); head(full)

# Pegar nome completo dos países
colnames(full) <- c('mk', 'from'); head(full)
full <- merge(full, countries, by='mk')

colnames(full) <- c('t', 'mk', 'sim', 'to'); head(full)
full <- merge(full, countries, by='mk')

colnames(full) <- c('f', 't', 'sim', 'from', 'to'); head(full)
full <- full[,c(-1,-2)]

heatMap.mg <- 
  ggplot(data = full, aes(x = to, y = from)) +
  geom_tile(aes(fill = sim)) +
  scale_fill_gradient(low = colors[4], high = colors[1], name = "Similarity"
  ) +
  theme_minimal() + 
  labs(title="Similarity between countries based on musical genre",
       y="Countries", 
       x="Countries") +
  theme(axis.text.x=element_text(angle=60, hjust=1, size=10),
        axis.text.y=element_text(size=8))


#######

market.top5genre.deg.dist2 <- degree_distribution(market.top5genre.projec$proj2, 
                                                 cumulative=F, mode="all")

plot(x=0:max(degree(market.top5genre.projec$proj2)), y=market.top5genre.deg.dist2, pch=19, 
     cex=1.2, col=colors[4], xlab="Grau", ylab="Frequência Acumulada",
     xaxt='n'); axis(side = 1, at=seq(0,35,5))

hist(market.top5genre.deg, breaks=1:vcount(market.top5genre.projec$proj1)-1, 
     main="Filtro 2",
     xlab="Nós", ylab="Frequência", col=colors[4],
     ylim=c(0, 35),
     xlim = c(0,57), xaxt='n'); axis(side = 1, at=seq(0,55,5))

