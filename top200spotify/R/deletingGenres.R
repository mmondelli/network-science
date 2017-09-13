source('getData.R')

## Genre - Country (all genres) #####################################################

# Simplificar dataframe market.genre
# Pegar uma ocorrência de determinado gênero por país
# Assim é possível verificar em quantos países cada gênero aparece
market.genre.u <- unique(market.genre)
colnames(countries) <- c('market','name'); 

# Para pegar o nome dos países ao invés da sigla
#market.genre.u <- merge(x = market.genre.u, 
#      y = countries, by = "market")
#market.genre.u <- market.genre.u[,c(3,2)]; colnames(market.genre.u) <- c('market', 'genre')

market.genre.u <- na.omit(market.genre.u); head(market.genre.u)
length(unique(market.genre.u$genre)) ## 367

# Quantos países cada gênero alcança
eachGenre.total <- as.data.frame(table(market.genre.u$genre))
eachGenre.total <- eachGenre.total[with(eachGenre.total, order(-Freq)), ]; head(eachGenre.total, 10)
unique.freq <- unique(eachGenre.total$Freq) # 44

plots <- list()
qtd.deleted.generos <- c()
market.genre.trans <- c(); max.c <- c(); qtd.c <- c(); dens <- c();

# For - excluindo gêneros
for (i in 1:length(unique.freq)) {
  genre.cntAll <- eachGenre.total[eachGenre.total$Freq>=unique.freq[i],]
  market.genre.u <- market.genre.u[!(market.genre.u$genre %in% genre.cntAll$Var1),]
  qtd.deleted.generos <- c(qtd.deleted.generos, nrow(genre.cntAll))
  
  # Matriz de adjacência 57 linhas, 70 colunas
  market.genre.am <- xtabs(~ market + genre, market.genre.u)
  market.genre.amdf <- as.data.frame(market.genre.am) #market, genre, #freq
  # Grafo da matriz de adjacência 
  market.genre.g <- graph_from_incidence_matrix(market.genre.am, weighted = TRUE)
  
  # Projeção (1 - País, 2 - Gênero) ############################
  market.genre.projec <- bipartite.projection(market.genre.g)
  
  # Plot Projec 1 ################################################
  V(market.genre.projec$proj1)$size <- 8
  V(market.genre.projec$proj1)$frame.color <- "white"
  V(market.genre.projec$proj1)$color <- colors[4]
  V(market.genre.projec$proj1)$label <- V(market.genre.projec$proj1)$name
  E(market.genre.projec$proj1)$arrow.mode <- 0
  E(market.genre.projec$proj1)$color <- colors[1]
  margins=c(0,0)
  # Plot
  #plot.igraph(market.genre.projec$proj1)
  #p <- recordPlot()
  #plots[[i]] <- p
  
  # Metrics
  market.genre.trans <- c(market.genre.trans, 
                          transitivity(market.genre.projec$proj1, type="global"))
  
  # Extract the components
  market.genre.components <- clusters(market.genre.projec$proj1)
  # Which is the largest component
  max.c <- c(max.c, max(market.genre.components$csize))
  qtd.c <- c(qtd.c, market.genre.components$no)
  
  dens <- c(dens, edge_density(market.genre.projec$proj1, loops = FALSE))
}

# Plots 
# Clustering #####
market.genre.trans <- as.data.frame(market.genre.trans)
market.genre.trans['unique.freq'] <- unique.freq
colnames(market.genre.trans) <- c('trans', 'unique.freq')

market.genre.transPlot <- 
  ggplot(market.genre.trans, aes(y=trans, x=seq(1:44)), na.rm = TRUE) +
  geom_point(colour=colors[4],size = 2) +
  theme_minimal() +
    theme(panel.background = element_rect(colour = "grey50", size=0.3),
          legend.title=element_blank(),
          panel.grid.major = element_line(colour = "grey80"),
          axis.text=element_text(size=11),
          axis.title=element_text(size=12)) +
    labs(
      subtitle=" ",
      y="Clusterização", 
      x="Rede")
  
summary(market.genre.trans)

# Density #####
dens <- as.data.frame(dens)
dens['unique.freq'] <- unique.freq
colnames(dens) <- c('dens', 'unique.freq')

market.genre.transPlot <- 
  ggplot(dens, aes(y=dens, x=seq(1:44)), na.rm = TRUE) +
  geom_point(colour=colors[4],size = 2) +
  theme_minimal() +
  theme(panel.background = element_rect(colour = "grey50", size=0.3),
        legend.title=element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12)) +
  labs(
    subtitle=" ",
    y="Clusterização", 
    x="Rede")

# Porcentagem de gêneros deletados #####
qtd.deleted.generos <- as.data.frame(qtd.deleted.generos)
qtd.deleted.generos['unique.freq'] <- unique.freq
qtd.deleted.generos['percent'] <- (qtd.deleted.generos$qtd*100)/367
colnames(qtd.deleted.generos) <- c('qtd', 'unique.freq', 'percent')

qtd.deleted.generosPlot <- 
  ggplot(qtd.deleted.generos, aes(y=percent, x=seq(1:44)), na.rm = TRUE) +
  geom_line(colour=colors[4], size = 1) +
  theme_minimal() +
  theme(panel.background = element_rect(colour = "grey50", size=0.3),
        legend.title=element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12)) +
  labs(
    subtitle=" ",
    y="Gêneros excluídos", 
    x="Rede")

# Tamanho do maior componente #####
max.cPlot <- 
  ggplot(as.data.frame(max.c), aes(y=max.c, x=seq(1:44)), na.rm = TRUE) +
  geom_point(colour=colors[4], size = 2) +
  theme_minimal() +
  theme(panel.background = element_rect(colour = "grey50", size=0.3),
        legend.title=element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12)) +
  labs(
    subtitle=" ",
    y="Tamanho do maior componente", 
    x="Rede") + scale_y_continuous(breaks = seq(0, 70, by = 10))

# Quantidade de componentes e tamanho do maior componente em cada caso #####
qtd.c <- data.frame(x=seq(1:44), y=qtd.c, Componentes = as.factor('Quantidade'))
max.c <- data.frame(x=seq(1:44), y=max.c, Componentes = as.factor('Maior'))
df.merged <- rbind(qtd.c, max.c)

qtd.cPlot <- 
  ggplot(df.merged, 
         aes(x, y, fill=Componentes, colour=Componentes),
         na.rm = TRUE) +
  geom_point() +
  theme_minimal() +
  scale_colour_manual(values=colors[c(1,4)]) +
  theme(panel.background = element_rect(colour = "grey50", size=0.3),
        legend.title=element_blank(),
        legend.position="top",
        legend.text = element_text(size = 12),
        panel.grid.major = element_line(colour = "grey80"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12)) + 
  labs(
    subtitle=" ",
    y="Componente", 
    x="Rede") + scale_y_continuous(breaks = seq(0, 70, by = 10))

# Ng/N #########################################################
ngn <- data.frame(x=seq(1:44), y=max.c/57, tipo = as.factor('Ng/N'))
trans <- data.frame(x=seq(1:44), y=market.genre.trans$trans, tipo = as.factor('Clusterização'))
df.merged <- rbind(ngn, trans)

ngn.transPlot <- 
  ggplot(df.merged, 
         aes(x, y, fill=tipo, colour=tipo),
         na.rm = TRUE) +
  geom_point() +
  theme_minimal() +
  scale_colour_manual(values=colors[c(1,4)]) +
  theme(panel.background = element_rect(colour = "grey50", size=0.3),
        legend.title=element_blank(),
        legend.position="top",
        legend.text = element_text(size = 12),
        panel.grid.major = element_line(colour = "grey80"),
        axis.text=element_text(size=11),
        axis.title=element_text(size=12)) + 
  labs(
    subtitle=" ",
    y="", 
    x="Rede") 


######
library(gridExtra)

pdf("plots.pdf", onefile = TRUE)
multiplot(plotlist = plots, cols = 3)
dev.off()

