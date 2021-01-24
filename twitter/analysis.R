# Pacotes #####
library(rtweet)
library(ggplot2)
library(forcats)
library(dplyr)
library(tidytext)
library(stringr)
library(data.table)
library(tm)
library(wordcloud)
library(stringr)
library(ggraph)
library(igraph)
library(scales)

# Configurações para a coleta de dados do Twitter #####
# Os espaços indicados por <preencher> devem conter suas credenciais
# de acesso à API.
appname <- "<preencher>"
key <- "<preencher>"
secret <- "<preencher>"
access_token <- "<preencher>"
access_secret <- "<preencher>"

twitter_token <- create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret,
    access_token = access_token,
    access_secret = access_secret)

# Definição de uma paleta de cores para as visualizações
paleta_its <- c("#A3248B","#54318C","#255C8E","#00A6B7","#E1F1FD", "#8a8a8a")

# 1. Buscar tweets com a hashtag #VotoImpressoEm2022;
# Busca feita com a opção de coleta dos tweets mais recentes
tweets <- search_tweets(q = "#VotoImpressoEm2022", n = 100, type = "recent", parse = T)

# Rápida análise exploratória sobre os dados
# Quantos usuários únicos
usuarios <- unique(tweets$screen_name)
# Quantos tweets são retweets ou replies
total_retweets <- nrow(tweets[which(tweets$is_retweet == 'TRUE'),])
total_replies <- nrow(tweets[which(tweets$reply_to_user_id != '') ,])

# 2. Base organizada em variaveis;
# A busca anterior retorna uma tabela, separada por 90 colunas (atributos)
# que pode ser salva em arquivo para consulta/manipulação posterior
fwrite(tweets, file ="tweets_votoimpresso2022_100.csv")

# 3. Indicar os cinco perfis (@) que tuitaram a hashtag mais vezes;
# Contagem feita a partir do nome dos usuários, uma vez que cada linha
# da base de dados representa um tweet

top5users_tweets <- tweets %>%
                        count(screen_name) %>%
                        arrange(-n) %>%
                        top_n(5) %>%
                        mutate(screen_name = paste0("@", screen_name))
#Ajuste no dataframe para manter a ordenação no gráfico
top5users_tweets$screen_name <- factor(top5users_tweets$screen_name,
                                       levels = top5users_tweets$screen_name[order(top5users_tweets$n)])

# Visualização do resultado
ggplot(top5users_tweets, aes(x = screen_name, y = n)) +
    geom_bar(stat = "identity", fill = paleta_its[1]) +
    geom_text(aes(label = n), vjust = 0.5, hjust = -0.5,
              color = paleta_its[1], alpha = 0.7) +
    labs(title = "Top 5 usuários que mais mencionaram #VotoImpressoEm2022",
         subtitle = 'e o total de tweets com menções à hashtag',
         caption = paste0('Período: ',
                          format(as.POSIXlt(min(tweets$created_at)), "%d de %B de %Y"), " às ",
                          format(as.POSIXct(max(tweets$created_at)), "%H:%M")),
         x = "", y = "") +
    coord_flip() +
    theme_minimal()

# 4. Indicar os cinco perfis (@) que receberam mais curtidas seus tweets;
# Contagem feita também a partir do nome dos usuários, agregando pelo somatório de likes
# que cada tweet recebeu

top5users_favorite <- tweets %>%
                        group_by(screen_name) %>%
                        summarise(total_favorite = sum(favorite_count)) %>%
                        arrange(-total_favorite) %>%
                        top_n(5) %>% # Ajustar aqui para pegar só 5
                        mutate(screen_name = paste0("@", screen_name))
top5users_favorite$screen_name <- factor(top5users_favorite$screen_name,
                                       levels = top5users_favorite$screen_name[order(top5users_favorite$total_favorite)])

# Visualização do resultado
ggplot(top5users_favorite[1:5,], aes(x = screen_name, y = total_favorite)) +
    geom_bar(stat = "identity", fill = paleta_its[2]) +
    geom_text(aes(label = total_favorite), vjust = 0.5,
              hjust = -0.5,
              color = paleta_its[2], alpha = 0.7) +
    labs(title = "Top 5 usuários com mais tweets curtidos",
         subtitle = 'e o somatório de curtidas considerando os tweets que mencionam #VotoImpressoEm2022',
         caption = paste0('Período: ',
                          format(min(tweets$created_at), "%d de %B de %Y, de %H:%M"), " às ",
                          format(max(tweets$created_at), "%H:%M.")),
         x = "", y = "") +
    coord_flip() +
    theme_minimal()

# 5. Apresentar uma visualização gráfica da evolução desses tweets no tempo, indicando o dia com mais tweets contendo a hashtag;
# Aqui, a quantidade de tweets publicados é referente à intervalos de 10 minutos

tweets$bm <- cut(tweets$created_at, breaks = "10 min")
freq_table <- count(tweets, bm)
freq_table$bm <- format(strptime(freq_table$bm, "%Y-%m-%d %H:%M:%S"), format="%H:%M")
max_freq <- freq_table[which(freq_table$n == max(freq_table$n)),]

# Visualização do resultado
ggplot(freq_table, aes(x = bm, y = n, group = 1)) +
    geom_line(color = paleta_its[4], alpha = 0.5) +
    geom_point(color = paleta_its[4], alpha = 0.5) +
    geom_point(data = max_freq,
               aes(x = bm, y = n),
               color = paleta_its[1],
               size = 3) +
    labs(title = "Evolução de tweets no tempo",
         subtitle = paste0('contendo o total de publicações a cada intervalo de 10 minutos'),
         caption = paste0('Período: ',
                          format(min(tweets$created_at), "%d de %B de %Y, de %H:%M"), " às ",
                          format(max(tweets$created_at), "%H:%M.")),
         x = "", y = "") +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, max_freq$n + 1, by = 2)) +
    theme(axis.text.x = element_text(angle=45))

# 6. Apresentar uma visualização de rede (grafo) das relações entre esses usuários;
# Uma forma de pensar na relação entre usuários pode ser através de replies e retweets
# Para isso, é necessário ajustar os dados para definir essas relações

# Obter a relação  usuário - retweetou - usuário
retweet <- tweets[which(tweets$is_retweet == 'TRUE'),
                  c(1,4,54,55)]
retweet$type <- 'rtweet'

# Obter a relação  usuário - reply - usuário
reply <- tweets[which(tweets$reply_to_user_id != ''),
                c(1,4,9,10)]
reply$type <- 'reply'

# Ajustes para juntar as duas tabelas em uma só
colnames(retweet) <- colnames(reply) <- c('from_user_id', 'from',
                                          'to_user_id', 'to', 'type')
interacoes <- rbind(retweet, reply)
interacoes <- interacoes[,c(2,4,1,3,5)]

## Construção da rede
user_nodes <- unique(c(tweets$screen_name, interacoes$to)) # Obter a listagem de usuários únicos
net <- graph_from_data_frame(d = interacoes, vertices = user_nodes, directed = T) # Construir a rede
V(net)$color <- paleta_its[2] # Definir uma cor para os nós da rede

# Visualização da rede
ggraph(net, layout = 'kk') +
    geom_edge_fan(aes(colour = type), width = 0.4) +
    geom_node_point(color=V(net)$color, size = 3) +
    #geom_edge_link(aes(colour = type), width = 0.4,
    #               arrow = arrow(length = unit(2, 'mm'))) +
    theme_void() +
    theme(legend.position="top",
          legend.box = "horizontal",
          legend.title = element_blank()) +
    scale_edge_color_manual(values = paleta_its[c(1,4)]) +
    labs(title = "Interações entre usuários",
         subtitle = "considerando replies e retweets",
         caption = paste0('Período: ',
                          format(min(tweets$created_at), "%d de %B de %Y, de %H:%M"), " às ",
                          format(max(tweets$created_at), "%H:%M.")),
         x = "", y = "")

# Checando centralidade de grau
in_degree <- degree(net, mode = 'in') # Grau de entrada
in_degree <- data.frame(screen_name = names(in_degree), in_degree = in_degree, row.names=NULL)
out_degree <- degree(net, mode = 'out') # GRau de saída
out_degree <- data.frame(screen_name = names(out_degree), out_degree = out_degree, row.names=NULL)
all_degrees <- merge(in_degree, out_degree) # Combinando em uma única tabela

top3out_degree <- all_degrees %>%
    arrange(-out_degree) %>%
    top_n(3) %>%
    mutate(screen_name = paste0("@", screen_name))
top3out_degree$screen_name <- factor(top3out_degree$screen_name,
                                         levels = top3out_degree$screen_name[order(top3out_degree$out_degree)])

top3in_degree <- all_degrees %>%
    arrange(-in_degree) %>%
    top_n(3) %>%
    mutate(screen_name = paste0("@", screen_name))

# Visualização do resultado de out_degree
ggplot(top3out_degree, aes(x = screen_name, y = out_degree)) +
    geom_bar(stat = "identity", fill = paleta_its[3]) +
    geom_text(aes(label = out_degree), vjust = 0.5, hjust = -0.5,
              color = paleta_its[3], alpha = 0.7) +
    labs(title = "Top 3 usuários com maior centralidade de grau",
         subtitle = 'considerando as conexões que saem deles (out degree)',
         caption = paste0('Período: ',
                          format(min(tweets$created_at), "%d de %B de %Y, de %H:%M"), " às ",
                          format(max(tweets$created_at), "%H:%M.")),
         x = "", y = "") +
    coord_flip() +
    theme_minimal()

# 7. Apresentar uma visualização de quais outros temas ou hashtags estão relacionados a hashtag inicial;
# Para as hashtags, é feita uma contagem de quantas vezes aparecem junto à #VotoImpresso2022
outras_hashtags <- tweets %>%
    unnest_tokens(hashtags, text, "tweets", to_lower = FALSE) %>%
    filter(str_detect(hashtags, "^#"),
           hashtags != "#VotoImpressoEm2022") %>%
    count(hashtags, sort = TRUE)
outras_hashtags$hashtags <- factor(outras_hashtags$hashtags,
                                         levels = outras_hashtags$hashtags[order(outras_hashtags$n)])

# Visualização do resultado com as 15 mais frequentes
ggplot(outras_hashtags[1:15,], aes(x = hashtags, y = n)) +
    geom_bar(stat = "identity", fill = paleta_its[4]) +
    geom_text(aes(label = n), vjust = 0.5, hjust = -0.5,
              color = paleta_its[4], alpha = 0.7, size = 3) +
    labs(title = "Top 15 hashtags mencionadas juntamente com #VotoImpressoEm2022",
         subtitle = 'e o total de vezes em que aparecem',
         caption = paste0('Período: ',
                          format(min(tweets$created_at), "%d de %B de %Y, de %H:%M"), " às ",
                          format(max(tweets$created_at), "%H:%M.")),
         x = "", y = "") +
    coord_flip() +
    theme_minimal()

# Para verificar outros temas, é considerada a coluna 'text' que contém o texto do tweet
# Esses tweets compreendem então um corpus e é feita uma limpeza para retirar as hashtagsm
# menções à outros usuários, urls, emojis, números, pontuações e stopwords
text <- tweets$text
removeHashtag <- content_transformer(function(x) gsub("#([a-z|A-Z|0-9|_|á-ú])*",'', x, perl = F))
removeMencao <- content_transformer(function(x) gsub('@([a-z|A-Z|0-9|_])*', ' ', x, perl = F))
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", '', x, perl = T))
removeEmoji <- content_transformer(function(x) gsub('[^[:graph:]]', ' ', x, text, perl = F))
corpus <- Corpus(VectorSource(text))
f <- content_transformer(function(x) iconv(x, to = 'latin1', from = "UTF8", sub=''))
corpus <- tm_map(corpus, f)
corpus <- tm_map(corpus, removeEmoji)
corpus <- tm_map(corpus, removeURL)
corpus <- tm_map(corpus, removeMencao)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeHashtag)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords('pt'))
# Visualização do resultado em uma nuvem de palavras
wordcloud(corpus, min.freq = 2, max.words = 100, random.order = F, colors = paleta_its)
