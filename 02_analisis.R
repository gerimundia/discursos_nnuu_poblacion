rm(list=ls())
gc()

pacman::p_load(dplyr, tidytext, tm, tidyverse, stringi, stringr, data.table,
               ggplot2, igraph, ggraph, tidygraph, ggwordcloud)


# Datos ----
load("discursos_nnuu.RData")

# Palabras vacias ----

pvacias1 <- tibble(word = tm::stopwords('en'), lexicon = 'tm')
head(pvacias1)

data(stop_words)
pvacias2 <- stop_words

pvacias3 <- data.frame(word=c("united", "nations", "assembly"), lexicon="propio")

pvacias <- rbind(pvacias1, pvacias2, pvacias3)
pvacias <- as.data.frame(unique(pvacias$word))
names(pvacias) <- c("word")
rm(pvacias1, pvacias2, pvacias3, stop_words)
gc()

# Tokenizando ----
discursos$id <- c(1:nrow(discursos))
texto <- discursos[, c("id", "texto")]

corpus_0 <- texto %>%
  unnest_tokens(palabra, texto)

# Limpieza y normalizacion ----
corpus_0$palabra <- str_trim(corpus_0$palabra)

corpus_0 <- corpus_0 %>%
  anti_join(pvacias, by = c("palabra" = "word"))

corpus_0 <-  corpus_0 %>% 
  filter(!grepl('null', palabra))
 
# Eliminando palabras con frecuencias iguales o inferiores a 20
pp_remover <- corpus_0 %>%  
  count(palabra, sort=T) %>% 
  filter(n<=20)

# Anio y pais ----
corpus <- corpus_0 %>% 
  left_join(discursos, by = "id") %>% 
  select(-c("texto", "archivo"))


# Ajuste a plurales ----
corpus <- data.table(corpus)

corpus[palabra=="country", palabra:="countries"]
corpus[palabra=="region", palabra:="regions"]
corpus[palabra=="live", palabra:="lives"]


# Bigramas ----
es_numerico <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

corpus_1 <- corpus %>% 
  nest(data = palabra) %>% 
  mutate(texto = map(data,unlist), 
         texto = map_chr(texto, paste, collapse = " ")) %>% 
  select(-data)

corpus_1$texto <- str_trim(corpus_1$texto)

bigramas <- corpus_1 %>% 
  unnest_tokens(bigrama, texto, token='ngrams', n=2) %>% 
  filter(!is.na(bigrama)) 

bigramas2 <- bigramas %>% 
  separate(bigrama, c("p1", "p2"), sep=" ")

bigramas2$decada <- floor(bigramas2$anio / 10) * 10

freqbigramas <- bigramas2 %>% 
  group_by(decada) %>% 
  count(p1, p2,sort=T)

freqbigramas <- freqbigramas %>%
  filter(!(es_numerico(p1) & es_numerico(p2)))

freqbigramas <- freqbigramas %>%  
  filter(n>=2)

# Trigramas ----

trigramas <- corpus_1 %>% 
  unnest_tokens(trigrama, texto, token='ngrams', n=3) %>% 
  filter(!is.na(trigrama)) 

trigramas2 <-  trigramas %>% 
  separate(trigrama, c("p1", "p2", "p3"), sep=" ")

trigramas2$decada <- floor(trigramas2$anio / 10) * 10

freqtrigramas <- trigramas2 %>% 
  group_by(decada) %>% 
  count(p1, p2, p3, sort=T)

freqtrigramas <- freqtrigramas %>%
  filter(!(es_numerico(p1) & es_numerico(p2) & es_numerico(p3)))

freqtrigramas <- freqtrigramas %>%
  filter(!(es_numerico(p1) & es_numerico(p2)))

freqtrigramas <- freqtrigramas %>%
  filter(!(es_numerico(p2) & es_numerico(p3)))

freqtrigramas <- freqtrigramas %>%  
  filter(n>=2)


# Guardar ----
save(corpus, bigramas, trigramas,
     freqbigramas, freqtrigramas, 
     file="corpus.RData")

# Visualizacion

pop_bigrama2 <- freqbigramas %>% 
  filter(p1=="population") %>% 
  filter(!(es_numerico(p2))) %>% 
  filter(decada>=1950) %>% 
  group_by(decada) %>%
  arrange(desc(n)) %>%   
  ungroup()

# Palabras adicionales a remover
ppremover <- c("including", "held", "continue", "countries")

pop_bigrama2 <- pop_bigrama2 %>% 
  filter(!(p2 %in% ppremover))

pop_bigrama_top <- pop_bigrama2 %>%
  group_by(decada) %>%
  mutate(n_escalado = 100 * n / max(n, na.rm = TRUE),
         rank = rank(-n, ties.method = "first"),
         color = ifelse(rank<=5, "top5", 
                 ifelse((rank>=6 & rank<=10), "top10", "resto"))) %>%
  ungroup()

set.seed(123)
ggplot(pop_bigrama_top, 
       aes(label=p2, size=n_escalado, color=color)) +
  geom_text_wordcloud_area(rm_outside=T) +
  scale_size_area(max_size = 15) +
  scale_color_manual(values = c("top5" = "darkred", 
                                "top10" = "firebrick1",
                                "rest" = "grey60")) +
  facet_wrap(~decada, ncol = 2) +
  theme_minimal()

ggsave(last_plot(), filename = "grafico.jpg", units = "cm", 
       width=14, height=16, dpi = 500)

