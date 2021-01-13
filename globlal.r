################################################################
# Carga librerias

## librerias utilizadas
load.lib<-c("easypackages","tm","zoom","stringr","ldatuning",
            "parallel","SnowballC","koRpus","wordcloud","topicmodels",
            "tokenizers","reshape2","ggplot2","newsanchor","readxl",
            "dplyr","tidyr","plotly","hrbrthemes","lubridate","TTR","ggfortify","shiny",
            "forecast","tseries","normtest","lmtest","TSA","urca","TSstudio","textstem","tidyr"
            ,"dplyr","tidytext")
print("<<<< Validación librerias instaladas >>>>")
install.lib<-load.lib[!load.lib %in% installed.packages()]
# install necessary packages
for(lib in install.lib) install.packages(lib,dependencies=TRUE, verbose=F)
sapply(load.lib,require,character=TRUE)

print(">>>>>cargando librerias")
library("easypackages")
libraries(load.lib)




## Finaliza instalación y carga de librerias
##################################################################

## set API KEY

api_key   = Sys.getenv()

Sys.setenv(api_key = "c703e5d02dc74cdaab1994c3c4451c83")
a<-Sys.getenv("api_key")


## Carga de DATOS desde el API

dfOxf <- get_everything_all(query = "ChAdOx1", language = "en",api_key= a)

dfAD5 <- get_everything_all(query = "AD5-nCoV", language = "en",api_key= a)


dfOxf$results_df$title




length(dfOxf$results_df$title)

dfOxf$results_df$description

dfOxf$results_df$published_at
dfOxf$results_df$name

dfOxfDATA <- paste(dfOxf$results_df$title,dfOxf$results_df$description)
dfAD5DATA <- paste(dfAD5$results_df$title,dfAD5$results_df$description)


## Guardar en archivos para CORPUS

## indice vacuna Rusa

j <- length(dfOxfDATA)+1

  for(i in 1:length(dfOxfDATA))
  {
  write.table(dfOxfDATA[i],paste("data/",i,dfOxf$results_df$name[[i]],".txt"))
  
  }


for(i in 1:length(dfAD5DATA))
{
  write.table(dfAD5DATA[i],paste("data/",j,dfAD5$results_df$name[[i]],".txt"))
  j <- j+1
}

docsList <- c(seq(1,86, by=1))


## Carga de archivos en Corpus

text <- Corpus(DirSource("data/",encoding="UTF-8" ), 
               readerControl = list(reader = readPlain, language = "english"))

## text

## Funcion elimina brackets



bracketXtract <-function(txt){
  txt1<-gsub("\\(.*\\)", "", txt)
  txt2<-gsub("\\{.*\\}", "", txt1)
  txt3<-gsub("\\[.*\\]", "", txt2)
  return(txt3)
}



## Función limpieza de texto

fn_text_clean <- function(corpus_txt)
{
  for(i in 1:length(corpus_txt))
  {
    corpus_txt[[i]]$content <-bracketXtract(corpus_txt[[i]]$content)
    corpus_txt[[i]]$content<-str_replace_all(corpus_txt[[i]]$content,"\016"," ")
    corpus_txt[[i]]$content <-gsub("[^a-zA-Z0-9]","  ", corpus_txt[[i]]$content)
  }
  
  # remover stop word usando tm 
  corpus_txt <- tm_map(corpus_txt, removeWords, stopwords('english'))
  
  # cambiar a minusculas las palabras
  corpus_txt <- tm_map(corpus_txt, content_transformer(tolower))
  
  # remover numero
  corpus_txt <- tm_map(corpus_txt, removeNumbers)
  # stop words personalizadas
  stopwords_C <- read.csv("dic/stop-word-list.csv", header = FALSE)
  stopwords_C <- as.character(stopwords_C$V1)
  #remover stop words personañizadas
  corpus_txt <- tm_map(corpus_txt, removeWords, stopwords_C) 
  #remove espacios en blano extras
  corpus_txt <- tm_map(corpus_txt, stripWhitespace)
  
  return(corpus_txt)
}

## lematización

steam_lema <- 
  function(txt){
    for(i in 1:length(txt))
    {
      words <- tokenize_words(txt[[i]]$content)
      y<-stem_words(words[[1]],lenguage="english")
      y<-lemmatize_words(y,dictionary = lexicon::hash_lemmas)
      txt[[i]]$content<-paste(y, collapse = ' ')
    }
    
    return(txt)
  }





textCln<-fn_text_clean(text)


textLemaSte<-steam_lema(textCln)


## Matriz Termino Documento

tdm <- TermDocumentMatrix(textLemaSte)

## Matriz Documento Termino

dtm <- DocumentTermMatrix(textLemaSte)

## Remocion terminos
dtm <- removeSparseTerms(dtm , 0.990)

sel_idx <- slam::row_sums(dtm) > 0

dtm <- dtm[sel_idx, ]

m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# head(d, 10)
# 
# pal <- colorRampPalette(colors = c("blue", "lightblue"))(20)
# 
# barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
#         col = pal,
#         #c("#E69F00", "#56B4E9", "#009E73")
#         main ="Top 10 Palabras mas Frecuentes",
#         ylab = "Frecuencia de Palabras")

## Word Cloud
set.seed(1234)
# wordCL <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8, "Dark2"))



## Numero tópicos

result <- FindTopicsNumber(
  dtm,
  topics = 
    seq(from = 6, to = 86, by =1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed=77), 
  mc.cores = 6,
  verbose = TRUE
)

## plot tópicos

#FindTopicsNumber_plot(result)


### Parametros para LDA

control_LDA_Gibbs <- list(alpha = 0.2, estimate.beta = TRUE, verbose = 0, prefix = tempfile(), 
                          save = 0, keep = 0, seed = as.integer(Sys.time()), nstart = 1, best = TRUE,
                          ##delta = 0.1, 
                          iter = 500)


topicModel <- LDA(dtm, 8, control= control_LDA_Gibbs, method = "Gibbs")

topicNames <- apply(terms(topicModel, 10), 2, paste, collapse = " ")

topicModel2 <- tidy(topicModel,matrix = "beta")

## topicos palabras
top_terms_10 <- topicModel2 %>%
  group_by(topic) %>%
  top_n(10,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

plot_topic_10 <- top_terms_10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()



#plot_topic_5

## tpicos documentos
exampleIds <- c(1,6,85)

tmResult <- posterior(topicModel)
theta <- tmResult$topics
beta <- tmResult$terms

topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(exampleIds)), variable.name = "topic", id.vars = "document")  



ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = length(exampleIds))





## -----------




