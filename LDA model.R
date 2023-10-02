#read the data----
dd <- read_excel("dataset.xls")

colSums(is.na(dd))
dd[is.na(dd)] <- "none"

library(quanteda)
corp <- corpus(dd$Anthropomorphism)
# Tokenize SMS text messages.
train.tokens <- tokens(dd$Anthropomorphism, what = "word", 
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)

# Take a look at a specific SMS message and see how it transforms.
train.tokens[[3]]

# Lower case the tokens.
train.tokens <- tokens_tolower(train.tokens)
train.tokens[[3]]

# Use quanteda's built-in stopword list for English.
# NOTE - You should always inspect stopword lists for applicability to
#        your problem/domain.
train.tokens <- tokens_select(train.tokens, stopwords(), 
                              selection = "remove")
train.tokens <- tokens_select(train.tokens, c("human","chatbot","bot","í","t"), 
                              selection = "remove")

train.tokens[[3]]


# Perform stemming on the tokens.
train.tokens <- tokens_wordstem(train.tokens, language = "english")
train.tokens[[3]]


# Create our first bag-of-words model.
dfm <- dfm(train.tokens, tolower = FALSE)
dfm = dfm_trim(dfm, min_docfreq = 2)

library(ldatuning)
result <- ldatuning::FindTopicsNumber(
  dfm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result)

library(topicmodels)
dtm = convert(dfm, to = "topicmodels") 
set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 5,  control = list(alpha = 0.1))
m


words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing = T), n=50)
head(topwords)
terms(m, 10)

library(wordcloud)
dev.new()
wordcloud(names(topwords),topwords)

topic = 5
topic.docs = posterior(m)$topics[, topic] 
topic.docs = sort(topic.docs, decreasing=T)
head(topic.docs)
topdoc = names(topic.docs)[1]
topdoc_corp = corp[docnames(corp) == topdoc]
texts(topdoc_corp)
topdoc = names(topic.docs)[2]
topdoc_corp = corp[docnames(corp) == topdoc]
texts(topdoc_corp)
topdoc = names(topic.docs)[3]
topdoc_corp = corp[docnames(corp) == topdoc]
texts(topdoc_corp)
topdoc = names(topic.docs)[4]
topdoc_corp = corp[docnames(corp) == topdoc]
texts(topdoc_corp)
topdoc = names(topic.docs)[5]
topdoc_corp = corp[docnames(corp) == topdoc]
texts(topdoc_corp)
topdoc = names(topic.docs)[6]
topdoc_corp = corp[docnames(corp) == topdoc]
texts(topdoc_corp)


library(LDAvis)   
dtm = dtm[slam::row_sums(dtm) > 0, ]
phi = as.matrix(posterior(m)$terms)
theta <- as.matrix(posterior(m)$topics)
vocab <- colnames(phi)
doc.length = slam::row_sums(dtm)
term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

json = createJSON(phi = phi, theta = theta, vocab = vocab,
                  doc.length = doc.length, term.frequency = term.freq)
serVis(json)
