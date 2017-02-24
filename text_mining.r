library('stringr')
library('readr')
library('wordcloud')
library('tm')
library('SnowballC')
library('RSentiment')
library('data.table')
library('DT')
library('ggplot2')

debate_f <- read.csv("/Users/pranavnavandar/Documents/Study Material/BAR/debate_x.csv",stringsAsFactors = F)
debate_f$Speaker <- as.factor(debate_f$Speaker)
debate_f <- as.data.table(debate_f)

interventions <- debate_f[,.N,by=Speaker][order(-N)]
ggplot(interventions,aes(x = Speaker,y = N, fill=Speaker))+
geom_bar(stat = "identity") +
ggtitle("Number of Interventions")

trump <- debate_f$Text[debate_f$Speaker=="Trump"]
corpus = Corpus(VectorSource(list(trump)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english’))
corpus = tm_map(corpus, stemDocument)

dtm_trump = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_trump <- colSums(as.matrix(dtm_trump))

sentiments_trump = calculate_sentiment(names(freq_trump))
sentiments_trump = cbind(sentiments_trump, as.data.frame(freq_trump))
sent_pos_trump = sentiments_trump[sentiments_trump$sentiment == 'Positive',]
sent_neg_trump = sentiments_trump[sentiments_trump$sentiment == 'Negative',]

asd<-matrix(c(sum(sent_neg_trump$freq_trump),sum(sent_pos_trump$freq_trump)),ncol=2,byrow=TRUE)
colnames(asd)<-c("negative","positive")
barplot(asd)

m<-as.matrix(dtm_trump)
v <- sort(rowSums(m), decreasing=TRUE)
print(v)
myNames <- names(v)
print(myNames)
k <- which(names(v)=="child")
d <- data.frame(word=myNames, freq=v)
print(d)
wordcloud(d$word, d$freq,min.freq=3, colors= brewer.pal(5,"Set1"))

clinton_f <- debate_f$Text[debate_f$Speaker=="Clinton"]
corpus_f = Corpus(VectorSource(list(clinton_f)))
corpus_f = tm_map(corpus_f, removePunctuation)
corpus_f = tm_map(corpus_f, content_transformer(tolower))
corpus_f = tm_map(corpus_f, removeNumbers) 
corpus_f = tm_map(corpus_f, stripWhitespace)
corpus_f = tm_map(corpus_f, removeWords, stopwords('english'))
corpus_f = tm_map(corpus_f, stemDocument)

dtm_clinton_f = DocumentTermMatrix(VCorpus(VectorSource(corpus_f[[1]]$content)))

row.has.na <- apply(dtm_clinton_f, 1, function(x){any(is.na(x))})
sum(row.has.na)
dtm_clinton_f.filtered <- dtm_clinton_f[!row.has.na,]

freq_clinton_f <- colSums(as.matrix(dtm_clinton_f.filtered)) 

sentiments_clinton_f = calculate_sentiment(names(freq_clinton_f))
sentiments_clinton_f = cbind(sentiments_clinton_f, as.data.frame(freq_clinton_f))
sent_pos_clinton_f = sentiments_clinton_f[sentiments_clinton_f$sentiment == 'Positive',]
sent_neg_clinton_f = sentiments_clinton_f[sentiments_clinton_f$sentiment == 'Negative',]

Clint_graph<- matrix(c(sum(sent_neg_clinton_f$freq_clinton_f),sum(sent_pos_clinton_f$freq_clinton_f)),ncol = 2,byrow = true)
colnames(Clint_graph)<-c("Negative","Postive")
barplot(Clint_graph)

q<-as.matrix(dtm_clinton_f)
w <- sort(rowSums(m), decreasing=TRUE)
print(w)
myNames <- names(w)
print(myNames)
r <- which(names(w)=="child")
u <- data.frame(word=myNames, freq=w)
print(u)
wordcloud(d$word, d$freq,min.freq=3, colors= brewer.pal(5,"Set1"))

audience <- factor(debate_f$Text[debate_f$Speaker=="Audience"])
qplot(audience,fill=audience)+
  ggtitle("Audience Reactions")+
  ylab("Interventions")+xlab("Reaction")


tokenize_ngrams <- function(x, n=2) {  return(textcnt(x,method="string",n=n,decreasing=TRUE))}

bigram_vit <- function(corpus){
  sample_df <- data.frame(text=unlist(sapply(corpus, '[',"content")),stringsAsFactors=F)
  bigrams_df <- tokenize_ngrams(sample_df,n=2) 
  # bigrams_df <- sample_df %>% tokenize_ngrams(n=2) 
  bigrams_df <- data.frame(word = rownames(as.data.frame(unclass(bigrams_df))),
                           freq = unclass(bigrams_df))
  return(bigrams_df)
}

#For Clinton
corpus1 <- corpus_f
bigram_clin <- bigram_vit(corpus1)

#For Trump
corpus2 <- corpus
bigram_trum <- bigram_vit(corpus2)

plot_barplot(bigram_clin[1:10,],"Clinton")

colnames(bigram_clin)[2] <-"Clinton"
colnames(bigram_trum)[2] <-"Trump"
all_m <- merge(bigram_clin,bigram_trum,all = T)
all_m[is.na(all_m)]<-0
rownames(all_m) <- all_m$word
all_m <- all_m[,2:3]

#Subset shared terms
common_words <- subset(all_m,all_m[, 1] > 0 & all_m[, 2] > 0)
# Find most commonly shared words
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = TRUE), ]
top25_df <- data.frame(x = common_words[1:25, 1],
                       y = common_words[1:25, 2],
                       labels = rownames(common_words[1:25, ]))


pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels,
             main = "Words in Common",
             gap = 8, laxlab = 0:18,
             raxlab = 0:18, unit = NULL,
             top.labels = c("Clinton",
                            "Words",
                            "Trump"))

plot_barplot <- function(bigram,tit){
  # Create a barplot
  bigram$word <- factor(bigram$word, levels = bigram$word)
  ggplot(bigram,aes(x= reorder(word,freq), y= freq,fill= freq))+
    geom_bar(stat="identity")+coord_flip() + ggtitle(tit)+
    theme(axis.title.y = element_blank())
}
