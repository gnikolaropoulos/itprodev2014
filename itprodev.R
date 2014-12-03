install.packages(c("devtools", "rjson", "bit64", "httr", "ggplot2", "wordcloud","RColorBrewer","tm"))

library(devtools)
library(ggplot2)
library(wordcloud)


install_github("twitteR", username="geoffjentry")
library(twitteR)
library(tm)
library(RColorBrewer)

clean.text = function(x)
{
  # tolower
  #x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove control characters
  x = gsub('[[:cntrl:]]', '', x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    clean.text(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

setup_twitter_oauth("Fuon4OW3YSzKxm6WKRCJ3ZTyO", "uyihgk2oFLy81qtmDBY30OOPRuTOKpoQW1T1CxXy6cS0UPSUWS", "263075977-4nNeMdnJaWBGruV5wTYU4YXjfSI9yuQryGftxNpR", "1LTGqYHl7YRC9ZuQswNoG6432FKDot6nDCqzzFBU9mitK")
itpro.tweets = searchTwitteR('#Obama', n=1000)
itpro.text = lapply(itpro.tweets, function(t) t$getText())
positiveWords = scan('C:\Users\George\Dropbox\ITPRODEVConnections\positive-words.txt', what='character', comment.char=';')
negativeWords = scan('C:\Users\George\Dropbox\ITPRODEVConnections\negative-words.txt', what='character', comment.char=';')
itpro.scores = score.sentiment(itpro.text, positiveWords, negativeWords, .progress='text')
hist(itpro.scores$score)
qplot(itpro.scores$score)

itpro.tweets = searchTwitteR('#itprodev', n=1000)
itpro.sources = sapply(itpro.tweets, function(x) x$getStatusSource())
itpro.sources = gsub("</a>", "", itpro.sources)
itpro.sources = strsplit(itpro.sources, ">")
itpro.sources = sapply(itpro.sources, function(x) ifelse(length(x) > 1, x[2], x[1]))
source_table = table(itpro.sources)
pie(source_table[source_table > 20])

testTweets = searchTwitteR('#datamining', n=100) #userTimeline("vkalpias", n=1000)
df <- do.call("rbind", lapply(testTweets, as.data.frame))
myCorpus <- Corpus(VectorSource(df$text))
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopwords <- c(stopwords('english'), "available", "via", "and")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords, lazy=TRUE)
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
inspect(myDtm[266:270,31:40])
findFreqTerms(myDtm, lowfreq=3)
matrix = as.matrix(myDtm)
# calculate the frequency of words
freqs = sort(rowSums(matrix), decreasing=TRUE)
d <- data.frame(word=names(freqs), freq=freqs)
wordcloud(d$word, d$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))