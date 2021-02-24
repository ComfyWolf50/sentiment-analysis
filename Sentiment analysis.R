rm(list=ls())
require(tm)
require(quanteda)
require(ggplot2)
require(scales)
require(rvest)

defaultwd <- getwd()
time <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
wd <- paste0(defaultwd,"/","NewsVisuals_",time)
dir.create(wd)
setwd(wd)


# WEBSCRAPING

listTopics = function (pageURL, selectors) {
  page = read_html(pageURL)
  unlist(lapply(selectors, function(selector) {
    html_text(html_nodes(page, selector))
  }))
}

topics = list()

# Washington Post
pageURL = "https://www.washingtonpost.com/"
selectors = c("div.headline.x-large.normal-style.text-align-inherit a",
              "div.headline.x-small.normal-style.text-align-inherit a",
              "div.headline.xx-small.normal-style.text-align-inherit a")
topics = unlist(c(topics, listTopics(pageURL, selectors)))

# The Wall Street Journal
pageURL = "https://www.wsj.com/"
selectors = c("a.wsj-headline-link")
topics = unlist(c(topics, listTopics(pageURL, selectors)))

# USA TODAY
pageURL = "https://www.usatoday.com/"
selectors = c("p.hfwmm-primary-image-hed",
              "span.js-asset-headline.hfwmm-list-hed.hfwmm-secondary-hed.placeholder-hide",
              "span.js-asset-headline.hfwmm-list-hed.hfwmm-tertiary-hed.placeholder-hide",
              "p.hgpfm-image-hed.js-asset-headline.placeholder-hide")
topics = unlist(c(topics, lapply(listTopics(pageURL, selectors), function(topic) {
  gsub("\n +", "", topic)
})))

# Slate
pageURL = "https://slate.com/"
selectors = c("h3.story-teaser__headline",
              "h2.story-card__headline")
topics = unlist(c(topics, listTopics(pageURL, selectors)))

# The New Yorker
pageURL = "https://www.newyorker.com/"
selectors = c("h1.Hero__hed___3qGU4",
              "h4.River__hed___re6RP",
              "h3.Card__hed___3aD8c")
topics = unlist(c(topics, listTopics(pageURL, selectors)))

# POLITICO
pageURL = "https://www.politico.com/"
selectors = c("h1 a.js-tealium-tracking",
              "h3 a.js-tealium-tracking")
topics = unlist(c(topics, lapply(listTopics(pageURL, selectors), function(topic) {
  gsub("\r$", "", topic)
})))

# The New York Times
pageURL = "https://www.nytimes.com/"
selectors = c("section.top-news h1.story-heading a",
              "section.top-news h2.story-heading a")
topics = unlist(c(topics, lapply(listTopics(pageURL, selectors), function(topic) {
  gsub("\n +", "", gsub(" +$", "", topic))
})))

# Huffington Post
pageURL = "https://www.huffingtonpost.com/"
selectors = c("a.card__link.bn-card-headline.yr-card-headline",
              "a.card__link.bn-splash-headline.yr-card-headline")
topics = unlist(c(topics, lapply(listTopics(pageURL, selectors), function(topic) {
  gsub("^\n", "", gsub("\n$", "", topic))
})))

# NBC News
pageURL = "https://www.nbcnews.com/"
selectors = c("h3.item-heading.item-heading_overlay.item-heading_xlg.item-heading_lead.visible-xs-inline.visible-sm-inline.visible-md-block.visible-lg-block.is-breaking",
              "div.row.js-top-stories-content h3.item-heading.item-heading_md")
topics = unlist(c(topics, lapply(listTopics(pageURL, selectors), function(topic) {
  gsub("\n +", "", topic)
})))

# The Guardian
pageURL = "https://www.theguardian.com/"
selectors = c("a.u-faux-block-link__overlay.js-headline-text")
topics = unlist(c(topics, listTopics(pageURL, selectors)))


# Fox News
pageURL = "http://www.foxnews.com/"
selectors = c("div.collection.collection-spotlight h2.title a")
topics = unlist(c(topics, lapply(listTopics(pageURL, selectors), function(topic) {
  gsub("^\n", "", gsub(" +$", "", topic))
})))

# CBS News
pageURL = "https://www.cbsnews.com/"
selectors = c("div.module.module-listing h3.title",
              "li.item-full-lead h3.title")
topics = unlist(c(topics, listTopics(pageURL, selectors)))

# Bloomberg
pageURL = "https://www.bloomberg.com/"
selectors = c("a.hero-v6-story__headline-link",
              "a.highlights-v6-story__headline-link")
topics = unlist(c(topics, listTopics(pageURL, selectors)))

# ABC News
pageURL = "http://abcnews.go.com/"
selectors = c("article.hero.hero-two.row-item a.white-ln",
              "article.headlines.inbox.single.row-item a.black-ln")
topics = unlist(c(topics, lapply(listTopics(pageURL, selectors), function(topic) {
  gsub("^\n\t+", "", gsub("\n\t+$", "", topic))
})))

# BBC
pageURL = "http://www.bbc.com/"
selectors = c("section.module.module--promo.module--highlight a.media__link",
              "section.module.module--news.module--collapse-images a.media__link")
topics = unlist(c(topics, lapply(listTopics(pageURL, selectors), function(topic) {
  gsub("^\n +", "", gsub(" +$", "", topic))
})))

topicsAll = paste(topics, collapse = "\n")
write(topicsAll, file = paste0("data",time,".txt"))


# SENTIMENT ANALYSIS & FREQUENT WORDS

input_file <- readLines(paste0("data",time,".txt"), encoding="UTF-8")
# generating unigram and bigram from the text
unigram_dfm <- dfm(input_file,ngrams = 1, remove = c(stopwords("english"),"s","t"), stem = FALSE, remove_punct = TRUE)
polygram_dfm <- dfm(input_file,ngrams = 1:3, concatenator = " ", remove = c(stopwords("english"),"s","t"), stem = FALSE, remove_punct = TRUE)



# generating sentiment labeled dfm
# determining headlines' sentiment via data_dictionary_LSD2015
sentiment_dfm <- dfm(input_file, dictionary = data_dictionary_LSD2015, remove = c(stopwords("english")), stem = FALSE, remove_punct = TRUE)

# using clustering for dfm
cluster_dfm <- dfm_trim(unigram_dfm,min_count = 3, min_docfreq = 0.01)
clusters <- kmeans(tf(cluster_dfm, "prop"), 15)


# converting to dataframe
wdf_sentiment <- as.data.frame(sentiment_dfm)

# determining overall sentiment score of the headlines
wdf_sentiment$over <- wdf_sentiment$positive + wdf_sentiment$neg_negative - wdf_sentiment$negative - wdf_sentiment$neg_negative

# reattaching original headlines to the dataframe
wdf_sentiment$text <- removePunctuation(input_file)
wdf_sentiment_ordered <- wdf_sentiment[order(-wdf_sentiment$over,wdf_sentiment$text),]
wdf_sentiment_ordered = wdf_sentiment_ordered[!duplicated(wdf_sentiment_ordered$text),]


# ten headlines with the highest score
top_ten <- head(wdf_sentiment_ordered$text,10)

# ten headlines with the highest score
bottom_ten <- tail(wdf_sentiment_ordered$text,10)


# labeling headlines based on the score
wdf_sentiment$sentiment <- ifelse(wdf_sentiment$over > 0,"Positive",ifelse(wdf_sentiment$over < 0,"Negative","Neutral"))
wdf_sentiment$color <- (wdf_sentiment$over - min(wdf_sentiment$over)) / (max(wdf_sentiment$over) -min(wdf_sentiment$over))

# summarizing the dataframe
table_sentiment <-  as.data.frame(table(wdf_sentiment$sentiment))
names(table_sentiment) <- c("Sentiment","Count")
table_sentiment$Percentage <- round(100 * table_sentiment$Count / sum(table_sentiment$Count))

# GRAPHICS

# pie chart depicting share of news with different emotion
pdf(paste0("sentimental_pie_",time,".pdf"),width=5,height=3)
ggplot(table_sentiment, aes(x="", y=table_sentiment$Percentage, fill = Sentiment)) +
                   geom_bar(width = 1, stat = "identity") +
                   coord_polar("y", start=0) +
                   scale_fill_manual(values=c("red", "grey", "green"), labels = paste(table_sentiment$Sentiment, table_sentiment$Percentage,"%")) +
                   theme(axis.line=element_blank(),axis.text.x=element_blank(),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.x=element_blank(),plot.title = element_text(hjust = 0.5)) +
                   xlab("") +
                   ggtitle("Daily News by Sentiment")
  
dev.off()

  

# unigrams wordcloud
pdf(paste0("wordcloud_unigram_freq_",time,".pdf"),width=5,height=3)
wordcloud_unigram_freq<- textplot_wordcloud(unigram_dfm, min.freq = 6, random.order = FALSE,
                         rot.per = .25, 
                         colors = RColorBrewer::brewer.pal(8,"Dark2"))
dev.off()

# polygrams wordcloud
pdf(paste0("wordcloud_polygram_freq_",time,".pdf"),width=5,height=3)
wordcloud_polygram_freq<- textplot_wordcloud(polygram_dfm, min.freq = 5, random.order = FALSE,
                                            rot.per = .25, 
                                            colors = RColorBrewer::brewer.pal(8,"Dark2"))
dev.off()

# wordcloud colored by clusters
pdf(paste0("wordcloud_clusters_",time,".pdf"),width=5,height=3)
wordcloud_clusters <- textplot_wordcloud(cluster_dfm, min.freq = 2, random.order = FALSE,
                         rot.per = .1, 
                         colors = clusters$cluster)
dev.off()

# dataframe with most frequent words
most_freq_words <- as.data.frame(topfeatures(unigram_dfm, decreasing = TRUE,n = 10))
most_freq_dfr <- as.data.frame(most_freq_words[,1])
most_freq_dfr$Occurences <- most_freq_dfr[,1]
most_freq_dfr$Word <- names(topfeatures(unigram_dfm, decreasing = TRUE,n = 10))



pdf(paste0("most_frequent_words_",time,".pdf"),width=6,height=4)
ggplot(most_freq_dfr, aes(x = Word,y = Occurences, fill = Occurences)) +
  geom_bar(stat = "identity") +
  ggtitle("Most frequently used words") +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
dev.off()

message <- c(paste("There are altogether", sum(table_sentiment$Count), "headlines.", sep = " "),"\n",
             "Out of those:", "\n", paste("",table_sentiment[3,2]," (",table_sentiment[3,3]," %) are positive.","\n",
                                          table_sentiment[2,2]," (",table_sentiment[2,3]," %) are neutral.","\n",
                                          table_sentiment[1,2]," (",table_sentiment[1,3]," %) are negative.", sep = ""))

# Writing ten most positive and ten most negative headlines into a text file
emotional_list <- file(paste0("mostEmotionalPhrases_",time,".txt"))
writeLines(c("SUMMARY", "\n",message, "\n", "TEN MOST POSITIVE HEADLINES","\n",top_ten,"\n", "TEN MOST NEGATIVE HEADLINES","\n", bottom_ten), emotional_list)
close(emotional_list)



setwd(defaultwd)


