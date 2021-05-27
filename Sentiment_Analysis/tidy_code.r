# necessary libraries
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(stopwords)
library(scales)
library(textstem)
library(textdata)
library(wordcloud)
library(forcats)
library(tm)
library(igraph)
library(ggraph)
library(RTextTools)
library(e1071)

# import dataset as a tibble
ford = tibble(read.csv(file = "FordAutoReviews.csv"))

# making everything lowercase for consistency
ford$text = tolower(ford$text)

# combining the phrases that are numbers space numbers to be an actual number
ford <-  as.data.frame(sapply(ford, function(x) gsub("(\\d) (\\d)", "\\1\\2", x)))

# turning the number k into number thousand (ie 75K to 75000)
ford <-  as.data.frame(sapply(ford, function(x) gsub("(\\d)k", "\\1000", x)))

# linking the number of years and year(s)
ford <- as.data.frame(sapply(ford, function(x) gsub("(\\d) year", "\\1year", x)))
ford <- as.data.frame(sapply(ford, function(x) gsub("(\\d) years", "\\1years", x)))

# doing the same with 2 and 4 wheel drive
ford <- as.data.frame(sapply(ford, function(x) gsub("(\\d) wheel", "\\1wheel", x)))

# fix the weird contractions like don t
ford <-  as.data.frame(sapply(ford, function(x) gsub("146", "", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub(" ll ", "'ll ", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub(" s ", "'s ", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub(" t ", "'t ", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub(" ve ", "'ve ", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub(" d ", "'d ", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub(" re ", "'re ", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub(" m ", "'m ", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub("do not", "don't", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub("can not", "can't", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub("cannot", "can't", x)))
ford <-  as.data.frame(sapply(ford, function(x) gsub("did not", "don't", x)))

# removing unclassified reviews
# test = ford[!is.na(ford$class),]

# can now do word count of the reviews
# duplicating the dataset so I can change the text column from strings into lists of words
ford_dup = ford
# converting it into a list of words by separating the string at spaces
ford_dup$text = as.character(ford_dup$text)
ford_dup$text = strsplit(ford_dup$text, ' ')

# converting text from factor to character bc needs to be
ford$text = as.character(ford$text)

# adding a column to the original, ford dataset with word count
for (i in seq_along(ford_dup$text))
    ford$nwords[i] = length(ford_dup$text[[i]])

# getting the number of characters in each review
for (i in seq_along(ford$text))
    ford$nchar[i] = nchar(ford$text[[i]])

# looking at character length diffs between the reviews
aggregate(nchar~class, ford, sum)
#   class   nchar
# 1   Neg 1699590
# 2   Pos 1528507
aggregate(nchar~class, ford, summary)
#   class nchar.Min. nchar.1st Qu. nchar.Median nchar.Mean nchar.3rd Qu.
# 1   Neg    511.000      1137.000     1757.000   2458.913      2858.500
# 2   Pos    494.000      1016.000     1490.000   2211.650      2405.000
#   nchar.Max.
# 1  19630.000
# 2  21144.000

# creating a boxplot of the character lengths of the positive and negative reviews
jpeg("ford_nchar.jpeg")
ggplot(ford, aes(x=class, y = nchar)) + 
    geom_boxplot()
    geom_jitter(position=position_jitter(0.2))
dev.off()


# looking at word length diffs between the reviews
aggregate(nwords~class, ford, sum)
#   class nwords
# 1   Neg 357503
# 2   Pos 323504

aggregate(nwords~class, ford, summary)
#   class nwords.Min. nwords.1st Qu. nwords.Median nwords.Mean nwords.3rd Qu.
# 1   Neg      1.0000       179.0000      295.0000    316.2952       441.0000
# 2   Pos      2.0000       166.0000      268.0000    292.9479       409.5000
#   nwords.Max.
# 1    670.0000
# 2    672.0000


# creating a boxplot for the number of words ...was graphing VERY weirdly
# jpeg("ford_nwords.jpeg")
# ggplot(ford, aes(x=class, y = nwords)) + 
#     geom_boxplot()
#     geom_jitter(position=position_jitter(0.2))
# dev.off()


# looks like the negative reviews are slightly longer than the positives

# running an ANOVA on nchar and nwords to see if the means are different
results = aov(ford$nchar ~ ford$class, data=ford)
print(results)
a = summary(results) 
a
#               Df    Sum Sq  Mean Sq F value Pr(>F)  
# ford$class     1 2.112e+07 21123587   3.989  0.046 *
# Residuals   1380 7.308e+09  5295438 

# the number of characters in positive and negative reviews is significantly different
results = aov(ford$nwords ~ ford$class, data=ford)
print(results)
a = summary(results)

#               Df   Sum Sq Mean Sq F value Pr(>F)  
# ford$class     1   188331  188331   6.485  0.011 *
# Residuals   1380 40074082   29039   

# looks like difference in word numbers is almost sig different

# but if you look at the boxplots they don't look super different. I thinkk the outliers might be skewing things.

# using mutate to add line numbers (or in this case review IDs)
ford = mutate(ford, linenumber = row_number())

# converting ford$text to a character bc it is currently a factor and that will not work with the unnest_tokens function:
ford$text = as.character(ford$text)

# unnesting the tokens so there is one word/token per row:
tidy_ford = ford %>%
  unnest_tokens(word, text)

# looking at word count before stop words are removed
jpeg("word_count_before.jpeg")
  tidy_ford %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
dev.off()

# creating a list of stopwords based on the readily available one
stop_words = stopwords(language = "en")
# stopw = c( "br", "brbr", "vehicle", "ford", stop_words[1:38], stop_words[54:80], stop_words[93:94], stop_words[98:132], stop_words[135:164], stop_words[166], stop_words[168:172], stop_words[175])
stopw = c( "br", "brbr", "car", "truck", "vehicle", "ford", "be", stop_words)

stopw = tibble(stopw)
colnames(stopw) = c("words")

clean_ford = tidy_ford %>% anti_join(stopw, by = c("word" = "words"))
clean_ford$word = lemmatize_words(clean_ford$word, dictionary = lexicon::hash_lemmas)

# look at word frequencies without the stop words

jpeg("word_count.jpeg")
  clean_ford %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
dev.off()

# calculate the frequencies of the words (so that later can plot word frequencies in positive vs negative reviews)
frequency = clean_ford%>%
count(class, word) %>% 
group_by(class)  %>% 
mutate(proportion = n / sum(n))   %>% 
select (-n)  %>% 
pivot_wider(names_from = class, values_from = proportion)  %>% 
pivot_longer('Neg', names_to = "class", values_to = "proportion")

# going to plot the frequency values in a chart that plots proportion of positive vs negative reviews
jpeg('Pos_v_Neg_Freq.jpeg')
ggplot(frequency, aes(x = proportion, y = Pos, color = abs(Pos - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_distiller(palette = "PuRd") +
  facet_wrap(~class, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Pos", x = NULL)
dev.off()

# calculating the correlation of the word frequencies between positive and negative. USing just the words going forward
cor.test(data = frequency[frequency$class == "Neg",], ~ proportion + Pos)

#         Pearson's product-moment correlation

# data:  proportion and Pos
# t = 254.85, df = 5809, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9559071 0.9601316
# sample estimates:
#       cor 
# 0.9580714  

# creating a word cloud of the words most commonly associated with positive and negative reviews
jpeg('neg_wordcloud.jpeg')
clean_ford  %>% 
filter(class == "Neg")  %>% 
count(word, class)  %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('pos_wordcloud.jpeg')
clean_ford  %>% 
filter(class == "Pos")  %>% 
count(word, class)  %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()

# can see word "good" very large in the neg wordcloud so let's try to fix it by analyzing a different way:

# getting the nrc sentiments and adding them to each word
clean_ford = clean_ford  %>% 
right_join(get_sentiments("nrc"))

# wordcloud of the positive sentiments from the nrc dataset in the positive reviews
jpeg('posxpos.jpeg', width=12,height=8, units='in', res=300)
clean_ford %>% 
filter(class == "Pos" & sentiment == "positive") %>% 
count(word) %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()

# wordcloud of the negative sentiments from the nrc dataset in the negative reviews
jpeg('negxneg.jpeg', width=12,height=8, units='in', res=300)
clean_ford %>% 
filter(sentiment == "negative" & class == "Neg") %>% 
count(word) %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()



# wordclouds of all the individual sentiments
jpeg('neg.jpeg')
p1 = plot.new()
clean_ford %>% 
filter(sentiment == "negative")  %>% 
count(word) %>%
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('fear.jpeg')
p2 = clean_ford %>% 
filter(sentiment == "fear")  %>% 
count(word) %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('sadness.jpeg')
p3 = clean_ford %>% 
filter(sentiment == "sadness")  %>% 
count(word) %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('anger.jpeg')
p4 = clean_ford %>% 
filter(sentiment == "anger")  %>% 
count(word) %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('disgust.jpeg')
p5 = clean_ford %>% 
filter(sentiment == "disgust")  %>% 
count(word)  %>%  
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('surprise.jpeg')
p1 = plot.new()
clean_ford %>% 
filter(sentiment == "surprise")  %>% 
count(word) %>%
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('joy.jpeg')
p2 = clean_ford %>% 
filter(sentiment == "joy")  %>% 
count(word) %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('anticipation.jpeg')
p3 = clean_ford %>% 
filter(sentiment == "anticipation")  %>% 
count(word) %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('trust.jpeg')
p4 = clean_ford %>% 
filter(sentiment == "trust")  %>% 
count(word) %>% 
with(wordcloud(word, n, max.words = 100))
dev.off()

jpeg('positive.jpeg')
p5 = clean_ford %>% 
filter(sentiment == "positive")  %>% 
count(word)  %>%  
with(wordcloud(word, n, max.words = 100))
dev.off()

# doing tf - idf
clean_ford$class = as.character(clean_ford$class)
ford_words = clean_ford  %>% 
count(class, word, sort = TRUE)

total_words = ford_words  %>% 
group_by(class)  %>% 
summarize(total = sum(n))

ford_words = left_join(ford_words, total_words)
ford_tf_idf = ford_words  %>% 
bind_tf_idf(word, class, n)  %>% 
arrange(desc(tf_idf))

ford_tf_idf  %>% 
select(-total)  %>% 
arrange(desc(tf_idf))

jpeg('tf_idf.jpeg')
ford_tf_idf  %>% 
filter(class == "Neg" | class == "Pos")  %>% 
group_by(class)  %>% 
slice_max(tf_idf, n = 15)  %>% 
ungroup()  %>% 
ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = class)) + 
geom_col(show.legend = FALSE) +
facet_wrap(~class, ncol = 2, scales = "free") +
labs(x = "tf-idf", y = NULL)
dev.off()

# doing n grams to look at pairs of consecutive words
ford_bigrams = ford  %>% 
unnest_tokens(bigram, text, token = "ngrams", n = 2)

# counting the bigrams to see the highest frequency
bigram_count = ford_bigrams  %>% 
count(bigram, sort = TRUE)

# telling R that the bigram column is made up of two words separated by a space
bigrams_separated = ford_bigrams  %>% 
separate(bigram, c("word1", "word2"), sep = " ")

# removin any pairs of words that are in our stopwords list called stopw
bigrams_filtered = bigrams_separated  %>% 
filter(!word1 %in% stopw$words)  %>% 
filter(!word2 %in% stopw$words)

# now redoing the bigram counts without the stopwords
bigram_count = bigrams_filtered  %>% 
count(word1, word2, sort = TRUE)

# recombining the separated column for ease of referencing
bigrams_united = bigrams_filtered  %>% 
unite(bigram, word1, word2, sep = " ")

# lemmatizing the list to bring words down to their roots
bigrams_united$bigrams = lemmatize_strings(bigrams_united$bigram, dictionary = lexicon::hash_lemmas)

# counts after lemmatization
bigram_count =  bigrams_united %>%
count(bigram, sort = TRUE) 

# making a word cloud of all bigrams, positive review, and negative review
jpeg('bigrams.jpeg')
bigrams_united %>%  
count(bigram)  %>%  
with(wordcloud(bigram, n, max.words = 100))
dev.off()


# had to tweak the wordcloud code for pos and neg bc wasn't fitting everything on the page
jpeg('pos_bigrams.jpeg')
bigrams_united %>%  
filter(class == "Pos")  %>% 
count(bigram)  %>%  
{wordcloud(.$bigram, .$n, max.words = 100)}
dev.off()

jpeg('neg_bigrams.jpeg')
bigrams_united %>%  
filter(class == "Neg")  %>% 
count(bigram)  %>%  
{wordcloud(.$bigram, .$n, max.words = 100)}
dev.off()


# calculating bigram tf - idf
bigram_tf_idf = bigrams_united  %>% 
count(class, bigram)  %>% 
bind_tf_idf(bigram, class, n)  %>% 
arrange(desc(tf_idf))

# graphing the high tf-idf words

jpeg('bigram_tf_idf.jpeg')
bigram_tf_idf  %>% 
mutate(bigram = factor(bigram, levels = rev(unique(bigram))))  %>%
group_by(class)  %>% 
slice_max(tf_idf, n = 15)  %>% 
ungroup()  %>% 
ggplot(aes(bigram, tf_idf), fill = class) + 
geom_col(show.legend = FALSE) +
facet_wrap(~class, ncol = 2, scales = "free") +
labs(x = "tf-idf", y = NULL) +
coord_flip()
dev.off()

# creating a visualization network of bigrams
bigram_counts =  bigrams_filtered %>%
count(word1, word2, sort = TRUE) 

bigram_graph = bigram_counts  %>% 
filter(n>20)  %>% 
graph_from_data_frame()

# have to do this for ggraph
set.seed(2017)

# creating the network visualization
jpeg('bigram_graph.jpeg')
ggraph(bigram_graph, layout = "fr") +
geom_edge_link() +
geom_node_point() +
geom_node_text(aes(label = name), vjust = 1, hjust = 1)
dev.off()


set.seed(2020)
a = grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

########################################################################
#############sentiment analysis and machine learning####################
# used the tutorial on https://www.r-bloggers.com/2016/01/sentiment-analysis-with-machine-learning-in-r/

# import the dataset
ford = read.csv(file = "FordAutoReviews.csv")

# shuffling the dataset
set.seed(42)
rows = sample(nrow(ford))
shuffle_ford = ford[rows,]

# building the dtm
matrix = create_matrix(shuffle_ford[,2], language="english", removeStopwords=FALSE, removeNumbers=TRUE, stemWords=FALSE)

# train the model using 30% of the data
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:414,], as.factor(shuffle_ford[1:414,1]))

# test the validity
predicted = predict(classifier, mat[415:1382,]); predicted
table(shuffle_ford[415:1382, 1], predicted)
recall_accuracy(shuffle_ford[415:1382,1], predicted)


# looking at the predicted classifications vs actual classifications
  #    predicted
  #     Neg Pos
  # Neg 444  46
  # Pos 403  75

  # accuracy was 0.536157

# repeating to see if predictions are more accurate when the model is learned with 50% of the data
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:691,], as.factor(shuffle_ford[1:691,1]))

# test the validity
predicted = predict(classifier, mat[692:1382,]); predicted
table(shuffle_ford[692:1382, 1], predicted)
recall_accuracy(shuffle_ford[692:1382,1], predicted)
     
  #    predicted
  #     Neg Pos
  # Neg  51 296
  # Pos  14 330

  # 0.5513748 percent accuracy
