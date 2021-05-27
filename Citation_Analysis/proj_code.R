# Project ideas:
# total number of publications, how long his career has been, avg publication per year
# Number of papers per year over time
# Average paper length?
# Distribution of journals published in total
# frequency of author keywords
# number of citations per paper
# Number of citations per publication year
# relateed citations per publication year and number of papers published that
# Author keywords in the most cited papers
# Document type: Article vs book chapter vs review frequency
# Buzzwords in the titles of the papers who cite them. See if similar or different topic breakdowns


#########find a list of journal impact factors and look into his average impact factor. Also see if average impact factor per year increases over time
            # indicative of improved research/more interesting topics?

library(ggplot2)
library(sqldf)
library(plyr)
library(dplyr)
library(ggforce)
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
library(SnowballC)
library(analogue)
# import the dataset retrieved from web of science
bazer = read.csv(file = "bazer.csv")

# looking at publication stats
summary(bazer$Publication.Year)
dim(bazer)
# has published 920 publiccations over 59 year, averaging 16 publications per year

# look at frequency distribution per year
table(bazer$Publication.Year)
# 1962 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 
#    1    2    4    5    1    7    9   10    8    7    9    5    6   10    7    5 
# 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 
#   16    8   17   13   16   13   10   20   18   26    9   11   16   14   22   20 
# 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 
#   21   30   29   24   13   21   15   10   17   26   29   32   28   29   37   32 
# 2014 2015 2016 2017 2018 2019 2020 2021 
#   25   22   23   35   38   15   21    3 

# creating a histogram of publications per year
jpeg("papers_per_year.jpeg")
ggplot(bazer, aes(x=Publication.Year)) + 
geom_histogram(fill = "#3792cb", col = "black") + 
scale_x_continuous(breaks=c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) + 
labs(title = "Total Publications per Year", x = "Year", y = "Total Publications") +
theme(plot.title = element_text(hjust = 0.5))
dev.off()

# looking at average paper length
head(bazer$Number.of.Pages)
# removing those with zero pages
page_length = bazer[bazer$Number.of.Pages != 0,]
summary(page_length$Number.of.Pages)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.000   2.000   8.000   7.954  11.000  36.000



#looking at the distribution of journals he has published in
# pie chart of breakdown of all papers
journal.freq = table(bazer$Journal.Abbreviation)
pie(journal.freq)
# very crazy, so only displaying the ones with frequency 10 or more and grouping the rest into "other"
journal.freq = as.data.frame(journal.freq)
colnames(journal.freq) = c("Journal", "Frequency")
dim(journal.freq)
# He has published in 140 different journals
# now removing the rows in the dataframe with frequency of 10 or greater
journal.freq.10 = journal.freq[journal.freq$Frequency >= 10,]
# calculating the percents
journal.freq.10$percent = as.numeric(journal.freq.10$Frequency) /920 * 100
# summing the percents column and subtracting from 100 to see how much the "other" slice needs to be
100 - sum(as.numeric(journal.freq.10$percent))
# 29.56522
# adding "OTHER" to the dataframe
920 - sum(as.numeric(journal.freq.10$Frequency))
# 272
other = c("OTHER", 272, 29.56522)
# in order to do rbind, need to change the Jounrnal column from factor to character
journal.freq.10$Journal = as.character(journal.freq.10$Journal)
journal.freq.clean = rbind(journal.freq.10, other)

# one of the journals had a name change so need to combine two rows
# creating a new Reproduction row with rows 10 and 14 combined
repro = c("REPRODUCTION", 55, 5.978261) 
journal.freq.clean = rbind(journal.freq.clean, repro)
#resetting row numbers
rownames(journal.freq.clean) = c(1:nrow(journal.freq.clean))
# removing rows 10 and 14
journal.freq.clean = journal.freq.clean[-c(10,14),]
# resetting row numbers again
rownames(journal.freq.clean) = c(1:nrow(journal.freq.clean))
#removing any levels that are hanging around despite subsetting and deleting rows
journal.freq.clean = droplevels(journal.freq.clean)

# creating a pie chart
# to create a piechart from a dataframe have to go about it a different way
# create a basic bar chart
pie = ggplot(journal.freq.clean, aes( x ="", y = Frequency, fill = Journal)) + geom_bar(stat = "identity", width = 1)
# convert to pie (aka polar coordinates) and add labels
pie = pie + coord_polar("y", start = 0) + geom_text(aes(label = round(as.numeric(percent))), color = "white", position = position_stack(vjust = 0.5))
# add color scale (with hex colors). I'm doing one of the journal's associated colors to be extra
pie = pie + scale_fill_manual(values = c("#ff781f", "#1261A0", "#9897A9", "#072F5F", "#A6D609", "#1b3048", "#4B92DB", "#1b047c", "#9E1A1A", "#358856", "#FEE227", "#ca2c92", "#FF6347", "#C29200", "#800020" ))
# remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Journal Publishing Percentage")

# tidying up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5))

jpeg('journal_publishing_percent.jpeg')
pie
dev.off()


freq = journal.freq.clean$Frequency
journal = journal.freq.clean$Journal
pie(freq, journal)


# looking at most frequent keywords
# I want the double word keywords to be counted together so I am replacing the space between them with a hyphen
# bazer.tib$Author.Keywords = gsub("([[:alnum:]])\ ([[:alnum:]])", "\\1-\\2", bazer.tib$Author.Keywords)

bazer.tib = tibble(read.csv(file = "bazer.csv"))
# removing the semicolons from the column
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("([[:alnum:]]); ([[:alnum:]])", "\\1 \\2", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("estradiol", "estrogen", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("pregnancy", "pregnant", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("pregnancies", "pregnant", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("uterine", "uterus", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("interferon tau", "interferontau", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("interferons", "interferon", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("conceptuses", "conceptus", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("grow ", "growth", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("grows", "growth", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("growing", "growth", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("endometrial", "endometrium", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("amino acid", "aminoacid", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("amino acids", "aminoacid", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("trophoblasts", "trophoblast", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("placentas", "placenta", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("implanting", "implantation", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("implants", "implantation", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("cell ", "cells", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("apoptotic", "apoptosis", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("gene", "genome", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("genes", "genome", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("genetic", "genome", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("develops", "development", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("developing", "development", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("develop ", "development", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("l ", "l", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("proliferate", "proliferation", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("reproductive", "reproduction", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("proteins", "protein", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("pigs", "pig", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("ovine", "sheep", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("porcine", "pig", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("bovine", "cow", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("cows", "cow", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("fetal", "fetus", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("chicken", "chicken", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("er ", "er", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("placental", "placenta", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("cytokines", "cytokine", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("swine", "pig", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("1 ", "1", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("2 ", "2", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("secrete", "secretion", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("secreted", "secretion", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("secretes", "secretion", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("ovarian", "ovary", x)))
bazer.tib$Author.Keywords = as.character(sapply(bazer.tib$Author.Keywords, function(x) gsub("ovaries", "ovary", x)))



bazer.tib$Author.Keywords = as.character(bazer.tib$Author.Keywords)
# converting the df to a tibble

# putting each keyword on its own line
tidy.bazer = bazer.tib  %>% unnest_tokens(word, Author.Keywords)
jpeg('frequent_keywords.jpeg')  
  tidy.bazer %>%
  count(word, sort = TRUE) %>%
  filter(n >= 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
  dev.off()

# putting those words into a word cloud
jpeg('keyword_wordcloud.jpeg')
tidy.bazer  %>% 
count(word)  %>% 
with(wordcloud(word, n, colors = brewer.pal(8, "Set1"), random.order = FALSE, max.words = 50))
dev.off()

jpeg('keyword_wordcloud_all.jpeg', width=12,height=8, units='in', res=300)
tidy.bazer  %>% 
count(word)  %>% 
with(wordcloud(word, n, colors = brewer.pal(8, "Set1"), random.order = FALSE))
dev.off()

# looking at how many times he is cited and any trends
# looking at summary
# looking at if his citations increase over the years. Do those increases correlate with more papers that year?
# looking at the buzzwords associated with his most cited papers

# looking at the summary
summary(bazer$Times.Cited..All.Databases)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0.00    2.00   18.00   41.45   53.25 1616.00 

# looking at if his citations increase over the years. Do those increases correlate with more papers that year?
citesxyear = aggregate(Times.Cited..All.Databases~Publication.Year, bazer, FUN = sum)
colnames(citesxyear) = c("year", "cites")
pubsxyear = as.dataframe(table(bazer$Publication.Year))
colnames(pubsxyear) = c("year", "pubs")
cites_pubsxyear = sqldf('SELECT c.*, p.pubs FROM citesxyear AS c left join pubsxyear AS p ON c.year = p.year')

# creating the first line
jpeg("cites_and_pubs_xyear.jpeg")
par(mar = c(5,5,2,5))
with(cites_pubsxyear, plot(year, cites, type = "l", col = "#0e6b0e", 
    ylab = "Total Citations/Publication Year"))

# creating second line on top of it
par(new = T)
with(cites_pubsxyear, plot(year, pubs, type = "l", col = "blue", axes = F, xlab = NA, ylab = NA, cex = 1.2))
axis(side = 4)
mtext(side = 4, line = 3, "Total Publications/Year")
legend("topleft", legend = c("Citations", "Publications"), lty = c(1,1), col = c("#0e6b0e", "blue"))
dev.off()
# looking at Citations/Publication per year
cites_pubsxyear$citesxpubs = cites_pubsxyear$cites / cites_pubsxyear$pubs

jpeg('citesxpubsxyear.jpeg')
plot(cites_pubsxyear$year, cites_pubsxyear$citesxpubs, type = "l", col = "red", ylab = "Citations per Publication per Year")
dev.off()

# looking at the buzzwords associated with his most cited papers and also buzzwords associated with the years with the most citations per paper
wordsxcites = aggregate(Times.Cited..All.Databases~word, tidy.bazer, FUN = sum)
colnames(wordsxcites) = c("word", "cites")
wordsxcites = sqldf('SELECT * from wordsxcites ORDER BY cites DESC')

# creating a wordcloud of the keywords with the most citations
jpeg('keyxcite_100_wordcloud.jpeg')
wordcloud(words = wordsxcites$word, freq = wordsxcites$cites, colors = brewer.pal(8, "Set1"), random.order = FALSE, max.words = 100)
dev.off()


# Document type: Article vs book chapter vs review frequency
table(bazer$Publication.Type)

#   B   J   S 
#   7 903  10 

# looking at the keywords of those who cited him
citers = tibble(read.csv(file = "citers.csv"))

# cited by 15569 publications
dim(citers)
# [1] 15569    68

# removing the semicolons from the column
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("([[:alnum:]]); ([[:alnum:]])", "\\1 \\2", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("estradiol", "estrogen", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("pregnancy", "pregnant", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("pregnancies", "pregnant", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("uterine", "uterus", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("interferon tau", "interferontau", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("interferons", "interferon", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("conceptuses", "conceptus", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("grow ", "growth", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("grows", "growth", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("growing", "growth", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("endometrial", "endometrium", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("amino acid", "aminoacid", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("amino acids", "aminoacid", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("trophoblasts", "trophoblast", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("placentas", "placenta", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("implanting", "implantation", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("implants", "implantation", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("cell ", "cells", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("apoptotic", "apoptosis", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("gene", "genome", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("genes", "genome", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("genetic", "genome", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("develops", "development", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("developing", "development", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("develop ", "development", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("l ", "l", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("proliferate", "proliferation", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("reproductive", "reproduction", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("proteins", "protein", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("pigs", "pig", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("ovine", "sheep", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("porcine", "pig", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("bovine", "cow", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("cows", "cow", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("fetal", "fetus", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("chicken", "chicken", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("er ", "er", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("placental", "placenta", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("cytokines", "cytokine", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("swine", "pig", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("1 ", "1", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("2 ", "2", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("secrete", "secretion", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("secreted", "secretion", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("secretes", "secretion", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("ovarian", "ovary", x)))
citers$Author.Keywords = as.character(sapply(citers$Author.Keywords, function(x) gsub("ovaries", "ovary", x)))



citers$Author.Keywords = as.character(citers$Author.Keywords)

tidy.citers = citers  %>% unnest_tokens(word, Author.Keywords)
jpeg('citer_keywords.jpeg')  
  tidy.citers %>%
  count(word, sort = TRUE) %>%
  filter(5 < n < 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
  dev.off()

# putting those words into a word cloud
jpeg('citer_wordcloud.jpeg', width=12,height=8, units='in', res=300)
tidy.citers  %>% 
count(word)  %>% 
with(wordcloud(word, n, colors = brewer.pal(8, "Set1"), min.freq = 25, random.order = FALSE))
dev.off()

# years = c("1962", "1967", "1968", "1969")
# looking to see if interests change over time
# jpeg('1962_1969.jpeg', width=12,height=8, units='in', res=300)
# tidy.bazer  %>% 
# count(word, Publication.Year)  %>% 
# filter(Publication.Year %in% years)  %>% 
# with(wordcloud(word, n, colors = brewer.pal(8, "Set2"), random.order = FALSE, max.words = 50))
# dev.off()

jpeg('2020_2021.jpeg', width=12,height=8, units='in', res=300)
tidy.bazer  %>% 
count(word, Publication.Year)  %>% 
filter(Publication.Year %in% c("2020", "2021"))  %>% 
with(wordcloud(word, n, colors = brewer.pal(8, "Set2"), random.order = FALSE, max.words = 50))
dev.off()

# for (var in 1962:2021):
#     {
#     value = paste(num + ".jpeg")
#     jpeg(value, width=12,height=8, units='in', res=300)
#     tidy.bazer  %>% 
#     count(word, Publication.Year)  %>% 
#     filter(Publication.Year == num)  %>% 
#     with(wordcloud(word, n, colors = brewer.pal(8, "Set2"), random.order = FALSE, max.words = 50))
#     dev.off()
#     }