####Ways to measure importance of tokens in text
#1) tf(Counting the number of times each word appears in a document)
#2) tf-idf(Calculating the frequency that each word appears in a document out of all the words in the document.)
#3) bag of words model(counting number of words in each document. simple and inexpensive method)

######### a part #############################
library('tidyverse')
#setwd('C:\Users\robin\Downloads\IDS572')
setwd('C:\\Users\\rahul\\Documents\\572\\assignment4')

# the data file uses ';' as delimiter, and for this we use the read_csv2 function
resReviewsData <- read_csv2('yelpResReviewSample.csv')

#number of reviews by star-rating
resReviewsData %>% group_by(stars) %>% count()
hist(resReviewsData$stars)

#to check words are distributed based on stars.
ggplot(resReviewsData, aes(x= funny, y=stars)) +geom_point()
ggplot(resReviewsData, aes(x= cool, y=stars)) +geom_point()
ggplot(resReviewsData, aes(x= useful, y=stars)) +geom_point()

############# b part ###################################

#The reviews are from various locations -- check
#Number of reviews from each state
resReviewsData %>%   group_by(state) %>% tally() %>% view()
#Can also check the postal-codes
resReviewsData %>% group_by(postal_code) %>% tally() %>% view()

#If you want to keep only the those reviews from 5-digit postal-codes  
#Use filter() find rows/cases where conditions are true
#str_detect checks the presence of a string. Here tries to find the postal code whose first digit is 0 to 9 and is of 1 to 5 digits.
rrData <- resReviewsData %>% filter(str_detect(postal_code, "^[0-9]{1,5}"))


#####Use tidytext for tokenization, removing stopwords, stemming/lemmatization, etc.
###Stemming is a process that removes endings such as affixes.
#Lemmatization is the process of grouping inflected
#forms together as a single base form.
library(tidytext)
library(SnowballC)   #stemmer library
library(textstem)    #for stemming and lemmatization of text.

#tokenize the text of the reviews in the column named 'text'
#converted the text to tokens. unnest_tokens is a part of tidy_text.
rrTokens <- rrData %>% select(review_id, stars, text ) %>% unnest_tokens(word, text)

#How many tokens?
rrTokens %>% distinct(word) %>% dim()
#### 68204
####remove stopwords
#Anti-Join returns those rows from the left side of the predicate for which there is no match on the right.
#checking the stop words from stop_words in R.
rrTokens <- rrTokens %>% anti_join(stop_words)  #took rrTokens and removed stopwords from it.
#compare with earlier - what fraction of tokens were stopwords?
rrTokens %>% distinct(word) %>% dim()
####67505
##### 699 distinct words were stopwords and removed

#count the total occurrences of differet words, & sort by most frequent
rrTokens %>% count(word, sort=TRUE) %>% top_n(10)

#Are there some words that occur in a large majority of reviews, or which are there in very few reviews?   Let's remove the words which are not present in at least 10 reviews
rareWords <-rrTokens %>% count(word, sort=TRUE) %>% filter(n<10) #appears in less than 10 reviews.
xx<-anti_join(rrTokens, rareWords)
xx %>% distinct(word) %>% dim()
# 8854 - these are pruned set of terms #################
#check the words in xx .... 
xx %>% group_by(word) %>% summarise(count=n()) %>% arrange(desc(count)) %>% view()
#or
xx %>% count(word, sort=FALSE) %>% view()
   #you will see that among the least frequently occurring words are those starting with or including numbers (as in 6oz, 1.15,...).  To remove these
xx2<- xx %>% filter(str_detect(word,"[0-9]")==FALSE)
   #the variable xx, xx2 are for checking ....if this is what we want, set the rrTokens to the reduced set of words.  And you can remove xx, xx2 from the environment.
rrTokens<- xx2
dim(rrTokens)

rm(xx)
rm(xx2)
######Analyze words by star ratings 

#Check words by star rating of reviews
rrTokens %>% group_by(stars) %>% count(word, sort=TRUE)
#or...
rrTokens %>% group_by(stars) %>% count(word, sort=TRUE) %>% arrange(desc(stars)) %>% view()


#proportion of word occurrence by star ratings
ws <- rrTokens %>% group_by(stars) %>% count(word, sort=TRUE)
ws<-  ws %>% group_by(stars) %>% mutate(prop=n/sum(n))

#check the proportion of 'love' among reviews with 1,2,..5 stars 
ws %>% filter(word=='love')

#what are the most commonly used words by star rating
ws %>% group_by(stars) %>% arrange(stars,desc(prop)) %>% view()

#to see the top 20 words by star ratings
ws %>% group_by(stars) %>% arrange(stars, desc(prop)) %>% filter(row_number()<=20L) %>% view()

#To plot this
ws %>% group_by(stars) %>% arrange(stars, desc(prop)) %>% filter(row_number()<=20L) %>% ggplot(aes(word, prop))+geom_col()+coord_flip()+facet_wrap((~stars))


#Can we get a sense of which words are related to higher/lower star raings in general? 
#One approach is to calculate the average star rating associated with each word - can sum the star ratings associated with reviews where each word occurs in.
#Can consider the proportion of each word among reviews with a star rating.

xx<- ws %>% group_by(word) %>% summarise(totWS=sum(stars*prop))
xx %>% top_n(20)
#### ?? another approach
#### another or more suitable approach according to us is
### take prop at word level rather than star level.
## It will give average rating for each word.
### This seems a better interpretation.

xx1<- ws %>% group_by(word) %>% mutate(prop1 = n/sum(n)) %>% summarize(totWS=sum(stars*prop1))

#What are the 20 words with highest and lowerst star rating
xx1 %>% top_n(20)
### it's visible that each word has average rating between 1 to 5.
xx1 %>% top_n(-20)
xx1 %>% group_by(word) %>% arrange(desc(totWS),word) %>% view()

#Q - does this 'make sense'?
###### It makes more sense because now we can provide a sentiment-positive/
## negative according to this rating of the word.

################ c part ###############################
##Stemming and Lemmatization
rrTokens_stem<-rrTokens %>%  mutate(word_stem = SnowballC::wordStem(word))
rrTokens_lemm<-rrTokens %>%  mutate(word_lemma = textstem::lemmatize_words(word)) 

#Check the original words, and their stemmed-words and word-lemmas
head(rrTokens_stem)
head(rrTokens_lemm)

## lemmatization is better because it makes all the words like - serve,serves,served as serve
## which makes more sense according to the business logic

#tokenize, remove stopwords, and lemmatize (or you can use stemmed words instead of lemmatization)
rrTokens<-rrTokens %>%  mutate(word = textstem::lemmatize_words(word))

#We may want to filter out words with less than 3 characters and those with more than 15 characters
rrTokens<-rrTokens %>% filter(str_length(word)>=3 & str_length(word)<=15)

rrTokens<- rrTokens %>% group_by(review_id, stars) %>% count(word)

###############finding tf, idf, and tf-idf##################

#count total number of words by review, and add this in a column
totWords<- rrTokens  %>% group_by(review_id) %>%  count(word, sort=TRUE) %>% summarise(total=sum(n))
xx<-left_join(rrTokens, totWords)
# now n/total gives the tf values
xx <- xx %>% mutate(tf=n/total)
head(xx)

#We can use the bind_tfidf function to calculate the tf, idf and tfidf values
# (https://www.rdocumentation.org/packages/tidytext/versions/0.2.2/topics/bind_tf_idf)
rrTokens<-rrTokens %>% bind_tf_idf(word, review_id, n)
head(rrTokens)

###Sentiment analysis using the 3 sentiment dictionaries available with tidytext (use library(textdata))
###AFINN http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010
##bing  https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html 
##nrc http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

library(textdata)

#take a look at the wordsin the sentimennt dictionaries
get_sentiments("bing") %>% view()
####6786 terms
get_sentiments("nrc") %>% view()
#####13901 terms
get_sentiments("afinn") %>% view()
### 2477 terms

#### Matching terms in bing and nrc
get_sentiments("bing") %>% inner_join(get_sentiments("nrc"), by="word") %>% nrow()
### 6169 terms
get_sentiments("bing") %>% inner_join(get_sentiments("nrc"), by="word") %>% 
   select(word) %>% unique() %>% nrow()
### unique terms - 2483 terms

#### Matching terms in bing and afinn
get_sentiments("bing") %>% inner_join(get_sentiments("afinn"), by="word") %>% nrow()
### 1315 terms
get_sentiments("bing") %>% inner_join(get_sentiments("afinn"), by="word") %>% 
   select(word) %>% unique() %>% nrow()
### unique terms - 1314 terms

#### Matching terms in afinn and nrc
get_sentiments("afinn") %>% inner_join(get_sentiments("nrc"), by="word") %>% nrow()
### 2802 terms
get_sentiments("afinn") %>% inner_join(get_sentiments("afinn"), by="word") %>% 
   select(word) %>% unique() %>% nrow()
### unique terms - 2477 terms

#sentiment of words in rrTokens
#rrSenti_bing<- rrTokens %>% left_join(get_sentiments("bing"), by="word")

########## For bing ###############
#if we want to retain only the words which match the sentiment dictionary, do an inner-join
rrSenti_bing<- rrTokens %>% inner_join(get_sentiments("bing"), by="word")

###Matching words when bing is joined
rrSenti_bing %>% group_by(word) %>% tally() %>% nrow()
#### 935

#Analyze Which words contribute to positive/negative sentiment - we can count the ocurrences of positive/negative sentiment words in the reviews
xx<-rrSenti_bing %>% group_by(word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))
 #negate the counts for the negative sentiment words
xx<- xx %>% mutate (totOcc=ifelse(sentiment=="positive", totOcc, -totOcc))

#the most positive and most negative words
xx<-ungroup(xx)
xx %>% top_n(25)
xx %>% top_n(-25)

#You can plot these
#rbind(top_n(xx, 25), top_n(xx, -25)) %>% ggplot(aes(word, totOcc, fill=sentiment)) +geom_col()+coord_flip()

#or, with a better reordering of words
rbind(top_n(xx, 25), top_n(xx, -25)) %>% mutate(word=reorder(word,totOcc)) %>% ggplot(aes(word, totOcc, fill=sentiment)) +geom_col()+coord_flip()


#### For nrc
rrSenti_nrc<-rrTokens %>% inner_join(get_sentiments("nrc"), by="word") %>%
   mutate(new_sentiment = ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), "negative","positive"))%>%
   group_by (word, new_sentiment) %>% summarise(totOcc=sum(n)) %>%
   arrange(new_sentiment, desc(totOcc))

###Matching words when nrc is joined
rrSenti_nrc %>% group_by(word) %>% tally() %>% nrow()
#### 1308

xx<-rrSenti_nrc %>% mutate(goodBad=ifelse(new_sentiment=="negative",-totOcc, totOcc))
#xx <- xx %>% mutate(new_sentiment = ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), "negative","positive"))
#How many words are there for the different sentiment categories
xx %>% group_by(new_sentiment) %>% summarise(count=n(), sumn=sum(goodBad))
#top few words for different sentiments
xx %>% group_by(new_sentiment) %>% arrange(new_sentiment, desc(goodBad)) %>% top_n(10) %>% view()
xx<-ungroup(xx)
xx %>% top_n(25)
xx %>% top_n(-25)
rbind(top_n(xx, 25), top_n(xx, -25)) %>% 
   mutate(word=reorder(word,goodBad)) %>%
   ggplot(aes(word, goodBad, fill=new_sentiment)) +geom_col()+coord_flip()

##### For afinn
rrSenti_afinn<- rrTokens %>% inner_join(get_sentiments("afinn"), by="word") %>%
   mutate(sentiment = ifelse(value>0,"positive","negative"))

###Matching words when afinn is joined
rrSenti_afinn %>% group_by(word) %>% tally() %>% nrow()
#### 516

xx<-rrSenti_afinn %>% group_by(word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))
xx<- xx %>% mutate (totOcc=ifelse(sentiment=="positive", totOcc, -totOcc))
xx<-ungroup(xx)
xx %>% top_n(25)
xx %>% top_n(-25)
rbind(top_n(xx, 25), top_n(xx, -25)) %>% mutate(word=reorder(word,totOcc)) %>% ggplot(aes(word, totOcc, fill=sentiment)) +geom_col()+coord_flip()

#Q - does this 'make sense'?  Do the different dictionaries give similar results; do you notice much difference?
### YES, explain in report

##########Analysis by review sentiment

###So far, we have analyzed overall sentiment across reviews, now let's look into sentiment by review and see how that relates to review's star ratings

#####################  For Bing ##########################
#summarise positive/negative sentiment words per review
revSenti_bing <- rrSenti_bing %>% group_by(review_id, stars) %>% summarise(nwords=n(),posSum=sum(sentiment=='positive'), negSum=sum(sentiment=='negative'))
revSenti_bing<- revSenti_bing %>% mutate(posProp=posSum/nwords, negProp=negSum/nwords)
revSenti_bing<- revSenti_bing %>% mutate(sentiScore=posProp-negProp)
revSenti_bing %>% group_by(stars) %>% summarise(avgPos=mean(posProp), avgNeg=mean(negProp), avgSentiSc=mean(sentiScore))
##### Considering 1 and 2 as low , 3 and 4 as high
revSenti_bing <- revSenti_bing %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 )))
revSenti_bing <- revSenti_bing %>% mutate(pred_hiLo=ifelse(sentiScore >0, 1, -1))
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
xx<-revSenti_bing %>% filter(hiLo!=0)
library(caret)
confusionMatrix(as.factor(xx$pred_hiLo),as.factor(xx$hiLo))
### 81.42 - accuracy

#####################  For NRC ##########################
#summarise positive/negative sentiment words per review
rrSenti_nrc<-rrTokens %>% inner_join(get_sentiments("nrc"), by="word") %>%
   mutate(new_sentiment = ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative', 'surprise'), "negative","positive"))
revSenti_nrc <- rrSenti_nrc %>% group_by(review_id, stars) %>% summarise(nwords=n(),posSum=sum(new_sentiment=='positive'), negSum=sum(new_sentiment=='negative'))
revSenti_nrc<- revSenti_nrc %>% mutate(posProp=posSum/nwords, negProp=negSum/nwords)
revSenti_nrc<- revSenti_nrc %>% mutate(sentiScore=posProp-negProp)

#Do review star ratings correspond to the the positive/negative sentiment words
revSenti_nrc %>% group_by(stars) %>% summarise(avgPos=mean(posProp), avgNeg=mean(negProp), avgSentiSc=mean(sentiScore))
###### surprise is used here as a negative word #######
#### and 2nd star rating is coming as positive
rrSenti_nrc %>% group_by(stars,sentiment) %>% count()
revSenti_nrc <- revSenti_nrc %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 )))
revSenti_nrc <- revSenti_nrc %>% mutate(pred_hiLo=ifelse(sentiScore >0, 1, -1))
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
xx<-revSenti_nrc %>% filter(hiLo!=0)
confusionMatrix(as.factor(xx$pred_hiLo),as.factor(xx$hiLo))
###76.78 - accuracy
######## performing worse than other dictionaries ###############

#####################  For Afinn ##########################
#summarise positive/negative sentiment words per review
revSenti_afinn <- rrSenti_afinn %>% group_by(review_id, stars) %>% summarise(nwords=n(),sentiSum=sum(value))
revSenti_afinn %>% group_by(stars) %>%
   summarise(avgLen=mean(nwords), avgSenti=mean(sentiSum))
##### Considering 1 and 2 as low , 3 and 4 as high
revSenti_afinn <- revSenti_afinn %>% mutate(hiLo=ifelse(stars<=2,-1, ifelse(stars>=4, 1, 0 )))
revSenti_afinn <- revSenti_afinn %>% mutate(pred_hiLo=ifelse(sentiSum >0, 1, -1))
#filter out the reviews with 3 stars, and get the confusion matrix for hiLo vs pred_hiLo
xx<-revSenti_afinn %>% filter(hiLo!=0)
confusionMatrix(as.factor(xx$pred_hiLo),as.factor(xx$hiLo))
### 81.92 - accuracy


########## afinn is performing slightly better than bing and nrc worst.

################## d part

######################## For bing ###########################
#use pivot_wider to convert to a dtm form where each row is for a review and columns correspond to words   (https://tidyr.tidyverse.org/reference/pivot_wider.html)
#revDTM_sentiBing <- rrSenti_bing %>%  pivot_wider(id_cols = review_id, names_from = word, values_from = tf_idf)

#####using tf_idf
#Or, since we want to keep the stars column
rrSenti_bing<- rrTokens %>% inner_join(get_sentiments("bing"), by="word")
revDTM_sentiBing <- rrSenti_bing %>%  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf)  %>% ungroup()

#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and this grouping is retained by default, and can cause problems in the later steps

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentiBing <- revDTM_sentiBing %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#how many review with 1, -1  'class'
revDTM_sentiBing %>% group_by(hiLo) %>% tally()

#develop a random forest model to predict hiLo from the words in the reviews

library(ranger)

#replace all the NAs with 0
revDTM_sentiBing<-revDTM_sentiBing %>% replace(., is.na(.), 0)

revDTM_sentiBing$hiLo<- as.factor(revDTM_sentiBing$hiLo)

####took 10k data for modelling
library(rsample)
nr<-nrow(revDTM_sentiBing)
set.seed(25)
revDTM_index<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review <- revDTM_sentiBing[revDTM_index, ]
nr <- nrow(revDTM_review)
set.seed(25)
revDTM_index2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_sentiBing_trn <- revDTM_review[revDTM_index2, ]
revDTM_sentiBing_tst <- revDTM_review[-revDTM_index2, ]


#########################RF MODEL

#running a grid search for best hyperparameters.
#hyper_grid <- expand.grid(
#   num.trees = seq(200,700,100),
 #  OOB_RMSE   = 0
#)

#system.time(
   #for(i in 1:nrow(hyper_grid)) {
      # train model
   #   rfModel <- ranger(
      #   dependent.variable.name = "hiLo", 
     #    data           = revDTM_sentiBing_trn %>% select(-review_id),
    #     num.trees      = hyper_grid$num.trees[i],
   #      importance = 'impurity')
      # add OOB error to grid
  #    hyper_grid$OOB_RMSE[i] <- sqrt(rfModel$prediction.error)
 #  })
#position = which.min(hyper_grid$OOB_RMSE)
#ntree=600	OOBERROR : 0.349

rfModel1<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiBing_trn %>% dplyr::select(-review_id), num.trees = 600, importance='permutation',probability = T,max.depth = 6)

#which variables are important
importance(rfModel1) %>% view()

#Obtain predictions, and calculate performance
revSentiBing_predTrn<- predict(rfModel1, revDTM_sentiBing_trn %>% select(-review_id))$predictions
revSentiBing_predTst<- predict(rfModel1, revDTM_sentiBing_tst %>% select(-review_id))$predictions

table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_predTrn[,2]>0.5)
table(actual=revDTM_sentiBing_tst$hiLo, preds=revSentiBing_predTst[,2]>0.5)
#Q - is 0.5 the best threshold to use here?  Can find the optimal threshold from the     ROC analyses

library(pROC)
rocTrn <- roc(revDTM_sentiBing_trn$hiLo, revSentiBing_predTrn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentiBing_tst$hiLo, revSentiBing_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
        col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
table(actual=revDTM_sentiBing_trn$hiLo, preds=revSentiBing_predTrn[,2]>as.numeric(bThr))
table(actual=revDTM_sentiBing_tst$hiLo, preds=revSentiBing_predTst[,2]>as.numeric(bThr))

auc(as.numeric(revDTM_sentiBing_trn$hiLo),as.numeric(revSentiBing_predTrn[,2]))
auc(as.numeric(revDTM_sentiBing_tst$hiLo),as.numeric(revSentiBing_predTst[,2]))


############################Naive-Bayes model
library(e1071)
nbModel1<-naiveBayes(hiLo ~ ., data=revDTM_sentiBing_trn %>% select(-review_id), laplace = 1)

revSentiBing_NBpredTrn<-predict(nbModel1, revDTM_sentiBing_trn, type = "class")
revSentiBing_NBpredTst<-predict(nbModel1, revDTM_sentiBing_tst, type = "class")

auc(as.numeric(revDTM_sentiBing_trn$hiLo), as.numeric(revSentiBing_NBpredTrn))
auc(as.numeric(revDTM_sentiBing_tst$hiLo), as.numeric(revSentiBing_NBpredTst))
table(revDTM_sentiBing_trn$hiLo, revSentiBing_NBpredTrn)
table(revDTM_sentiBing_tst$hiLo, revSentiBing_NBpredTst)
table(revDTM_sentiBing_tst$hiLo, revSentiBing_NBpredTst)  

############################ Multinomial bayes #########################
library(naivebayes)
revDTM_sentiBing_mul<- rrSenti_bing %>%  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = n)  %>% ungroup()
revDTM_sentiBing_mul <- revDTM_sentiBing_mul %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#replace all the NAs with 0
revDTM_sentiBing_mul<-revDTM_sentiBing_mul %>% replace(., is.na(.), 0)
revDTM_sentiBing_mul$hiLo<- as.factor(revDTM_sentiBing_mul$hiLo)

####took 10k data for modelling
library(rsample)
nr<-nrow(revDTM_sentiBing_mul)
set.seed(25)
revDTM_index<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review <- revDTM_sentiBing_mul[revDTM_index, ]
nr <- nrow(revDTM_review)
set.seed(25)
revDTM_index2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_sentiBing_mul_trn <- revDTM_review[revDTM_index2, ]
revDTM_sentiBing_mul_tst <- revDTM_review[-revDTM_index2, ]

x_train = revDTM_sentiBing_mul_trn %>% dplyr::select(-review_id,-hiLo)
y_train = revDTM_sentiBing_mul_trn %>% dplyr::select(hiLo)
x_tst = revDTM_sentiBing_mul_tst %>% dplyr::select(-review_id,-hiLo)
y_tst = revDTM_sentiBing_mul_tst %>% dplyr::select(hiLo)
y_train <- as.factor(as.matrix(y_train))
library(Matrix)
x_train <- Matrix(as.matrix(x_train), sparse = TRUE)

nb2 <- multinomial_naive_bayes(x_train, y_train, laplace=1)
#summary(mnb1)
pred_nb2_train <- predict(nb2,x_train)
table(y_train,pred_nb2_train)
#           pred_nb2_train
#act       -1    1
#     -1    1315 413
#      1    196 5076

confusionMatrix(y_train, pred_nb2_train)

x_tst <- Matrix(as.matrix(x_tst), sparse = TRUE)
y_tst <- as.factor(as.matrix(y_tst))
pred_nb2_tst <- predict(nb2,x_tst)
confusionMatrix(y_tst, pred_nb2_tst)
#Prediction   -1    1
#-1  548  188
#1   121 2143

auc(as.numeric(pred_nb2_train),as.numeric(y_train))
auc(as.numeric(pred_nb2_tst),as.numeric(y_tst))

############################Lasso###############
library(glmnet)
xd <-revDTM_sentiBing_trn %>% dplyr::select(-review_id,-hiLo)
yd <- revDTM_sentiBing_trn %>% dplyr::select(hiLo)
test_xd <- revDTM_sentiBing_tst %>% dplyr::select(-review_id,-hiLo)
test_yd <- revDTM_sentiBing_tst %>% dplyr::select(hiLo)
ls <- cv.glmnet(as.matrix(xd),as.matrix(yd),alpha =1 ,type.measure = ("auc"), nfolds = 10, family="binomial")
ls
plot(ls)
# lamda which gives minimum cross-validated error
ls$lambda.min
#lamda which gives the most regularized model having error within 1 std error of the min error
ls$lambda.1se
assess.glmnet(ls,as.matrix(xd),as.matrix(yd), family = c("binomial"))
Predtrain = predict(ls, as.matrix(xd), type="class")
confusionMatrix(as.factor(as.matrix(yd)), as.factor(Predtrain))

Predtst = predict(ls, as.matrix(test_xd), type="class")
confusionMatrix(as.factor(as.matrix(test_yd)),as.factor(Predtst), positive = "1")

auc(as.numeric(Predtrain),as.numeric(as.matrix(yd)))
auc(as.numeric(Predtst),as.numeric(as.matrix(test_yd)))

################ Bing completed #################################
###################################### NRC Dictionary ################################################
#with "nrc" dictionary
memory.limit(2000000)
rrSenti_nrc<-rrTokens %>% inner_join(get_sentiments("nrc"), by="word") %>%
   select(-sentiment)%>% unique()
#One hot encoding: choose if want tf, idf, tf-idf or count(n)
#revDTM_sentinrc <- rrSenti_nrc %>%  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = n)  %>% ungroup()
revDTM_sentinrc <- rrSenti_nrc %>% pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf) %>% ungroup()

#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and this grouping is retained by default, and can cause problems in the later steps

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentinrc <- revDTM_sentinrc %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#how many review with 1, -1  'class'
revDTM_sentinrc %>% group_by(hiLo) %>% tally()

#develop a random forest model to predict hiLo from the words in the reviews

library(ranger)

#replace all the NAs with 0
revDTM_sentinrc<-revDTM_sentinrc %>% replace(., is.na(.), 0)

revDTM_sentinrc$hiLo<- as.factor(revDTM_sentinrc$hiLo)

####took 10k data for modelling
library(rsample)
nr<-nrow(revDTM_sentinrc)
revDTM_index_nrc<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review_nrc <- revDTM_sentinrc[revDTM_index_nrc, ]
nr <- nrow(revDTM_review_nrc)
revDTM_index_nrc2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_sentinrc_trn <- revDTM_review_nrc[revDTM_index_nrc2, ]
revDTM_sentinrc_tst <- revDTM_review_nrc[-revDTM_index2, ]

################RF Model
#running a grid search for best hyperparameters.
#hyper_grid <- expand.grid(
#   num.trees = seq(100,700,100),
#   OOB_RMSE   = 0
#)

#system.time(
#   for(i in 1:nrow(hyper_grid)) {
#      # train model
#      rfModel2 <- ranger(
#         dependent.variable.name = "hiLo", 
#         data           = revDTM_sentinrc_trn %>% select(-review_id),
#         num.trees      = hyper_grid$num.trees[i],
#         importance = 'impurity')
      # add OOB error to grid
#      hyper_grid$OOB_RMSE[i] <- sqrt(rfModel2$prediction.error)
#   })
#position = which.min(hyper_grid$OOB_RMSE)
#ntree=400	OOBERROR : 0.373

rfModel2<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentinrc_trn %>% dplyr::select(-review_id), num.trees = 400,importance = 'permutation',probability = T)
#which variables are important
importance(rfModel2) %>% view()

#Obtain predictions, and calculate performance
revSentinrc_predTrn<- predict(rfModel2, revDTM_sentinrc_trn %>% select(-review_id))$predictions
revSentinrc_predTst<- predict(rfModel2, revDTM_sentinrc_tst %>% select(-review_id))$predictions
table(actual=revDTM_sentinrc_trn$hiLo, preds=revSentinrc_predTrn[,2]>0.5)
table(actual=revDTM_sentinrc_tst$hiLo, preds=revSentinrc_predTst[,2]>0.5)
#Q - is 0.5 the best threshold to use here?  Can find the optimal threshold from the     ROC analyses

library(pROC)
rocTrn <- roc(revDTM_sentinrc_trn$hiLo, revSentinrc_predTrn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentinrc_tst$hiLo, revSentinrc_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
table(actual=revDTM_sentinrc_trn$hiLo, preds=revSentinrc_predTrn[,2]>as.numeric(bThr))
table(actual=revDTM_sentinrc_tst$hiLo, preds=revSentinrc_predTst[,2]>as.numeric(bThr))

auc(as.numeric(revDTM_sentinrc_trn$hiLo),as.numeric(revSentinrc_predTrn[,2]))
auc(as.numeric(revDTM_sentinrc_tst$hiLo),as.numeric(revSentinrc_predTst[,2]))


############################ Multinomial bayes #########################
library(naivebayes)
library(caret)
revDTM_sentinrc_mul<- rrSenti_nrc %>%  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = n)  %>% ungroup()
revDTM_sentinrc_mul <- revDTM_sentinrc_mul %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#replace all the NAs with 0
revDTM_sentinrc_mul<-revDTM_sentinrc_mul %>% replace(., is.na(.), 0)
revDTM_sentinrc_mul$hiLo<- as.factor(revDTM_sentinrc_mul$hiLo)

####took 10k data for modelling
library(rsample)
nr<-nrow(revDTM_sentinrc_mul)
set.seed(25)
revDTM_index<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review <- revDTM_sentinrc_mul[revDTM_index, ]
nr <- nrow(revDTM_review)
set.seed(25)
revDTM_index2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_sentinrc_mul_trn <- revDTM_review[revDTM_index2, ]
revDTM_sentinrc_mul_tst <- revDTM_review[-revDTM_index2, ]

x_train = revDTM_sentinrc_mul_trn %>% dplyr::select(-review_id,-hiLo)
y_train = revDTM_sentinrc_mul_trn %>% dplyr::select(hiLo)
x_tst = revDTM_sentinrc_mul_tst %>% dplyr::select(-review_id,-hiLo)
y_tst = revDTM_sentinrc_mul_tst %>% dplyr::select(hiLo)
y_train <- as.factor(as.matrix(y_train))
library(Matrix)
x_train <- Matrix(as.matrix(x_train), sparse = TRUE)

nb2 <- multinomial_naive_bayes(x_train, y_train, laplace=1)
#summary(mnb1)
pred_nb2_train <- predict(nb2,x_train)
table(y_train,pred_nb2_train)
#           pred_nb2_train
#act       -1    1
#     -1    1323 449
#      1    264 4964

confusionMatrix(y_train, pred_nb2_train)
x_tst <- Matrix(as.matrix(x_tst), sparse = TRUE)
y_tst <- as.factor(as.matrix(y_tst))
pred_nb2_tst <- predict(nb2,x_tst)
confusionMatrix(y_tst, pred_nb2_tst)
#Prediction   -1    1
#-1  520  216
#1   144 2120

auc(as.numeric(pred_nb2_train),as.numeric(y_train))
auc(as.numeric(pred_nb2_tst),as.numeric(y_tst))

############################Lasso###############
library(glmnet)
xd <-revDTM_sentinrc_trn %>% dplyr::select(-review_id,-hiLo)
yd <- revDTM_sentinrc_trn %>% dplyr::select(hiLo)
test_xd <- revDTM_sentinrc_tst %>% dplyr::select(-review_id,-hiLo)
test_yd <- revDTM_sentinrc_tst %>% dplyr::select(hiLo)
ls <- cv.glmnet(as.matrix(xd),as.matrix(yd),alpha =1 ,type.measure = ("auc"), nfolds = 10, family="binomial")
ls
plot(ls)
# lamda which gives minimum cross-validated error
ls$lambda.min
#lamda which gives the most regularized model having error within 1 std error of the min error
ls$lambda.1se
assess.glmnet(ls,as.matrix(xd),as.matrix(yd), family = c("binomial"))
Predtrain = predict(ls, as.matrix(xd), type="class")
confusionMatrix(as.factor(as.matrix(yd)), as.factor(Predtrain))

Predtst = predict(ls, as.matrix(test_xd), type="class")
confusionMatrix(as.factor(as.matrix(test_yd)),as.factor(Predtst), positive = "1")

auc(as.numeric(Predtrain),as.numeric(as.matrix(yd)))
auc(as.numeric(Predtst),as.numeric(as.matrix(test_yd)))

################ NRC completed #################################

###################################################AFINN Model#########################################################
#with AFINN dictionary words....following similar steps as above, but noting that AFINN assigns negative to positive sentiment value for words matching the dictionary

#with "AFINN" dictionary
rrSenti_afinn<- rrTokens %>% inner_join(get_sentiments("afinn"), by="word")
#One hot encoding: choose if want tf, idf, tf-idf or count(n)

revDTM_sentiafinn <- rrSenti_afinn %>% pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf) %>% ungroup()

#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and this grouping is retained by default, and can cause problems in the later steps

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentiafinn <- revDTM_sentiafinn %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#how many review with 1, -1  'class'
revDTM_sentiafinn %>% group_by(hiLo) %>% tally()

#develop a random forest model to predict hiLo from the words in the reviews

library(ranger)

#replace all the NAs with 0
revDTM_sentiafinn<-revDTM_sentiafinn %>% replace(., is.na(.), 0)

revDTM_sentiafinn$hiLo<- as.factor(revDTM_sentiafinn$hiLo)

####took 10k data for modelling
library(rsample)
nr<-nrow(revDTM_sentiafinn)
revDTM_index_afinn<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review_afinn <- revDTM_sentiafinn[revDTM_index_afinn, ]
nr <- nrow(revDTM_review_afinn)
revDTM_index_afinn2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_sentiafinn_trn <- revDTM_review_afinn[revDTM_index_afinn2, ]
revDTM_sentiafinn_tst <- revDTM_review_afinn[-revDTM_index2, ]

################RF Model
#running a grid search for best hyperparameters.
#hyper_grid <- expand.grid(
 #  num.trees = seq(100,700,100),
  # OOB_RMSE   = 0
#)

#system.time(
 #  for(i in 1:nrow(hyper_grid)) {
      # train model
  #    rfModel3 <- ranger(
   #      dependent.variable.name = "hiLo", 
    #     data           = revDTM_sentiafinn_trn %>% select(-review_id),
     #    num.trees      = hyper_grid$num.trees[i],
      #   importance = 'impurity')
      # add OOB error to grid
   #   hyper_grid$OOB_RMSE[i] <- sqrt(rfModel3$prediction.error)
   #})
#position = which.min(hyper_grid$OOB_RMSE)
#ntree=400	OOBERROR : 0.3621

rfModel3<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiafinn_trn %>% dplyr::select(-review_id), num.trees = 400,importance = 'permutation',probability = T)
#which variables are important
importance(rfModel3) %>% view()

#Obtain predictions, and calculate performance
revSentiafinn_predTrn<- predict(rfModel3, revDTM_sentiafinn_trn %>% select(-review_id))$predictions
revSentiafinn_predTst<- predict(rfModel3, revDTM_sentiafinn_tst %>% select(-review_id))$predictions
table(actual=revDTM_sentiafinn_trn$hiLo, preds=revSentiafinn_predTrn[,2]>0.5)
table(actual=revDTM_sentiafinn_tst$hiLo, preds=revSentiafinn_predTst[,2]>0.5)

library(pROC)
rocTrn <- roc(revDTM_sentiafinn_trn$hiLo, revSentiafinn_predTrn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentiafinn_tst$hiLo, revSentiafinn_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
table(actual=revDTM_sentiafinn_trn$hiLo, preds=revSentiafinn_predTrn[,2]>as.numeric(bThr))
table(actual=revDTM_sentiafinn_tst$hiLo, preds=revSentiafinn_predTst[,2]>as.numeric(bThr))

auc(as.numeric(revDTM_sentiafinn_trn$hiLo),as.numeric(revSentiafinn_predTrn[,2]))
auc(as.numeric(revDTM_sentiafinn_tst$hiLo),as.numeric(revSentiafinn_predTst[,2]))


############################ Multinomial bayes #########################
library(naivebayes)
library(caret)
revDTM_sentiafinn_mul<- rrSenti_afinn %>%  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = n)  %>% ungroup()
revDTM_sentiafinn_mul <- revDTM_sentiafinn_mul %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#replace all the NAs with 0
revDTM_sentiafinn_mul<-revDTM_sentiafinn_mul %>% replace(., is.na(.), 0)
revDTM_sentiafinn_mul$hiLo<- as.factor(revDTM_sentiafinn_mul$hiLo)

####took 10k data for modelling
library(rsample)
nr<-nrow(revDTM_sentiafinn_mul)
set.seed(25)
revDTM_index<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review <- revDTM_sentiafinn_mul[revDTM_index, ]
nr <- nrow(revDTM_review)
set.seed(25)
revDTM_index2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_sentiafinn_mul_trn <- revDTM_review[revDTM_index2, ]
revDTM_sentiafinn_mul_tst <- revDTM_review[-revDTM_index2, ]

x_train = revDTM_sentiafinn_mul_trn %>% dplyr::select(-review_id,-hiLo)
y_train = revDTM_sentiafinn_mul_trn %>% dplyr::select(hiLo)
x_tst = revDTM_sentiafinn_mul_tst %>% dplyr::select(-review_id,-hiLo)
y_tst = revDTM_sentiafinn_mul_tst %>% dplyr::select(hiLo)
y_train <- as.factor(as.matrix(y_train))
library(Matrix)
x_train <- Matrix(as.matrix(x_train), sparse = TRUE)

nb2 <- multinomial_naive_bayes(x_train, y_train, laplace=1)
#summary(mnb1)
pred_nb2_train <- predict(nb2,x_train)
table(y_train,pred_nb2_train)
#           pred_nb2_train
#act       -1    1
#     -1    1153 545
#      1    242 5060

confusionMatrix(y_train, pred_nb2_train)
x_tst <- Matrix(as.matrix(x_tst), sparse = TRUE)
y_tst <- as.factor(as.matrix(y_tst))
pred_nb2_tst <- predict(nb2,x_tst)
confusionMatrix(y_tst, pred_nb2_tst)
#Prediction   -1    1
#-1  492  279
#1   115 2114

auc(as.numeric(pred_nb2_train),as.numeric(y_train))
auc(as.numeric(pred_nb2_tst),as.numeric(y_tst))

############################Lasso###############
library(glmnet)
xd <-revDTM_sentiafinn_trn %>% dplyr::select(-review_id,-hiLo)
yd <- revDTM_sentiafinn_trn %>% dplyr::select(hiLo)
test_xd <- revDTM_sentiafinn_tst %>% dplyr::select(-review_id,-hiLo)
test_yd <- revDTM_sentiafinn_tst %>% dplyr::select(hiLo)
ls <- cv.glmnet(as.matrix(xd),as.matrix(yd),alpha =1 ,type.measure = ("auc"), nfolds = 10, family="binomial")
ls
plot(ls)
# lamda which gives minimum cross-validated error
ls$lambda.min
#lamda which gives the most regularized model having error within 1 std error of the min error
ls$lambda.1se
assess.glmnet(ls,as.matrix(xd),as.matrix(yd), family = c("binomial"))
Predtrain = predict(ls, as.matrix(xd), type="class")
confusionMatrix(as.factor(as.matrix(yd)), as.factor(Predtrain))

Predtst = predict(ls, as.matrix(test_xd), type="class")
confusionMatrix(as.factor(as.matrix(test_yd)),as.factor(Predtst), positive = "1")

auc(as.numeric(Predtrain),as.numeric(as.matrix(yd)))
auc(as.numeric(Predtst),as.numeric(as.matrix(test_yd)))

################ Afinn completed #################################

######################### All dictionary ######################################################


#with "ALL" dictionary
rrSenti_all<- rrTokens %>% left_join(get_sentiments("bing"), by="word") %>%
   left_join(get_sentiments("nrc"), by="word") %>%
   left_join(get_sentiments("afinn"), by="word")

######### Taking only words present in dictionary ###############
rrSenti_all <- rrSenti_all[!is.na(rrSenti_all$sentiment.x)&!is.na(rrSenti_all$sentiment.y)&!is.na(rrSenti_all$value),] %>%
   select(-sentiment.y) %>% unique()
#One hot encoding: choose if want tf, idf, tf-idf or count(n)

revDTM_sentiall <- rrSenti_all %>% pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = tf_idf) %>% ungroup()

#Note the ungroup() at the end -- this is IMPORTANT;  we have grouped based on (review_id, stars), and this grouping is retained by default, and can cause problems in the later steps

#filter out the reviews with stars=3, and calculate hiLo sentiment 'class'
revDTM_sentiall <- revDTM_sentiall %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#how many review with 1, -1  'class'
revDTM_sentiall %>% group_by(hiLo) %>% tally()

#develop a random forest model to predict hiLo from the words in the reviews

library(ranger)

#replace all the NAs with 0
revDTM_sentiall<-revDTM_sentiall %>% replace(., is.na(.), 0)

revDTM_sentiall$hiLo<- as.factor(revDTM_sentiall$hiLo)

####took 10k data for modelling
library(rsample)
nr<-nrow(revDTM_sentiall)
revDTM_index_all<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review_all <- revDTM_sentiall[revDTM_index_all, ]
nr <- nrow(revDTM_review_all)
revDTM_index_all2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_sentiall_trn <- revDTM_review_all[revDTM_index_all2, ]
revDTM_sentiall_tst <- revDTM_review_all[-revDTM_index2, ]

################# RF Model
#running a grid search for best hyperparameters.
#hyper_grid <- expand.grid(
 #  num.trees = seq(100,700,100),
  # OOB_RMSE   = 0
#)

#system.time(
 #  for(i in 1:nrow(hyper_grid)) {
      # train model
 #     rfModel4 <- ranger(
  #       dependent.variable.name = "hiLo", 
   #      data           = revDTM_sentiall_trn %>% select(-review_id),
    #     num.trees      = hyper_grid$num.trees[i],
    #     importance = 'impurity')
      # add OOB error to grid
    #  hyper_grid$OOB_RMSE[i] <- sqrt(rfModel4$prediction.error)
#   })
#position = which.min(hyper_grid$OOB_RMSE)
#ntree=400	OOBERROR : 0.3856

rfModel4<-ranger(dependent.variable.name = "hiLo", data=revDTM_sentiall_trn %>% dplyr::select(-review_id), num.trees = 400,importance = 'permutation',probability = T)
#which variables are important
importance(rfModel4) %>% view()

#Obtain predictions, and calculate performance
revSentiall_predTrn<- predict(rfModel4, revDTM_sentiall_trn %>% select(-review_id))$predictions
revSentiall_predTst<- predict(rfModel4, revDTM_sentiall_tst %>% select(-review_id))$predictions
table(actual=revDTM_sentiall_trn$hiLo, preds=revSentiall_predTrn[,2]>0.5)
table(actual=revDTM_sentiall_tst$hiLo, preds=revSentiall_predTst[,2]>0.5)

library(pROC)
rocTrn <- roc(revDTM_sentiall_trn$hiLo, revSentiall_predTrn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_sentiall_tst$hiLo, revSentiall_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
table(actual=revDTM_sentiall_trn$hiLo, preds=revSentiall_predTrn[,2]>as.numeric(bThr))
table(actual=revDTM_sentiall_tst$hiLo, preds=revSentiall_predTst[,2]>as.numeric(bThr))

auc(as.numeric(revDTM_sentiall_trn$hiLo),as.numeric(revSentiall_predTrn[,2]))
auc(as.numeric(revDTM_sentiall_tst$hiLo),as.numeric(revSentiall_predTst[,2]))


############################ Multinomial bayes #########################
library(naivebayes)
library(caret)
revDTM_sentiall_mul<- rrSenti_all %>%  pivot_wider(id_cols = c(review_id,stars), names_from = word, values_from = n)  %>% ungroup()
revDTM_sentiall_mul <- revDTM_sentiall_mul %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#replace all the NAs with 0
revDTM_sentiall_mul<-revDTM_sentiall_mul %>% replace(., is.na(.), 0)
revDTM_sentiall_mul$hiLo<- as.factor(revDTM_sentiall_mul$hiLo)

####took 10k data for modelling
library(rsample)
nr<-nrow(revDTM_sentiall_mul)
set.seed(25)
revDTM_index<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review <- revDTM_sentiall_mul[revDTM_index, ]
nr <- nrow(revDTM_review)
set.seed(25)
revDTM_index2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_sentiall_mul_trn <- revDTM_review[revDTM_index2, ]
revDTM_sentiall_mul_tst <- revDTM_review[-revDTM_index2, ]

x_train = revDTM_sentiall_mul_trn %>% dplyr::select(-review_id,-hiLo)
y_train = revDTM_sentiall_mul_trn %>% dplyr::select(hiLo)
x_tst = revDTM_sentiall_mul_tst %>% dplyr::select(-review_id,-hiLo)
y_tst = revDTM_sentiall_mul_tst %>% dplyr::select(hiLo)
y_train <- as.factor(as.matrix(y_train))
library(Matrix)
x_train <- Matrix(as.matrix(x_train), sparse = TRUE)

nb2 <- multinomial_naive_bayes(x_train, y_train, laplace=1)
#summary(mnb1)
pred_nb2_train <- predict(nb2,x_train)
table(y_train,pred_nb2_train)
#           pred_nb2_train
#act       -1    1
#     -1    1088 607
#      1    275 5030

confusionMatrix(y_train, pred_nb2_train)
x_tst <- Matrix(as.matrix(x_tst), sparse = TRUE)
y_tst <- as.factor(as.matrix(y_tst))
pred_nb2_tst <- predict(nb2,x_tst)
confusionMatrix(y_tst, pred_nb2_tst)
#Prediction   -1    1
#-1  449  242
#1   146 2163

auc(as.numeric(pred_nb2_train),as.numeric(y_train))
auc(as.numeric(pred_nb2_tst),as.numeric(y_tst))

############################Lasso###############
library(glmnet)
xd <-revDTM_sentiall_trn %>% dplyr::select(-review_id,-hiLo)
yd <- revDTM_sentiall_trn %>% dplyr::select(hiLo)
test_xd <- revDTM_sentiall_tst %>% dplyr::select(-review_id,-hiLo)
test_yd <- revDTM_sentiall_tst %>% dplyr::select(hiLo)
ls <- cv.glmnet(as.matrix(xd),as.matrix(yd),alpha =1 ,type.measure = ("auc"), nfolds = 10, family="binomial")
ls
plot(ls)
# lamda which gives minimum cross-validated error
ls$lambda.min
#lamda which gives the most regularized model having error within 1 std error of the min error
ls$lambda.1se
assess.glmnet(ls,as.matrix(xd),as.matrix(yd), family = c("binomial"))
Predtrain = predict(ls, as.matrix(xd), type="class")
confusionMatrix(as.factor(as.matrix(yd)), as.factor(Predtrain))

Predtst = predict(ls, as.matrix(test_xd), type="class")
confusionMatrix(as.factor(as.matrix(test_yd)),as.factor(Predtst), positive = "1")

auc(as.numeric(Predtrain),as.numeric(as.matrix(yd)))
auc(as.numeric(Predtst),as.numeric(as.matrix(test_yd)))

################ All dictionaries done #################################

######################### Using Broader terms ######################################################
#### Use terms other than dictionaries ######
rrTokens[rrTokens$stars==3,] %>% nrow()

rWords<-rrTokens %>% group_by(word) %>% summarise(nr=n()) %>% arrange(desc(nr))
reduced_rWords <- rWords %>% filter(nr< 6000 & nr > 30)
reduced_rrTokens <- left_join(reduced_rWords, rrTokens)
reduced_rrTokens <- reduced_rrTokens %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1))
revDTM <- reduced_rrTokens %>% pivot_wider(id_cols = c(review_id,stars), names_from = word,
                                           values_from = tf_idf) %>% ungroup()

## calculate hiLo sentiment 'class'
revDTM <- revDTM %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#how many review with 1, -1  'class'
revDTM %>% group_by(hiLo) %>% tally()

#develop a random forest model to predict hiLo from the words in the reviews

#replace all the NAs with 0
revDTM<-revDTM %>% replace(., is.na(.), 0)

revDTM$hiLo<- as.factor(revDTM$hiLo)

####took 10k data for modeling
nr<-nrow(revDTM)
revDTM_index_all<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review_all <- revDTM[revDTM_index_all, ]
nr <- nrow(revDTM_review_all)
revDTM_index_all2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_trn <- revDTM_review_all[revDTM_index_all2, ]
revDTM_tst <- revDTM_review_all[-revDTM_index_all2, ]

################# RF Model
#running a grid search for best hyperparameters.
#hyper_grid <- expand.grid(
 #  num.trees = seq(100,700,100),
 #  OOB_RMSE   = 0
#)

#system.time(
 #  for(i in 1:nrow(hyper_grid)) {
      # train model
  #    rfModel5 <- ranger(
   #      dependent.variable.name = "hiLo", 
   #      data           = revDTM_trn %>% select(-review_id),
    #     num.trees      = hyper_grid$num.trees[i],
     #    importance = 'impurity')
      # add OOB error to grid
   #   hyper_grid$OOB_RMSE[i] <- sqrt(rfModel5$prediction.error)
   #})
#position = which.min(hyper_grid$OOB_RMSE)
#ntree=300	OOBERROR : 0.334

rfModel5<-ranger(dependent.variable.name = "hiLo", data=revDTM_trn %>% dplyr::select(-review_id), num.trees = 300,importance = 'permutation',probability = T, max.depth =10)

#which variables are important
importance(rfModel5) %>% view()

#Obtain predictions, and calculate performance
rev_predTrn<- predict(rfModel5, revDTM_trn %>% select(-review_id))$predictions
rev_predTst<- predict(rfModel5, revDTM_tst %>% select(-review_id))$predictions
table(actual=revDTM_trn$hiLo, preds=rev_predTrn[,2]>0.5)
table(actual=revDTM_tst$hiLo, preds=rev_predTst[,2]>0.5)

library(pROC)
rocTrn <- roc(revDTM_trn$hiLo, rev_predTrn[,2], levels=c(-1, 1))
rocTst <- roc(revDTM_tst$hiLo, rev_predTst[,2], levels=c(-1, 1))

plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
       col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#Best threshold from ROC analyses
bThr<-coords(rocTrn, "best", ret="threshold", transpose = FALSE)
table(actual=revDTM_trn$hiLo, preds=rev_predTrn[,2]>as.numeric(bThr))
table(actual=revDTM_tst$hiLo, preds=rev_predTst[,2]>as.numeric(bThr))

auc(as.numeric(revDTM_trn$hiLo),as.numeric(rev_predTrn[,2]))
auc(as.numeric(revDTM_tst$hiLo),as.numeric(rev_predTst[,2]))


############################ Multinomial bayes #########################
library(naivebayes)
library(caret)
revDTM_mul<- reduced_rrTokens %>% pivot_wider(id_cols = c(review_id,stars), names_from = word,
                                              values_from = n) %>% ungroup()

revDTM_mul <- revDTM_mul %>% filter(stars!=3) %>% mutate(hiLo=ifelse(stars<=2, -1, 1)) %>% dplyr::select(-stars)

#replace all the NAs with 0
revDTM_mul<-revDTM_mul %>% replace(., is.na(.), 0)
revDTM_mul$hiLo<- as.factor(revDTM_mul$hiLo)

####took 10k data for modelling
library(rsample)
nr<-nrow(revDTM_mul)
set.seed(25)
revDTM_index<- sample(1:nr, size = 10000, replace=FALSE)
revDTM_review <- revDTM_mul[revDTM_index, ]
nr <- nrow(revDTM_review)
set.seed(25)
revDTM_index2<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
revDTM_mul_trn <- revDTM_review[revDTM_index2, ]
revDTM_mul_tst <- revDTM_review[-revDTM_index2, ]

x_train = revDTM_mul_trn %>% dplyr::select(-review_id,-hiLo)
y_train = revDTM_mul_trn %>% dplyr::select(hiLo)
x_tst = revDTM_mul_tst %>% dplyr::select(-review_id,-hiLo)
y_tst = revDTM_mul_tst %>% dplyr::select(hiLo)
y_train <- as.factor(as.matrix(y_train))
library(Matrix)
x_train <- Matrix(as.matrix(x_train), sparse = TRUE)

nb2 <- multinomial_naive_bayes(x_train, y_train, laplace=1)
#summary(mnb1)
pred_nb2_train <- predict(nb2,x_train)
confusionMatrix(y_train, pred_nb2_train)
x_tst <- Matrix(as.matrix(x_tst), sparse = TRUE)
y_tst <- as.factor(as.matrix(y_tst))
pred_nb2_tst <- predict(nb2,x_tst)
confusionMatrix(y_tst, pred_nb2_tst)
#Prediction   -1    1
#-1  584  142
#1   162 2112

auc(as.numeric(pred_nb2_train),as.numeric(y_train))
auc(as.numeric(pred_nb2_tst),as.numeric(y_tst))

############################Lasso###############
library(glmnet)
xd <-revDTM %>% dplyr::select(-review_id,-hiLo)
yd <- revDTM %>% dplyr::select(hiLo)
test_xd <- revDTM_tst %>% dplyr::select(-review_id,-hiLo)
test_yd <- revDTM_tst %>% dplyr::select(hiLo)
ls <- cv.glmnet(as.matrix(xd),as.matrix(yd),alpha =1 ,type.measure = ("auc"), nfolds = 10, family="binomial")
ls
plot(ls)
# lamda which gives minimum cross-validated error
ls$lambda.min
#lamda which gives the most regularized model having error within 1 std error of the min error
ls$lambda.1se
assess.glmnet(ls,as.matrix(xd),as.matrix(yd), family = c("binomial"))
Predtrain = predict(ls, as.matrix(xd), type="class")
confusionMatrix(as.factor(as.matrix(yd)), as.factor(Predtrain))

Predtst = predict(ls, as.matrix(test_xd), type="class")
confusionMatrix(as.factor(as.matrix(test_yd)),as.factor(Predtst), positive = "1")

auc(as.numeric(Predtrain),as.numeric(as.matrix(yd)))
auc(as.numeric(Predtst),as.numeric(as.matrix(test_yd)))

################ Broader terms #################################
