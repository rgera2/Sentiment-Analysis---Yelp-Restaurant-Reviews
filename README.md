# Sentiment-Analysis---Yelp-Restaurant-Reviews

# Background

This project involved the text mining of user-review data and sentiment analyses. It is based on a collection of reviews and accompanying star ratings from Yelp. A sample of the original dataset (over 4 million review by over a million users for 144K businesses) was used here, to keep the assignment task manageable.
We examined the effectiveness of different sentiment ‘dictionaries’, and developed and evaluated the classification models to help predict sentiment polarity (negative, positive).
The star ratings were used here to indicate the sentiment label. For binary classification, we converted the 1-5 scale rating values to {positive(1), negative(0)} values.

# Dataset
Full data is available from https://www.yelp.com/dataset_challenge

The data was given in json files. The reviews data file contained the reviews and includes reviewID, businessID, businessName, the review text, star rating and other attributes. I pre-processed the data to get the business type, review text, star rating, and how many users found this review to be cool, funny, useful, into a single file.

# Analysis

Proper treatment of text data was done initially and TF-IDF was used instead of Term frequency. I used 3 different dictionaries - NRC, Bing and Afinn. NRC library provides a list of
words denoting different sentiment whereas Bing specifies a list of positive and negative words.
AFINN on the other hand provides a list of words with each word being associated with a value
from -5 to +5.

# Model

Five types of data was used - 3 for each dictionary, 1 for no dictionary and 1 for a combination of all.

Models built for each set of data were - Random Forest, Multinomial Naive Bayes and Lasso regression.

# Evaluation

Evaluation metric used was Test Accuracy. The model which performed the best was Random Forest using NRC
dictionary and the second best was Random Forest using AFINN dictionary while also
considering sensitivity-specificity ratio.
