### Natural Language Processing ### 

#---------------------------------------------------------#
# NLP in R

'
Analyze texts (books, reviews, html webpages)
We will predict reviews and predict of sentiment is positive or negative

Can apply this to 
- predict genre of a book, song
- analyze webpages

Best to utilize tsv files (tab separated values)
'

# Importing the dataset
setwd("~/Personal/OneDrive/ML A-Z Python & R/code-and-data/Machine Learning A-Z (Codes and Datasets)/Part 7 - Natural Language Processing/Section 36 - Natural Language Processing/R")
dataset_original = read.delim("Restaurant_Reviews.tsv", 
                     quote = '',  #ignore anything thats in quotes
                     stringsAsFactors = FALSE)  #to avoid identifying reviews as factors
#---------------------------------------------------------#
# Cleaning the text
# We want to clean data to get most relevant words (regarding sentiment)
# initialize a corpus
library(tm)
library(SnowballC)  #used to be for stopwords() function
corpus = tm::VCorpus(VectorSource(dataset$Review))

# convert all to lowercase to make sure we have one instance for each word
corpus = tm::tm_map(corpus, 
                    tm::content_transformer(tolower))

# want to remove all numbers wherever it appears (most of the time irrelevant)
corpus = tm::tm_map(corpus, removeNumbers)

# want to remove all punctuation
corpus = tm::tm_map(corpus, removePunctuation)

# want to remove all irrelevant words
corpus = tm::tm_map(corpus, removeWords, stopwords())

# want to now stem (find root of each word)
corpus = tm::tm_map(corpus, stemDocument)

# want to remove extra spaces from previous steps
corpus = tm::tm_map(corpus, stripWhitespace)
#---------------------------------------------------------#
# Build sparse matrix of features
dtm = tm::DocumentTermMatrix(corpus)

# filter non-frequent words to reduce sparsity
dtm = tm::removeSparseTerms(dtm, 0.999)
#---------------------------------------------------------#
# Build Bag of Words classification model
'
For NLP classification we use:
- Naive Bayes
- Decision Trees
- Random Forest (will go with this)
'

# convert matrix to dataframe; also need to make sure we have expected matrix type
dataset = as.data.frame(as.matrix(dtm))
dataset$Liked = dataset_original$Liked

# Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest to the Training set
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
cm

nlpAccuracy = (77 + 82) / (82 + 77 + 18 + 23)  # 0.795
nlpPrecision = 77 / (77 + 18)  # 0.81
nlpRecall = 77 / (77 + 23)  # 0.77
nlpF1 = 2 * nlpPrecision * nlpRecall / (nlpPrecision + nlpRecall)  # 0.79

################################################################################

'
Homework
Hello students,

congratulations for having completed Part 7 - Natural Language Processing.

If you are up for some practical activities, here is a little challenge:

1. Run the other classification models we made in Part 3 - Classification, other than the one we used in the last tutorial.

2. Evaluate the performance of each of these models. Try to beat the Accuracy obtained in the tutorial. 
But remember, Accuracy is not enough, so you should also look at other performance metrics like 
Precision (measuring exactness), Recall (measuring completeness) and the F1 Score (compromise between Precision and Recall). 
Please find below these metrics formulas (TP = # True Positives, TN = # True Negatives, FP = # False Positives, FN = # False Negatives):

Accuracy = (TP + TN) / (TP + TN + FP + FN)

Precision = TP / (TP + FP)

Recall = TP / (TP + FN)

F1 Score = 2 * Precision * Recall / (Precision + Recall)

3. Try even other classification models that we havent covered in Part 3 - Classification. Good ones for NLP include:
  
  CART
  C5.0
  Maximum Entropy

'

### Attempting with Naive Bayes ### 
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# no need to scale

# Fit Naive Bayes to the training set
library(e1071)
classifier = naiveBayes(x = training_set[-692],
                        y = training_set$Liked)

y_pred = predict(classifier, newdata = test_set[-692])
y_pred

cm = table(test_set$Liked, y_pred)
cm

nbAccuracy = (91 + 3) / (200)  # 0.47
nbPrecision = 94 / (97 + 91)  # 0.5
nbRecall = 94 / (91 + 9)  # 0.94

nbF1 = 2 * nbPrecision * nbRecall / (nbPrecision + nbRecall)  # 0.65

'
https://medium.com/@klintcho/explaining-precision-and-recall-c770eb9c69e9

https://scikit-learn.org/stable/auto_examples/model_selection/plot_precision_recall.html

This happens bc classes are very imbalanced. We need to put more weight on the label (dataset$Liked) 
High recall but low precision means that most of its predicted labels are incorrect compared to training labels.

'

################################################################################

### Decision Trees ### 
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Decision Tree Classifier to the Training set
library(rpart)
classifier = rpart(formula = Liked ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])  # returns matrix of 2 probabilities bc classifier is decision tree 
y_pred

'notice that sum of cells (rows) = 1 as they are probabilities'

y_pred = predict(classifier, newdata = test_set[-692], type = 'class')  # returns vector which is what we want

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
cm

dtAccuracy = (52 + 98) / 200  # 0.75
dtPrecision = 52 / (52 + 2)  # .96
dtRecall = 52 / (52 + 48)  # 0.52 

dtF1 = 2 * dtPrecision * dtRecall / (dtPrecision + dtRecall)  # 0.67

'
high precision typically means low recall rate
'