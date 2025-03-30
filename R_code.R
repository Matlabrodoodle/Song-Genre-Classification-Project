
### Song Genre Bag of Words Classification Project ###

# Written by Alvito DiStefano


# Million Song Datasets sourced from:

# http://millionsongdataset.com/musixmatch/
# http://millionsongdataset.com/lastfm/

# SMS Spam Collection sourced from:

# https://archive.ics.uci.edu/dataset/228/sms+spam+collection


## NOTE ##

# This code is designed to run without any preexisting R libraries or data downloaded,
# and consequently it will take a long time to run (expect at least an hour).
# Due to the size of the data it is advised to run the code on a machine with at least
# 32 GB of RAM. 





# Install necessary libraries

if(!require(data.table)){
  install.packages("data.table", repos = "http://cran.us.r-project.org")
}

if(!require(tidyverse)){
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
}

if(!require(caret)){
  install.packages("caret", repos = "http://cran.us.r-project.org")
}

if(!require(tictoc)){
  install.packages("tictoc", repos = "http://cran.us.r-project.org")
}

if(!require(doSNOW)){
  install.packages("doSNOW", repos = "http://cran.us.r-project.org")
}

if(!require(parallel)){
  install.packages("parallel", repos = "http://cran.us.r-project.org")
}

if(!require(quanteda)){
  install.packages("quanteda", repos = "http://cran.us.r-project.org")
}

if(!require(RSQLite)){
  install.packages("RSQLite", repos = "http://cran.us.r-project.org")
}

if(!require(DBI)){
  install.packages("DBI", repos = "http://cran.us.r-project.org")
}


# Load necessary libraries
library(data.table)

library(tidyverse)

library(caret)

library(tictoc)

library(doSNOW)

library(parallel)

library(quanteda)

library(RSQLite)

library(DBI)


# Set seed to a completely arbitrary number
set.seed(8675309)


## FUNCTIONS ##

# All function used in preparing or altering data are defined here


# Function to count the number of instances each word occurs in the input dataset

Get_Word_Totals <- function(data,start_col) {
  
  # Create data table to store totals
  word_totals <- as.data.table(matrix(NA_real_,nrow=1,ncol=ncol(data)))
  
  # Set names equal to song data
  setnames(word_totals,names(data))
  
  # Sum counts of each word in song data
  for (i in start_col:ncol(data)) {
    
    word_totals[1,i] <- sum(data[[i]])
    
  }
  
  return (word_totals)
}


# Function to perform TF-IDF transformation for bag of words data
# If incl_IDF is FALSE then it only performs the TF operation

TF_IDF <- function(data,incl_IDF,start_col) {
  
  # Create inverse doc frequency term, if desired
  
  # Capture starting column count 
  N_Col <- ncol(data)
  
  # Calculate IDF if desired
  if(incl_IDF){
    
    N_Row <- nrow(data)
    
    dfreq <- sapply(data,function(col) sum(col!=0))
    
    # Calculate IDF with log base 10
    idf <- log10(N_Row/(dfreq+1))
    
  }
  
  # Add column for total word count in each track
  new_col_name <- "total_words"
  
  # Sum total words in each track, startin with first bag of words column
  data[,(new_col_name):=rowSums(.SD),.SDcols=start_col:N_Col]#(ncol(data)-1)]
  
  # Convert from absolute word counts to word term-frequencies
  data[,(start_col:N_Col):=
              lapply(.SD,function(x)x/data[[ncol(data)]]),
            .SDcols = start_col:N_Col]
  
  # Remove totals columns as it is no longer needed
  data[,ncol(data)] <- NULL
  
  # Multiply term frequencies by IDF factors, if desired
  if(incl_IDF){
    
  data[,start_col:ncol(data)] <- 
    mapply("*",data[,start_col:ncol(data)],
           idf[start_col:length(idf)],
           SIMPLIFY=FALSE)
  
  }
  
  return (data)
  
}


# Function remove data columns with low or zero variance
# Samples data randomly size samp_div for expediency
# nearZeroVar cutoff parameters also input to function

Remove_NZV <- function(data, start_col,samp_div,fcut,ucut) {
  
  # Ensure data is a dataframe 
  setDF(data)
  
  # Sample portion of data at random for expediency
  idx <- sample(seq_len(nrow(data)),size=floor(nrow(data)/samp_div),replace=FALSE)
  
  data_sample <- data[idx,start_col:ncol(data)]
  
  # Use caret's nearZeroVar with specified input parameters
  nZV <- nearZeroVar(data_sample,freqCut = fcut,uniqueCut = ucut)
  
  # Because data_sample's index is offset from the input data we must adjust accordingly
  offset <- start_col - 1
  nZV <- nZV + offset
  
  # If zero variance values are found, drop the corresponding columns
  if (length(nZV)>0) {
    
    data <- data[,-nZV,drop=FALSE]
    
  }
  
  return(data)
  
}


# This function takes an input genre and sample size and samples positive
# cases of that genre of that size and combines it with an equal number
# of negative cases for a more or less 50/50 split

Create_Genre_Sample <- function(data,genre_name,size,genre_col){
  
  # Split data into Y and N cases by desired genre
  data_Y <- data[data[[genre_name]]=="Y",]
  data_N <- data[data[[genre_name]]=="N",]
  
  # Randomly sample desired amount of positive/negative cases
  sample_Y <- data_Y[sample(nrow(data_Y),size),]
  sample_N <- data_N[sample(nrow(data_N),size),]
  
  # Combine into a single data table and shuffle
  data_sample <- rbind(sample_Y,sample_N)
  data_sample <- data_sample[sample(nrow(data_sample)),]
  
  # Drop all other genre columns except the one at index genre_col
  # which corresponds to genre_name
  drop_cols <- setdiff(2:9,genre_col)
  data_sample <- data_sample[,-drop_cols]
  
  return(data_sample)
  
}


## MILLION SONG DATASET RETRIEVAL ##


tic("Song Data Retrieval")

# The MSD is not in an immediately usable form and requires some pre-processing steps

# Download MSD datasets

# Increase timeout to half an hour to allow ample time for downloads
options(timeout=1800)

# Download musiXmatch data (their server is slow so this will take around 15 minutes)
download.file(
  
  url = "http://millionsongdataset.com/sites/default/files/AdditionalFiles/mxm_dataset.db",
  destfile = "mxm_dataset.db",
  mode = "wb"
  
)

# Download LastFM data
download.file(
  
  url = "http://labrosa.ee.columbia.edu/~dpwe/tmp/lastfm_tags.db",
  destfile = "lastfm.db",
  mode = "wb"
  
)

# Certain tracks are noted as having matching errors on the MSD website
# These are retrieved from the link below and will be removed from training data
track_errors <- readLines("http://millionsongdataset.com/sites/default/files/tasteprofile/sid_mismatches.txt")

# Since there is no delimiter we manually extract the track IDs from this data using sub()
track_errors <- sub(".*<(.*?)>.*", "\\1", track_errors)
track_errors <- sub(".* ","",track_errors)

track_errors <-read.table(text=track_errors,header=FALSE)

# Connect to the LastFM database file
con <- dbConnect(RSQLite::SQLite(),"lastfm.db")

# Concatenate the LastFM tag and track information into one dataframe
lastfm_concatenated <- dbGetQuery(con, "
SELECT
  tid_tag.tid,
  tid_tag.tag,
  tid_tag.val,
  LOWER(tags.tag) AS tagname,
  tids.tid  AS trackId
FROM tid_tag

JOIN tags
  ON tid_tag.tag = tags.ROWID
  
JOIN tids
  ON tid_tag.tid = tids.ROWID
")

# Disconnect from LastFM database
dbDisconnect(con)

# Create temporary tables for tracks identified as specific genres
# Regexes minimize false identification of genres from LastFM tags
# For example, "underground rap" will register as rap but "trap classics" will not
indie_tracks <- lastfm_concatenated %>%
  filter(str_detect(tagname, "(^|[[:space:]-])indie([[:space:]-]|$)")) %>%
  distinct(trackId)

rock_tracks <- lastfm_concatenated %>%
  filter(str_detect(tagname, "(^|[[:space:]-])rock([[:space:]-]|$)")) %>%
  distinct(trackId)

country_tracks <- lastfm_concatenated %>%
  filter(str_detect(tagname, "(^|[[:space:]-])country([[:space:]-]|$)")) %>%
  distinct(trackId)

blues_tracks <- lastfm_concatenated %>%
  filter(str_detect(tagname, "(^|[[:space:]-])blues([[:space:]-]|$)")) %>%
  distinct(trackId)

pop_tracks <- lastfm_concatenated %>%
  filter(str_detect(tagname, "(^|[[:space:]-])pop([[:space:]-]|$)")) %>%
  distinct(trackId)

metal_tracks <- lastfm_concatenated %>%
  filter(str_detect(tagname, "(^|[[:space:]-])metal([[:space:]-]|$)")) %>%
  distinct(trackId)

punk_tracks <- lastfm_concatenated %>%
  filter(str_detect(tagname, "(^|[[:space:]-])punk([[:space:]-]|$)")) %>%
  distinct(trackId)

rap_tracks <- lastfm_concatenated %>%
  filter(str_detect(tagname, "(^|[[:space:]-])rap([[:space:]-]|$)")) %>%
  distinct(trackId)

# Connect to the musiXmatch database
con <- dbConnect(RSQLite::SQLite(),"mxm_dataset.db")

# Write data frames to the musiXmatch database for further querying
dbWriteTable(con, "lastfm_concatenated", lastfm_concatenated, overwrite = TRUE)
dbWriteTable(con, "track_errors", track_errors, overwrite = TRUE)
dbWriteTable(con, "indie_tracks", indie_tracks, overwrite = TRUE)
dbWriteTable(con, "rock_tracks", rock_tracks, overwrite = TRUE)
dbWriteTable(con, "country_tracks", country_tracks, overwrite = TRUE)
dbWriteTable(con, "blues_tracks", blues_tracks, overwrite = TRUE)
dbWriteTable(con, "pop_tracks", pop_tracks, overwrite = TRUE)
dbWriteTable(con, "metal_tracks", metal_tracks, overwrite = TRUE)
dbWriteTable(con, "punk_tracks", punk_tracks, overwrite = TRUE)
dbWriteTable(con, "rap_tracks", rap_tracks, overwrite = TRUE)

# Create table joining common tracks between LastFM and musiXmatch
# Exclude track IDs noted for having errors on the MSD website
dbExecute(con,"
CREATE TABLE lastfm_mxm_combined AS

SELECT 
  lyrics.count, 
  lyrics.track_id, 
  lyrics.word
FROM lyrics

INNER JOIN (
  SELECT DISTINCT trackId
  FROM lastfm_concatenated
) AS distinct_tracks
  ON distinct_tracks.trackId = lyrics.track_id
  
LEFT OUTER JOIN track_errors
  ON track_errors.V1 = lyrics.track_id
WHERE track_errors.V1 IS NULL;
")

# Add columns to main table for genre categoricals (Y/N)
dbExecute(con, "ALTER TABLE lastfm_mxm_combined ADD COLUMN is_indie TEXT")
dbExecute(con, "ALTER TABLE lastfm_mxm_combined ADD COLUMN is_rock TEXT")
dbExecute(con, "ALTER TABLE lastfm_mxm_combined ADD COLUMN is_country TEXT")
dbExecute(con, "ALTER TABLE lastfm_mxm_combined ADD COLUMN is_blues TEXT")
dbExecute(con, "ALTER TABLE lastfm_mxm_combined ADD COLUMN is_pop TEXT")
dbExecute(con, "ALTER TABLE lastfm_mxm_combined ADD COLUMN is_metal TEXT")
dbExecute(con, "ALTER TABLE lastfm_mxm_combined ADD COLUMN is_punk TEXT")
dbExecute(con, "ALTER TABLE lastfm_mxm_combined ADD COLUMN is_rap TEXT")

dbExecute(con, "
UPDATE lastfm_mxm_combined
SET
    is_country = CASE
        WHEN track_id IN (SELECT trackId FROM country_tracks) THEN 'Y'
        ELSE 'N'
    END,
    is_rock = CASE
        WHEN track_id IN (SELECT trackId FROM rock_tracks) THEN 'Y'
        ELSE 'N'
    END,
    is_pop = CASE
        WHEN track_id IN (SELECT trackId FROM pop_tracks) THEN 'Y'
        ELSE 'N'
    END,
    is_indie = CASE
        WHEN track_id IN (SELECT trackId FROM indie_tracks) THEN 'Y'
        ELSE 'N'
    END,
    is_metal = CASE
        WHEN track_id IN (SELECT trackId FROM metal_tracks) THEN 'Y'
        ELSE 'N'
    END,
    is_punk = CASE
        WHEN track_id IN (SELECT trackId FROM punk_tracks) THEN 'Y'
        ELSE 'N'
    END,
    is_blues = CASE
        WHEN track_id IN (SELECT trackId FROM blues_tracks) THEN 'Y'
        ELSE 'N'
    END,
    is_rap = CASE
        WHEN track_id IN (SELECT trackId FROM rap_tracks) THEN 'Y'
        ELSE 'N'
    END;
")

# Store results in a dataframe
song_data <- dbGetQuery(con, "SELECT * FROM lastfm_mxm_combined")

# Disconnect from musiXmatch database
dbDisconnect(con)
rm(con)

rm(lastfm_concatenated)

# Pivot bag-of-words lyrics and convert to data.table type
song_data <- pivot_wider(song_data,names_from = "word",values_from = "count")
setDT(song_data)

# Replace NAs with zeroes
song_data[is.na(song_data)] <- 0

# Clean up column names and make them unique
names(song_data) <- make.names(names(song_data),unique=TRUE)

toc()

# Show data head
head(song_data)


## EDA ##


# Explore how many songs are in the dataset
paste("Total tracks in dataset:",nrow(song_data))

# Use function to extract total word appearances as array
word_totals <- as.matrix(Get_Word_Totals(song_data,10))

# Drop NA values
word_totals <- word_totals[,apply(word_totals,2,function(x)!any(is.na(x)))]

# Get indices and sort largest to smallest
word_indices <- order(word_totals,decreasing=TRUE)

word_totals <- word_totals[word_indices]

# Explore word frequencies in song data
paste("The most common word is",names(word_totals)[which.max(word_totals)])

paste("The least common word is",names(word_totals)[which.min(word_totals)])

par(mgp=c(3,.7,0))

# Plot the most and least common words as bar plots
barplot(word_totals[1:20],
        col="orange",
        ylab="Word Frequency",
        las=2,
        main="Most Common Words",
        cex.axis= .6,
        cex.names = 0.8)

barplot(word_totals[(length(word_totals)-19):length(word_totals)],
        col="orange",
        ylab="Word Frequency",
        las=2,
        main="Least Common Words",
        cex.names = 0.8)

# Plot the percentage genre representation for each genre among all tracks
genre_percents <- 100*colMeans(song_data[,2:9]=="Y")

barplot(genre_percents,
        col="orange",
        ylim = c(0,max(genre_percents)+10),
        ylab = "Genre Percent Representation in Song Data",
        names.arg=tools::toTitleCase(substring(colnames(song_data)[2:9],4)),
        main = "Genre Percentage in Data",
        )


## SMS DATA PREPARATION ## 


# Prepare SMS baseline data
# Download SMS data from the UC Irvine repository
download.file(
  
  url = "https://archive.ics.uci.edu/static/public/228/sms+spam+collection.zip",
  destfile = "sms_data.zip",
  mode = "wb"
  
)

# Unzip SMS data
unzip("sms_data.zip",exdir = "data_unzipped")

# Prepare SMS baseline data
SMS_data <- read.table(
  "data_unzipped/SMSSpamCollection",
  sep = "\t",
  header = FALSE,
  stringsAsFactors = FALSE,
  quote = "",             
  fill = TRUE,            
  encoding = "UTF-8"      
)

# Set V1 (labels) as factor
SMS_data$V1 <- as.factor(SMS_data$V1)

# Print frequencies of spam and ham classes
print(prop.table(table(SMS_data$V1)))

# Rename columns Labels and Message
names(SMS_data)[names(SMS_data)=="V1"] <- "Labels"
names(SMS_data)[names(SMS_data)=="V2"] <- "Message"

# Tokenize text message data into individual words
# Remove all numbers, punctuation, symbols, and hyphens
train_tokens <- tokens(SMS_data$Message,what="word",
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)                     

# Set words to lowercase
train_tokens <- tokens_tolower(train_tokens)

# Remove common stopwords from the list of tokens
train_tokens <- tokens_select(train_tokens,stopwords(language="en"),selection="remove")

# Stem the tokens
train_tokens <- tokens_wordstem(train_tokens,language="english")

# Convert text tokens into document feature matrix
train_tokens_dfm <- dfm(train_tokens, tolower = FALSE)
train_tokens_matrix <- as.matrix(train_tokens_dfm)

# Convert DFM to dataframe and bind to message labels
SMS_data <- cbind(Label=SMS_data$Labels,
                  convert(train_tokens_dfm,to="data.frame"))

# Convert to data.table
setDT(SMS_data)

# Drop texts which do not retain any words
SMS_data <- SMS_data[rowSums(SMS_data[,3:ncol(SMS_data)])!=0,]

# Remove train tokens
rm(train_tokens)
rm(train_tokens_dfm)
rm(train_tokens_matrix)

# Rename column titled "data" (otherwise it will cause errors)
names(SMS_data)[names(SMS_data)=="data"]<-"data_d"

# Clean up column names and make them unique
names(SMS_data)<-make.names(names(SMS_data),unique=TRUE)


## TRAINING AND HOLDOUT DATASET PREPARATION ##


# Implement TF-IDF weights by calling the function above
tic("Implement TF-IDF")

song_data <- TF_IDF(song_data,TRUE,10)
SMS_data <- TF_IDF(SMS_data,TRUE,3)

toc()

# Reduce data dimensionality by removing columns with low variance
# Note this is one instance where the song and SMS data are treated differently
# as different inputs to NearZeroVar are necessary to reduce dimensionality
# by an acceptable amount (the goal being to reduce size by roughly two thirds)
tic("Remove NZV")

song_data <- Remove_NZV(song_data,10,100,50,0.7)
SMS_data <- Remove_NZV(SMS_data,3,10,50,0.1)

toc()

# Replace NAs with zeroes
song_data[is.na(song_data)] <- 0
SMS_data[is.na(SMS_data)] <- 0

# Create training and testing data partitions for song data
samp_size <- floor(0.8*nrow(song_data))
train_index <- sample(seq_len(nrow(song_data)),size=samp_size)

# Create train and test datasets from song data
train_data <- song_data[train_index,]

test_data <- song_data[-train_index,]

# Create training and testing data partitions for SMS data
samp_size_SMS <- floor(0.8*nrow(SMS_data))
SMS_train_index <- sample(seq_len(nrow(SMS_data)),size=samp_size_SMS)

# Create baseline SMS train and test sets
SMS_train_data <- SMS_data[SMS_train_index,]

SMS_test_data <- SMS_data[-SMS_train_index,]

# Remove original datasets to save memory
rm(song_data)
rm(SMS_data)

# Determine genre with the fewest positive cases
# This will be used to determine how many rows to sample
# To maintain consistency across all genres
least_genre <- min(
  sum(train_data$is_indie=="Y"),
  sum(train_data$is_rock=="Y"),
  sum(train_data$is_country=="Y"),
  sum(train_data$is_blues=="Y"),
  sum(train_data$is_pop=="Y"),
  sum(train_data$is_metal=="Y"),
  sum(train_data$is_punk=="Y"),
  sum(train_data$is_rap=="Y"))

# Create training samples of each genre enforcing an even split between
# positive and negative case each of size least_genre
train_indie <- Create_Genre_Sample(train_data,"is_indie",least_genre,2)
train_rock <- Create_Genre_Sample(train_data,"is_rock",least_genre,3)
train_country <- Create_Genre_Sample(train_data,"is_country",least_genre,4)
train_blues <- Create_Genre_Sample(train_data,"is_blues",least_genre,5)
train_pop <- Create_Genre_Sample(train_data,"is_pop",least_genre,6)
train_metal <- Create_Genre_Sample(train_data,"is_metal",least_genre,7)
train_punk <- Create_Genre_Sample(train_data,"is_punk",least_genre,8)
train_rap <- Create_Genre_Sample(train_data,"is_rap",least_genre,9)

# Drop full train data set as we no longer need it
rm(train_data)

genre_list <- list()
genre_list <- c(
 genre_list, 
  "indie",
  "rock",
  "country",
  "blues",
  "pop",
  "metal",
  "punk",
  "rap"
)


## MODEL TRAINING ##


# Convert to dataframe 
setDF(SMS_train_data)

# Create cross validation folds from training data using caret
SMS_CV_folds <- createMultiFolds(SMS_train_data$Label,k=10,times=3)
SMS_CV_control <- trainControl(method="repeatedcv",number=10,repeats=3,index=SMS_CV_folds,search="random")

# Allocate additional CPU cores using doSNOW and parallel libraries
# Dynamically count cores and use all except one to avoid overloading
num_cores <- detectCores()
CL <- makeCluster(num_cores[1]-1,type="SOCK")

# Register cluster
registerDoSNOW(CL)

# WARNING: Due to the size of data training may take quite some time
# (This will of course depend on the size of your core cluster)
tic("SMS Model Training")

# Train a ham-spam classifier using SMS data to serve as a performance baseline
SMS_Classifier <- train(Label~.,
                        data=SMS_train_data[,-c(2)], # Drop ID column
                        method="rpart",
                        metric="Kappa",
                        trControl=SMS_CV_control,
                        tuneLength=10)
toc()


tic("Genre Classifier Training")

# Loop through each song genre in the previously created list
for (genre in genre_list){
  
  # Create proper format to access genre training data and label column
  Label_name <- paste0("is_",genre)
  Data_name <- paste0("train_",genre)
  
  # Name classifier for this genre
  Model_name <- paste(genre,"classifier")
  
  # Retrieve training dataframe
  genre_data <- get(Data_name)
  
  # Create cross validation folds from training data
  CV_folds <- createMultiFolds(genre_data[[Label_name]],k=10,times=3)
  CV_control <- trainControl(method="repeatedcv",number=10,repeats=3,index=CV_folds,search="random") 
  
  # Train rpart classifier to identify this genre
  Label_formula <- as.formula(paste0(Label_name," ~."))
  
  Genre_Classifier <- train(Label_formula,
                          data=genre_data[,-c(1)], # Drop track ID column
                          method="rpart",
                          metric="Kappa",
                          trControl=CV_control,
                          tuneLength=10)
  
  # Rename classifier for this specific genre
  assign(Model_name,Genre_Classifier,envir=.GlobalEnv)
  
  # Remove fold variables and loop
  rm(CV_folds)
  rm(CV_control)
  
}

toc()

# Training is complete so now the cluster can be removed
stopCluster(CL)


## RESULTS ##


tic("Generate results")
# Create dataframe to store results
results_table <- data.frame(
  Case = character(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  F1Score = numeric()
)

# Print SMS modeling results
print(SMS_Classifier)

print(table(predict(SMS_Classifier,type="raw")))

# Create confusion matrix comparing predicted and actual values
SMS_preds <- predict(SMS_Classifier,newdata = SMS_test_data)
SMS_actual <- as.factor(SMS_test_data$Label)

cm_SMS <- confusionMatrix(data=SMS_preds,
                          reference = SMS_actual,
                          positive="spam",
                          mode="prec_recall")
print(cm_SMS)

# Add SMS results to summary table
results_table <- rbind(results_table,
      data.frame(Case="SMS Baseline",
                 Accuracy = cm_SMS$overall["Accuracy"],
                 Kappa = cm_SMS$overall["Kappa"],
                 Precision = cm_SMS$byClass["Precision"],
                 Recall = cm_SMS$byClass["Recall"],
                 F1Score = cm_SMS$byClass["F1"])
      )

# Create confusion matrices for each genre and add results to table
for (genre in genre_list){
  
  # Retrieve model name by referencing genre
  classifier_name <- paste(genre,"classifier")
  Label_name <- paste0("is_",genre)
  classifier <- get(classifier_name)
  
  # Create confusion matrix comparing predicted and actual values
  genre_preds <- predict(classifier,newdata = test_data)
  genre_actual <- as.factor(test_data[[Label_name]])
  
  cm_genre <- confusionMatrix(data=genre_preds,
                              reference=genre_actual,
                              positive = "Y",
                              mode="prec_recall")
  
  # Show confusion matrix
  print(paste(genre, "confusion matrix:"))
  print(cm_genre)
  
  # Add results to results dataframe
  results_table <- rbind(results_table,
                         data.frame(Case=genre,
                                    Accuracy = cm_genre$overall["Accuracy"],
                                    Kappa = cm_genre$overall["Kappa"],
                                    Precision = cm_genre$byClass["Precision"],
                                    Recall = cm_genre$byClass["Recall"],
                                    F1Score = cm_genre$byClass["F1"])
                        )
  
}

# Print final results
print(results_table,row.names = FALSE)

toc()
