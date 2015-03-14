# ***************************************************************************************
# ***************************************************************************************
# ************************ Analyzing Yelp, Inc. Business Reviews ************************ 
# *********** Written by David W. Vinson and Rick Dale for the purpose of demonstration *
# ******************** PLEASE DO NOT DISTRIBUTE WITHOUT PERMISSION **********************  
# ***************************************************************************************
# ***************************************************************************************

# This script analyzes Yelp.Inc data to extract 'information' measures for reviews

###### loading / setting directories #####
library(rjson) #extracting Yelp, Inc reviews from json files
library(tm) #text editing
library(RWeka) #for NgramTokenizer
library(entropy) #simple entropy measures

setwd('~/Documents/yelp_dataset_mil/')

#load reviews 
revs = read.csv('/Users/Dave/Documents/yelp_dataset_mil/yelp_academic_dataset_review.json',sep='\n',quote = "")
colnames(revs) = list('u')  
revs$u = as.character(revs$u)

##### load frequency distributions  
#Note: distributions were built in python (see code here: )

#unigram distribution
across_uni_matrix <- read.table('unis.csv', sep=',', header=FALSE) 
uni_full <- as.matrix(across_uni_matrix[, -1]) 
row.names(uni_full) <- across_uni_matrix[, 1] 

#bigram distribution
across_bigram_matrix <- read.table('bigs.csv', sep=',', header=FALSE)
big_rows = as.matrix(paste(across_bigram_matrix$V1,across_bigram_matrix$V2, sep=' '))   
big_full <- as.matrix(across_bigram_matrix[,-(1:2)]) 
row.names(big_full) <- big_rows[,1] 

########### variables ##########################
business = c() #the business ID
user_id = c() #the user ID
stars = c() #star rating
useful = c() #reader rating
funny = c() #reader rating
cool= c() #reader rating
date = c() #date of review (in seconds)
len = c() #word count
diversity = c() #unique words/total word count
uni_ent = c() #unigram entropy per reviewer
big_ent = c() #bigram entropy per reviewer
uni  = c() #unigram information (across reviews)
big =  c() #bigram information (across reviews)
univar = c() #unigram channel capacity (variability over review)
bigvar = c() #bigram channel capacity (variability over review)

########### Loop through reviews ###############
for (i in 1:length(revs$u)) { 
  print (i)
  rev = fromJSON(revs$u[i])

  #meta data 
  user_id = c(user_id,rev$user_id)
  business = c(business,rev$business_id)
  date = c(date,as.numeric(as.POSIXct(rev$date)))
  stars  =c(stars,rev$stars)
  useful = c(useful,rev$votes$useful) #reader rating
  funny = c(funny,rev$votes$funny) #reader rating
  cool= c(cool,rev$votes$cool) #reader rating
  
  ## clean text
  revtext = rev$text
  revtext = tolower(rev$text)
  ts = Corpus(VectorSource(revtext))
  ts <- tm_map(ts, removeWords, stopwords("english"))  # remove stopwords
  ts <- tm_map(ts, stripWhitespace)   # eliminate extra whitespace
  removepunct <- function(x) { return(gsub("[[:punct:]]","",x)) } 
  ts <- tm_map(ts, removepunct)   # eliminate punctuation
  removenum <- function(x) { return(gsub("[0-9]","",x)) }
  ts <- tm_map(ts, removenum)   # eliminate numbers
  revtext = sapply(ts, '[', 1) 
  
  #basic text measures
  len = c(len,length(MC_tokenizer(revtext))) 
  diversity = c(diversity,length(unique(MC_tokenizer(revtext)))/length(MC_tokenizer(revtext)))
   
  #within review unigrams
  df <- data.frame(V1 = revtext, stringsAsFactors = FALSE)
  corp <- Corpus(DataframeSource(df))
  unigram <- TermDocumentMatrix(corp)
  uni_as_matrix = data.matrix(unigram, rownames.force = NA)
  uni_ent = c(uni_ent,entropy(uni_as_matrix)) #unigram entropy
   
  #within review bigrams
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  bigrams <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
  bi_as_matrix = data.matrix(bigrams, rownames.force = NA)
  big_ent =  c(big_ent,entropy(bi_as_matrix)) #bigram entropy
  
  #across review unigrams
  rows.to.keep<-which(rownames(uni_full) %in% rownames(uni_as_matrix))
  across_uni_mat_net = as.matrix(uni_full[rows.to.keep,]) 
  uni = c(uni,mean(-log2(across_uni_mat_net/sum(uni_full)))) #across review unigram
 
  #across bigrams
  rows.to.keep.bigs<-which(rownames(big_full) %in% rownames(bi_as_matrix)) 
  across_big_mat_net = as.matrix(big_full[rows.to.keep.bigs,]) 
  big = c(big,mean(-log2(across_big_mat_net/sum(big_full)))) #across review bigram
  
  #chan caps UID measures
  univar =  c(univar,sd(-1*log2(across_uni_mat_net/length(across_uni_mat_net)))) #across review unigram variability 
  bigvar =  c(bigvar,sd(-1*log2(across_big_mat_net/length(across_big_mat_net)))) #across review bigram variability 
}

results = data.frame(user_id,business,date,stars,useful,funny,cool,len,diversity,uni_ent,big_ent,uni,big,univar,bigvar)
write.csv(tip_results, file = "review_info_results.csv", row.names = FALSE)


