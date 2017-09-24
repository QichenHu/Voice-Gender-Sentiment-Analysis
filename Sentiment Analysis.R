#Part 3:Sentiment Analysis

#######3.1: install and load needed packages
#install.packages("tm")
#install.packages("stringr")
library(tm) # Text mining
library(stringr)

######3.2: Set positive and negative dictionary
anasheet <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)#Read the text files
names(anasheet) <- c('vocab', 'slevel')#Give two column names 
attach(anasheet)
nlevel=ifelse(slevel<=0,0,1) 
anasheet$slevel=nlevel#We only analyze negative and positive words and ingore levels based on dataset,so remove the neural words from the original dictionary 
Neg <- anasheet$vocab[anasheet$slevel==0]
Pos<-anasheet$vocab[anasheet$slevel==1]#split the negative words and positive words

######3.3: Text Mining
comvoice <- read.csv("/Users/qichenhu/comvoice.csv")
#View(comvoice)
attach(comvoice) #load and attach the transcript dataset of the former voice
TRANSCRIPT<-gsub('[[:punct:]]', '', TRANSCRIPT) #remove punctuations
TRANSCRIPT<-gsub('\\d+', '', TRANSCRIPT) #remove digits
TRANSCRIPT <- tolower(TRANSCRIPT) #Make sure all the words are in lower case
comvoice$TRANSCRIPT<-TRANSCRIPT  #Use new content replace the original one
#comvoice[33,]$TRANSCRIPT
######## 3.4: Analyze each row of transcription and make prediction based on the negative and positive words
Predict=vector()
for (i in 1:nrow(comvoice))
{
  TRANSList <- str_split(comvoice[i,]$TRANSCRIPT, '\\s+')
  TRANSCRIPT <- unlist(TRANSList) #Split all words in each row
  Posti <- match(TRANSCRIPT, Pos)
  Nega<-match(TRANSCRIPT,Neg)  #Check all the sentiment words in each row
  Posnum<-sum(ifelse(!is.na(Posti),Posti,0))
  Negnum<-sum(ifelse(!is.na(Nega),Nega,0)) #sum the number of negative and positive words
  score<-ifelse(Posnum>Negnum,1,0) #Guess the sentiment of each row based on the positive and negative words
  Predict=c(Predict,score) #Get all the predictions of each row
}

######3.5: Get the predict accuracy
senti<-cbind(comvoice,Predict)
senti2<-senti[c("Predict","Sentiment")] #Get the predict and actual sentiment data frame
#View(senti2)
res_pred<-table(senti2)
prop.table(res_pred)  # Take a look at probability and number of actual and predict sentiment


