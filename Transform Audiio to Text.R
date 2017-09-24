
#####2.1: Install and Load packages#########
#install.packages("tuneR")
#install.packages("ProjectTemplate")
#install.packages("transcribeR")
library(tuneR)
library(ProjectTemplate)
library(transcribeR)

##########2.2: Set working environment###########
Spewd <- getwd()  ###Align working directory a name
print("First, printing language codes.")  
printLanguages()  ####print languages which can be used 
Sys.sleep(3)    ###Set sleep system 3 second
API_KEY='592a0786-f054-463d-baed-8e6ff3d5787a'   
Sys.sleep(3)   ###Get API KEY of speech recognition and set sleep system 3 second

########2.3: Transcribe voice to text and retrive text
sendAudioGetJobs(wav.dir = Spewd,
                 api.key = API_KEY,
                 interval = "-1", 
                 encode = "multipart",
                 existing.csv = NULL,        
                 csv.location = "/Users/qichenhu/CSV_LOCATION",
                 language = "en-US", 
                 verbose = TRUE)  # Print the uploading progress 
Sys.sleep(5)   #######Use sendAudioGetJobs function recognize the speech
print("Waiting before requesting transcriptions.")
Sys.sleep(60) # Set enough delay time to make the API get the proper transcription
retrieveText(job.file = "/Users/qichenhu/CSV_LOCATION", api.key = API_KEY)  #####Use retrieveText function to get the Transcription of the vedio











