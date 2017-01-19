#####################################################################



# Change MRAN Snapshot
options(repos = c(CRAN = "https://mran.revolutionanalytics.com/snapshot/2017-01-05"))

# Install these packages if you haven't already
list.of.packages <- c("httr", "XML","stringr","ggplot2","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
library(httr)
library(XML)
library(stringr)
library(ggplot2)
library(dplyr)
require(devtools)
install_github("flovv/Roxford")
require(Roxford)







#####################################################################
# Microsoft Cognative Services: 
# Face Detection API
#####################################################################

# Define image source (if you haven't already done so above )
img.url = 'https://pbs.twimg.com/profile_images/677589103710306304/m56O6Wgf.jpg'
browseURL(img.url)



# Define Microsoft API URL to request data
faceURL = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age"


# Define access key (access key is available via: https://www.microsoft.com/cognitive-services/en-us/face-api)
source('C:/Users/Aclark/Dropbox/WMRUG/Meeting 20170117/secretkeys.R')
# facekey = 'XXXXXXXXXXXXXXXXXXXXXXXXXX' # Insert your key here! Don't leave the X's


# Define image
mybody = list(url = img.url)


# Request data from Microsoft
faceResponse = POST(      # Post file to a server
  url = faceURL, 
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
  body = mybody,
  encode = 'json'
)


# Show request results (if Status=200, request is okay)
faceResponse


# Reuqest results from face analysis AND view them
hadley = content(faceResponse)[[1]]
hadley

# Content includes
# (1) faceRectangle: top left coordinates, and rectangle width and height
# (2) faceLandmarks: x y coordinates for LOTS of facial landmarks
# (3) faceAttributes: age estimate


# Define results in data frame and clean it up
OR<-as.data.frame(as.matrix(hadley$faceLandmarks))

OR$V2 <- lapply(strsplit(as.character(OR$V1), "\\="), "[", 2)
OR$V2 <- lapply(strsplit(as.character(OR$V2), "\\,"), "[", 1)
colnames(OR)[2] <- "X"
OR$X<-as.numeric(OR$X)

OR$V3 <- lapply(strsplit(as.character(OR$V1), "\\y = "), "[", 2)
OR$V3 <- lapply(strsplit(as.character(OR$V3), "\\)"), "[", 1)
colnames(OR)[3] <- "Y"
OR$Y<-as.numeric(OR$Y)

OR$V1<-NULL
View(OR)








######################################################################
# Estimate my age. This photo was taken of me when I was about 25.5-26 years old
img.url = 'https://media.licdn.com/mpr/mpr/shrinknp_200_200/AAEAAQAAAAAAAAJAAAAAJGMzNDE2NzQ0LTgzMWYtNDU2NC04NDA0LWQ4YjhlMzRkYjk5Nw.jpg'
browseURL(img.url)

# Define image
mybody = list(url = img.url)


# Request data from Microsoft
faceResponse = POST(
  url = faceURL, 
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
  body = mybody,
  encode = 'json'
)


# Reuqest results from face analysis AND view them
aaron = content(faceResponse)[[1]]
aaron$faceAttributes
paste(aaron$faceAttributes$age - 26,"years off")









############################################
# What if there's two people in the photo?

img.url = 'http://d3lp4xedbqa8a5.cloudfront.net/s3/digital-cougar-assets/WomansDay/2014/08/13/36651/katie_holmes_jimmy_fallon_1.jpg?mode=max&quality=80&width=1024'
browseURL('http://www.womansday.com.au/celebrity/hollywood-stars/katie-holmes-was-told-her-eyes-are-too-far-apart-to-be-beautiful-7071')


# Define image
mybody = list(url = img.url)

# Request data from Microsoft
faceResponse = POST(
  url = faceURL, 
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = facekey)),
  body = mybody,
  encode = 'json'
)


# Reuqest results from face analysis
katie = content(faceResponse)[[1]]
katie


# Reformat the data into an organized structure
# Define results in data frame
OR<-as.data.frame(as.matrix(katie$faceLandmarks))

# Make some transformation to data frame
OR$V2 <- lapply(strsplit(as.character(OR$V1), "\\="), "[", 2)
OR$V2 <- lapply(strsplit(as.character(OR$V2), "\\,"), "[", 1)
colnames(OR)[2] <- "X"
OR$X<-as.numeric(OR$X)

OR$V3 <- lapply(strsplit(as.character(OR$V1), "\\y = "), "[", 2)
OR$V3 <- lapply(strsplit(as.character(OR$V3), "\\)"), "[", 1)
colnames(OR)[3] <- "Y"
OR$Y<-as.numeric(OR$Y)

OR$V1<-NULL
View(OR)



# What's the distance between her two eyes
# Use the Pythagorean Theorem!
KatieEyeDist<-sqrt(((OR["pupilRight",]$X - OR["pupilLeft",]$X)**2) + ((OR["pupilRight",]$Y - OR["pupilLeft",]$Y)**2))




# Reuqest results from face analysis
jimmy = content(faceResponse)[[2]]
jimmy


# Reformat the data into an organized structure
# Define results in data frame
OR<-as.data.frame(as.matrix(jimmy$faceLandmarks))

# Make some transformation to data frame
OR$V2 <- lapply(strsplit(as.character(OR$V1), "\\="), "[", 2)
OR$V2 <- lapply(strsplit(as.character(OR$V2), "\\,"), "[", 1)
colnames(OR)[2] <- "X"
OR$X<-as.numeric(OR$X)

OR$V3 <- lapply(strsplit(as.character(OR$V1), "\\y = "), "[", 2)
OR$V3 <- lapply(strsplit(as.character(OR$V3), "\\)"), "[", 1)
colnames(OR)[3] <- "Y"
OR$Y<-as.numeric(OR$Y)

OR$V1<-NULL
View(OR)

# What's the distance between his two eyes
# Use the Pythagorean Theorem!
JimmyEyeDist<-sqrt(((OR["pupilRight",]$X - OR["pupilLeft",]$X)**2) + ((OR["pupilRight",]$Y - OR["pupilLeft",]$Y)**2))


JimmyEyeDist
KatieEyeDist












########################################################
# Microsoft Cognative Services: 
# Emotion API
########################################################


# Potential Application: Measure emotions during some sort of trial or testing
# Define image source and open the URL in your default browser
img.url     = 'http://www.shopformom.com/wp-content/uploads/2011/04/boy_veggies.jpg'
browseURL(img.url)


# Define Microsoft API URL to request data
URL.emoface = 'https://api.projectoxford.ai/emotion/v1.0/recognize'

# Define access key (access key is available via: https://www.microsoft.com/cognitive-services/en-us/emotion-api)
source('C:/Users/Aclark/Dropbox/WMRUG/Meeting 20170117/secretkeys.R')
# emotionkey = 'XXXXXXXXXXXXXXXXXXXXXXXXXXX'

# Define image
mybody = list(url = img.url)

# Request data from Microsoft
faceEMO = POST( # POST is from the httr package. It allows you to post a file to a server
  url = URL.emoface,
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = emotionkey)),
  body = mybody,
  encode = 'json'
)

# Show request results (if Status=200, request is okay)
faceEMO


# Extract results from face analysis request. Content is also from the httr package.
# options(scipen=999)
# options(scipen=0)
veggies = content(faceEMO)[[1]]
veggies


# Content includes:
# faceRectangle: top left coordinates, and face height and width
# scores: Emotion Scores


# Define results in data frame
o<-as.data.frame(as.matrix(veggies$scores))

# Make some transformation (gets rid of scientific notation)
o$V1 <- lapply(strsplit(as.character(o$V1 ), "e"), "[", 1)
o$V1<-as.numeric(o$V1)
colnames(o)[1] <- "Level"

# Define names
o$Emotion<- rownames(o)

# find sum of all levels
tot<-as.numeric(summarise(o,tot=sum(Level)))
o$pct<-round(100*(o$Level/tot),1)

# Make plot
ggplot(data=o, aes(x=Emotion, y=pct)) +
  geom_bar(stat="identity")+ 
  geom_text(aes(x=Emotion, y=pct,label=paste0(pct,"%")),vjust=-.5) +
  ylim(c(0,max(o$pct)*1.05))+
  ggtitle("Veggie Facial Emotions")
  









############################################
# Applications in sport?
img.url  = "http://media.syracuse.com/post-standard/photo/2009/02/172323-standard.jpg"
browseURL(img.url)


# Define image
mybody = list(url = img.url)

# Request data from Microsoft
faceEMO = POST( # POST is from the httr package. It allows you to post a file to a server
  url = URL.emoface,
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = emotionkey)),
  body = mybody,
  encode = 'json'
)

# Show request results (if Status=200, request is okay)
faceEMO

# Extract results from face analysis request. Content is also from the httr package.
bball = content(faceEMO)[[1]]
bball


# Define results in data frame
o<-as.data.frame(as.matrix(bball$scores))

# Make some transformation (gets rid of scientific notation)
o$V1 <- lapply(strsplit(as.character(o$V1 ), "e"), "[", 1)
o$V1<-as.numeric(o$V1)
colnames(o)[1] <- "Level"

# Define names
o$Emotion<- rownames(o)

# find sum of all levels
tot<-as.numeric(summarise(o,tot=sum(Level)))
o$pct<-round(100*(o$Level/tot),1)

# Make plot
ggplot(data=o, aes(x=Emotion, y=pct)) +
  geom_bar(stat="identity")+ 
  geom_text(aes(x=Emotion, y=pct,label=paste0(pct,"%")),vjust=-.5) +
  ylim(c(0,max(o$pct)*1.05))+
  ggtitle("Bball Facial Emotions")











######################################
# measure your ability to produce incite a goal emotion. For example, haunted house, theatre audience, etc

img.url= 'http://themeparkuniversity.com/wp-content/uploads/2014/09/Dec01pic0136.jpg'
browseURL(img.url)


# Define image
mybody = list(url = img.url)

# Request data from Microsoft
faceEMO = POST( # POST is from the httr package. It allows you to post a file to a server
  url = URL.emoface,
  content_type('application/json'), add_headers(.headers = c('Ocp-Apim-Subscription-Key' = emotionkey)),
  body = mybody,
  encode = 'json'
)

# Show request results (if Status=200, request is okay)
faceEMO

# Extract results from face analysis request. Content is also from the httr package.
haunt = content(faceEMO)[[1]] # 1 = right, 2 = left
haunt


# Define results in data frame
o<-as.data.frame(as.matrix(haunt$scores))

# Make some transformation (gets rid of scientific notation)
o$V1 <- lapply(strsplit(as.character(o$V1 ), "e"), "[", 1)
o$V1<-as.numeric(o$V1)
colnames(o)[1] <- "Level"

# Define names
o$Emotion<- rownames(o)

# find sum of all levels
tot<-as.numeric(summarise(o,tot=sum(Level)))
o$pct<-round(100*(o$Level/tot),1)

# Make plot
ggplot(data=o, aes(x=Emotion, y=pct)) +
  geom_bar(stat="identity")+ 
  geom_text(aes(x=Emotion, y=pct,label=paste0(pct,"%")),vjust=-.5) +
  ylim(c(0,max(o$pct)*1.05))+
  ggtitle("Facial Emotions")










######################################################
# Vision API: describe images
######################################################
# facekey = ''   #look it up on your subscription site

## using local images
# getFaceResponse("out/snap00169.png", facekey)

## or providing a url to a remote local image
# f<-getFaceResponseURL("https://www.whitehouse.gov/sites/whitehouse.gov/files/images/first-family/44_barack_obama[1].jpg", facekey)


source('C:/Users/Aclark/Dropbox/WMRUG/Meeting 20170117/secretkeys.R')
# visionkey = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
url<-"http://sizlingpeople.com/wp-content/uploads/2015/10/Kim-Kardashian-2015-21.jpg"
browseURL(url)
kim <- getDescriptionResponseURL(url, visionkey) #Roxford::

# Before we see all the objects that are tagged in this image, let's make our own list
View(kim)

## can  be used to classify with domain specific models provided by Microsoft.
# run  getDomainModels(visionkey) first to get a list with available models
c<-getDomainModelResponseURL(url, visionkey, 'celebrities')
c









###############################################
# what about multiple celebrities in the shot?

# Oscars 2014 Selfie
url = 'http://i.telegraph.co.uk/multimedia/archive/02839/oscarsselfie_2839758b.jpg'
browseURL(url)
selfie <- getDescriptionResponseURL(url, visionkey) #Roxford::

# Does it recognize all the celebrities?
selfie["description",]$captions.text
selfie["description",]$captions.confidence

# What else is tagged in the photo?
View(selfie)

c<-getDomainModelResponseURL(url, visionkey, 'celebrities')
c






#############################################
# What about some photography... where there are no people or celebs in the shot?

url = 'http://www.thecanyon.com/assets/css/images/grandcanyon1.jpg'
browseURL(url)
canyon <- getDescriptionResponseURL(url, visionkey) #Roxford::

# Does it recognize all the celebrities?
canyon["description",]$captions.text
canyon["description",]$captions.confidence

# What else is tagged in the photo?
View(canyon)




