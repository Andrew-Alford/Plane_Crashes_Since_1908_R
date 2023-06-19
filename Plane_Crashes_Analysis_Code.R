library(tidyverse)
library(ggplot2)
library(maps)
library(ggmap)
library(mapproj)
library(dplyr)
library(usmap)
library(writexl)
library(dplyr)
library(data.table)
library(plyr)
library(stringr)


#From www.kaggle.com/datasets/cgurkan/airplane-crash-data-since-1908
crashes <- read_csv('/Users/ryan_summers/Documents/Syracuse - M.S. Applied Data Science/IST 687 - Applied Data Science/Final Project/Airplane_Crashes_and_Fatalities_Since_1908_20190820105639.csv')
is.na(crashes) <- crashes == "NULL" #this data has the word NULL for missing data, this coverts NULL to NA
year <- as.numeric(substring(crashes$Date,7,10)) #extracting "year" from the "date" and making it numeric
month <- as.numeric(substring(crashes$Date,1,2)) #extracting "month" from the "date" and making it numeric
crashes <- add_column(crashes,month,year,.after = "Date") #adding "month" and "year" as numeric columns


#Making other character columns numeric - WORK FROM HERE**********
crashes$Aboard <- as.numeric(crashes$Aboard)
crashes$`Aboard Passangers` <- as.numeric(crashes$`Aboard Passangers`)
crashes$`Aboard Crew`<- as.numeric(crashes$`Aboard Crew`)
crashes$Fatalities <- as.numeric(crashes$Fatalities)
crashes$`Fatalities Crew` <- as.numeric(crashes$`Fatalities Crew`)
crashes$`Fatalities Passangers`<-as.numeric(crashes$`Fatalities Passangers`)
crashes$Ground <- as.numeric(crashes$Ground)
crashes <- crashes[,-10:-11] #removing registration number, and serial number of the aircraft

location <-crashes$Location
loc1 <- unlist(lapply(strsplit(location, split=","),"[",1))
head(loc1)
loc2 <- unlist(lapply(strsplit(location, split=","),"[",2))
length(loc2)
loc3 <- unlist(lapply(strsplit(location, split=","),"[",3))
loc4 <- unlist(lapply(strsplit(location, split=","),"[",4))
crashes <- add_column(crashes,loc1,loc2,loc3,loc4,.after = "Location")


####### Ryan's contribution #######

# Load world cities/country data 
data(world.cities)

# remove any punctuations from location
locRaw <- gsub('[[:punct:]\n]', "", location)

# extract US states
extrStates <- str_extract(locRaw, 
                          paste(state.name, collapse='|'))
# extract Countries
extrCountries <- str_extract(locRaw, 
                             paste(world.cities$country.etc, collapse='|')) # (world.cities$country.etc)

# extract ocean names
# [^\\s] - extract anything but white space 
# \\s- extract any white space
# {1} - extract one word before ocean
extrOceans <- str_extract(locRaw, "([^\\s]+\\s+){1}Ocean")

# convert lists to df
stateList <- data.frame(Reduce(rbind, extrStates))
countryList <- data.frame(Reduce(rbind, extrCountries))
oceanList <- data.frame(Reduce(rbind, extrOceans))


# add columns to crashes df
crashes <- add_column(crashes,
                      stateList, countryList, oceanList,
                      .after = 'Location')
# rename new columns
colnames(crashes)[6] <- 'state_list'
colnames(crashes)[7] <- 'country_list'
colnames(crashes)[8] <- 'ocean_list'


# convert state names to USA to merge with country_list column
crashes$us_list <- ifelse(crashes$state_list %in% state.name, 'USA', NA)
# move US_list closer to other location variables
crashes <- relocate(crashes, us_list,
                    .after=state_list)

# merge us_list with country_list based on adjacent value
crashes$country_list[is.na(crashes$country_list)] <- 
  crashes$us_list[is.na(crashes$country_list)]


#### Text Mining ###

extrText <- unnest_tokens(crashes, bigram, Summary, 
                          token = "ngrams", 
                          n = 2) 
extrText %<>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>%
  unite(bigram, word1, word2, sep=" ") 

bigramCounts <- count(extrText$bigram)
head(bigramCounts)

# remove NA in x column
bgc <- bigramCounts[!grepl("NA", bigramCounts$x),]
bgc %<>%
  filter(!is.na(x)) %>%
  arrange(desc(freq)) %>%
  slice(1:20)

# bar graph
ggplot(bgc) + 
  aes(x=reorder(x, freq), y=freq, group=1, fill=freq) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  scale_fill_continuous(trans='reverse',
                        name='Word Freq') 

# k-means clustering 


# add additional words to stop_words as they aren't informative
custom_stop_words <- tribble(
  # Column names should match stop_words
  ~word,  ~lexicon,
  # Add crashed, aircraft, plane 
  "crashed", "CUSTOM",
  "aircraft",  "CUSTOM",
  "plane", "CUSTOM",
  "en", "CUSTOM",
  "route", "CUSTOM",
  "international", "CUSTOM",
  "airport", "CUSTOM"
)

# create own df
my_stop_words <- stop_words %>% 
  bind_rows(custom_stop_words)

ktest <- unnest_tokens(crashes, bigram, Summary,  token = "ngrams", n = 2) 
ktest %<>%
  separate(bigram, c("word1","word2"), sep = " ") %>%
  filter(!word1 %in% my_stop_words$word) %>%
  filter(!word2 %in% my_stop_words$word) %>%
  unite(bigram, word1, word2, sep=" ") %>%
  select(18:25) 

# Only include rows that have a complete set of text strings
ktest <- ktest[complete.cases(ktest),] 

# group by word1 and sum all columns
ktestGrouped <- ktest %>% 
  group_by(bigram) %>%
  summarise_each(list(sum)) %>%
  arrange(desc(Fatalities)) %>%
  slice(1:500)

# convert column name to row name - kmeans won't work with header
ktestGrouped %<>% column_to_rownames(var = 'bigram')


# ****Computationally Intensive**** START

# Elbow method
fviz_nbclust(ktest, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(bCounts, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
fviz_nbclust(bCounts, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

# **** Computationally Intensive**** END

# As the final result of k-means clustering result is sensitive to the random 
# starting assignments, we specify nstart = 25. This means that R will try 25 
# different random starting assignments and then select the best results
# corresponding to the one with the lowest within cluster variation. The default
# value of nstart in R is one. But, it’s strongly recommended to compute k-means 
# clustering with a large value of nstart such as 25 or 50, in order to have a
# more stable result.

set.seed(42)
# create k-means model
km.res <- kmeans(ktestGrouped, 4, nstart=21)

# check clusters
print('Clusters:')
table(km.res$cluster)

# visualize clusters # ellipse.type = 'norm'
kmap <- fviz_cluster(km.res, ktestGrouped, 
                     ellipse.type = "convex",
                     labelsize=10) # testing diff sizes

kmapDF <- kmap$data
kmapDF %>%
  group_by(cluster) %>%
  top_n(n=10)

kmapDF[kmapDF$cluster==4,1]

# HAVING TROUBLE FINDING A WAY TO GET FREQUENCIES****
ft1 <- findFreqTerms(kMatrix[km.res$cluster==1,], 25)
ft2 <- findFreqTerms(kMatrix[km.res$cluster==2,], 25)
ft3 <- findFreqTerms(kMatrix[km.res$cluster==3,], 25)
ft4 <- findFreqTerms(kMatrix[km.res$cluster==4,], 25)

# show fatality numbers per cluster
print('Fatalities in Cluster 1:')
sum(crashes$Fatalities[which(km.res$cluster==1)])
print('Fatalities in Cluster 2:')
sum(crashes$Fatalities[which(km.res$cluster==2)])
print('Fatalities in Cluster 3:')
sum(crashes$Fatalities[which(km.res$cluster==3)])
print('Fatalities in Cluster 4:')
sum(crashes$Fatalities[which(km.res$cluster==4)])


## k-means clustering using Corpus - single word analysis

# set any NAs in fatalities to 0
crashes$Fatalities[is.na(crashes$Fatalities)] = 0

# create corpus
crashesCorpus = VCorpus(VectorSource(crashes$Summary)) #Vcorp works on df
crashesCorpus = tm_map(crashesCorpus, tolower)
crashesCorpus = tm_map(crashesCorpus, PlainTextDocument)
crashesCorpus = tm_map(crashesCorpus, removePunctuation)
crashesCorpus = tm_map(crashesCorpus, removeWords, c(stopwords("english"),
                                                     'crashed', # adding addt'l words that aren't informative
                                                     "aircraft",
                                                     "plane",
                                                     "en")) 
# create DTM - rows=terms, col=docs
crashesDTM = DocumentTermMatrix(crashesCorpus)
crashesDTM = removeSparseTerms(crashesDTM, 0.95)

# 100 most freq terms
crashesFT = findFreqTerms(crashesDTM, 100)
print('Top 100 Terms:')
crashesFT

# turn DTM to df for graphing
crashesDF <- tidy(crashesDTM)

### Remove empty documents (sentences)
nRows = apply(crashesDTM, 1, sum)
crashesDTM = crashesDTM[nRows> 0, ]

# weigh the terms by their frequency
dtmFreq = weightTfIdf(crashesDTM)

### k-means 

# need to turn dtmFreq into matrix for kmeans to work
dtmMatrix = as.matrix(dtmFreq)
rownames(dtmMatrix) = 1:nrow(dtmMatrix)

# center and scale the data
#preproc = preProcess(dtmMatrix)
#m_norm = predict(preproc, dtmMatrix)
#cl = kmeans(m_norm, centers = 7)
crashesK <- kmeans(dtmMatrix, centers = 4) # debating between 4 & 7

# visualize clusters
fviz_cluster(crashesK, dtmMatrix, 
             ellipse.type = "convex",
             labelsize=0)

# show number of words per cluster
print('Clusters:')
table(crashesK$cluster)

# freq of terms in each cluster
freqTerms1 <- findFreqTerms(crashesDTM[crashesK$cluster==1,], 25)
freqTerms2 <- findFreqTerms(crashesDTM[crashesK$cluster==2,], 25)
freqTerms3 <- findFreqTerms(crashesDTM[crashesK$cluster==3,], 25)
freqTerms4 <- findFreqTerms(crashesDTM[crashesK$cluster==4,], 25)

print('Top Terms by Freq in Cluster 1:')
freqTerms1
print('Top Terms by Freq in Cluster 2:')
freqTerms2
print('Top Terms by Freq in Cluster 3:')
freqTerms3
print('Top Terms by Freq in Cluster 4:')
freqTerms4


# show fatality numbers per cluster
print('Fatalities in Cluster 1:')
sum(crashes$Fatalities[which(crashesK$cluster==1)])
print('Fatalities in Cluster 2:')
sum(crashes$Fatalities[which(crashesK$cluster==2)])
print('Fatalities in Cluster 3:')
sum(crashes$Fatalities[which(crashesK$cluster==3)])
print('Fatalities in Cluster 4:')
sum(crashes$Fatalities[which(crashesK$cluster==4)])



####### Ryan's contribution -END- #######


####### Andrew's contribution #######

#Creating new column for civilian versus military crashes
crashes$Civilian_Or_Military <- substr(crashes$Operator,1,8)


#Filling in missing values in Civilian_Or_Military
crashes$Civilian_Or_Military[c(2,68,70,115,376,856,4812)] = "Civilian"
crashes$Civilian_Or_Military[c(18,682,2051)] = "Military"


#Filling in missing values in Operator
crashes$Operator[c(2,68,70,115,376,856,4812)] = "Private"
crashes$Operator[18] = "Royal Airship Works"
crashes$Operator[682] = "Royal Air Force"
crashes$Operator[2051] = "Unknown"


#Converting all civilian operators to word "Civilian"
crashes$Civilian_Or_Military <- ifelse(crashes$Civilian_Or_Military != "Military", "Civilian", "Military")


#Relocating Civilian_Or_Military next to Operator
crashes <- crashes %>% relocate(Civilian_Or_Military, .after = Operator)


#Cleaning up Operator Column
crashes$Operator <- gsub("Military", "", crashes$Operator)
crashes$Operator <- gsub("-", "", crashes$Operator)
crashes$Operator <- trimws(crashes$Operator)

#Standardizing column names
colnames(crashes)[14] <- "operator"
colnames(crashes)[15] <- "civilian_or_military"

####### Andrew's contribution -END- #######

#BELOW NUMERICAL DATA IS BEING SUMMED UP BY YEAR (109 YEARS)
fatalities_by_year <- crashes %>% group_by(year) %>%
  summarise(tot_aboard=sum(Aboard,na.rm=TRUE),
            pas_aboard=sum(`Aboard Passangers`,na.rm=TRUE),
            crew_aboard=sum(`Aboard Crew`,na.rm=TRUE),
            tot_fatalities=sum(Fatalities,na.rm=TRUE),
            pas_fatalities=sum(`Fatalities Passangers`,na.rm=TRUE),
            crew_fatalities=sum(`Fatalities Crew`,na.rm=TRUE),
            grd_fatalities=sum(Ground,na.rm=TRUE))
view(fatalities_by_year)

#READING IN WORLD PASSENGER TRAVEL FROM 1980-2020.
passengers_per_year <- read_csv('/Users/ryan_summers/Documents/Syracuse - M.S. Applied Data Science/IST 687 - Applied Data Science/Final Project/world-air-passenger-traffic-evolution-1980-2020.csv')
passengers_per_year<-passengers_per_year[-1:-3,] #removing the first 3 rows which are just descriptions
colnames(passengers_per_year)=c("raw_string")
passengers_per_year <- add_column(passengers_per_year,
                                  as.numeric(substring(passengers_per_year$raw_string,1,4)),
                                  as.numeric(substring(passengers_per_year$raw_string,6,10)),
                                  as.numeric(substring(passengers_per_year$raw_string,12,16)),
                                  as.numeric(substring(passengers_per_year$raw_string,18,22)))
colnames(passengers_per_year)=c("raw_string","year","total passenger travel (BB)","domestic","international")
passengers_per_year<-passengers_per_year[-41,] #removing the year 2020 as our crash data only goes to 2019
passengers_per_year<-passengers_per_year[,-1] #removing the "raw string data" that was read in and cleansed
passengers_per_year #passengers_per_year is now clean, numerical, from 1980-2019

#Merging passenger data by year with fatalities by year since 1980
pas_vs_fatalities_since_1980 <- merge(fatalities_by_year,passengers_per_year,"year")
view(pas_vs_fatalities_since_1980)

#write_xlsx(crashes,path='/Users/ryan_summers/Documents/Syracuse - M.S. Applied Data Science/IST 687 - Applied Data Science/Final Project')
write_xlsx(crashes, 'crashes_bill1.xls')


getwd()
setwd('/Users/ryan_summers/Documents/Syracuse - M.S. Applied Data Science/IST 687 - Applied Data Science/Final Project')