#Librarying several packages
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
library(tidytext)
library(magrittr)
library(factoextra)
library(tm)

#Set working directory
setwd('/Users/Bill/Desktop/Syracuse_MIS/IST 687/Project')

### INITIAL DATA CLEANSING ###

#From www.kaggle.com/datasets/cgurkan/airplane-crash-data-since-1908
crashes <- read_csv('Airplane_Crashes_and_Fatalities_Since_1908.csv')
is.na(crashes) <- crashes == "NULL" #this data has the word NULL for missing data, this coverts NULL to
NA
year <- as.numeric(substring(crashes$Date,7,10)) #extracting "year" from the "date" and making it
numeric
month <- as.numeric(substring(crashes$Date,1,2)) #extracting "month" from the "date" and making it
numeric
crashes <- add_column(crashes,month,year,.after = "Date") #adding "month" and "year" as numeric
columns

#Reading in custom function for later analysis
get_mode <- function(x){
return(names(sort(table(x), decreasing = T, na.last = T)[1:10]))
}

#Making other character columns numeric - WORK FROM HERE**********
crashes$Aboard <- as.numeric(crashes$Aboard)
crashes$`Aboard Passangers` <- as.numeric(crashes$`Aboard Passangers`)
crashes$`Aboard Crew`<- as.numeric(crashes$`Aboard Crew`)
crashes$Fatalities <- as.numeric(crashes$Fatalities)
crashes$`Fatalities Crew` <- as.numeric(crashes$`Fatalities Crew`)
crashes$`Fatalities Passangers`<-as.numeric(crashes$`Fatalities Passangers`)
crashes$Ground <- as.numeric(crashes$Ground)
1
R-Code for Final Project – Plane Crashes Since 1908

Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis

# remove registration and cn/ln

crashes <- select(crashes, -c("Registration","cn/ln"))

#crashes <- crashes[,-10:-11] #removing registration number, and serial number of the aircraft

# column name cleanup
colnames(crashes) <- tolower(colnames(crashes))
colnames(crashes)[which(names(crashes) == "flight #")] <- "flight#"
colnames(crashes)[which(names(crashes) == "ac type")] <- "ac_type"
colnames(crashes)[which(names(crashes) == "aboard passangers")] <- "aboard_passengers"
colnames(crashes)[which(names(crashes) == "aboard crew")] <- "aboard_crew"
colnames(crashes)[which(names(crashes) == "fatalities passangers")] <- "fatalities_passengers"
colnames(crashes)[which(names(crashes) == "fatalities crew")] <- "fatalities_crew"
colnames(crashes)[which(names(crashes) == "Operator")] <- "operator"
colnames(crashes)[which(names(crashes) == "Location")] <- "location"
location <-crashes$location

#loc1 <- unlist(lapply(strsplit(location, split=","),"[",1))
#head(loc1)
#loc2 <- unlist(lapply(strsplit(location, split=","),"[",2))
#length(loc2)
#loc3 <- unlist(lapply(strsplit(location, split=","),"[",3))
#loc4 <- unlist(lapply(strsplit(location, split=","),"[",4))
#crashes <- add_column(crashes,loc1,loc2,loc3,loc4,.after = "Location")


####### Ryan's contribution -START-#######

# Load world cities/country data
data(world.cities)

# remove any punctuation from location
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
2
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis
countryList <- data.frame(Reduce(rbind, extrCountries))
oceanList <- data.frame(Reduce(rbind, extrOceans))

# add columns to crashes df
crashes <- add_column(crashes,
stateList, countryList, oceanList,
.after = 'location')

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

# add additional words to stop_words as they aren't informative
custom_stop_words <- tribble(

# Column names should match stop_words
~word, ~lexicon,

# Add crashed, aircraft, plane
"crashed", "CUSTOM",
"aircraft", "CUSTOM",
"plane", "CUSTOM",
"en", "CUSTOM",
"route", "CUSTOM",
"international", "CUSTOM",
"airport", "CUSTOM"
)

# create own df
my_stop_words <- stop_words %>%
bind_rows(custom_stop_words)
extrText <- unnest_tokens(crashes, bigram, summary,
token = "ngrams",
n = 2)
3
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis
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
ktest <- unnest_tokens(crashes, bigram, summary, token = "ngrams", n = 2)
ktest %<>%
separate(bigram, c("word1","word2"), sep = " ") %>%
filter(!word1 %in% my_stop_words$word) %>%
filter(!word2 %in% my_stop_words$word) %>%
unite(bigram, word1, word2, sep=" ") %>%
select(14:21)

# Only include rows that have a complete set of text strings
ktest <- ktest[complete.cases(ktest),]

# group by word1 and sum all columns
ktestGrouped <- ktest %>%
group_by(bigram) %>%
summarise_each(list(sum)) %>%
arrange(desc(fatalities)) %>%
slice(1:500)

# convert column name to row name - kmeans won't work with header
ktestGrouped %<>% column_to_rownames(var = 'bigram')
4
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis

# ****Computationally Intensive**** START
# Elbow method
#fviz_nbclust(ktest, kmeans, method = "wss") +
# geom_vline(xintercept = 4, linetype = 2) +
# labs(subtitle = "Elbow method")
# Silhouette method
#fviz_nbclust(bCounts, kmeans, method = "silhouette")+
# labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
#fviz_nbclust(bCounts, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
# labs(subtitle = "Gap statistic method")
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

#ft1 <- findFreqTerms(kMatrix[km.res$cluster==1,], 25)
5
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis

#ft2 <- findFreqTerms(kMatrix[km.res$cluster==2,], 25)

#ft3 <- findFreqTerms(kMatrix[km.res$cluster==3,], 25)

#ft4 <- findFreqTerms(kMatrix[km.res$cluster==4,], 25)

# show fatality numbers per cluster
print('Fatalities in Cluster 1:')
sum(crashes$fatalities[which(km.res$cluster==1)])
print('Fatalities in Cluster 2:')
sum(crashes$fatalities[which(km.res$cluster==2)])
print('Fatalities in Cluster 3:')
sum(crashes$fatalities[which(km.res$cluster==3)])
print('Fatalities in Cluster 4:')
sum(crashes$fatalities[which(km.res$cluster==4)])

## k-means clustering using Corpus - single word analysis

# set any NAs in fatalities to 0
crashes$fatalities[is.na(crashes$fatalities)] = 0

# create corpus
crashesCorpus = VCorpus(VectorSource(crashes$summary)) #Vcorp works on df
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
6
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis

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
sum(crashes$fatalities[which(crashesK$cluster==1)])
print('Fatalities in Cluster 2:')
sum(crashes$fatalities[which(crashesK$cluster==2)])
print('Fatalities in Cluster 3:')
sum(crashes$fatalities[which(crashesK$cluster==3)])
print('Fatalities in Cluster 4:')
sum(crashes$fatalities[which(crashesK$cluster==4)])
7
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis

####### Ryan's contribution -END-#######

####### Andrew's contribution #######

#Creating new column for civilian versus military crashes
crashes$civilian_or_military <- substr(crashes$operator,1,8)

#Filling in missing values in civilian_or_military
crashes$civilian_or_military[c(2,68,70,115,376,856,4812)] = "Civilian"
crashes$civilian_or_military[c(18,682,2051)] = "Military"

#Filling in missing values in operator
crashes$operator[c(2,68,70,115,376,856,4812)] = "Private"
crashes$operator[18] = "Royal Airship Works"
crashes$operator[682] = "Royal Air Force"
crashes$operator[2051] = "Unknown"

#Converting all civilian operators to word "Civilian"
crashes$civilian_or_military <- ifelse(crashes$civilian_or_military != "Military", "Civilian", "Military")

#Relocating civilian_or_military next to operator
crashes <- crashes %>% relocate(civilian_or_military, .after = operator)

#Cleaning up operator Column
crashes$operator <- gsub("Military", "", crashes$operator)
crashes$operator <- gsub("-", "", crashes$operator)
crashes$operator <- trimws(crashes$operator)

####### Andrew's contribution -END- #######

### Bill’s Data Munging ###

#BELOW NUMERICAL DATA IS BEING SUMMED UP BY YEAR (109 YEARS)

# must detach plyr due to conflicts with dplyr
detach(package:plyr)
fatalities_by_year <- crashes %>% group_by(year) %>%
summarise(tot_aboard=sum(aboard,na.rm=TRUE),
pas_aboard=sum(aboard_passengers,na.rm=TRUE),
crew_aboard=sum(aboard_crew,na.rm=TRUE),
8
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis
tot_fatalities=sum(fatalities,na.rm=TRUE),
pas_fatalities=sum(fatalities_passengers,na.rm=TRUE),
crew_fatalities=sum(fatalities_crew,na.rm=TRUE),
grd_fatalities=sum(ground,na.rm=TRUE))
view(fatalities_by_year)

#READING IN WORLD PASSENGER TRAVEL FROM 1980-2020.
passengers_per_year <- read_csv('world-air-passenger-traffic-evolution-1980-2020.csv')
passengers_per_year<-passengers_per_year[-1:-3,] #removing the first 3 rows which are just
descriptions
colnames(passengers_per_year)=c("raw_string")
passengers_per_year <- add_column(passengers_per_year,
as.numeric(substring(passengers_per_year$raw_string,1,4)),
as.numeric(substring(passengers_per_year$raw_string,6,10)),
as.numeric(substring(passengers_per_year$raw_string,12,16)),
as.numeric(substring(passengers_per_year$raw_string,18,22)))
colnames(passengers_per_year)=c("raw_string","year","total passenger travel
(BB)","domestic","international")
passengers_per_year<-passengers_per_year[-41,] #removing the year 2020 as our crash data only goes
to 2019
passengers_per_year<-passengers_per_year[,-1] #removing the "raw string data" that was read in and
cleansed
passengers_per_year #passengers_per_year is now clean, numerical, from 1980-2019

#Merging passenger data by year with fatalities by year since 1980
pas_vs_fatalities_since_1980 <- merge(fatalities_by_year,passengers_per_year,"year")
view(pas_vs_fatalities_since_1980)

### BILL’S YEAR AND MONTH CRASH ANALYSIS ###
day <- as.numeric(substring(crashes$date,4,5))
crashes <- add_column(crashes,day,.after = 'year')
crashes_by_year <- crashes %>%
group_by(year) %>%
summarise(military_count=sum(civilian_or_military=='Military'),
civilian_count=sum(civilian_or_military=='Civilian'),
tot_crashes=as.numeric(length(date)),
tot_aboard=sum(aboard,na.rm=TRUE),
pas_aboard=sum(aboard_passengers,na.rm=TRUE),
crew_aboard=sum(aboard_crew,na.rm=TRUE),
tot_fatalities=sum(fatalities,na.rm=TRUE),
pas_fatalities=sum(fatalities_passengers,na.rm=TRUE),
crew_fatalities=sum(fatalities_crew,na.rm=TRUE),
grd_fatalities=sum(ground,na.rm=TRUE))
only_crashes <- crashes_by_year[,1:4]
crashes_melt <- melt(only_crashes,id="year")
ggplot(crashes_melt,aes(x=year,y=value,color=variable))+geom_line()+ geom_smooth()+
9
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis
ggtitle("Number of crashes by year since 1908")
crashes %>% ggplot(aes(x=year))+geom_bar()
crashes$crash_month_name <- ifelse(crashes$month==1,"Jan",
ifelse (crashes$month==2,"Feb",
ifelse (crashes$month==3,"Mar",
ifelse (crashes$month==4,"Apr",
ifelse (crashes$month==5,"May",
ifelse (crashes$month==6,"Jun",
ifelse (crashes$month==7,"Jul",
ifelse (crashes$month==8,"Aug",
ifelse (crashes$month==9,"Sep",
ifelse (crashes$month==10,"Oct",
ifelse (crashes$month==11,"Nov","Dec")))))))))))
level_order <- c('Jan', 'Feb', 'Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
crashes %>% ggplot()+geom_bar(aes(x=factor(crash_month_name,level=level_order)),
color="black",fill="light blue")+
theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5)) +
labs(title="Total Crashes by Month since 1908")+
xlab("Month")
crashes %>% ggplot()+geom_bar(aes(x=factor(day)),color="black",fill="light blue") +
labs(title="Total Crashes by Day since 1908")+
xlab("Day")

## BILL’S FATALITY ANALYSIS/VISUALIZATION
complete_year_df <- merge.data.frame(crashes_by_year,passengers_per_year,'year')
view(complete_year_df)
glimpse(complete_year_df)
complete_year_df %>% ggplot(aes(x=year,y=tot_fatalities)) + geom_line()+ geom_smooth()+
labs (title="Passenger Fatalities by year since 1980")+
ylab("Fatalities")
complete_year_df %>% ggplot(aes(x=year,y=tot_fatalities/`total passenger travel (BB)`)) +
geom_smooth()+ geom_line()+
labs(title="Passenger Fatalities per Billion Travelers by year since 1980")+
ylab("Fatalities per Billion Traverlers")
only_fatalities <- complete_year_df[,-2:-8]
only_fatalities <- only_fatalities[,1:4]
fatalities_melt <- melt(only_fatalities,id="year")
fatalities_melt %>% ggplot(aes(fill=variable,x=year,y=value))+
geom_bar(position="stack",stat="identity")+
ggtitle("Fatalities by Year")
10
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis
fatalities_melt_no_grd <- fatalities_melt[fatalities_melt$variable!="grd_fatalities",]
fatalities_melt_no_grd %>% ggplot(aes(fill=variable,x=year,y=value))+
geom_bar(position="stack",stat="identity")+
ggtitle("Fatalities by Year without ground fatalities")
fatalities_by_year %>% ggplot(aes(x=year,y=(tot_fatalities/tot_aboard*100)))+ geom_line()+
geom_smooth() + geom_line()+
labs(title="Percent of Fatalities vs All Onboard by Year since 1908")+
ylab("Perentage of Fatalities")
complete_year_df %>% ggplot(aes(x=year,y=(tot_fatalities/tot_aboard*100)))+
geom_smooth() + geom_line()+
labs(title="Percent of Fatalities vs All Onboard by Year since 1980")+
ylab("Perentage of Fatalities")
complete_year_df %>% ggplot(aes(x=year)) +
geom_line( aes(y=tot_fatalities), color="red") +
geom_line( aes(y=`total passenger travel (BB)`*1000), color="blue")+
scale_y_continuous(
name = "Total Fatalities",
sec.axis = sec_axis(~.,name="Total Passenger Volume (in millions)")) +
theme(
axis.title.y = element_text(color = "red", size=10),
axis.title.y.right = element_text(color = "blue", size=10)) +
ggtitle("Improvement in Safety")

### END BILL’S VISUALIZATION CODE###

### ANDREW’S VISUALIZATION CODE ###

#Pie chart for civ vs mil crashes
sum(crashes$civilian_or_military=="Civilian")
sum(crashes$civilian_or_military=="Military")
category <- c("Civilian", "Military")
amount <- c(4231,736)
pie_chart <- data.frame(category,amount)
pie_labels <- paste0(round(100 * amount/sum(amount), 2), "%")
ggplot(pie_chart, aes(x = "", y = amount, fill = category)) +
geom_col() +
coord_polar(theta = "y") +
theme_void() +
geom_text(aes(label=paste0(pie_labels)), position=position_stack(vjust=0.5)) +
labs(x=NULL, y=NULL, fill=NULL, title = "Percentages of Civilian and Military Crashes")

#Bar chart of top ten crashes, both civilian and military.
11
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis
target <- get_mode(crashes$operator)
top_ten_crashes_civ_and_mil <- filter(crashes, operator %in% target)
ttccamc <- count(top_ten_crashes_civ_and_mil$operator)
ggplot(ttccamc) +
aes(x=reorder(x, -freq), y=freq, group=1, fill=freq) +
geom_col(color="black") +
scale_fill_gradient2(low = "white", high = "orange") +
theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(face = "bold")) +
labs(y= "Frequency", x = "Airline Operator", title = "Total Crashes by Civilian and Military Operators", fill
= "Frequency")

#Bar chart of the top ten crashes by civilian only.
crashes_civilian <- crashes %>% filter(civilian_or_military=="Civilian")
target <- get_mode(crashes_civilian$operator)
top_ten_crashes_civ <- filter(crashes_civilian, operator %in% target)
ttccc <- count(top_ten_crashes_civ$operator)
ggplot(ttccc) +
aes(x=reorder(x, -freq), y=freq, group=1, fill=freq) +
geom_col(color="black") +
scale_fill_gradient2(low = "white", high = "blue") +
theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(face = "bold")) +
labs(y="Frequency", x = "Airline Operator", title = "Total Crashes by Civilian Airlines", fill = "Frequency")

#Bar chart of the top ten crashes by military only.
crashes_military <- crashes %>% filter(civilian_or_military=="Military")
target <- get_mode(crashes_military$operator)
top_ten_crashes_mil <- filter(crashes_military, operator %in% target)
ttcmc <- count(top_ten_crashes_mil$operator)
ggplot(ttcmc) +
aes(x=reorder(x, -freq), y=freq, group=1, fill=freq) +
geom_col(color="black") +
scale_fill_gradient2(low = "white", high = "red") +
theme(axis.text.x = element_text(angle = 45, hjust=1), axis.text = element_text(face = "bold")) +
labs(y="Frequency", x = "Airforce", title = "Total Crashes by Military Airforce", fill = "Frequency")

#Creating data frame for scatter plot.
crashes_scatterplot <- count(crashes$operator)
crashes_scatterplot <- crashes_scatterplot[order(crashes_scatterplot$freq, decreasing = TRUE),]
crashes_scatterplot <- crashes_scatterplot[1:50,]
crashes_scatterplot_civ_or_mil <- c("Civilian", "Military", "Civilian", "Civilian", "Military", "Civilian",
"Civilian", "Military", "Civilian", "Civilian",
"Civilian", "Civilian", "Civilian", "Civilian", "Civilian", "Civilian", "Military", "Military", "Civilian",
"Civilian", "Civilian", "Civilian", "Civilian", "Civilian", "Civilian", "Civilian", "Military", "Civilian",
"Civilian", "Civilian", "Civilian", "Civilian", "Civilian", "Civilian", "Military", "Civilian", "Civilian",
"Military", "Military", "Civilian", "Civilian", "Civilian", "Civilian", "Civilian", "Civilian", "Civilian",
"Civilian", "Civilian", "Civilian", "Civilian")
crashes_scatterplot_final <- data.frame(crashes_scatterplot, crashes_scatterplot_civ_or_mil)
12
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis
colnames(crashes_scatterplot_final)[which(names(crashes_scatterplot_final) ==
"crashes_scatterplot_civ_or_mil")] <- "civ_or_mil"
crashes_scatterplot_final$key_values <- crashes_scatterplot_final$civ_or_mil == "Military"

#Using ggplot2 to create the scatter plot.
ggplot(crashes_scatterplot_final, aes(x = 1:nrow(crashes_scatterplot_final), y = freq)) +
geom_point(aes(shape = civ_or_mil, size = 1, color = key_values)) +
geom_line(linewidth = 0.8, color = "black") +
theme(axis.text = element_text(face = "bold")) +
labs(y="Frequency", x = "Operator", title = "Scatter Plot of Crashes by Top Fifty Operators", size = "Size
of Data Values", color = "Color of Data Values", shape = "Civilian or Military")

### END OF ANDREW’S VISUALIZATION CODE ###

#####Anand Contribution start#####

#Bar chart of the top ten crashes by Civilian Aircraft Type only.
crashes_civilian_AC_type <- crashes %>% filter(civilian_or_military=="Civilian")
top_civ_ac_type_crashes <- get_mode(crashes_civilian_AC_type$ac_type)
top_ten_civ_ac_type_crashes <- filter(crashes_civilian_AC_type, ac_type %in%
top_civ_ac_type_crashes)
topcacc <- count(top_ten_civ_ac_type_crashes$ac_type)
ggplot(topcacc) +
 aes(x=reorder(x, -freq), y=freq, group=1, fill=freq) +
 geom_col(color="black") +
 scale_fill_gradient2(low = "black", high = "orange") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(face = "bold"))
+
 labs(y="Frequency", x = "Aircraft Type", title = "Total Crashes by Civilian AirCraft Type", fill =
"Frequency")

#Bar chart of the top ten crashes by Military Aircraft Type only.
crashes_military_AC_type <- crashes %>% filter(civilian_or_military=="Military")
top_mil_ac_type_crashes <- get_mode(crashes_military_AC_type$ac_type)
top_ten_mil_ac_type_crashes <- filter(crashes_military_AC_type, ac_type %in%
top_mil_ac_type_crashes)
topmacc <- count(top_ten_mil_ac_type_crashes$ac_type)
ggplot(topmacc) +
 aes(x=reorder(x, -freq), y=freq, group=1, fill=freq) +
 geom_col(color="black") +
 scale_fill_gradient2(low = "black", high = "navy") +
13
R-Code for Final Project – Plane Crashes Since 1908
Team D – Andrew Alford, Ryan Summers, Bill Steel, Anand Louis
 theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(face = "bold"))
+
 labs(y="Frequency", x = "Aircraft Type", title = "Total Crashes by Military AirCraft Type", fill =
"Frequency")

## Choropleth of US/Alaska/Hawaii
library(usmap)
library(dplyr)
crashes$state_list<-tolower(crashes$state_list)
crashes_us_states<-crashes%>% filter(!is.na(crashes$state_list))
dfSimple<-plyr::count(crashes_us_states$state_list)
dfSimple<-arrange(dfSimple,desc(dfSimple$freq))
states_df <- usmap::us_map()
states_df$full<-tolower(states_df$full)
mergeUs<-merge(states_df,dfSimple,all.x=TRUE,by.x="full",by.y="x")

# restore the order for the points (lat/long) in the polygon

# merging often rearranges the order
mergeUs %<>% arrange(order)
p3 <- plot_usmap("states", labels = FALSE ,alpha = 1.00 ) +
geom_polygon(data=mergeUs,color="black",aes(x=x,y=y,group=group,fill=freq)) +
scale_color_gradient(low="purple",high="orange") +
labs(title="Frequency of Aircrashes across US states (1908 to 2019)") +
theme(plot.title = element_text(face="bold",size=15)) +

# reverses color gradient scale - more intuitive this way
scale_fill_continuous(trans='reverse')
p3
