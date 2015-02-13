#install.packages("R.utils")
#install.packages("data.table")
#install.packages("lubridate")
#install.packages("hash")
require("R.utils")
require("plyr")
require("lubridate")
require("hash")
require("ggplot2")

##functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#PROPDMGEXP : A multiplier where Hundred (H), Thousand (K), Million (M), Billion (B)
#CROPDMGEXP : A multiplier where Hundred (H), Thousand (K), Million (M), Billion (B)
multiplier <- function(x) {
  m <- hash()
  clear(m)
  keys<- sort(unique(x))
  .set(m, keys, rep(0, length(keys)))
  .set(m, "H",100)
  .set(m, "K",1000)
  .set(m, "M",10^6)
  .set(m, "B",10^9)
  
  m[[if (is.na(x)) 0 else toupper(x)]]
}

#require("data.table")
urlfile <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipfile <- "repdata-data-StormData.csv.bz2"
csvfile <- "repdata-data-StormData.csv"

if (!file.exists(zipfile))
  download.file(urlfile, destfile=zipfile)

if (!file.exists(zipfile))
  bunzip2("repdata-data-StormData.csv.bz2", overwrite=T, remove=F)

#open large file
small = read.csv(file = csvfile, header = TRUE, nrow=5, stringsAsFactors=FALSE);
classes = sapply(small,class)

classes["BGN_TIME"]<- "character"
classes["F"]<- "character"
classes[classes == "logical"] = "character"

data = read.csv(file = csvfile, header = TRUE, na.strings = "", colClasses = classes, stringsAsFactors=FALSE);
data_bak <- data

data <- data_bak

#number of observations
num_rows <- dim(data)[1]
num_rows
#format(num_rows, big.mark=" ")

#eliminate row with no event
data <- subset(data, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)

#number of observations
num_rows <- dim(data)[1]
num_rows

##
colnames(data)

#keep variables
data <- data[,c("BGN_DATE","STATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]

#extract year
data$YEAR <- year(mdy_hms(data$BGN_DATE, truncated = 3))

## histogram of the total number of steps taken each day
histo <- ggplot(data, aes(x= YEAR)) +
  geom_histogram(fill="lightblue", colour="black", binwidth=1) +  
  xlab("Year") + ylab("frequency")
  ggtitle("Storm frequency by year in US")
histo

#aggegate data and keep the min year where observations greater or equal to 10000 
c<-ddply(data, .(YEAR), summarise, count = length(YEAR) )
min_year <- min(c[c$count >= 10000,])
min_year

#filter
data <-subset(data, YEAR >= min_year)

#cleannig events type
data$EVTYPE <- toupper(tolower(data$EVTYPE))
data$EVTYPE <- gsub("  "," ", x = data$EVTYPE)
data$EVTYPE <- trim(data$EVTYPE)

sort(unique(data$EVTYPE))

data$EVTYPE[data$EVTYPE == "AVALANCE"] = "AVALANCHE"
data$EVTYPE[data$EVTYPE == "COASTALSTORM"] = "COASTAL STORM"
data$EVTYPE[grep("BLIZZARD*", data$EVTYPE)] = "BLIZZARD"
data$EVTYPE[grep("THUNDERSTORM*", data$EVTYPE)] = "THUNDERSTORM"
data$EVTYPE[grep("WATERSPOUT*", data$EVTYPE)] = "WATERSPOUT"
data$EVTYPE[grep("HAIL*", data$EVTYPE)] = "HAIL"
data$EVTYPE[grep("HEAVY RAIN*", data$EVTYPE)] = "RAIN"
data$EVTYPE[grep("UNSEASONAL RAIN*", data$EVTYPE)] = "RAIN"
data$EVTYPE[grep("COASTAL FLOOD*", data$EVTYPE)] = "COASTAL FLOOD"
data$EVTYPE[grep("FLASH FLOOD*", data$EVTYPE)] = "FLASH FLOOD"
data$EVTYPE[grep("FLOOD/FLASH*", data$EVTYPE)] = "FLASH FLOOD"

data$EVTYPE[grep("HEAVY SNOW*", data$EVTYPE)] = "SNOW"
data$EVTYPE[grep("^SNOW", data$EVTYPE)] = "SNOW"


data$EVTYPE[grep("FREEZING*", data$EVTYPE)] = "FREEZING"


data$EVTYPE[data$EVTYPE == "TORNDAO"] = "TORNADO"
data$EVTYPE[grep("TORNADO*", data$EVTYPE)] = "TORNADO"
data$EVTYPE[grep("TROPICAL STORM*", data$EVTYPE)] = "TROPICAL STORM"
data$EVTYPE[grep("TSTM*", data$EVTYPE)] = "TSTM"

data$EVTYPE[grep("THUNDE[A-Z]+ WIND*", data$EVTYPE)] = "WIND"
data$EVTYPE[grep("^WIND", data$EVTYPE)] = "WIND"
data$EVTYPE[grep("WIND$", data$EVTYPE)] = "WIND"

data$EVTYPE[grep("UNSEASONABL[A-Z] COLD", data$EVTYPE)] = "COLD"
data$EVTYPE[grep("UNSEASONABLY WARM*", data$EVTYPE)] = "WARM"
data$EVTYPE[grep("^WARM*", data$EVTYPE)]= "WARM"
data$EVTYPE[grepl("URBAN FLOOD*", data$EVTYPE) |
            grepl("^URBAN[^a-zA-Z0-9_]+SMALL*", data$EVTYPE, perl = TRUE) |
            grepl("^URBAN[^a-zA-Z0-9_]+SML[A-Z0 ]*", data$EVTYPE, perl = TRUE) |
            grepl("^URBAN[A-Z0 ]+SMALL*", data$EVTYPE, perl = TRUE)
            ] = "URBAN FLOOD"
data$EVTYPE[grep("WINTER STORM*", data$EVTYPE)] = "WINTER STORM"
data$EVTYPE[grep("^WILD[^a-zA-Z0-9_]+", data$EVTYPE)] = "WILD FIRE"
data$EVTYPE[grep("WILDFIRE", data$EVTYPE)] = "WILD FIRE"
data$EVTYPE[grep("WINTER WEATHER*", data$EVTYPE)] = "WINTER WEATHER"

sort(unique(data$EVTYPE))

#data$TIME_ZONE <- toupper(tolower(data$TIME_ZONE))
#data$TIME_ZONE[data$TIME_ZONE == "CST" ] <- "America/Chicago"
#data$TIME_ZONE[data$TIME_ZONE == "CEST" ] <- "Europe/Paris"


##Across the United States, 
## which types of events (as indicated in the EVTYPE variable) 

## are most harmful with respect to population health?
#data <- data.table(data)

d <- ddply(data, .(EVTYPE), summarise, 
                      tot_fatalities = sum(FATALITIES, na.rm = TRUE),
                      tot_injuries = sum(INJURIES, na.rm = TRUE),
                      tot = tot_injuries + tot_fatalities)
d<-setorder(as.data.table(d), -tot)
t<- head(d, n = 10)

dam <- melt(head(data[order(-data$FATALITIES, -data$INJURIES), ], 10))

harm <- ggplot(data = t,aes(x=EVTYPE, y=tot, fill=EVTYPE)) + 
  geom_histogram(stat="identity", binwith=10) + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) + coord_flip()
harm

harm <- ggplot(data = t,aes(x=EVTYPE, y=factor(tot_fatalities), fill=factor(tot_injuries))) + 
  #geom_histogram(stat="identity", binwith=10) + 
  stat_summary(fun.y = sum, position = "stack", geom = "bar") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) + coord_flip()
harm








