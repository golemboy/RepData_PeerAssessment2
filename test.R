#install.packages("R.utils")
#install.packages("data.table")
#install.packages("lubridate")
#install.packages("hash")
require("R.utils")
require("plyr")
require("lubridate")
#require("hash")
require("ggplot2")
require("reshape2")

##functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#require("data.table")
urlfile <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
zipfile <- "repdata-data-StormData.csv.bz2"
csvfile <- "repdata-data-StormData.csv"
csvCPIurl <- "https://www.quandl.com/api/v1/datasets/RATEINF/CPI_USA.csv"
csvCPIfile <- "CPI_USA.csv"

storm_event <- read.csv("StormDataEventTable.txt", fill = TRUE, header = FALSE, col.names = c("EVNAME"))
storm_event$EVNAME <- gsub(x = toupper(trim(storm_event$EVNAME)), pattern = " [CMZ]$",replacement = "")

if (!file.exists(zipfile))
  download.file(urlfile, destfile=zipfile)

if (!file.exists(csvCPIurl))
  download.file(csvCPIurl, destfile=csvCPIfile)

if (!file.exists(zipfile))
  bunzip2("repdata-data-StormData.csv.bz2", overwrite=T, remove=F)

##read CPI CSV file
us_cpi <- read.csv(csvCPIfile)

#aggregate by year
us_cpi$YEAR <- year(ymd(us_cpi$Date))
agg_us_cpi <- ddply(us_cpi,.(YEAR), summarise, CPIm=mean(CPI))

#duplicate CPI data and merge with 1 year gap
agg_us_cpi_1 <- agg_us_cpi[-1,]
agg_us_cpi_1$YEAR <- agg_us_cpi_1$YEAR - 1
colnames(agg_us_cpi_1)[2] <- "CPIm_1"
agg_us_cpi <- merge(x= agg_us_cpi, y = agg_us_cpi_1, by.x = "YEAR")
agg_us_cpi$rates <- (agg_us_cpi$CPIm_1 - agg_us_cpi$CPIm)/agg_us_cpi$CPIm * 100

us_rates <- agg_us_cpi[,c(1,4)]


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

## histogram of the storm frequency by year in US
histo <- ggplot(data, aes(x= YEAR)) +
  geom_histogram(fill="lightblue", colour="black", binwidth=1) +  
  xlab("Year") + ylab("frequency")
  ggtitle("Storm frequency by year in US")
histo

#aggegate data and keep the min year where observations greater or equal to 10000 
c<-ddply(data, .(YEAR), summarise, count = length(YEAR) )
min_year <- min(c[c$count >= 10000,])
min_year
max(data$YEAR)
#filter
data <-subset(data, YEAR >= min_year)
data_bak <- data

#number of observations
num_rows <- dim(data)[1]
num_rows

#cleannig events type
data$EVTYPE <- toupper(tolower(data$EVTYPE))
data$EVTYPE <- gsub("  "," ", x = data$EVTYPE)
data$EVTYPE <- trim(data$EVTYPE)
data$EVTYPE[grep("HURRICANE*", data$EVTYPE)] = "HURRICANE (TYPHOON)"
data$EVTYPE[grep("FLASH FLOOD*", data$EVTYPE)] = "FLASH FLOOD"
data$EVTYPE[grep("FLOOD/FLASH*", data$EVTYPE)] = "FLASH FLOOD"
data$EVTYPE[grep("WATERSPOUT*", data$EVTYPE)] = "WATERSPOUT"
data$EVTYPE[grep("TORNADO*", data$EVTYPE)] = "TORNADO"
data$EVTYPE[grep("WINTER STORM*", data$EVTYPE)] = "WINTER STORM"
data$EVTYPE[grep("TROPICAL STORM*", data$EVTYPE)] = "TROPICAL STORM"
data$EVTYPE[grep("HEAVY RAIN*", data$EVTYPE)] = "HEAVY RAIN"
data$EVTYPE[grep("HEAVY SNOW*", data$EVTYPE)] = "HEAVY SNOW"
data$EVTYPE[grep("TSTM WIND*", data$EVTYPE)] = "THUNDERSTORM WIND"
data$EVTYPE[grep("THUNDERSTORM WIND*", data$EVTYPE)] = "THUNDERSTORM WIND"
data$EVTYPE[grep("THUNDERSTORM[A-Z]+ WIND*", data$EVTYPE)] = "THUNDERSTORM WIND"
data$EVTYPE[grep("^HAIL", data$EVTYPE)] = "HAIL"
data$EVTYPE[grepl("HAIL", data$EVTYPE) & data$EVTYPE != "MARINE HAIL"]  = "HAIL"

#data_bak <- data
#data <- data_bak
sort(unique(np_data$EVTYPE))
storm_event[order(storm_event$EVNAME),]

#filtering permitting events
p_data<- data[(data$EVTYPE %in% storm_event$EVNAME),]
np_data<- data[!(data$EVTYPE %in% storm_event$EVNAME),]

#estimate coef for damage
coef_propdmg <- data.frame(PROPDMGEXP=c("H","K","M","B","+","-","?",as.character(0:8)),
                           coef_prop=c(100,1000,10^6,10^9,1,0,0,rep(1,9))
)
coef_cropdmg <- data.frame(CROPDMGEXP=c("H","K","M","B","+","-","?",as.character(0:8)),
                           coef_crop=c(100,1000,10^6,10^9,1,0,0,rep(1,9))
)

p_data<- merge(x = p_data, y = coef_propdmg, by = "PROPDMGEXP", all.x = TRUE)
p_data<- merge(x = p_data, y = coef_cropdmg, by = "CROPDMGEXP", all.x = TRUE)

np_data <- merge(x = np_data, y = coef_propdmg, by = "PROPDMGEXP", all.x = TRUE)
np_data <- merge(x = np_data, y = coef_cropdmg, by = "CROPDMGEXP", all.x = TRUE)

np_aggreg <- ddply(np_data, .(EVTYPE), summarise, 
                  tot_people = sum(FATALITIES) + sum(INJURIES), 
                  tot_damage = sum(PROPDMG * coef_prop) + sum(CROPDMG * coef_crop)
                  )
p_aggreg <- ddply(p_data, .(EVTYPE), summarise, 
                   tot_people = sum(FATALITIES) + sum(INJURIES), 
                   tot_damage = sum(PROPDMG * coef_prop) + sum(CROPDMG * coef_crop)
)

head(np_aggreg[order(-np_aggreg$tot_damage, -np_aggreg$tot_people),], 10)
head(p_aggreg[order(-p_aggreg$tot_damage, -p_aggreg$tot_people),], 10)

head(np_aggreg[order(-np_aggreg$tot_people, -np_aggreg$tot_damage),], 10)
head(p_aggreg[order(-p_aggreg$tot_people, -p_aggreg$tot_damage),], 10)

np_aggreg[order(-np_aggreg$tot_damage, -np_aggreg$tot_people),]



unique(np_data$PROPDMGEXP)


##Across the United States, 
## which types of events (as indicated in the EVTYPE variable) 
## are most harmful with respect to population health?
#data <- data.table(data)

d <- ddply(data, .(EVTYPE), summarise, 
           FATALITIES = sum(FATALITIES, na.rm = TRUE),
           INJURIES = sum(INJURIES, na.rm = TRUE),
           TOTAL = INJURIES + FATALITIES)

d<-d[order(-d$TOTAL),]
t<- head(d, n = 10)
t


m<- melt(t)
harm <- ggplot(data = m,aes(x=EVTYPE, y=factor(value), fill=factor(variable)), color=factor(variable)) + 
  stat_summary(fun.y = sum, position = position_dodge(), geom = "bar") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) + coord_flip()+
  ylab("Number of injuries / fatalities") + xlab("Event type")+
  guides(fill=guide_legend(title=NULL))
harm

data<- merge(x = data, y = coef_propdmg, by = "PROPDMGEXP", all.x = TRUE)
data<- merge(x = data, y = coef_cropdmg, by = "CROPDMGEXP", all.x = TRUE)

d <- ddply(data, .(EVTYPE), summarise, 
           PROPERTY_DAMAGE = sum(PROPDMG * coef_prop, na.rm = TRUE),
           CROP_DAMAGE = sum(CROPDMG * coef_crop, na.rm = TRUE),
           TOTAL = CROP_DAMAGE + PROPERTY_DAMAGE)

d<-d[order(-d$TOTAL),]
t<- head(d, n = 10)
t

m<- melt(t)
brks <- pretty(range(m$value), n = nclass.FD(m$value), min.n = 1)
bwidth <- brks[2]-brks[1]
nclass.FD(m$value)
bwidth

harm <- ggplot(data = m,aes(x=EVTYPE, y=value, fill=factor(variable)), color=factor(variable)) +   
  #geom_histogram(binwidth=bwidth, position="dodge", stat="identity") + 
  stat_summary(fun.y = sum, position = position_dodge(), geom = "bar", binwidth=bwidth) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) + #coord_flip()+
  ylab("Number of injuries / fatalities") + xlab("Event type")+
  guides(fill=guide_legend(title=NULL))
harm








