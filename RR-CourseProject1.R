# Create data directory and download project files
if(!file.exists("./data")) {dir.create("./data")}
fileUrl = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data/RRactivityData.zip", method="curl")

# Unzip files
unzip("./data/RRactivityData.zip", exdir = "./data")

activity <- read.csv(file = "./data/activity.csv")

head(activity)

