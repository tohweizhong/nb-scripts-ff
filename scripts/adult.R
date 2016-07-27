
library(ff)
library(ffbase)
options("fftempdir" = "C:/development/ff-nb-scripts/ff-tmp")
options("ffbatchbytes" = 170634772 * 10)

# open file connection
filecon <- file("C:/development/ff-nb-scripts/data/adult.txt")

# import data
system.time(
  df <- read.csv.ffdf(file = filecon, colClasses = NA, header = TRUE)
)

# save
#ffsave(list = "df", file = "C:/development/income-ffdata/")

## is.na <-
df[is.na(df[,1]),1] <- 0

## subset
df <- subset(df, sex == " Female")

## binning
### creating a new column, then cbind

newffcol <- as.ffdf(data.frame(rep(0, nrow(df))))
for(i in 1:nrow(df)){
  
  if(df$education[i] == " Bachelors")
    newffcol[i,1] <- 1
  else
    newffcol[i,1] <- 0
}

## splitting for CV (try caret)
library(caret)
train_idx <- caret::createDataPartition(as.ram(df$age), p = 0.7, list = FALSE)[,1] %>% as.ff()

df_train <- ffdfindexget(df, train_idx)


## which
idx <- which(df[,"education"] == " Bachelors")

df2 <- df[idx,] # data.frame in RAM, not ffdf
df3 <- as.ffdf(df2) # number of columns files (.ff) doubled in ff-tmp

## ffwhich

idx <- ffwhich(df, education == " Bachelors")
df2 <- df[idx,][,]

## for loop
newcol <- NULL
for(i in 1:nrow(df)){
  
  if(df[i,"education"] == " Bachelors")
    newcol <- c(newcol, "isBachelors")
  else
    newcol <- c(newcol, "isNotBachelors")
}

## cbind

df4 <- ccbind(df, newcol %>% data.frame)

## regression
library(biglm)

c1 <- df[1:10000,]
c2 <- df[10001:20000,]
c3 <- df[20001:nrow(df),]

logmod <- bigglm(sex ~ age + education.num, data = c1, family = binomial(link = "logit"))

lmmod <- bigglm(age ~ sex, data = df, family = gaussian())

lmmod2 <- biglm(data = df, age ~ education.num + workclass)

logmod <- bigglm()
