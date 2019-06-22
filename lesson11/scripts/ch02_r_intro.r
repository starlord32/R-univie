## Load libraries

OUTDIR = "../img/ch02"
dir.create(OUTDIR, FALSE, TRUE)

## A Short Introduction to R

1 + 2

1 / (2 + 17) - 1.5 + sqrt(2)

x <- 1 + 2
x

x/2
x

y <- x/2
y

class(x=y)
class(y)

class(x=class)

ls()
ls(envir=baseenv())

# Numeric Vectors

vecObj <- c(1,10,100)
vecObj

class(vecObj)

vecObj + 10
vecObj + vecObj

c(1,2,3,4,5,6) + c(100,200)

1:84

length(x=10:42)

## Logical Vectors

numericVec <- 1:10
logicalVec <- (numericVec >= 5)
logicalVec

class(logicalVec)

numericVec == 4

logicalVec <- c(TRUE,TRUE,FALSE,TRUE)
class(logicalVec)

logicalVec1 <- c(FALSE,FALSE,TRUE,TRUE)
logicalVec2 <- c(FALSE,TRUE,FALSE,TRUE)
logicalVec1 | logicalVec2
logicalVec1 & logicalVec2
!logicalVec1

logicalVec1 + logicalVec2
logicalVec1 + 1
class(logicalVec1 + 1)

# Subsetting

vectorObj <- 101:132
vectorObj[20]

vectorObj[17] <- -2
vectorObj

vectorObj[c(6,17,20,30)]
vectorObj[5:10]

vectorObj[c(20,20,20)]

vectorObj[length(vectorObj):1]

vectorObj[1:10] = 1
vectorObj

vectorObj <- 101:132
vectorObj[-1]

vectorObj[-c(1,length(vectorObj))]
vectorObj <- 1:8
vectorObj[c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE)]

logicalIndex <- vectorObj > 5
logicalIndex

newVectorObj <- vectorObj[logicalIndex]
newVectorObj

vectorObj <- 1:100
vectorObj[vectorObj <= 25] <- 25
vectorObj[vectorObj >= 75] <- 75
vectorObj

## Character Vectors

stringVec <- c("pear","apple","pineapple")
class(stringVec)
stringVec

paste(stringVec, "juice", sep = " ")

substr(stringVec, start=2, stop=4)

nchar(stringVec)
substr(stringVec, 3, nchar(stringVec))

index <- grep(pattern="apple", x=stringVec)
index

stringVec[index]

# Error: "1" + 1

paste(4:12, "th", sep = "")

as.numeric("1") + 1

## Matrices and Data Frames

mat <- matrix(data=1:12, nrow=3, ncol=4, byrow=TRUE)
mat

class(mat)

mat <- matrix(data=1:12, nrow=3, ncol=4, byrow=TRUE)
mat
class(mat)

mat[2,3]
class(mat[2,3])

mat[1:2,1:2]

class(mat[1:2,1:2])

mat[,2:3]

class(mat[1,])
class(mat[1,,drop=FALSE])

matCol <- matrix(data=1:12, nrow=3, ncol=4, byrow=FALSE)
matCol + 1

mat + matCol

# Error: mat + matCol[,2:3]

df <- data.frame(a = 1:5, b=21:25, c=1:5 + 0.5)
df

class(df)

dim(df)

colnames(df)
rownames(df)

colnames(df)[2] <- "newName"
df

df$newName
class(df$newName)

df$newColumn <- 5:1
df

## Data I/O

getwd()

# Set to your own directory: setwd("/Users/myUserName/Desktop")
getwd()

dir()

fruitData <- read.csv(file="../data/ch02/fruitData.csv", as.is=TRUE)
fruitData

class(fruitData)

class(fruitData$Juice)

write.csv(x=fruitData$Juice,file="../data/ch02/fruitDataJuice.csv")

fruitDataJuice <- read.csv(file="../data/ch02/fruitDataJuice.csv")
class(fruitDataJuice)

saveRDS(object=fruitData$Juice, file="../data/ch02/fruitDataJuice.rds")

fruitDataJuice <- readRDS(file="../data/ch02/fruitDataJuice.rds")
class(fruitDataJuice)

fruitDataJuice

## Advanced Subsetting

index <- (fruitData$Color == "red" | fruitData$Color == "green")
index

fruitData[index,]

fruitData$Color %in% c("red", "green")

fruitData[fruitData$Color %in% c("red", "green"),]

fruitData[fruitData$Color %in% fruitData$Fruit,]

index <- order(fruitData$Juice,decreasing=TRUE)
index
fruitData[index,]

fruitNutr <- read.csv("../data/ch02/fruitNutrition.csv", as.is=TRUE)
fruitNutr

index <- match(x=fruitData$Fruit, table=fruitNutr$Fruit)
index

is.na(index)

fruitData$Calories <- NA

fruitData$Calories[!is.na(index)] <-
                     fruitNutr$Calories[index[!is.na(index)]]
fruitData
