#---------------------------------------
### Basic calculator
#---------------------------------------

5+10
2-3
8*exp(10)/log(2)

### Vectors, matrices and various objects
# Empty vector

vec <- c()

## Vectors
vec2 <- c(1,2,3,4)
colors <- c("blue", "red", "yellow", "green")

# Check type of data in vectors
typeof(vec)
typeof(vec2)
typeof(colors)

# Length of vectors
length(vec)
length(vec2)
length(colors)

# Accessing vector value
colors[1]

## Matrix
mat <- matrix(NA, ncol=2, nrow=2)
mat2 <- matrix(vec2, ncol=2, nrow=2)

# Access value of matrix : mat[row, column]
mat2[1,2]
mat2[2,1]

# Replace element of matrix
mat2[1,1] <- 0
mat2[1,1]

# Dimension
dim(mat2)

## Dataframe
df <- as.data.frame(cbind(vec2, colors))
print(df)

# Display row / column
df[1, ]
df[ ,2]

# Access column by their name
df[1,"colors"]

# Rename columns
colnames(df) <- c("col1","col2")
print(df)

# Access column by special nomenclature
df$col1

#---------------------------------------
### Uploading data
#---------------------------------------

## Working directory
# Check
getwd()

# Set
setwd("C:/Users/adria/OneDrive/Documents/HEC/4e année (MScM-BA)/1st semester/01. Quantitative Methods for Management/02. Exercices 2020/00. R tutorial")

## Data file
# Read

dataframe <- read.csv("00. dataex.csv")

# Modifying the dataframe
seq1 <- rep(1,50)
seq2 <- seq(from=1, to=50, by=1) # can also be written as seq(1,50,1)
dataframe2 <- as.data.frame(cbind(dataframe, seq1, seq2))

# Write

write.csv(dataframe2, "dataframe2.csv", row.names=FALSE)

### Basic statistical tools
