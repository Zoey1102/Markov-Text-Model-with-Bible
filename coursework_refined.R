# Import Data directly (just FYI)
# install.packages("gutenbergr"); library(gutenbergr)
# bible <- gutenberg_download(1581)

# Read the text file
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license


# TEXT CLEANSING: 
# Remove the punctuation mark attached to each word, 
#   and insert it back as a new entry after the word it came from.

# First, define a function split_punct:
split_punct <- function(x, y=","){
  im <- grep(y,x, fixed = TRUE) ## indices of "a specific mark" words
  if(!!length(im)){ ## If length(im) > 0, !!(length(im)) is TRUE, then go next step
    x <- gsub(y,"",x,fixed = TRUE) ## remove the mark
    xm <- rep("",length(x)+length(im)) ## Initializing the new vector
    iim <- im + 1:length(im) ## indices of these marks in xm
    xm[iim] <- y ## insert the mark
    xm[-iim] <- x ## insert x in unoccupied place
    x <- xm ## update x
  }
  return(x)
}

# Then Apply the function to marks ",", ".", ";", "!", ":", "?"
split.a <- split_punct(a) |>     ## stable version Base R pipe |> since R-4.1
  split_punct(".") |> split_punct(";") |> 
  split_punct("!") |> split_punct(":") |>  split_punct("?")


# To find the vector of unique words:
lower.a <- tolower(split.a) ## bring all words to lower case
unique.a <- unique(lower.a) 
# Indices of those unique words corresponding to each words in text
index <- match(lower.a, unique.a) 
# Find the frequency of each words
freq <- tabulate(index)

# Search for the threshold required to retain about m numbers of words
words_df <- data.frame(words = unique.a, ##  combine the unique words with it's frequencies in a data frame
                       words_freq = freq) 
order_words_df <- words_df[order(words_df$words_freq, decreasing = TRUE),] ## order the data frame by frequency
m = 1000 ## consider 1000 most common words
threshold <- order_words_df[m,2] ## the threshold number of occurrences is 90
sum(order_words_df$words_freq >= 90)  ## 1004
sum(order_words_df$words_freq > 90)  ## 995

# The 1004 most commonly occurring words is therefore:
b_df <- subset(order_words_df[1], order_words_df$words_freq >= threshold) ## subset with the threshold
#rownames(b_df) <- NULL
#colnames(b_df) <- NULL
b <- as.matrix(b_df) ## change the data frame into a matrix/vector



# Indices of those most common words corresponding to each words in text
ib <- match(lower.a, b)


# The pairwise comparison of consecutive indices:
b_pair <- cbind(ib[-length(ib)], ib[-1])
# Get rid of NA pairs:
pair_valid <- subset(b_pair, is.na(rowSums(b_pair)) == 0)
colnames(pair_valid) <- c("Now", "Next")

# Vector of the probability of getting the next most common words:
#f_vector <- prop.table(table(pair_valid))[,2]

# the A matrix is given by f_vector by Transpose(f_vector)
#A <- f_vector %*% t(f_vector)

A <- matrix(rep(0, 1004*1004), 1004, 1004) # Initializing A
For (i = 1:1004){
  j_number <- unique(subset(pair_valid[,2], pair_valid[,1] == i))
  
}





