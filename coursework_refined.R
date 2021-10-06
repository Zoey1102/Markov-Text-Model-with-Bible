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
lower.a <- tolower(iconv(split.a,"WINDOWS-1252", "UTF-8")) ## bring all words to lower case
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
sum(order_words_df$words_freq >= threshold)  ## 1004
sum(order_words_df$words_freq > threshold)  ## 995

# The 1004 most commonly occurring words is therefore:
b_df <- subset(order_words_df, order_words_df$words_freq >= threshold) ## subset with the threshold
#rownames(b_df) <- NULL
#colnames(b_df) <- NULL
b <- as.matrix(b_df[1]) ## change the data frame into a matrix/vector



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

A <- matrix(rep(0, length(b)*length(b)), length(b), length(b)) # Initializing A
for (i in 1:length(b)){
  subA <- pair_valid[pair_valid[, "Now"]==i,] ## find all the 'i' Now words in pair_valid and make a subset
  for (j in subA[,2]){ ## extract the Next word
    A[i,j] = A[i,j] + 1 ## pairs adding a 1 to A[i,j]
  }
  n <- nrow(subA) ## find the sum of each row i
  A[i,] <- A[i,]/n ## standardize the ith row of A, sum to 1
}



capital.a <- split.a[!is.na(ib)]
unique.cap <- unique(capital.a)
index.cap <- match(capital.a, unique.cap) 
freq.cap <- tabulate(index.cap)
capital_df <- data.frame(words = unique.cap,
                         capital_freq = freq.cap)
icap <- match(capital_df[,1],b)
capital_only <- subset(capital_df, is.na(capital_df[,1][icap]))
ilow <- match(tolower(capital_only[,1]),b)
lower_only <- subset(b_df[ilow,])
captial_lower <- cbind(capital_only, lower_only)
captial_lower$Capital <- ifelse(2*captial_lower$capital_freq > captial_lower$words_freq, 'True', 'False')
icap_true <- match(captial_lower[,3][captial_lower$Capital=='True'],b)
b_df$Capital <- ifelse(rownames(data.frame(rownames(b_df))) %in% icap_true, 'True', 'False')
for (i in 1:length(b)){
  if (b_df$Capital[i]=='True'){
    b_df[i,1] <- gsub('(^|[[:space:]])([[:alpha:]])', '\\1\\U\\2', b_df[i,1], perl=T) 
    }
}

