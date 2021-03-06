# Read the text file
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

# Write the function to split punctuation marks
split_punct <- function(x){
  # use a-f to store the location information of punctuation
  a <- grep(",",x)
  # length(a) != 0 means that here are some "," in x, then go next step
  if(length(a) != 0){
    x <- gsub(",","",x) # remove ","
    as <- a + 1:length(a) ## use as to store the position of "," in the new vector
    xs_a <- rep("",length(x)+length(a)) ## create a new empty vector
    xs_a[as] <- c(",") ## insert ","
    xs_a[-as] <- x ## insert the origin x
    x <- xs_a ## update x
  }
  # following code are similar to the previous code so I will not attach more note
  b <- grep("\\.",x)
  if(length(b) != 0){
    x <- gsub("\\.","",x)
    bs <- b + 1:length(b)
    xs_b <- rep("",length(x)+length(b))
    xs_b[bs] <- c(".")
    xs_b[-bs] <- x
    x <- xs_b
  }
  c <- grep(";",x)
  if(length(c) != 0){
    x <- gsub(";","",x)
    cs <- c + 1:length(c)
    xs_c <- rep("",length(x)+length(c))
    xs_c[cs] <- c(".")
    xs_c[-cs] <- x
    x <- xs_c
  }
  d <- grep("!",x)
  if(length(d) != 0){
    x <- gsub("!","",x)
    ds <- d + 1:length(d)
    xs_d <- rep("",length(x)+length(d))
    xs_d[ds] <- c("!")
    xs_d[-ds] <- x
    x <- xs_d
  }
  e <- grep(":",x)
  if(length(e) != 0){
    x <- gsub(":","",x)
    es <- e + 1:length(e)
    xs_e <- rep("",length(x)+length(e))
    xs_e[es] <- c(":")
    xs_e[-es] <- x
    x <- xs_e
  }
  f <- grep("\\?",x)
  if(length(f) != 0){
    x <- gsub("\\?","",x)
    fs <- f + 1:length(f)
    xs_f <- rep("",length(x)+length(f))
    xs_f[fs] <- c("?")
    xs_f[-fs] <- x
    x <- xs_f
  }
  return(x)
}

# Apply the split function to the text file a
split.a <- split_punct(a)

lower.a <- tolower(split.a) ## lower all the words
unique.a <- unique(lower.a) ## find the unique words in text
index <- match(lower.a,unique.a) ## match each words in text back to the unique words
freq <- tabulate(index) ## find the frequency of each words
words_df <- data.frame(words = unique.a, ##  combine the unique words with ites frequencies in a data frame
                       words_freq = freq) 
order_words_df <- words_df[order(words_df$words_freq, decreasing = TRUE),] ## order the data frame by frequency
m = 1000 ## consider 1000 most common words
threshold <- order_words_df[m,2] ## find the frequency threshold

# Creat vector b with 1000 most commonly occurring words
b <- subset(order_words_df[1], order_words_df$words_freq >= threshold) ## subset with the threshold
rownames(b) <- NULL
colnames(b) <- NULL
b <- as.matrix(b) ## change the data frame into a matrix