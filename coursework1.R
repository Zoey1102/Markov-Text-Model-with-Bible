a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a<- a[-((n-2909:n))]
split_punct <- function(x)
{
  #create an empty vector to store the final result
  result<-c()
  #use a-f to store the location information of punctuation
  a <- grep(",",x)
  b <- grep("\\.",x)
  c <- grep(";",x)
  d <- grep("!",x)
  e <- grep(":",x)
  f <- grep("\\?",x)
  #remove the punctuation from the text
  x <- gsub(",","",x)
  x <- gsub("\\.","",x)
  x <- gsub(";","",x)
  x <- gsub("!","",x)
  x <- gsub(":","",x)
  x <- gsub("\\?","",x)
  #use pos to store the position of all the punctuation mark
  pos <- c(a,b,c,d,e,f)
  pos_new <- sort(pos)
  #j as a start point of index,and give the initial value 1
  j=1
  for (i in pos_new)
  {
    #the following conditional statement means to add corresponding punctuation
    #following the words
    if (i %in% a) result<-c(result,x[j:i],",")
    if (i %in% b) result<-c(result,x[j:i],".")
    if (i %in% c) result<-c(result,x[j:i],";")
    if (i %in% d) result<-c(result,x[j:i],"!")
    if (i %in% e) result<-c(result,x[j:i],":")
    if (i %in% f) result<-c(result,x[j:i],"?")
    #update j
    j=i+1
  }
  #add remaining words to the tail of 'result'
  if(pos_new[length(pos_new)]!=length(x))result<-c(result,x[pos_new[length(pos_new)]:length(x)])
  return(result)
}