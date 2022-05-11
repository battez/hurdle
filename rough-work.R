x <- c("doggy", "check")
letters <- vector("character")
for (i in 1:5) {
  exploded <- unlist(lapply(x, substring, i, i))
  letters <- append(letters, exploded )
}

df <- as.data.frame(letters)
df$letters <- as.factor(df$letters)
df
 
  ggplot(df, aes(x=letters) ) +
    geom_histogram(stat="count" ) 
