rm(list = ls())

df = read.csv('segments.csv')

apply(df[, c('age', 'income', 'kids')], 2, mean)
lapply(c(1, 2, 3), sum)
sapply(list(c(1, 2, 3), c(2, 3, 4)), sum)
tapply(iris$Sepal.Length, iris$Species, mean)
mapply(sum, 1:4, 4:1)

apply(df[df$Segment == 'Moving up', ][, c('age', 'income', 'kids')], 2, mean)
by(df$income, df$Segment, mean)
aggregate(df$income, list(Segment = df$Segment), mean)
aggregate(df$age ~ df$Segment + df$gender, df, mean)

aggregated_df = aggregate(df$income ~ df$gender + df$subscribe, df, mean)
cut(
  aggregated_df$`df$income`,
  c(0, 20000, 30000,  Inf),
  c('0-20000', '20000-30000', '30000-')
)

various_apples = c("apple", "Apple", "apple2", "bbapple")
various_apples[grep('ap', various_apples)]
