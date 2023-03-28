x = list(a = 1:5,
         b = 1:10,
         c = c(T, F, T, F))
lapply(x[c(1, 2)], mean)
sapply(x[c(1, 2)], mean)
lapply(seg.df[c(1, 3, 4)], mean)

mapply(cat, 1:2, 2:1, '\n\n')
