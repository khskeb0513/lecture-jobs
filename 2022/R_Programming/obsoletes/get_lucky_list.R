print(Sys.time())
lucky_list <- c()
for (i in 1:6) {
  append(sort(sample(1:45, 6, replace = F)), lucky_list)
}
print(lucky_list)