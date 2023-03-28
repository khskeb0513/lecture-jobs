# q1

rm(list = ls())

student_num = c(11, 15, 3, 8, 6, 6, 8, 13)
x.scaled = (student_num - min(student_num)) / (max(student_num) - min(student_num))
x.scaled
