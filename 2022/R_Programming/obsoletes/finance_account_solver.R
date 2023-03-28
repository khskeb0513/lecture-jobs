rm(list = ls())
# install.packages('stringr')
library(stringr)

data = read.csv('fdata.csv', header = F)
colnames(data) = c('debit_account',
                   'debit_amount',
                   'credit_account',
                   'credit_amount')

debitAmounts = c()
for (v in str_extract_all(data$debit_amount, '\\d')) {
  debitAmounts = append(debitAmounts, as.numeric(paste0(v, collapse = '')))
}
debitAmounts = -debitAmounts
data[2] = debitAmounts
rm(debitAmounts)

creditAmounts = c()
for (v in str_extract_all(data$credit_amount, '\\d')) {
  creditAmounts = append(creditAmounts, as.numeric(paste0(v, collapse = '')))
}
# creditAmounts = -creditAmounts
data[4] = creditAmounts
rm(creditAmounts)

tempAccount = c(data$debit_amount, data$credit_amount)
names(tempAccount) = c(data$debit_account, data$credit_account)
tempAccount = tempAccount[!is.na(tempAccount)]
 
results = c()

for (numOfTempAccountToChoose in 1:length(tempAccount)) {
  for (extractedTempAccount in data.frame(combn(names(tempAccount), numOfTempAccountToChoose))) {
    results = append(results, sum(tempAccount[extractedTempAccount]))
    names(results)[length(results)] = paste0(extractedTempAccount, collapse = ',')
  }
}

results[results == 1200]
