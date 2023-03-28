df = read.csv('credits.csv')
colnames(df)
colnames(df) = c(
  'payment_date',
  'used_card',
  'where',
  'paid_amount',
  'nth installment',
  'debt_amount',
  'fee',
  'discount_type',
  'discount_amount',
  'remain_amount_after_discount',
  'credited_point_rate'
)
df$credited_point_rate = as.numeric(gsub('%', '', df$credited_point_rate)) / 100
mean(df$credited_point_rate, na.rm = T)
df$debt_amount = as.numeric(gsub(',', '', df$debt_amount))
sum(df$debt_amount)
aggregated = aggregate(df$debt_amount ~ df$used_card + df$where, df, sum)
aggregated = aggregated[order(aggregated$`df$debt_amount`, decreasing = T),]
colnames(aggregated) = c('used_card', 'where', 'debt_amount')
aggregated$amount_categorize = cut(aggregated$debt_amount,
                                   c(-Inf, 10000, Inf),
                                   labels = c('10,000이하', '10,000이상'))


