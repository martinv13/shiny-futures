


tickers <- futuresData$tickers %>% filter(Type %in% c("Agriculture", "Energy", "Metals"))

returns <- tickers$`Quandl Code` %>%
  lapply(function(code){
    Contract$new(code)$monthlyRoll(contractNumber=3)$series %>%
      mutate(genericCode = code)
  }) %>% 
  bind_rows() %>%
  group_by(genericCode, period) %>%
  filter(Date == min(Date)) %>%
  group_by(genericCode) %>%
  arrange(Date) %>%
  mutate(periodReturns = lead(Value)/Value-1)

source <- returns %>%
  select(Date, period, genericCode, periodReturns) %>%
  full_join(futuresData$data, by=c("Date", "genericCode")) %>%
  filter(!is.na(period)) %>%
  group_by(period, genericCode) %>%
  mutate(frontExp = min(expDate)) %>%
  mutate(yearExp = as.Date(paste(year(frontExp+1)+1,month(frontExp+1),mday(frontExp+1),sep="-"), format="%Y-%m-%d")-1)

carr <- source %>% filter(expDate == frontExp) %>%
  left_join(source %>% 
              transmute(Date=Date, genericCode=genericCode, yearExp=expDate, yearSettle=Settle), 
            by=c("Date", "genericCode", "yearExp")) %>%
  transmute(
    Date = Date,
    genericCode = genericCode,
    carry = Settle/yearSettle-1,
    periodReturns = (periodReturns+1)^12-1) %>%
  filter(!is.na(carry))

carr %>% ggplot(aes(y = periodReturns, x = carry)) +
  geom_point()

m <- lm( periodReturns~carry, data=carr)
  

