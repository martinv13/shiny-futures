
test <- self$data %>%
  group_by(genericCode, Date) %>%
  filter(row_number(expDate) == 1)
test %<>% mutate(period = (year(Date)-1970)*12+month(Date))

# compute factor weight for each period
weights <- lapply(10:max(test$period), function(p) {
  print(p)
  pca <- test %>% 
    ungroup() %>%
    filter(period < p) %>%
    filter(Date >= max(Date)-500) %>%
    group_by(genericCode) %>%
    mutate(npoints=n()) %>%
    ungroup() %>%
    filter(npoints>0.7*max(npoints)) %>%
    group_by(genericCode) %>%
    arrange(Date) %>%
    mutate(logValue = cumsum(logReturns)) %>%
    select(Date, genericCode, logValue) %>%
    spread(genericCode, logValue) %>%
    fill(-Date) %>%
    gather(genericCode, logValue, -Date) %>%
    filter(!is.na(logValue)) %>%
    group_by(genericCode) %>%
    arrange(Date) %>%
    mutate(rollReturns = c(rep(NA,10),exp(diff(logValue, lag=10))-1)) %>%
    filter(!is.na(rollReturns)) %>%
    select(Date, genericCode, rollReturns) %>%
    spread(genericCode, rollReturns) %>%
    na.omit() %>%
    select(-Date) %>% prcomp()
  weights <- pca$rotation %>% data.table() %>% select(1:5)
  weights$genericCode <- rownames(pca$rotation)
  weights %>% gather(fact, weight, -genericCode) %>% mutate(period=p)
}) %>% bind_rows()

factTrend <- lapply(unique(weights$period), function(p) {
  print(p)
  ret <- test %>% ungroup() %>%
    filter(period < p) %>%
    filter(Date > max(Date) - 600) %>%
    full_join(weights %>% filter(period == p), by="genericCode", copy=TRUE) %>%
    group_by(fact, Date) %>%
    summarise(ret = log(sum((exp(logReturns)-1)*weight)+1)) %>%
    group_by(fact) %>%
    arrange(Date) %>%
    mutate(Value = cumsum(ret)) %>%
    select(Date, fact, Value)
  ret %>%
    group_by(fact) %>%
    arrange(desc(Date)) %>%
    mutate(rnum = row_number(desc(Date))) %>%
    filter(rnum %in% c(1,30,100,300)) %>%
    mutate(rnum=paste0("X",rnum)) %>%
    select(-Date) %>%
    spread(rnum, Value) %>%
    mutate(period = p)
}) %>% bind_rows()

factPos <- factTrend %>%
  transmute(period = period,
            fact = fact,
            Position = (((X1>X30)+(X1>X100)+(X1>X300))-
                          ((X1<X30)+(X1<X100)+(X1<X300)))/3)

assetsPos <- factPos %>% 
  full_join(weights, by=c("period", "fact"), copy=TRUE) %>%
  group_by(period, genericCode) %>%
  summarise(Position = sum(Position*weight)*0.2)

periodReturns <- test %>% 
  group_by(genericCode, period) %>%
  summarise(Date = min(Date),
            periodRet = sum(logReturns)) %>%
  left_join(assetsPos, by=c("period", "genericCode")) %>%
  filter(!is.na(Position)) %>%
  group_by(period) %>%
  summarise(Date = min(Date),
    periodRet = log(sum((exp(periodRet)-1)*Position)+1)) %>%
  arrange(Date) %>%
  mutate(Value = cumsum(periodRet))

periodReturns %>% select(Date, Value) %>% dygraph() %>% dyRangeSelector()




pcas <- test %>%
  group_by(period)

test2 <- test %>%
  group_by(genericCode) %>%
  arrange(Date) %>%
  mutate(logValue = cumsum(logReturns)) %>%
  select(Date, genericCode, logValue) %>%
  spread(genericCode, logValue) %>%
  fill(-Date) %>%
  gather(genericCode, logValue, -Date) %>%
  filter(!is.na(logValue)) %>%
  group_by(genericCode) %>%
  arrange(Date) %>%
  mutate(rollReturns = c(rep(NA,10),exp(diff(logValue, lag=10))-1)) %>%
  filter(!is.na(rollReturns)) %>%
  select(Date, genericCode, rollReturns) %>%
  spread(genericCode, rollReturns)

correl <- cor(test2%>%select(-Date), use="complete.obs")

test3 <- test2%>%na.omit()

pca <- prcomp(test2%>%na.omit()%>%select(-Date))

weights <- pca$rotation %>% data.table() %>% select(1:5)
weights$genericCode <- rownames(pca$rotation)
weights %<>% gather(fact, weight, -genericCode)

factorReturns <- test %>% ungroup() %>%
  full_join(weights, by="genericCode", copy=TRUE) %>%
  group_by(fact, Date) %>%
  summarise(ret = log(sum((exp(logReturns)-1)*weight)+1)) %>%
  group_by(fact) %>%
  arrange(Date) %>%
  mutate(Value = cumsum(ret)) %>%
  select(Date, fact, Value) %>%
  spread(fact, Value)

factorReturns %>% dygraph() %>% dyRangeSelector()
  

