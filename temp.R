
gl_data %>% filter(genericCode == "CME/NG") %>%
  mutate(monthExp=month(expDate)) %>%
  group_by(monthExp) %>%
  summarise(moy=mean(Settle)) %>% 
  ggplot(aes(x = monthExp, y=moy)) +
  geom_line()

gl_data %>% filter(genericCode == "CME/NG") %>%
  mutate(ttExp=month(expDate)) %>%
  


library(ggplot2)