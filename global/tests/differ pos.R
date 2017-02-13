
difer <- positionsLoc %>% 
  mutate(Position.x = round(Position.x*10000)/10000,
         Position.y = round(Position.y*10000)/10000,
         Weight.x = round(Weight.x*10000)/10000,
         Weight.y = round(Weight.y*10000)/10000,
         multiple.x = round(multiple.x*10000)/10000,
         multiple.y = round(multiple.y*10000)/10000,
         rounded.x = round(rounded.x*10000)/10000,
         rounded.y = round(rounded.y*10000)/10000,
         netPosition.x = round(netPosition.x*10000)/10000,
         netPosition.y = round(netPosition.y*10000)/10000) %>%
  filter(completeCode.x!=completeCode.y | 
         Position.x!=Position.y |
         Weight.x!=Weight.y |
         multiple.x!=multiple.y |
         rounded.x!=rounded.y |
         netPosition.x!=netPosition.y) %>%
  select(period, Date, 
         completeCode.x, completeCode.y,
         Position.x, Position.y,
         Weight.x, Weight.y,
         multiple.x, multiple.y,
         rounded.x, rounded.y,
         netPosition.x, netPosition.y)