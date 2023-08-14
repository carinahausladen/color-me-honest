df_lng %>%
  left_join(read_csv("totaldist-annotated.csv", show_col_types = FALSE) %>%
              select(!treatment),
            by="participant.code") %>%
#  filter(treatment=="NCP"|treatment=="NCNP") %>%
  filter(type=="A") %>%
  
  group_by(participant.code) %>%

  mutate(x = x-x[which.min(t)], # starting point 0,0
         y = y-y[which.min(t)]) %>%

  mutate(x_last=x[which.max(t)],
         y_last=y[which.max(t)],
         angle = atan2(y_last, x_last)*-1) %>% #define rotation angle
  
  mutate(x_new = x * cos(angle) - y * sin(angle), #rotate
         y_new = x * sin(angle) + y * cos(angle),
         sum_y=sum(y_new)) %>%
  
  mutate(y_new = ifelse(sum_y > 0, y_new, -y_new)) %>%
  ungroup() %>%
  select(participant.code, treatment, x_new, y_new,t, report)->df_lng_rot
  
 # write.csv("/Users/carinaines/df_lng.csv")

