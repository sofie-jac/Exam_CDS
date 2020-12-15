#plot the data with thresholds
plot_SAD_scales<- function(scores){
  pacman::p_load(RColorBrewer)
  scores<- scores %>% 
    mutate(Age_factor = case_when(
      Age < 20 ~ 'Late Teens', 
      between(Age, 20, 22) ~ 'Early Twenties',
      between(Age, 23, 26) ~ 'Mid Twenties', 
      between(Age, 27, 29) ~ 'Late Twenties',
      Age > 29 ~ 'Thirty +'))
  scores$Age_factor <- factor(scores$Age_factor, levels = c("Late Teens", "Early Twenties", "Mid Twenties", "Late Twenties", "Thirty +"))
  scores$Gender <- as.factor(scores$Gender)
  plot <- ggplot(data = scores)+
  geom_smooth(method = 'lm', aes(SIAS, CUSADOS), color = 'black') +
  geom_point(aes(SIAS, CUSADOS, color = Age_factor, pch = Gender)) +
  geom_vline(xintercept = 43, color = "gray") + #sias threshold    
  geom_hline(yintercept = 16, color = "gray") + #cusados threshold
  theme_minimal() +
  scale_colour_viridis_d() +
  labs(color = "Age")
  return(plot)
}

