# Figura 1a ####
summ <- summarise(df, sd = sd(pheno), 
                  pheno = mean(pheno), count=n(), 
                  se = sd/sqrt(count), ci=1.96*se)

df %>%
  ggplot(aes(x=pheno)) + 
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 105-3, ymax = 105+3,
  #          fill = "#136389", alpha = .10) +
  # geom_errorbarh(data=summ, aes(y = 105, xmin = pheno-ci, 
  #                               xmax = pheno+ci), color = '#136389',
  #                height = 5)  +
  geom_point(data=summ, y = 105, size = 3, shape=21, 
             color = '#136389', fill = '#136389') +
  geom_histogram(color = 'black', fill = '#136389', bins = 30) +
  xlab('Consciousness Ratings') +
  ylab('Number of Participants') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  theme_classic() 

# Figura 1b ####

df %>%
  ggplot(aes(x=use_often, color=use_often, y = pheno)) + 
  geom_jitter(width = .2, height = 0, alpha = .35, shape = 16) +
  stat_summary() +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = 'Usage Frequency',
       y = 'Consciousness Ratings') +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

df %>%
  drop_na(use_often) %>%
  mutate(use_often = factor(use_often, 
                          levels = c("Never", "About once a month", 
                                     "About once every two weeks", 
                                     "About once a week", 
                                     "About once a day", 
                                     "More than once a day"))) %>%
  ggplot(aes(x=use_often, color=use_often, y = pheno)) + 
  geom_jitter(width = .2, height = 0, alpha = .35, shape = 16) +
  stat_summary() +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = 'Usage Frequency',
       y = 'Consciousness Ratings') +
  theme_classic() +
  theme(legend.position = "none") +
  coord_flip()

# La versión del paper
df %>%
  mutate(use_often_rec = ifelse(use_yn != "Yes", "Never", use_often),
         use_often_rec = recode(use_often_rec, "About once a month" = "1/mth",
                                "About once every two weeks" = "2/mth",
                                "About once a week" = "1/wk",
                                "About once a day" = "1/day",
                                "More than once a day" = ">1/day"),
         use_often_rec = factor(use_often_rec, levels = c(
           "Never", "1/mth", "2/mth", "1/wk", "1/day", ">1/day"))) -> df

df %>%
  group_by(use_often_rec) %>%
  summarise(mean = mean(pheno), sd = sd(pheno), 
            count = n(), se = sd / sqrt(count), 
            ci = se * qt(1-(0.05/2), count-1))  %>%
  ggplot(aes(x=use_often_rec, color=use_often_rec)) + 
  geom_jitter(data = df, aes(y = pheno), 
              width = .2, height = 0, alpha = .35, shape = 16) +
  geom_point(aes(y = mean), size = 3) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), width = .2, linewidth=1) +
  scale_colour_manual(values = scales::seq_gradient_pal(
    "#1A89BD", "#0C3F57")(seq(0,1,length.out=6))) +
  theme(legend.position = "none") +
  xlab('Usage Frequency') +
  ylab('Consciousness Ratings')  +
  theme(axis.title.y = element_text(margin = margin(0, 2, 0, 0)))