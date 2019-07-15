source("R/paper2/utils.R")

df_metric_list <- 
  read_rds("data/paper2/df_metric_list.rds")

df_metric_list <- 
  df_metric_list %>% 
  select(error_percent, train_n, model, metric_ls__e) %>% 
  mutate(metric_ls__e = map(metric_ls__e, bind_rows)) %>% 
  unnest()



get_metrics_for_params <- function(m = "review_pct_99", e = 4000){
  df_metric_list %>% 
    filter(metric == m,
           train_n == e) 
}



df_metric_list %>% 
  filter(metric %in% c("f1")) %>% 
  ggplot(aes(train_n, value, color = error_percent, group = error_percent)) +
  geom_smooth(se=F) + 
  facet_wrap(.~model, nrow = 3)

df_metric_list %>% 
  filter(metric %in% c("review_pct_99")) %>% 
  ggplot(aes(train_n, value, color = error_percent, group = error_percent)) +
  geom_smooth(se=F) + 
  facet_wrap(.~model, nrow = 3)


###########################################################

# - RF performs significantly better
# - 

###########################################################


df_model_collection <- 
  read_rds("data/paper2/df_model_collection.rds")

df_messed_collection <- 
  read_rds("data/paper2/df_messed_collection.rds")


df_model_metrics <-
  df_model_collection %>%
  mutate(metrics = pmap(list(e = error_percent,
                             n = train_n,
                             pred_probs = pred_probs), 
                        function(e, n, pred_probs){
                          
                          df_ts <- 
                            df_messed_collection %>% 
                            filter(error_percent == e,
                                   train_n == n) %>% 
                            pull(df_test) %>% 
                            .[[1]]
                          calculate_metrics(df_ts, pred_probs)
                        })) %>% 
  select(model, error_rate = error_percent, train_n, metrics) %>%
  unnest(metrics) %>% 
  arrange(error_rate) %>% 
  mutate()



df_model_metrics %>% 
  spread(model, value) %>% 
  filter(
    metric %in% c("review_pct_100", "review_pct_98", "accuracy", "f1"),
    train_n == 4000,
    error_rate %in% c(0, 0.6)
  ) %>% 
  mutate(ratio_nn = (rf-nn)/rf,
         ratio_svm = (rf-svm)/rf)


df_model_metrics %>% 
  filter(metric == "f1") %>% 
  ggplot(aes(error_rate, value, color = model, group = model)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 0.6, 0.1), label = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~train_n) +
  labs(x = "Error Rate", 
       y = "F1 score",
       title = "Error rate vs F1 score for different models at different amounts of training data") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("R/paper2/plots/error_f1_n.png", dpi = 500, width = 10, height = 6)


df_model_metrics %>% 
  filter(metric == "f1", train_n == 4000) %>% 
  ggplot(aes(error_rate, value, color = model, group = model)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 0.6, 0.1), 
                     label = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(.85, 1, .05),
                     limits = c(0.85, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  # facet_wrap(~train_n) +
  labs(x = "Error Rate", 
       y = "F1 score",
       title = "Error rate vs F1 score for different models at 4000 training samples") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("R/paper2/plots/error_f1.png", dpi = 500, width = 10, height = 7)


df_model_metrics %>% 
  filter(metric == "review_pct_100", train_n == 4000) %>% 
  ggplot(aes(error_rate, sqrt(value), color = model, group = model)) +
  geom_line() +
  # geom_smooth(method="lm", se = F) +
  scale_x_continuous(breaks = seq(0, 0.6, 0.1), 
                     label = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 1, .2),
                     limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  # facet_wrap(~train_n) +
  labs(x = "Error Rate", 
       y = "Review Percent",
       title = "Error rate vs Review Percent at 100% PPV & NPV for different models at 4000 training samples") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("R/paper2/plots/error_review100.png", dpi = 500, width = 10, height = 7)


df_model_metrics %>% 
  filter(metric == "review_pct_100") %>% 
  ggplot(aes(error_rate, sqrt(value), color = model, group = model)) +
  geom_line() +
  # geom_smooth(method="lm", se = F) +
  scale_x_continuous(breaks = seq(0, 0.6, 0.1), 
                     label = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 1, .2),
                     limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~train_n) +
  labs(x = "Error Rate", 
       y = "Review Percent",
       title = "Error rate vs Review Percent at 100% PPV & NPV for different models at 4000 training samples") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("R/paper2/plots/error_review100_n.png", dpi = 500, width = 10, height = 7)


df_model_metrics %>% 
  filter(metric == "review_pct_99", train_n == 4000) %>% 
  ggplot(aes(error_rate, sqrt(value), color = model, group = model)) +
  geom_line() +
  # geom_smooth(method="lm", se = F) +
  scale_x_continuous(breaks = seq(0, 0.6, 0.1), 
                     label = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 1, .2),
                     limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  # facet_wrap(~train_n) +
  labs(x = "Error Rate", 
       y = "Review Percent",
       title = "Error rate vs Review Percent at 99% PPV & NPV for different models at 4000 training samples") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("R/paper2/plots/error_review99.png", dpi = 500, width = 10, height = 7)


df_model_metrics %>% 
  filter(metric == "review_pct_99") %>% 
  ggplot(aes(error_rate, sqrt(value), color = model, group = model)) +
  geom_line() +
  # geom_smooth(method="lm", se = F) +
  scale_x_continuous(breaks = seq(0, 0.6, 0.1), 
                     label = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 1, .2),
                     limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~train_n) +
  labs(x = "Error Rate", 
       y = "Review Percent",
       title = "Error rate vs Review Percent at 99% PPV & NPV for different models at 4000 training samples") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("R/paper2/plots/error_review99_n.png", dpi = 500, width = 10, height = 7)






df_model_metrics %>% 
  filter(metric == "review_pct_98", train_n == 4000) %>% 
  ggplot(aes(error_rate, sqrt(value), color = model, group = model)) +
  geom_line() +
  # geom_smooth(method="lm", se = F) +
  scale_x_continuous(breaks = seq(0, 0.6, 0.1), 
                     label = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 1, .2),
                     limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  # facet_wrap(~train_n) +
  labs(x = "Error Rate", 
       y = "Review Percent",
       title = "Error rate vs Review Percent at 98% PPV & NPV for different models at 4000 training samples") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("R/paper2/plots/error_review98.png", dpi = 500, width = 10, height = 7)


df_model_metrics %>% 
  filter(metric == "review_pct_98") %>% 
  ggplot(aes(error_rate, sqrt(value), color = model, group = model)) +
  geom_line() +
  # geom_smooth(method="lm", se = F) +
  scale_x_continuous(breaks = seq(0, 0.6, 0.1), 
                     label = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 1, .2),
                     limits = c(0, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~train_n) +
  labs(x = "Error Rate", 
       y = "Review Percent",
       title = "Error rate vs Review Percent at 98% PPV & NPV for different models at 4000 training samples") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
  )

ggsave("R/paper2/plots/error_review98_n.png", dpi = 500, width = 10, height = 7)


# load_model_hdf5("data/paper2/models/")
  
