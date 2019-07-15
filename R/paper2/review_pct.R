# comp <- function(x){
#   x[x < 0] <- 0
#   x[x > 1] <- 1  
#   x
# }
# 
# gen_preds <- function(n, m, sd){
#   unmatch <- comp(rnorm(n/2, m[1], sd))
#   match <- comp(rnorm(n/2, m[2], sd))
#   tibble(
#     pred = c(rep("non-match", n/2), rep("match", n/2)),
#     prob = c(unmatch, match)
#   )
# }

# df <- 
#   gen_preds(26, c(0.2, 0.8), 0.30) 

df <- 
  tibble(
    pred = c(rep("non-match", 5), rep("match", 5)),
    prob = c(0.05, 0.25, 0.35, 0.65, 0.90, 0.2, 0.45, 0.80, 0.85, 0.95)
  )

plot_unc <- function(t1, t2){
  # browser()
  df %>% 
    ggplot(aes(prob, y = 0, color = pred)) +
    
    # number line
    geom_hline(yintercept = 0, color = "darkgrey") +
    
    #t1
    geom_rect(aes(xmin = t1, xmax=1, ymin = -0.025, ymax = 0.025), fill = "forestgreen", alpha = 0.1, color = NA) +
    #t2
    geom_rect(aes(xmin = 0, xmax=t2, ymin = -0.025, ymax = 0.025), fill = "forestgreen", alpha = 0.1, color = NA) +
    
    #uncertain
    geom_rect(aes(xmin = t2, xmax=t1, ymin = -0.025, ymax = 0.025), fill = "orange", alpha = 0.05, color = NA) +
    
    geom_vline(xintercept = t1, lty = "dashed") +
    geom_vline(xintercept = t2, lty = "dashed") +
    
    annotate("text", x = t2+0.02, y = -0.012, label = "T[2]", parse = T, size = 5) +
    annotate("text", x = t2+0.07, y = -0.012, label = glue(" = {round(t2*100,0)}%"), size = 5) +
    
    annotate("text", x = t1-0.09, y = 0.012, label = "T[1]", parse = T, size = 5) +
    annotate("text", x = t1-0.04, y = 0.012, label = glue(" = {round(t1*100,0)}%"), size = 5) +
    
    geom_point(alpha = 1, size = 10) +
    scale_color_manual(values = c("darkblue", "brown2"), name = NULL) +
    scale_y_continuous(limits = c(-0.025, 0.025)) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values=c("forestgreen", "gold"), labels = c("certain", "uncertain")) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size=15),
          legend.text = element_text(size=15),
          title = element_text(size=15),
          strip.text = element_text(size=12),
          panel.grid.major.y = element_line(colour = 'gray', size = 0.25)
    ) +
    theme(
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = "lightgrey"),
      
    )
  
}

plot_unc(0.925, 0.18) + labs(title = glue("PPV and NPV = 100%"), x = "Predicted Probabilites")
ggsave("R/paper2/plots/review_pct_str.png", 
       dpi = 500,
       width=10,
       height=5)


plot_unc(0.67, 0.43) + labs(title = glue("PPV and NPV = 75%"), x = "Predicted Probabilites")
ggsave("R/paper2/plots/review_pct_len.png", 
       dpi = 500,
       width=10,
       height=5)















(p <- df %>% 
    ggplot(aes(prob, y = 0, color = pred)) +
    
    # number line
    geom_hline(yintercept = 0, color = "darkgrey") +
    
    #t1
    geom_vline(xintercept = t1, lty = "dashed") +
   geom_rect(aes(xmin = t1, xmax=1, ymin = -0.025, ymax = 0.025), fill = "forestgreen", alpha = 0.1) +
    #t2
    geom_vline(xintercept = t2, lty = "dashed") +
    geom_rect(aes(xmin = 0, xmax=t2, ymin = -0.025, ymax = 0.025), fill = "forestgreen", alpha = 0.1) +
    
    
    geom_point(alpha = 0.7, size = 10) +
    scale_color_tableau() +
    scale_y_continuous(limits = c(-0.025, 0.025)) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    theme_custom() +
    theme(
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = "lightgrey")
    ))


(p <- df %>% 
    ggplot(aes(prob, y = 0, color = pred)) +
    
    # number line
    geom_hline(yintercept = 0, color = "darkgrey") +
    
    #t1
    geom_vline(xintercept = 0.67, lty = "dashed") +
    #t2
    geom_vline(xintercept = 0.42, lty = "dashed") +
    
    
    geom_point(alpha = 0.7, size = 10) +
    scale_color_tableau() +
    scale_y_continuous(limits = c(-0.025, 0.025)) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    theme_custom() +
    theme(
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(color = "lightgrey")
    ))


ggplotly(p)
  
