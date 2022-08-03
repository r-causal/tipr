library(ggdag)
set.seed(1234)
# set up DAG coordinates
coord_dag <- list(
  x = c(u = 0, c = 0,  x = 1, y = 2),
  y = c(u = -1, c = 1, x = 0, y = 0)
)
# nicer labels for the nodes
labels <- c(
  x = "exposure",
  y = "outcome",
  u = "unmeasured confounder",
  c = "measured confounder"
)
# visualize the dag
dagify(
  y ~  u + c,
  x ~ u + c,
  coords = coord_dag,
  labels = labels
) %>%
  ggdag(use_labels = "label", text = FALSE) +
  theme_void()

ggsave("fig-1.png", width = 4, height = 3)
