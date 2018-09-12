
source("utils.R")




# Analysis of linear weights

pre_data <- Teams %>%
  filter(yearID > 1980) %>%
  mutate(R_G = R / G,
         X1B = H - X2B - X3B - HR,
         X1B_G = X1B / G,
         X2B_G = X2B / G,
         X3B_G = X3B / G,
         HR_G = HR / G,
         BB_G = BB / G,
         SB_G = SB / G,
         CS_G = CS / G)

m_matrix <- model.matrix(
  R_G ~ X1B_G + X2B_G + X3B_G + HR_G + BB_G + SB_G + CS_G,
  data = pre_data
)

data <- list(
  N = nrow(pre_data),
  T = length(unique(pre_data$teamID)),
  Y = length(unique(pre_data$yearID)),
  K = ncol(m_matrix),
  X = m_matrix,
  y = pre_data$R_G,
  year = max(pre_data$yearID) - pre_data$yearID + 1
)


model1 <- stan(file = "stan/linear-weights.stan",
               data = data,
               iter = 1000,
               chains = 2,
               control = list(max_treedepth = 20,
                              adapt_delta = 0.9))


# Dynamic model processing

model1_summary <- summary(model1)$summary %>%
  as.data.frame() %>%
  mutate(parameter = rownames(.))

beta <- model1_summary %>%
  filter(grepl("beta", parameter),
         !grepl("mean", parameter)) %>%
  cbind(parameter_name = rep(colnames(m_matrix),
                      times = length(unique(pre_data$yearID))),
        year = rep(unique(pre_data$yearID), each = length(colnames(m_matrix)))) %T>%
                      {print(
                        ggplot(data = ., aes(year, mean, color = parameter_name)) +
                          geom_point() +
                          geom_line() +
                          geom_errorbar(aes(ymin = `25%`, ymax = `75%`))
                      )}





