library(dplyr)
library(ggeffects)

predict_my_responses <- function(model) {
  dat <- predict_response(model, terms = c("OpticFlow", "Vista", "LevelSize"),
                          margin = "empirical") %>%
    as.data.frame() %>%
    select(x, group, facet, predicted, everything())
  return(dat)
}

report_my_predictions <- function(predictions) {
  out <- predictions %>%
    mutate(
      OpticFlow = x,
      Vista = group,
      LevelSize = facet,
      `Predicted Value` = round(predicted, 2),
      `95% CI` = paste0("[", round(conf.low, 2), ", ", round(conf.high, 2), "]")
    ) %>%
    select(OpticFlow, Vista, LevelSize, `Predicted Value`, `95% CI`)
  return(out)
}

check_lmer_model <- function(model) {
  print(check_overdispersion(model))
  print(check_convergence(model))
  print(check_heteroscedasticity(model))
  print(check_homogeneity(model))
  print(check_normality(model))
}

report_group_predictions <- function(lmer_model) {
  size_pred <- report_predictor_prediction(lmer_model, "LevelSize") %>%
    mutate(Predictor = "Environment Size")
  of_pred <- report_predictor_prediction(lmer_model, "OpticFlow") %>%
    mutate(Predictor = "Optic Flow")
  vista_pred <- report_predictor_prediction(lmer_model, "Vista") %>%
    mutate(Predictor = "Vista")

  out <- bind_rows(vista_pred, size_pred, of_pred) %>%
    select(Predictor, x, `Predicted Value`, `95% CI`)

  return(out)
}

report_predictor_prediction <- function(lmer_model, term) {
  pred <- predict_response(lmer_model, terms = c(term), margin = "empirical") %>%
    as.data.frame() %>%
    mutate(`Predicted Value` = round(predicted, 2),
           `95% CI` = paste0("[", round(conf.low, 2), ", ", round(conf.high, 2), "]")) %>%
    select(x, `Predicted Value`, `95% CI`)
  return(pred)
}