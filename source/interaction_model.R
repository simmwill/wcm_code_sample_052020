#' Estimates predefined logistic model with customizable outcome
#'
#' Function that allows iterative specification of CHD outcome in multivariable logistic model testing exposure interaction.
#'
#' @param outcome 1 of 6 congenital heart defects. Must be quoted: `"lvoto"`, `"rvoto_noebstein"`, `"conotruncal"`,
#'   `"septal"`, `"asd2"`, `"vsdpm"`.
#'
#' @return Output from multivariable logistic model (called with `glm(..., family = "binomial")`) estimating log-ods of
#'   `outcome` definedabove. RHS model terms include `pm25mean`, `heat_days_90`, their cross-product, and predetermined
#'   covariates (`mean_dp`, `age_cat`, `race_cat`, `educ_cat`).
#'
#' @examples
#' interaction_model("lvoto")
#' interaction_model("asd2")

interaction_model = function(outcome) {

  glm(
    data = study_data_mod,
    formula =
      reformulate(
        "pm25mean + heat_days_90 + pm25mean*heat_days_90 + mean_dp + age_cat + race_cat + educ_cat",  # RHS variables
        paste(outcome)  # LHS variable ("lvoto", "rvoto", "conotruncal", "septal", "asd2", "vsdpm")
      ),
    family = "binomial"
  )

}
