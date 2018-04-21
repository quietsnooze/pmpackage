devtools::use_package('rlang')
devtools::use_package('tidyverse')
devtools::use_package('patchwork')


#' Logit of probability pr
#'
#' Adjusts for probability == 100% and probability == 0% by applying a ceiling and a floor
#'
#' @param pr
#'
#' @return
#' @export
#'
#' @examples
logit <- function(pr, use_ceiling_and_floor = TRUE) {
  if (use_ceiling_and_floor){
    pr <- pmin(0.999999999,pmax(0.00000001,pr))
  }
  log(pr/(1-pr))
}


#' Inverse logit of log odds lgt
#'
#' @param lgt
#'
#' @return
#' @export
#'
#' @examples
inv_logit <- function(lgt){
  1/(1+exp(-lgt))
}




#' Ratios and differences of variable
#'
#' @param df
#' @param varname
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_modelling_compare_player_opponent <- function(df,
                                                 varname){
  # build key vars and computations
  p_varname = rlang::sym(paste0('player_',varname))
  o_varname = rlang::sym(paste0('opponent_',varname))

  #apply them
  finaldf <- df %>%
    dplyr::mutate(xxx_ratio = !!p_varname / !!o_varname,
                  xxx_difference = !!p_varname - !!o_varname,
                  xxx_ratio_ln = log(xxx_ratio))

  names(finaldf) = names(finaldf) %>% stringr::str_replace_all(pattern = 'xxx', replacement = varname)

  #return it
  finaldf
}



#' Turn a dataframe with player_ and opponent_ data into a tidy dataframe
#'
#' Both players and opponents become players in this new world!
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_modelling_tidify_player_opponent_dataframe = function(df,
                                                         additional_variables=NA){

  core_variables = c('actualResult','winNum','match_date',additional_variables)
  core_variables = core_variables[!is.na(core_variables)]
  all_variables = unique(c(core_variables,additional_variables))
  playerVars = names(df) %>% stringr::str_subset(paste(unlist(c('player',all_variables)),collapse='|'))
  opponentVars = names(df) %>% stringr::str_subset(paste(unlist(c('opponent',all_variables)),collapse='|'))
  diff_variables = all_variables %>% stringr::str_subset('difference')
  ratio_variables = all_variables %>% stringr::str_subset('ratio')

  players = df %>%
    dplyr::select(c('player_name',playerVars))

  opponents = df %>%
    dplyr::select(c('opponent_name',opponentVars))

  names(opponents) = names(opponents) %>% stringr::str_replace_all(pattern = 'opponent', replacement = 'player')
  opponents <- opponents %>%
    dplyr::mutate(actualResult = 1 - actualResult) %>%
    purrr::modify_at(diff_variables,function(x) {x = -x}) %>%
    purrr::modify_at(ratio_variables,function(x) {x = 1/x})

  both = players %>%
    dplyr::bind_rows(opponents)

  both
}



#' Workout lifetime win and loss rates
#'
#' @param df
#' @param group_variables vector of variables to calculate lifetime vars by (e.g. 'surface')
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_modelling_lifetime_performance <- function(df,group_variables=NA){

  group_variables = c('player_name',group_variables)
  group_variables = group_variables[!is.na(group_variables)]

  newvarname_prefix = paste(c('player',group_variables[!group_variables=='player_name']),collapse = '_')

  core_variables = c('opponent_name','match_date','actualResult',group_variables)

  #convert players and opponents to same scale
  df_tidy <- df %>%
    select_(.dots = core_variables) %>%
    pm_modelling_tidify_player_opponent_dataframe(additional_variables = group_variables[!group_variables == 'player_name'])


  df_tidy_adj <- df_tidy %>%
    dplyr::group_by_(.dots=group_variables) %>%
    dplyr::arrange(match_date) %>%
    dplyr::mutate(inverseActualResult = 1- actualResult,
                  xxx_lifetime_wins = cumsum(actualResult) - actualResult,
                  xxx_lifetime_losses = cumsum(inverseActualResult) - inverseActualResult,
                  xxx_lifetime_matches = xxx_lifetime_wins + xxx_lifetime_losses,
                  xxx_lifetime_win_pct = xxx_lifetime_wins / xxx_lifetime_matches,
                  xxx_lifetime_loss_pct = xxx_lifetime_losses / xxx_lifetime_matches) %>%
    dplyr::select(-inverseActualResult,
                  -actualResult)

  names(df_tidy_adj) = names(df_tidy_adj) %>% stringr::str_replace_all(pattern = 'xxx', replacement = newvarname_prefix)


  df_tidy_adj_opponents <- df_tidy_adj
  names(df_tidy_adj_opponents) = names(df_tidy_adj_opponents) %>% stringr::str_replace_all(pattern = 'player', replacement = 'opponent')


  finaldf <- df %>%
    left_join(df_tidy_adj) %>%
    left_join(df_tidy_adj_opponents)


  #return it
  finaldf


}


pm_modelling_lag_variable <- function(df,varname){
  df <- df %>%
    mutate(pm_modelling_match_date_as_day = as.Date(match_date) - 1, #-1 since it needs to be the day before the match
           pm_modelling_match_date_as_day_90d = as.Date(match_date) - 91,
           pm_modelling_match_date_as_day_180d = as.Date(match_date) - 181,
           pm_modelling_match_date_as_day_365d = as.Date(match_date) - 361)

  df_tidy <- df %>%
    pmpackage::pm_modelling_tidify_player_opponent_dataframe(additional_variables = c(varname,
                                                                                      pm_modelling_match_date_as_day)) %>%
    dplyr::distinct(player_name,
                    pm_modelling_match_date_as_day,
                    .keep_all = TRUE) %>%
    select(player_name,
           pm_modelling_match_date_as_day,
           varname)

  df <- df %>%
    left_join(df_tidy %>% rename(paste0(varname,'_90d'),varname),
              by=c('player_name' = 'player_name',
                   'pm_modelling_match_date_as_day' = 'pm_modelling_match_date_as_day_90d')) %>%
    left_join(df_tidy %>% rename(paste0(varname,'_180d'),varname),
              by=c('player_name' = 'player_name',
                   'pm_modelling_match_date_as_day' = 'pm_modelling_match_date_as_day_180d')) %>%
    left_join(df_tidy %>% rename(paste0(varname,'_365d'),varname),
              by=c('player_name' = 'player_name',
                  'pm_modelling_match_date_as_day' = 'pm_modelling_match_date_as_day_365d')) %>%
    select(-pm_modelling_match_date_as_day,
           -pm_modelling_match_date_as_day_90d,
           -pm_modelling_match_date_as_day_180d,
           -pm_modelling_match_date_as_day_365d)


}



#' Plot the relationship between a variable and results
#'
#' @param df
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_modelling_plot_variable = function(df,
                                      var_name){
  df = df %>%
    dplyr::sample_n(size = min(nrow(df),2000))

  xlims = quantile(df %>% select_(var_name) %>% pull(),c(0.1,0.9),na.rm = TRUE)

  ggp1 <- ggplot2::ggplot(data=df,
                          ggplot2::aes(x=get(var_name),
                              y=actualResult)) +
    ggplot2::geom_point(alpha=0.1) +
    ggplot2::geom_smooth() +
    ggplot2::geom_smooth(colour='red',method='lm') +
    pmpackage::pm_ggplot_theme() +
    ggplot2::xlab(var_name) +
    ggplot2::ylim(0,1)


  ggp2 <- ggplot2::ggplot(data=df,
                          ggplot2::aes(x=get(var_name))) +
    ggplot2::geom_density() +
    pmpackage::pm_ggplot_theme() +
    ggplot2::xlab(var_name)


  ggp3 <- ggplot2::ggplot(data=df,
                          ggplot2::aes(x=get(var_name),
                              y=actualResult)) +
    ggplot2::geom_point(alpha=0.1) +
    ggplot2::geom_smooth() +
    ggplot2::geom_smooth(colour='red',method='lm') +
    pmpackage::pm_ggplot_theme() +
    ggplot2::xlim(xlims[1],xlims[2]) +
    ggplot2::xlab(var_name) +
    ggplot2::ylim(0,1)


  mygini_list <- pm_modelling_calc_gini(df,
                                        var_name,
                                        'actualResult')

  ggp_gini<- ggplot2::ggplot(data = mygini_list$gini_df) +
    ggplot2::geom_line(ggplot2::aes(x=pm_modelling_gini_pct_losses, y= pm_modelling_gini_pct_wins), colour='tomato1') +
    ggplot2::geom_abline(slope=1,intercept = 0) +
              pmpackage::pm_ggplot_theme() +
    ggplot2::ylab('Percentage of winners found') +
    ggplot2::xlab('Percentage of losers found') +
    ggplot2::ggtitle(paste0('Gini = ',round(mygini_list$gini*100,1),'%'))


  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))

  grid::grid.text(var_name, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))

  print(ggp2, vp = grid::viewport(layout.pos.row = 1,
                                  layout.pos.col = 2))
  print(ggp3, vp = grid::viewport(layout.pos.row = 2,
                                  layout.pos.col = 2))
  print(ggp_gini, vp = grid::viewport(layout.pos.row = 2,
                                  layout.pos.col = 1))
}



#' pm_modelling_calc_gini
#'
#' @param df
#' @param predvar
#' @param truevar
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_modelling_calc_gini <- function(df,
                                   predvar,
                                   truevar){

  #fix when predvar is negatively correlated with true outcome
  gini_df <- df %>%
    filter(!is.na(get(predvar)),
           !is.na(get(truevar)),
           !is.infinite((get(predvar))),
           !is.infinite((get(truevar))))
  if (cor(gini_df[[predvar]],gini_df[[truevar]]) < 0){
    gini_df[[predvar]] = - gini_df[[predvar]]
  }

  gini_df <- gini_df %>%
    dplyr::mutate(random_var_for_sorting = rnorm(n=nrow(gini_df))) %>% #don't want to accidentally benefit from pre-sorting
    dplyr::arrange(random_var_for_sorting) %>%
    dplyr::arrange(-get(predvar)) %>%
    dplyr::mutate(pm_modelling_gini_cumsum_wins = cumsum(get(truevar) == 1),
                  pm_modelling_gini_cumsum_losses = cumsum(get(truevar) == 0),
                  pm_modelling_gini_pct_wins = pm_modelling_gini_cumsum_wins / sum(get(truevar) == 1),
                  pm_modelling_gini_pct_losses = pm_modelling_gini_cumsum_losses / sum(get(truevar) == 0),
                  pm_modelling_gini_auc = (pm_modelling_gini_pct_wins) * (pm_modelling_gini_pct_losses - dplyr::lag(pm_modelling_gini_pct_losses,default = 0)))

  list('gini_df' = gini_df,
       'auc' = sum(gini_df$pm_modelling_gini_auc),
       'gini' = 1 - 2*(1-sum(gini_df$pm_modelling_gini_auc)))
}

