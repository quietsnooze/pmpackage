usethis::use_package('rlang')
usethis::use_package('patchwork')
usethis::use_package('rpart')


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

#' Calculate the information values for a variable in a dataframe
#'
#' @param df
#' @param dependent_var
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_modelling_iv <- function(dv,iv){
  iv_name <- deparse(substitute(iv))
  dv_name <- deparse(substitute(dv))

  tdf <- tibble(iv = iv,
                dv = dv)

  classvar <- tdf$iv[1]

  if(!(is.numeric(classvar))){
    #print('found factor')
    tdf$grouped_var <- tdf$iv
  } else if(length(unique(tdf$iv)) < 10){
    #print('found non-factor with few unique vals')
    tdf$grouped_var <- tdf$iv
  } else {
    #print('found non-factor')
    myq <- quantile(iv,probs=seq(from=0,to=1,length.out=min(10,unique(length(iv)))),na.rm=TRUE)
    tdf$grouped_var <- addNA(cut(tdf$iv,breaks = myq))
  }

  tdf <- tdf %>%
    dplyr::mutate(tot_event = sum(dv==1,na.rm=TRUE),
                  tot_nonevent = n() - tot_event) %>%
    dplyr::group_by(grouped_var) %>%
    dplyr::summarise(num = n(),
                     event = sum(dv,na.rm=TRUE),
                     nonevent = n() - event,
                     tot_event = max(tot_event),
                     tot_nonevent = max(tot_nonevent))%>%
    dplyr::ungroup() %>%
    dplyr::mutate(pct_event = event / tot_event,
                  pct_nonevent= nonevent / tot_nonevent,
                  woe = log(pct_event/pct_nonevent),
                  info_val = (pct_event - pct_nonevent) * woe)

  list(independent_varname = iv_name,
       dependent_varname = dv_name,
       infoval_df = tdf,
       info_val = sum(tdf$info_val))
}


#' Calculate the information values for an entire dataframe
#'
#' @param df
#' @param dependent_var
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_modelling_report_all_info_vals <- function(df,
                                              dependent_var){


  if(class(substitute(dependent_var)) == 'name'){
    dependent_var <- deparse(substitute(dependent_var))
  }
  dv <- dependent_var
  #print(paste(dv,class(dv)))

  iv_vector <- rep(NA,length(names(df)))
  for (n in 1:length(names(df))){
    vname = names(df)[n]
    #print(paste('iv = ',vname))
    #print(paste('dv = ',dv))
    iv_vector[n] <- pm_modelling_iv(iv = df[[vname]],
                                    dv =  df[[dv]])$info_val
  }
  dplyr::tibble(colname = names(df),
                info_val = iv_vector) %>%
    dplyr::filter(!is.infinite(info_val)) %>%
    dplyr::arrange(-info_val)
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
                                      dependent_var,
                                      independent_var,
                                      sample_size=Inf){

  if(class(substitute(independent_var))=='character'){
    iv = df[[independent_var]]
  } else{
    iv = eval(substitute(independent_var),df,parent.frame())
    #print(quote(iv))
    independent_var = deparse(substitute(independent_var))
  }

  if(class(substitute(dependent_var)) == 'character'){
    dv = as.numeric(df[[dependent_var]])
  } else{
    dve = eval(substitute(dependent_var),df,parent.frame())
    if (class(dve) == 'character'){
      dv = as.numeric(df[[dve]])
    } else{
      dv = as.numeric(dve)
    }
    dependent_var = deparse(substitute(dependent_var))
  }

  # allow user to specify a max number rows if plotting 100s of vars (speedy!)
  df = tibble(dv = dv,
              iv = iv) %>%
    dplyr::sample_n(size = min(nrow(df),sample_size))

  if(is.numeric(df %>% select(iv) %>% pull())){
    xlims = quantile(df %>% select(iv) %>% pull(),c(0.1,0.9),na.rm = TRUE)
  } else{
    xlims = c(NA,NA)
  }


  ggp1 <- ggplot2::ggplot(data=df,
                          ggplot2::aes(x=iv),
                          y=dv) +
    ggplot2::geom_point(alpha=0.1) +
    ggplot2::geom_smooth() +
    ggplot2::geom_smooth(colour='red',method='lm') +
    pmpackage::pm_ggplot_theme() +
    ggplot2::xlab(independent_var) +
    ggplot2::ylim(0,1)


  if(is.numeric(df %>% select(iv) %>% pull())){
    ggp2 <- ggplot2::ggplot(data=df,
                            ggplot2::aes(x=iv)) +
      ggplot2::geom_density() +
      pmpackage::pm_ggplot_theme() +
      ggplot2::xlab(independent_var)
  } else{
    ggp2 <- ggplot2::ggplot(data=df,
                            ggplot2::aes(x=iv)) +
      ggplot2::geom_histogram(stat='count') +
      pmpackage::pm_ggplot_theme() +
      ggplot2::xlab(independent_var)
  }


  if(is.numeric(df %>% select(iv) %>% pull())){
    ggp3 <- ggplot2::ggplot(data=df,
                            ggplot2::aes(x=iv,
                                         y=dv)) +
      ggplot2::geom_point(alpha=0.1) +
      ggplot2::geom_smooth() +
      ggplot2::geom_smooth(colour='red',method='lm') +
      pmpackage::pm_ggplot_theme() +
      ggplot2::xlab(independent_var) +
      ggplot2::ylim(0,1) +
      ggplot2::xlim(xlims)
  } else{
    ggp3 <- df %>%
      dplyr::group_by(iv) %>%
      dplyr::summarise(dv = mean(dv)) %>%
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x=iv,
                                   y=dv)) +
      ggplot2::geom_bar(stat='identity') +
      pmpackage::pm_ggplot_theme() +
      ggplot2::xlab(independent_var)  +
      ggplot2::ylab('prob')
  }

  mygini_list <- pm_modelling_calc_gini(df, iv, dv)

  ggp_gini<- ggplot2::ggplot(data = mygini_list$gini_df) +
    ggplot2::geom_line(ggplot2::aes(x=pm_modelling_gini_pct_losses, y= pm_modelling_gini_pct_wins), colour='tomato1') +
    ggplot2::geom_abline(slope=1,intercept = 0) +
    pmpackage::pm_ggplot_theme() +
    ggplot2::ylab('Percentage of winners found') +
    ggplot2::xlab('Percentage of losers found') +
    ggplot2::ggtitle(paste0('Gini = ',round(mygini_list$gini*100,1),'%')) +
    ggplot2::xlim(0,1) +
    ggplot2::ylim(0,1)


  myinfo <- pm_modelling_iv(dv = dv, iv= iv)
  name_info <- paste0(independent_var,'; IV = ',round(myinfo$info_val,2))

  #grid plot
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))

  gridtext <-
    grid::grid.text(name_info, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))

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

  if(class(substitute(predvar)) == 'character'){
    pv = as.numeric(df[[predvar]])
  } else{
    pve = eval(substitute(predvar),df,parent.frame())
    if (class(pve)=='character'){ #at this point we've been called with a variable name that contains a string that is a colname!
      pv = as.numeric(df[[pve]])
    } else{ #here we've been called by a colname directly
      pv = as.numeric(pve)
    }
  }


  if(class(substitute(truevar)) == 'character'){
    tv = as.numeric(df[[truevar]])
  } else{
    tve = eval(substitute(truevar),df,parent.frame())
    if (class(tve) == 'character'){
      tv = as.numeric(df[[tve]])
    } else{
      tv = as.numeric(tve)
    }
  }


  #fix when predvar is negatively correlated with true outcome
  gini_df <- tibble(predvar = pv,
                    truevar = tv) %>%
    filter(!is.na(predvar),
           !is.na(truevar),
           !is.infinite(predvar),
           !is.infinite(truevar))
  print(gini_df)
  if (cor(gini_df$predvar,gini_df$truevar) < 0){
    gini_df$predvar = - gini_df$predvar
  }

  gini_df <- gini_df %>%
    dplyr::group_by(predvar) %>%
    dplyr::summarise(truevar=sum(truevar,na.rm = TRUE),
                     inv_truevar = n() - truevar) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-predvar) %>%
    dplyr::mutate(pm_modelling_gini_cumsum_wins = cumsum(truevar),
                  pm_modelling_gini_cumsum_losses = cumsum(inv_truevar),
                  pm_modelling_gini_pct_wins = pm_modelling_gini_cumsum_wins / sum(truevar),
                  pm_modelling_gini_pct_losses = pm_modelling_gini_cumsum_losses / sum(inv_truevar),
                  pm_modelling_gini_auc = (dplyr::lag(pm_modelling_gini_pct_wins,default=0) + 0.5*(pm_modelling_gini_pct_wins - dplyr::lag(pm_modelling_gini_pct_wins,default=0))) * (pm_modelling_gini_pct_losses - dplyr::lag(pm_modelling_gini_pct_losses,default = 0))) %>%
    dplyr::bind_rows(tibble(predvar=NA,
                            truevar=NA,
                            pm_modelling_gini_cumsum_wins=0,
                            pm_modelling_gini_cumsum_losses=0,
                            pm_modelling_gini_pct_wins=0,
                            pm_modelling_gini_pct_losses=0,
                            pm_modelling_gini_auc=0))

  list('gini_df' = gini_df,
       'auc' = sum(gini_df$pm_modelling_gini_auc),
       'gini' = 1 - 2*(1-sum(gini_df$pm_modelling_gini_auc)))
}



#' pm_modelling_autoclass_var
#'
#' @param dependent_var
#' @param independent_var
#' @param mycp
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_modelling_autoclass_var <- function(dependent_var,
                          independent_var,
                          mycp = 0.005){

  tmp_tbl <- tibble(dv = dependent_var,
                    iv = independent_var) %>%
    mutate(rv = runif(n = length(dependent_var))) %>%
    arrange(rv) %>%
    select(-rv) %>%
    filter(!is.na(iv))

  my_decision_tree <- rpart::rpart(dv ~ iv, data=tmp_tbl, method='anova',cp=mycp)

  my_splits <- sort(c(-Inf,my_decision_tree$splits[,'index'],Inf))

  tmp_tbl$autoclassed <- cut(x = tmp_tbl$iv,
                             breaks = my_splits)

  autoclassed <- cut(x = independent_var,
                     breaks = my_splits)

  list(autoclassed_var = autoclassed,
       splits = my_splits,
       iv = my_decision_tree$variable.importance)
}




#' pm_modelling_full_autoclass_var
#'
#' @param df
#' @param dependent_var
#' @param independent_var
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_modelling_full_autoclass_var <- function(df,
                                            dependent_var,
                                            independent_var){

  if(class(substitute(independent_var)) == 'name'){
    iv = deparse(substitute(independent_var))
  } else {
    iv = independent_var
  }

  if(class(substitute(dependent_var)) == 'name'){
    dv = deparse(substitute(dependent_var))
  } else {
    dv = dependent_var
  }

  newname = paste0(iv,'_autoclass')

  df[[newname]] <- addNA(cut(df[[iv]],
                             breaks = pm_modelling_autoclass_var(df[[dv]],
                                                                df[[iv]])$splits))

  newnamewoe = paste0(newname,'_woe')
  tdf <- pm_modelling_iv(iv=df[[newname]],
                          dv=df[[dv]])$infoval_df %>%
    dplyr::select(grouped_var,
                  woe)
  names(tdf) = c(newname,
                 newnamewoe)
  df <- df %>%
    dplyr::left_join(tdf)

  df
}







