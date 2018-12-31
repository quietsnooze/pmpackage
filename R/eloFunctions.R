usethis::use_package('magrittr')
usethis::use_package('hashmap')
usethis::use_package('dplyr')
usethis::use_package('data.table')
usethis::use_package('doParallel')
usethis::use_package('foreach')
usethis::use_package('stringr')
usethis::use_package('Hmisc')
usethis::use_package('lubridate')
usethis::use_package('tibble')




#' pm_eloPrepDatabase
#'
#' Prepare hashmaps for playerScore and playerMatches for unseen data (i.e. new players).
#' If the database does not exist (eloDB=NA) a new database will be generated.
#'
#' @param eloDB a hashmap of player ELO scores, or NA to create a new one
#' @param matchDB a hashmap of matches a player has played, or NA
#' @param unseenDataPlayers list of players in the (as yet) unseen data
#' @param defaultScore the ELO for players without experience. Default of 1500
#' @param defaultMatches the number of matches for new players. Default of 0
#'
#' @return a list with two items: a hashmap for ELO scores, a hashmap for number of matches played
#'
#' @examples
#'
#'
#' @export
#' @importFrom magrittr "%>%"
pm_eloPrepDatabase = function(eloDB=NA,
                              matchDB=NA,
                              unseenDataPlayers,
                              defaultScore = 1500,
                              defaultMatches = 0){

  # if no database than create one
  if (is.na(eloDB)){
    eloDB = hashmap::hashmap(unseenDataPlayers,
                                   rep(defaultScore,length(unseenDataPlayers)))
    matchDB = hashmap::hashmap(unseenDataPlayers,
                                     rep(defaultMatches,length(unseenDataPlayers)))
  } else {
    newNames = unseenDataPlayers[!eloDB$has_keys(unseenDataPlayers)]

    newScores = rep(defaultScore,length(newNames))
    eloDB$insert(newNames,newScores)

    newMatches = rep(defaultMatches,length(newNames))
    matchDB$insert(newNames,newMatches)
  }

  #return updated database
  list(eloDB = eloDB,
       matchDB = matchDB)
}

#' pm_eloExtractDatabaseFromDataframe
#'
#' Prepare hashmaps for playerScore and playerMatches for seen data (i.e. from a dataframe that has already had pm_eloRunTimeSlice on it).
#'
#' @param eloDataframe A dataframe resulting from a call to pm_eloRunTimeSlice or pm_eloRunMultipleTimeSclies
#'
#' @return a list with two items: a hashmap for ELO scores, a hashmap for number of matches played
#'
#' @examples
#'
#'
#' @export
#' @importFrom magrittr "%>%"
pm_eloExtractDatabaseFromDataframe = function(eloDataframe){

  players = eloDataframe %>%
    dplyr::select(player_name, elo_player_post_elo, match_date, elo_player_post_matches)

  opponents = eloDataframe %>%
    dplyr::select(opponent_name, elo_opponent_post_elo, match_date, elo_opponent_post_matches) %>%
    dplyr::rename(player_name = opponent_name,
           elo_player_post_elo = elo_opponent_post_elo,
           elo_player_post_matches = elo_opponent_post_matches)

  both = players %>%
    dplyr::bind_rows(opponents) %>%
    dplyr::group_by(player_name) %>%
    dplyr::filter(match_date == max(match_date)) %>%
    dplyr::ungroup()

  message('pm_eloExtractDatabaseFromDataframe: both sides computed, now creating hashmap')

  eloDB = hashmap::hashmap(both$player_name,
                           both$elo_player_post_elo)

  matchDB = hashmap::hashmap(both$player_name,
                             both$elo_player_post_matches)
  #return updated database
  list(eloDB = eloDB,
       matchDB = matchDB)
}


#' pm_eloTidifyDataframe
#'
#' Convert an ELO dataframe to 'tidy' format (i.e. flatten player and opponent)
#'
#' @param eloDataframe A dataframe resulting from a call to pm_eloRunTimeSlice or pm_eloRunMultipleTimeSclies
#'
#' @return A dataframe in 'tidy' format (player_name, elo_ variables, match_date, actualResult)
#'
#' @examples
#'
#' @export
#' @importFrom magrittr "%>%"
pm_eloTidifyDataframe = function(eloDataframe){

  eloVars = names(eloDataframe) %>% stringr::str_subset('elo|match_date|actualResult')

  playerEloVars = eloVars[stringr::str_which(eloVars,'player_name|match_date|actualResult|elo_')]
  opponentEloVars = eloVars[stringr::str_which(eloVars,'opponent_name|match_date|actualResult|elo_')]

  players = eloDataframe %>%
    dplyr::select(c('player_name',playerEloVars))

  opponents = eloDataframe %>%
    dplyr::select(c('opponent_name',opponentEloVars)) %>%
    dplyr::mutate(actualResult = 1 - actualResult)

  names(opponents) = names(opponents) %>% stringr::str_replace_all(pattern = 'opponent', replacement = 'pm_elo_temp_name')
  names(opponents) = names(opponents) %>% stringr::str_replace_all(pattern = 'player', replacement = 'opponent')
  names(opponents) = names(opponents) %>% stringr::str_replace_all(pattern = 'pm_elo_temp_name', replacement = 'player')

  both = players %>%
    dplyr::bind_rows(opponents)

  both
}







#' pm_eloRunTimeSlice
#'
#' Compute and update ELO scores for a single time slice. Within the time slice all matches are assumed to be happening in parallel.
#'
#' @param eloDB a hashmap of player ELO scores
#' @param matchDB a hashmap of matches a player has played
#' @param simDF a dataframe of matches to process. Requires at least 'player_name', 'opponent_name' and 'actualResult' columns
#' @param simulateResults Whether to overwrite actualResult with simulated outcomes
#'
#' @return a list with three items: the updated eloDB, the updated matchDB and the input simDF updated with ELO columns. New columns are prefixed with 'elo_'
#'
#' @examples
#' tmpres = pm_eloPrepDatabase(unseenDataPlayers=c('bob','charlie'))
#' eloDB = tmpres$eloDB
#' matchDB = tmpres$matchDB
#' mysim = tibble::tribble(~player_name,~opponent_name,~match_date,~actualResult,
#' 'bob','charlie',as.Date('2007-01-01'),1)
#' tmpres = pm_eloRunTimeSlice(eloDB,matchDB,mysim)
#'
#' @export
pm_eloRunTimeSlice <- function(eloDB,
                               matchDB,
                               simDF,
                               simulateResults=FALSE,
                               tennisElo = TRUE,
                               Cfactor = 250,
                               Coffset = 5,
                               Cshape = 0.4){
  #message('pm_eloRunTimeSlice called with simulationResults = ',simulateResults)


  if (simulateResults){
    message('simulating results')
    simDF$actualResult = 0.5 #default prediction

    simDF = simDF %>%
      dplyr::mutate(
        elo_player_prior_elo = eloDB$find(player_name),
        elo_opponent_prior_elo = eloDB$find(opponent_name),
        elo_player_prior_matches = matchDB$find(player_name),
        elo_opponent_prior_matches = matchDB$find(opponent_name),
        elo_player_pred_score = jitter(1/(1+10^((elo_opponent_prior_elo - elo_player_prior_elo)/400)),0.00001),
        elo_Kfactor_player = Cfactor / ((elo_player_prior_matches + Coffset)^Cshape),
        elo_Kfactor_opponent = Cfactor / ((elo_opponent_prior_matches + Coffset)^Cshape),
        elo_random_number = runif(nrow(simDF),0,1),
        actualResult = dplyr::if_else(elo_random_number < elo_player_pred_score,1,0),
        elo_pchange = (actualResult - elo_player_pred_score),
        elo_player_post_elo = elo_player_prior_elo + elo_Kfactor_player * elo_pchange,
        elo_opponent_post_elo = elo_opponent_prior_elo + elo_Kfactor_opponent * (-elo_pchange),
        elo_player_post_matches = elo_player_prior_matches +1,
        elo_opponent_post_matches = elo_opponent_prior_matches + 1,
        elo_predictedResult = dplyr::if_else(elo_player_pred_score >= 0.5,1,0),
        elo_accuratePred = dplyr::if_else(actualResult == elo_predictedResult,1,0)
      )
  } else {
    if(any(is.na(simDF$actualResult))){
      stop('Missing actual results in elo dataframe')
    }
    simDF = simDF %>%
      dplyr::mutate(
        elo_simulate_Results = simulateResults,
        elo_player_prior_elo = eloDB$find(player_name),
        elo_opponent_prior_elo = eloDB$find(opponent_name),
        elo_player_prior_matches = matchDB$find(player_name),
        elo_opponent_prior_matches = matchDB$find(opponent_name),
        elo_player_pred_score = jitter(1/(1+10^((elo_opponent_prior_elo - elo_player_prior_elo)/400)),0.0001),
        elo_Kfactor_player = Cfactor / ((elo_player_prior_matches + Coffset)^Cshape),
        elo_Kfactor_opponent = Cfactor / ((elo_opponent_prior_matches + Coffset)^Cshape),
        elo_pchange = (actualResult - elo_player_pred_score),
        elo_player_post_elo = elo_player_prior_elo + elo_Kfactor_player * elo_pchange,
        elo_opponent_post_elo = elo_opponent_prior_elo + elo_Kfactor_opponent * (-elo_pchange),
        elo_player_post_matches = elo_player_prior_matches +1,
        elo_opponent_post_matches = elo_opponent_prior_matches + 1,
        elo_predictedResult = dplyr::if_else(elo_player_pred_score >= 0.5,1,0),
        elo_accuratePred = dplyr::if_else(actualResult == elo_predictedResult,1,0)
      )

  }


  #update databases
  eloDB$insert(simDF$player_name,simDF$elo_player_post_elo)
  eloDB$insert(simDF$opponent_name,simDF$elo_opponent_post_elo)
  matchDB$insert(simDF$player_name,simDF$elo_player_post_matches)
  matchDB$insert(simDF$opponent_name,simDF$elo_opponent_post_matches)

  #return list
  list(eloDB = eloDB,
       matchDB = matchDB,
       simDF = simDF)
}

#' pm_eloRunMulitpleTimeSlices
#'
#' Compute and update ELO scores for multiple time slices. Within each individual time slice all matches are assumed to be happening in parallel.
#'
#' @param eloDB a hashmap of player ELO scores
#' @param matchDB a hashmap of matches a player has played
#' @param eloDF a dataframe of matches to process. Requires at least 'player_name', 'opponent_name', 'match_date' columns
#'
#' @return a list with three items: the updated eloDB, the updated matchDB and the input eloDF updated with ELO columns. New columns are prefixed with 'elo_'
#'
#' @examples
#' tmpres = pm_eloPrepDatabase(unseenDataPlayers=c('bob','charlie'))
#' eloDB = tmpres$eloDB
#' matchDB = tmpres$matchDB
#' mysim = tibble::tribble(~player_name,~opponent_name,~match_date,~actualResult,
#' 'bob','charlie',as.Date('2007-01-01'),1,
#' 'bob','david',as.Date('2007-01-02'),0)
#' tmpres = pm_eloRunMultipleTimeSlices(eloDB,matchDB,mysim)
#'
#' @export
pm_eloRunMultipleTimeSlices <- function(eloDB,
                                        matchDB,
                                        eloDF,
                                        tennisElo = TRUE,
                                        Cfactor = 250,
                                        Coffset = 5,
                                        Cshape = 0.4) {

  alldates = sort(unique(eloDF$match_date))


  # create progress bar
  pb <- txtProgressBar(min = 1, max = length(alldates), style = 3)

  resList = list()
  for (i in 1:length(alldates)){
    #message('Running timeslice i=',i,'.')

    idate = alldates[i]

    simRes = pm_eloRunTimeSlice(eloDB = eloDB,
                                matchDB = matchDB,
                                simDF =  eloDF[eloDF$match_date == idate,],
                                tennisElo = tennisElo,
                                Cfactor = Cfactor,
                                Coffset = Coffset,
                                Cshape = Cshape
                                )
    eloDB = simRes$eloDB
    matchDB = simRes$matchDB

    resList[[i]] <- simRes$simDF

    setTxtProgressBar(pb, i)
  }
  message('All timeslices run')
  simAllRes = do.call(rbind,resList)

  close(pb)

  #return dataset and databases
  list(eloDB=eloDB,
       matchDB=matchDB,
       simAllRes = simAllRes)
}




#' pm_eloReportElosAtDateFromDatabase
#'
#' Prepare hashmaps for playerScore and playerMatches for unseen data (i.e. new players).
#' If the database does not exist (eloDB=NA) a new database will be generated.
#'
#' @param eloDF A dataframe containing elo_x data
#' @param snapshotDate A date for assessing the ELO scores
#' @param numMonthsToBeIneligible The number of months after which scores are ignored
#'
#' @return A dataframe with player_name and latest ELO rating at snapshotDate
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom lubridate "%m-%"
#' @export
pm_eloReportElosAtDateFromDatabase <- function(eloDF,
                                               snapshotDate,
                                               numMonthsToBeIneligible=3){
  message('Received snapshotDate = ',snapshotDate)
  oldestDate = snapshotDate %m-% months(numMonthsToBeIneligible)
  message('oldestDate = ',oldestDate)

  res = eloDF %>%
#    dplyr::filter(match_date < snapshotDate & match_date >= (snapshotDate - 30*numMonthsToBeIneligible))  #slick hack
    dplyr::filter(match_date < snapshotDate & match_date >= oldestDate)

  if (nrow(res) == 0){
    #stop('No data available for requested snapshotDate')
    message('Found no data for snapshotDate = ',snapshotDate,'; and oldestDate = ',oldestDate,'.')
    NA
  } else {



    playerside = res %>%
      dplyr::group_by(player_name) %>%
      dplyr::filter(match_date == max(match_date)) %>%
      dplyr::summarise(elo_player_post_elo = max(elo_player_post_elo),
                       match_date = max(match_date)) %>%
      dplyr::rename(player_match_date = match_date) %>%
      dplyr::select(player_name, elo_player_post_elo, player_match_date) %>%
      dplyr::ungroup()

    opponentside = res %>%
      dplyr::select(-player_name) %>%
      dplyr::group_by(opponent_name) %>%
      dplyr::filter(match_date == max(match_date)) %>%
      dplyr::summarise(elo_opponent_post_elo = max(elo_opponent_post_elo),
                       match_date = max(match_date)) %>%
      dplyr::rename(player_name = opponent_name,
                    opponent_match_date = match_date) %>%
      dplyr::select(player_name, elo_opponent_post_elo, opponent_match_date) %>%
      dplyr::ungroup()

    both = playerside %>%
      dplyr::full_join(opponentside,by='player_name') %>%
      dplyr::mutate(eloRating = dplyr::if_else(is.na(opponent_match_date),elo_player_post_elo,
                                               dplyr::if_else(is.na(player_match_date),elo_opponent_post_elo,
                                                              dplyr::if_else(player_match_date > opponent_match_date,elo_player_post_elo,elo_opponent_post_elo)))) %>%
      dplyr::select(player_name, eloRating)
    both
  }
}





#' pm_eloShowWinProbForScoreDiff
#'
#' Return the probability of a win
#'
#' @param scoreDiff The difference in score
#'
#' @return The difference in odds due to the score difference
#'
#'
#' @export
pm_eloShowWinProbForScoreDiff <- function(scoreDiff){
  1/(1+10^(-scoreDiff/400))
}




#' pm_eloOptimiseKfactor
#'
#' Return a set of optimal Kfactors
#'
#' @param eloSamp A dataset to optimise over.
#'
#' @return A list of 3 parameters: Cfactor, Coffset, Cshape
#'
#'
#' @importFrom magrittr "%>%"
#' @export
pm_eloOptimiseKfactor <- function(eloSamp){


    #fn that optimises
    evaluate_elo_params <- function(paramV,
                                  eloDF,
                                  tennisELO){
      message('evoluate elo params called with params[1] = ',paramV[1])
      message('evoluate elo params called with params[2] = ',paramV[2])
      message('evoluate elo params called with params[3] = ',paramV[3])

      #prep database
      tmpRes = pm_eloPrepDatabase(unseenDataPlayers = unique(c(eloSamp$player_name,eloSamp$opponent_name)))
      eloDB = tmpRes$eloDB
      matchDB = tmpRes$matchDB

      #run ELO
      tmpRes2 = pm_eloRunMultipleTimeSlices(eloDB = eloDB,
                                            matchDB = matchDB,
                                            eloDF = eloDF,
                                            tennisElo = tennisELO,
                                            Cfactor = paramV[1],
                                            Coffset = paramV[2],
                                            Cshape = paramV[3])


      gini = Hmisc::rcorr.cens(tmpRes2$simAllRes$elo_player_pred_score,tmpRes2$simAllRes$actualResult)[[2]]
      message('accuracy pct = ',mean(tmpRes2$simAllRes$elo_accuratePred))
      message('gini = ',gini)
      message('##############')


      #1 - mean(tmpRes2$simAllRes$elo_accuratePred)
      gini
    }


    #run optimisation
    # res = optim(#par=c(2.50,0.05,0.004),
    #             par=c(1,0.001,0.001),
    #             lower=c(0.000001,0.000001,0.0000001),
    #             upper=c(10,0.005,0.1),
    #             method="L-BFGS-B",
    #             fn=evaluate_elo_params,
    #             eloDF =eloSamp,
    #             tennisELO=TRUE)
    #
    # list(Cfactor=res$par[1],
    #      Coffset=res$par[2],
    #      Cshape=res$par[3])

    Cfactors = c(150,250,350)
    Coffsets = c(0.00001,1,5,10,20)
    Cshapes = c(0.1,0.4,0.8,1.2)

    parameters = expand.grid(Cfactors, Coffsets, Cshapes)
    gini = rep(NA,nrow(parameters))
    for (i in 1:nrow(parameters)){
      gini[i] = evaluate_elo_params(unlist(parameters[i,]),
                                    eloDF = eloSamp,
                                    tennisELO = TRUE)
    }

    parameters$gini = gini

    print(parameters %>% dplyr::arrange(desc(gini)))

    parameters %>% dplyr::arrange(desc(gini))
}





#' pm_eloRunTourneyELO
#'
#' Run a simulated Tournament based on input ELO databases and a dataframe that describes the draw
#'
#' @param eloDB a hashmap of player ELO scores, or NA to create a new one
#' @param matchDB a hashmap of matches a player has played, or NA
#' @param tournamentSetup A dataframe strucutred to describe the tournament
#' @param keyCols Key columns for reporting results (DO NOT CHANGE FOR NOW)
#' @param simCols Key columsn for debugging results (DO NOT CHANGE FOR NOW)
#'
#' @return A dataframe with winners (simWinner, winnerName) for each match in the Tournament
#'
#' @examples
#'
#' @export
#' @import data.table
#' @importFrom magrittr "%>%"
pm_eloRunTourneyELO = function(   tournamentSetup,
                            keyCols=c('roundNum','player_name','opponent_name','match_date','Tournament'),
                            simCols=c('predictions','simWinner','winnerName'),
                            roundeloDB,
                            roundmatchDB){
  x= data.table::copy(tournamentSetup)
  message('runTourney called with nrow(x) = ',nrow(x))

  #roundNum
  thisRoundNum = max(x$roundNum)
  message('roundNum = ',thisRoundNum)


  # first a simple prediction
  tmp = pmpackage::pm_eloRunTimeSlice(eloDB = roundeloDB,
                           matchDB = roundmatchDB,
                           simDF = x,
                           simulateResults = TRUE)
  x = tmp$simDF
  roundeloDB = tmp$eloDB
  roundmatchDB = tmp$matchDB

  x$predictions = x$actualResult

  #handle retirements
  retirement_rnums = runif(n=length(x$predictions),min=0,max=1)
  x$predictions = ifelse(retirement_rnums < 0.024007606 & retirement_rnums > 0.024007606/2,1,x$predictions)
  x$predictions = ifelse(                                 retirement_rnums < 0.024007606/2,0,x$predictions)


  #build round two
  x$simWinner = x$predictions
  x$winnerName = ifelse(x$simWinner,x$player_name,x$opponent_name)
  if(anyNA(x$simWinner)){
    print(x)
    stop('Unable to determine winner, stopping')
  }

  x = data.table::as.data.table(x)#bugfix
  # predictions done

  #num players making it forward to next round
  numPlayersProgressing = length(x$player_name)

  message('number of winners = ',numPlayersProgressing)
  print(x[,c(simCols,keyCols)])
  message('number of winners and table printed')

  #check if done
  if (numPlayersProgressing==1){
    message('simulation complete')
    x$nextMatch = rep(NA,numPlayersProgressing)
    x[,c(keyCols,simCols),with=FALSE]
  } else{
    message('computing next round')
    #build new dataset for next round
    numMatchesNextRound = length(x$player_name)/2
    dfNextRound = as.data.table(rep('Dean',numMatchesNextRound))
    names(dfNextRound) = 'dummy_column'

    #setup key vars
    dfNextRound$match_date = min(tournamentSetup$match_date,na.rm=TRUE)+2 #move matches on in the calendar
    dfNextRound$roundNum = 2^(log(thisRoundNum,2)-1)
    dfNextRound$Surface = tournamentSetup$Surface[1]
    dfNextRound$Tournament = tournamentSetup$Tournament[1]


    #assign draw
    x$nextMatch = rep(c(1,2),nrow(x)/2)
    dfNextRound$player_name = x[nextMatch==1,.(winnerName),]
    dfNextRound$opponent_name = x[nextMatch==2,.(winnerName),]
    dfNextRoundFinal = dfNextRound

    #configure this round's dataset for appending to the database
    x$win = ifelse(x$simWinner==1,TRUE,FALSE)
    x$winNum = x$simWinner


    #loop
    message('calling next round')
    rbind(x[,c(keyCols,simCols),with=FALSE],
          pm_eloRunTourneyELO(tournamentSetup=dfNextRoundFinal,
                        keyCols = keyCols,
                        simCols=simCols,
                        roundeloDB = roundeloDB,
                        roundmatchDB = roundmatchDB
          ))
  }
}



#' pm_eloSimulateTournamentInParallel
#'
#' Sets up a parallel simulation of a tournament
#'
#' @param eloDB a hashmap of player ELO scores, or NA to create a new one
#' @param matchDB a hashmap of matches a player has played, or NA
#' @param ds A dataframe structured to describe the tournament
#' @param keyCols Key columns for reporting results (DO NOT CHANGE FOR NOW)
#' @param simCols Key columsn for debugging results (DO NOT CHANGE FOR NOW)
#' @param runs Number of iterations (default = 10)
#' @param maxTimeSecs Maximum running time before it quites (defulat = a Very Big Number)
#' @param tournamentName The name of hte tournament _simulation_ (e.g. 'wimbledon 07')
#' @param resetDatabase Overwrite any existing predictions for tournamentName
#' @param forceSingleThreaded Force only a single thread to be used
#'
#' @return A dataframe with winners (simWinner, winnerName) for each match in the Tournament
#'
#' @examples
#'
#' @export
#' @import data.table
#' @import doParallel
#' @import foreach
#' @importFrom magrittr "%>%"
pm_eloSimulateTournamentInParallel = function(ds,
                                              runs=10,
                                              eloDBForSim,
                                              matchDBForSim,
                                              keyCols=c('roundNum','player_name','opponent_name','match_date','Tournament'),
                                              simCols=c('predictions','simWinner','winnerName'),
                                              maxTimeSecs=999999999999,
                                              tournamentName = 'default',
                                              resetDatabase = FALSE,
                                              forceSingleThreaded = FALSE){
  run_timestamp = Sys.time()

  # Option to wipe database and start again
  if (resetDatabase){
    message('resetting database')
    result = pm_eloRunTourneyELO(tournamentSetup=ds, roundeloDB = hashmap::clone(eloDBForSim), roundmatchDB=hashmap::clone(matchDBForSim))
    dfListOfSims = as.data.frame(result[length(result$player_name),winnerName,])
    names(dfListOfSims) = c('winnerName')
    dfListOfSims$run_timestamp = run_timestamp
    message('resetting database: first entry ready')
    print(dfListOfSims)
    save(dfListOfSims,file=paste0('sim_',tournamentName,'.rda'))
    message('resetting database: saved!')
  }


  #maxDuration
  startTime = Sys.time()

  # create progress bar
  pb <- txtProgressBar(min = 1, max = runs, style = 3)

  # parallel prep
  doParallel::registerDoParallel()




  for (i in 2:runs){
    message('**** simulation ',i,' of ',runs,' ***')
    #result = runTourney(tournamentSetup=ds,keyCols = keyCols,tennisModel = tennisModel,simCols = simCols)

    if (forceSingleThreaded){
      result=pm_eloRunTourneyELO(tournamentSetup=ds,
                           roundeloDB = hashmap::clone(eloDBForSim),
                           roundmatchDB=hashmap::clone(matchDBForSim))
      resultp = as.data.frame(result[length(result$player_name),winnerName,])
    } else {
      resultp = foreach::foreach(i=1:foreach::getDoParWorkers(), .combine=rbind) %dopar% {
        result = pm_eloRunTourneyELO(tournamentSetup=ds,
                               roundeloDB = hashmap::clone(eloDBForSim),
                               roundmatchDB=hashmap::clone(matchDBForSim)
        )
        result = as.data.frame(result[length(result$player_name),winnerName,])
        result
      }
    }

    message('parallel simulation complete')
    print(resultp)

    message('updating simulation log')
    names(resultp) = c('winnerName')
    resultp$run_timestamp = run_timestamp
    load(paste0('sim_',tournamentName,'.rda'))
    dfListOfSims = rbind(dfListOfSims,resultp)
    save(dfListOfSims,file=paste0('sim_',tournamentName,'.rda'))
    message('simulation log updated')

    setTxtProgressBar(pb, i)


    #time cut-off
    if (Sys.time() > startTime + maxTimeSecs){
      message('max run time reached, ending')
      break
    }
  }


  close(pb)


  # results print out
  names(dfListOfSims)[1]='winnerName'
  dfResultsOfSims = as.data.frame(tapply(dfListOfSims$winnerName,INDEX=dfListOfSims$winnerName,FUN = 'length'))
  names(dfResultsOfSims)[1]='frequency'
  dfResultsOfSims$player_name = rownames(dfResultsOfSims)
  totNum = sum(dfResultsOfSims$frequency)
  dfResultsOfSims$winPct = dfResultsOfSims$frequency / totNum
  dfResultsOfSims$odds = 1 + (1 - dfResultsOfSims$winPct) / dfResultsOfSims$winPct
  dfResultsOfSims$odds_excl_stake_eg_BetfairMarket = dfResultsOfSims$odds - 1
  print(dfResultsOfSims[order(dfResultsOfSims$frequency),])
  #prop.table(dfResultsOfSims[order(dfResultsOfSims$frequency),])

  #return the dataframe for detailed analysis
  dfResultsOfSims[order(dfResultsOfSims$frequency),]
}




#' pm_eloReportTournamentStats
#' Summarise a set of tournament results stored as one row per winner
#'
#' @param df
#'
#' @return a tibble
#' @export
#' @importFrom magrittr "%>%"
#' @import tibble
#' @import dplyr
#'
#'
pm_eloReportTournamentStats <- function(df){
  dfAllResults = tibble::as.tibble(df) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(totresults = n()) %>%
    dplyr::group_by(winnerName) %>%
    dplyr::summarise(winPct = n() / max(totresults),
                     odds = winPct / (1- winPct),
                     fairBettingOddsIncOrigStake = 1 + (1 - winPct) / winPct,
                     fairBettingOddsExcOrigStake = fairBettingOddsIncOrigStake - 1) %>%
    arrange(desc(winPct))
}
