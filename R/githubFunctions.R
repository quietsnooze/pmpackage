system('git config --global user.email "github@petermcintyre.me.uk"')
system('git config --global user.name "quietsnooze"')
system('git config --global --list')

# find the packages
#upstream is used for forking packages... and maybe I've got the synatx wrong!
#system('git remote add upstream https://github.com/quietsnooze/pmpackage')
#system('git remote rm upstream')
system('git remote add origin git@github.com:quietsnooze/pmpackage.git')
system('git remote -v')


#' pm_gh_config - configure github with username and email
#'
#' @param github_username Your username for github
#' @param github_email Your github email address
#'
#' @return
#' @export
#'
#' @examples
pm_gh_config <- function(github_username,
                         github_email){
  system(paste0('git config --global user.email "',github_email,'"'))
  system(paste0('git config --global user.name "',github_username,'"'))
  system('git config --global --list')
}



#' Pull down latest version of the project
#'
#' @return
#' @export
#'
#' @examples
pm_gh_pull_request <- function(){
  system('git pull origin master')
}




#' Push your (previously committed) github changes to github
#'
#' @return
#' @export
#'
#' @examples
pm_gh_push_request <- function(){
  system('git push -u origin master')
}
