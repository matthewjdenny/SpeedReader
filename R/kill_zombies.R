#' A function which takes no arguments and kills zombie R processes if the user is using a UNIX based machine
#'
#' @export
kill_zombies <- function(){
    includes <- '#include <sys/wait.h>'
    code <- 'int wstat; while (waitpid(-1, &wstat, WNOHANG) > 0) {};'
    wait <- inline::cfunction(body=code, includes=includes, convention='.C')
    wait()
}

