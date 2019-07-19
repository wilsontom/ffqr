#' @rdname AlcoholAHEI
#'
#' @description
#'
#' @include allGenerics.R
#' @include allClasses.R

setMethod(f = AlcoholAHEI, signature = 'AHEI',
          function(object, gender)
          {
            A10Raw <- object@rawValues %>% select(A10)


            gender_correction <- function(x, gender)
            {
              if (gender == 'M') {
                score_index <-
                  score_increments(start = 2.01,
                                   end = 3.49,
                                   n = 900) %>% mutate(Score = rev(Score))

                if (x >= 3.5) {
                  xcs <- 0
                }

                if (x <= 2) {
                  xcs <- 10
                }

                if (x > 2 &
                    x < 3.5) {
                  xcs <-
                    round(score_index$Score[which(score_index$Lower <= x &
                                                    score_index$Upper > x)] / 90, digits = 2)
                }

              }


              if (gender == 'F') {
                score_index <-
                  score_increments(start = 1.51,
                                   end = 2.49,
                                   n = 900) %>% mutate(Score = rev(Score))

                if (x >= 2.5) {
                  xcs <- 0
                }

                if (x <= 1.5) {
                  xcs <- 10
                }

                if (x > 1.5 &
                    x < 2.5) {
                  xcs <-
                    round(score_index$Score[which(score_index$Lower <= x &
                                                    score_index$Upper > x)] / 90, digits = 2)
                }

              }

              return(xcs)
            }


            object@componentScores <- object@componentScores %>%
              mutate(A10 = map2_dbl(A10Raw$A10, gender, gender_correction))


            object@AHEIScore <- apply(object@componentScores, 1, sum)

            return(object)

          })
