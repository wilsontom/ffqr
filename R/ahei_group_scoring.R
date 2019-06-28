#' Score AHEI-2010 Component Groups
#'
#' @param x a numeric value of the group raw value (ie, portions per day,grams per day, total mg, etc...)
#' @param group a character string indicating the group to score (`A1` - `A11`)
#' @return a numeric value of the AHEI-2010 score for the specified group
#' @export

ahei_group_scoring <- function(x, group)
{
  if (group == 'A1') {
    score_index <- score_increments(start = 0.01, end = 4.99, n = 900)

    if (x >= 5) {
      xsc <- 10
    }

    if (x == 0) {
      xcs <- 0
    }

    if (x > 0 &
        x < 5) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }

  }

  if (group == 'A2') {
    score_index <- score_increments(start = 0.01, end = 3.99, n = 900)

    if (x >= 4) {
      xsc <- 10
    }

    if (x == 0) {
      xcs <- 0
    }

    if (x > 0 &
        x < 4) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }

  }



  if (group == 'A3') {
    score_index <- score_increments(start = 0.01, end = 14.99, n = 900)

    if (x >= 15) {
      xsc <- 10
    }

    if (x == 0) {
      xcs <- 0
    }

    if (x > 0 &
        x < 15) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }

  }

  if (group == 'A4') {
    # Component A4 (Sugar-sweetend drinks) needs to be inverted

    score_index <-
      score_increments(start = 0.01, end = 0.99, n = 900) %>% mutate(Score = rev(Score))

    if (x >= 1) {
      xsc <- 0
    }

    if (x == 0) {
      xcs <- 10
    }

    if (x > 0 &
        x < 1) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }

  }


  if (group == 'A5') {
    score_index <-
      score_increments(start = 0.01, end = 0.99, n = 900)

    if (x >= 1) {
      xsc <- 10
    }

    if (x == 0) {
      xcs <- 0
    }

    if (x > 0 &
        x < 1) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }


  }


  if (group == 'A6') {
    score_index <-
      score_increments(start = 0.01, end = 1.49, n = 900) %>% mutate(Score = rev(Score))

    if (x >= 1.5) {
      xsc <- 0
    }

    if (x == 0) {
      xcs <- 10
    }

    if (x > 0 &
        x < 1.5) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }


  }




  if (group == 'A7') {
    score_index <-
      score_increments(start = 0.01, end = 249.99, n = 900)

    if (x <= 0) {
      xsc <- 0
    }

    if (x >= 250) {
      xcs <- 10
    }

    if (x > 0.01 &
        x < 249.99) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }


  }


  if (group == 'A8') {
    score_index <-
      score_increments(start = 2.01, end = 9.99, n = 900)

    if (x <= 2) {
      xsc <- 0
    }

    if (x >= 10) {
      xcs <- 10
    }

    if (x > 0.01 &
        x < 9.99) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }


  }

  if (group == 'A9') {
    xsc <- x * 90
  }



  if (group == 'A10') {
    # Component 10 (Alcohol) needs to be inverted

    score_index <-
      score_increments(start = 0.01, end = 3.0, n = 900) %>% mutate(Score = rev(Score))

    if (x >= 3) {
      xsc <- 0
    }

    if (x <= 1) {
      xcs <- 10
    }

    if (x > 0 &
        x < 1) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }

  }


  if (group == 'A11') {
    # Component 11 (Trans-fat) needs to be inverted

    score_index <-
      score_increments(start = 0.01, end = 4.0, n = 900) %>% mutate(Score = rev(Score))

    if (x >= 4) {
      xsc <- 0
    }

    if (x <= 0.5) {
      xcs <- 10
    }

    if (x > 0.5 &
        x < 4) {
      xsc <-
        score_index$Score[which(score_index$Lower <= x &
                                  score_index$Upper > x)]
    }



  }

  xsc <- round((xsc / 90), digits = 2)


  return(xsc)
}
