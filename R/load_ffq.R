#'
#'
#'
#'
#'
#'
#'


load_ffq <- function(x)
  {

  # remove all variables which are not food frequencies

  remove_cols <- check_value(x)
  x <- x[,-remove_cols] %>% t()

  ffqx <- tibble(Item = rownames(x), Frequency =  x[, 1])

  # convert frequencies to servings per day

  servings <- map(ffqx$Frequency, convert_servings) %>% unlist()

  ffq_proc <- ffqx %>% mutate(servings = servings)

  # create AHEI S4 object for data
  object <- new('FFQ')

  object@FFQ <- ffq_proc


  # match frequencies to portions and calculate grams per day

  portion_match <- match(object@FFQ$Item, Portions$Item)

  # check for no-matches

  # nalength <- which(is.na(portion_match))
  # if(length(nalength) == 0){
  # print('do-nothing')
  # }


  ffq_portions <- Portions[portion_match,]


  # if(portion_refernce == 'FSA'){}
  #
  # if(portion_reference == 'EPIC'){}


  gramsPerDay <- object@FFQ$servings * ffq_portions$EPIC_grams


  PortionTibble <- tibble(Item = object@FFQ$Item, Portion = gramsPerDay)

  object@Portions <- PortionTibble



  nut_match <- match(object@FFQ$Item, Nutrients$Item)

  ffq_nutrients <- Nutrients[nut_match,]

  # correct to per g

  ffq_nutrients[,-1] <- ffq_nutrients[,-1] / 100

  ffq_nutrients_daily <- object@Portions$Portion


  ffq_nutrients_daily <-
    (ffq_nutrients[, -1] * object@Portions$Portion) %>% add_column(Item = object@Portions$Item, .before = 'KCALS') %>% as_tibble()


  object@Nutrients <- ffq_nutrients_daily


  return(object)

  }
