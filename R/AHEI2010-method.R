#' @rdname AHEI2010
#'
#' @description Method to calculate the individual component scores for the Alternative Healthy Eating Index (AHEI) 2010 from a
#' Foor Frequency Questionaire (FFQ). The AHEI-2010 consists of 11 individual components which are all dietary components which are
#' linked to healthy eating and the incidence of chronic conditions.
#'
#' @include allGenerics.R
#' @include allClasses.R

setMethod(f = AHEI2010, signature = 'ffqr',
          function(object)
          {
            AHEI_SCORES <- list()
            for (i in seq_along(object@FFQ)) {
              AHEI_INDEX <-
                left_join(AHEI_ComponentIndex, object@FFQ[[i]], by = 'FOOD_COMPONENT') %>% left_join(., object@Nutrients[[i]], by = 'FOOD_COMPONENT')


              A1 <-
                AHEI_INDEX %>% filter(GROUP == 'A1') %>% summarise(Value = sum(SERVINGS)) %>% tibble::add_column(Component = 'A1', .before = 'Value')

              A2 <-
                AHEI_INDEX %>% filter(GROUP == 'A2') %>% summarise(Value = sum(SERVINGS)) %>% tibble::add_column(Component = 'A2', .before = 'Value')

              A3 <-
                AHEI_INDEX %>% filter(GROUP == 'A3') %>% summarise(Value = sum(FIBRE)) %>% tibble::add_column(Component = 'A3', .before = 'Value')

              A4 <-
                AHEI_INDEX %>% filter(GROUP == 'A4') %>% summarise(Value = sum(SERVINGS)) %>% tibble::add_column(Component = 'A4', .before = 'Value')

              A5 <-
                AHEI_INDEX %>% filter(GROUP == 'A5') %>% summarise(Value = sum(SERVINGS)) %>% tibble::add_column(Component = 'A5', .before = 'Value')

              A6 <-
                AHEI_INDEX %>% filter(GROUP == 'A6') %>% summarise(Value = sum(SERVINGS)) %>% tibble::add_column(Component = 'A6', .before = 'Value')

              A7 <-
                AHEI_INDEX %>% select(EICOSAPENTAENOIC_ACID, DOCOSAHEXAENOIC_ACID) %>% summarise_each(sum) %>% mutate(Value = sum(.) * 1000) %>%
                select(Value) %>% tibble::add_column(Component = 'A7', .before = 'Value')


              A8 <-
                AHEI_INDEX %>% select(KCALS, POLYUNSATURATED_FAT) %>% summarise(Energy = sum(KCALS),
                                                                                PUFA = sum(POLYUNSATURATED_FAT) * 9) %>%
                mutate(Value = (PUFA / Energy) * 100) %>% select(Value) %>% tibble::add_column(Component = 'A8', .before = 'Value')

              A9 <-
                AHEI_INDEX %>% summarise(Value = sum(SODIUM)) %>% tibble::add_column(Component = 'A9', .before = 'Value')

              A10 <-
                AHEI_INDEX %>% filter(GROUP == 'A10') %>% summarise(Value = sum(SERVINGS)) %>% tibble::add_column(Component = 'A10', .before = 'Value')

              A11 <-
                AHEI_INDEX %>% select(KCALS, TRANS_FAT) %>% summarise(Energy = sum(KCALS),
                                                                      TRANS_FAT = sum(TRANS_FAT) * 9) %>%
                mutate(Value = (TRANS_FAT / Energy) * 100) %>% select(Value) %>% tibble::add_column(Component = 'A11', .before = 'Value')

              AHEI_SCORES[[i]] <- bind_rows(A1,
                                            A2,
                                            A3,
                                            A4,
                                            A5,
                                            A6,
                                            A7,
                                            A8,
                                            A9,
                                            A10,
                                            A11) %>% mutate(Score = map2_dbl(.$Value, .$Component, ~
                                                                               {
                                                                                 ahei_group_scoring(.x, .y)
                                                                               }))



            }


           AHEIob <- new('AHEI')


           rawValues <-
             map(AHEI_SCORES, ~ {
               tibble(Component = .$Component,
                      rawValue = .$Value,
               )
             })

           for(i in seq_along(rawValues)) {
             rawValues[[i]] <- rawValues[[i]] %>% mutate(ID = rep(i))
           }


           AHEIob@rawValues <-
             rawValues %>% bind_rows() %>%
             tidyr::spread(Component, rawValue) %>%
             select(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)

           Scores <-
             map(AHEI_SCORES, ~ {
               tibble(Component = .$Component,
                      Score = .$Score,
               )
             })

           for(i in seq_along(Scores)) {
             Scores[[i]] <- Scores[[i]] %>% mutate(ID = rep(i))
           }

           AHEIob@componentScores <-
             Scores %>% bind_rows() %>%
             tidyr::spread(Component, Score) %>%
             select(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)


           AHEIob@componentScores <- AHEIob@componentScores  %>%
             mutate(A9 = (11 - ntile(A9, n = 10)))


           AHEIob@AHEIScore <- apply(AHEIob@componentScores, 1, sum)


            return(AHEIob)

          })
