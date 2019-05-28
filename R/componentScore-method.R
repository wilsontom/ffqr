#'
#'
#'
#'
#'
#'

setMethod(f = componentScores, signature = 'AHEI',
          function(object)
  {

  AHEI_INDEX <-
    full_join(object@FFQ, object@Portions, by = 'Item') %>% left_join(ComponentIndex, ., by = 'Item') %>%
    left_join(object@Nutrients, ., by = 'Item')


  A1 <-
    AHEI_INDEX %>% filter(Component == 'A1') %>% summarise(Value = sum(servings)) %>% add_column(Component = 'A1', .before = 'Value')

  A2 <-
    AHEI_INDEX %>% filter(Component == 'A2') %>% summarise(Value = sum(servings)) %>% add_column(Component = 'A2', .before = 'Value')

  A3 <-
    AHEI_INDEX %>% filter(Component == 'A3') %>% summarise(Value = sum(Portion)) %>% add_column(Component = 'A3', .before = 'Value')

  A4 <-
    AHEI_INDEX %>% filter(Component == 'A4') %>% summarise(Value = sum(servings)) %>% add_column(Component = 'A4', .before = 'Value')

  A5 <-
    AHEI_INDEX %>% filter(Component == 'A5') %>% summarise(Value = sum(servings)) %>% add_column(Component = 'A5', .before = 'Value')

  A6 <-
    AHEI_INDEX %>% filter(Component == 'A6') %>% summarise(Value = sum(servings)) %>% add_column(Component = 'A6', .before = 'Value')

  A7 <-
    AHEI_INDEX %>% select(`FOD18:3cn3`, `FOD20:5cn3`, `FOD22:6cn3`) %>% summarise_each(sum) %>% mutate(Value = sum(.) * 100) %>%
    select(Value) %>% add_column(Component = 'A7', .before = 'Value')


  A8 <-
    AHEI_INDEX %>% select(KCALS, POLYFOD) %>% summarise(Energy = sum(KCALS), PUFA = sum(POLYFOD) * 9) %>%
    mutate(Value = (PUFA / Energy) * 100) %>% select(Value) %>% add_column(Component = 'A8', .before = 'Value')

  A9 <-
    AHEI_INDEX %>% summarise(Value = sum(Sodium)) %>% add_column(Component = 'A9', .before = 'Value')

  A10 <-
    AHEI_INDEX %>% filter(Component == 'A10') %>% summarise(Value = sum(servings)) %>% add_column(Component = 'A10', .before = 'Value')

  A11 <-
    AHEI_INDEX %>% select(KCALS, FODTRANS) %>% summarise(Energy = sum(KCALS), TRANS = sum(FODTRANS) * 9) %>%
    mutate(Value = (TRANS / Energy) * 100) %>% select(Value) %>% add_column(Component = 'A11', .before = 'Value')

  component_tibble <- bind_rows(A1,
                                A2,
                                A3,
                                A4,
                                A5,
                                A6,
                                A7,
                                A8,
                                A9,
                                A10,
                                A11)

  object@Components <- component_tibble


return(component_tibble)

          }
)

