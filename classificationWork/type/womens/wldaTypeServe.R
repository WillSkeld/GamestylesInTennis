wpmdfS<- wplayerMatrixdfServe %>% mutate(`Player.Gamestyle` = 
                                         case_when(`Player.Gamestyle` == 'Aggressive baseliner' | 
                                                     `Player.Gamestyle` == 'Aggressive Baseliner' ~ 
                                                     'Aggressive Baseliner',
                                                   `Player.Gamestyle` == 'Big server / aggressive baseliner'|
                                                     `Player.Gamestyle` == 'Big server / Aggressive baseliner' |
                                                     `Player.Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                                                     'Big Server / Aggressive Baseliner',
                                                   `Player.Gamestyle` == 'All court player' ~ 'All Court Player',
                                                   `Player.Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher'
                                         ))

wpmdfT<- wplayerMatrixdfType %>% mutate(`Player.Gamestyle` = 
                                        case_when(`Player.Gamestyle` == 'Aggressive baseliner' | 
                                                    `Player.Gamestyle` == 'Aggressive Baseliner' ~ 
                                                    'Aggressive Baseliner',
                                                  `Player.Gamestyle` == 'Big server / aggressive baseliner'|
                                                    `Player.Gamestyle` == 'Big server / Aggressive baseliner' |
                                                    `Player.Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                                                    'Big Server / Aggressive Baseliner',
                                                  `Player.Gamestyle` == 'All court player' ~ 'All Court Player',
                                                  `Player.Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                                        ))
wpmdfTS<- wpmdfT %>% left_join(wpmdfS)
dim(wpmdfTS)
wlda_set <- wpmdfTS[,2:81]
wlda_set[,2:80]<- sapply(wlda_set[,2:80], as.numeric)
