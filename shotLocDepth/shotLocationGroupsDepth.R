sts <- sts %>% mutate(`Bounce Location` = case_when((`Bounce Location (Y)` > 757 ) ~ 'Out',
                                                  (`Bounce Location (X)` > 198 & `Bounce Location (X)` < 354 &  
                                                     `Bounce Location (Y)` < 297 ) ~ 'Adv. Backcourt',
                                                  (`Bounce Location (X)` > 353 & `Bounce Location (X)` < 513 &  
                                                     `Bounce Location (Y)` < 297 ) ~ 'Deuce Backcourt',
                                                  (`Bounce Location (X)` > 353 & `Bounce Location (X)` < 513 &  
                                                     `Bounce Location (Y)` < 459 ) ~ 'Deuce Frontcourt',
                                                  (`Bounce Location (X)` > 198 & `Bounce Location (X)` < 354 &  
                                                     `Bounce Location (Y)` < 459 ) ~ 'Adv. Frontcourt',
                                                  (`Bounce Location (X)` > 198 & `Bounce Location (X)` < 354 &  
                                                     `Bounce Location (Y)` < 620 ) ~ 'Opp. Deuce Frontcourt',
                                                  (`Bounce Location (X)` > 353 & `Bounce Location (X)` < 513 &  
                                                     `Bounce Location (Y)` < 620 ) ~ 'Opp. Adv. Frontcourt',
                                                  (`Bounce Location (X)` > 353 & `Bounce Location (X)` < 513 &  
                                                     `Bounce Location (Y)` < 758 ) ~ 'Opp Adv. Backcourt',
                                                  (`Bounce Location (X)` > 198 & `Bounce Location (X)` < 354 &  
                                                     `Bounce Location (Y)` < 758 ) ~ 'Opp. Deuce Backcourt',
                                                   ),
                      `Contact Location` = case_when((`Contact Location (Y)` < 159) ~ 'Out',
                                                  (`Contact Location (Y)` < 297 ) ~ 'Backcourt',
                                                  ( `Contact Location (Y)` < 459 ) ~ 'Frontcourt'))

                      