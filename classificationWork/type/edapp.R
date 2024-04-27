raf<-sts %>% filter(Player == "Rafael Nadal") %>% 
  count(Player,`Shot Type`)%>%mutate(prop = round(prop.table((n)),3))

rog<-sts %>% filter(Player == "Roger Federer") %>% 
  count(Player,`Shot Type`)%>%mutate(prop = round(prop.table((n)),3))

##################

serve_t<-pmdfS %>% group_by(Player.Gamestyle) %>% summarise(Ace = mean(as.numeric(Ace)),
                                                                 `Double Fault` = mean(as.numeric(DF)),
                                                                 `First Serve In` = mean(as.numeric(FSI)),
                                                                `Second Serve In` = mean(as.numeric(SSI)))
serve_t<- melt(serve_t[,c("Player.Gamestyle","Ace","Double Fault","First Serve In","Second Serve In")],id.vars=1)

ggplot(serve_t, aes(fill = variable, y= value, x = Player.Gamestyle)) + geom_col(position = "dodge") + 
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + ylab("Proportion")


#####################

sit_t<- sts %>% mutate(`Player Gamestyle` = 
                          case_when(`Player Gamestyle` == 'Aggressive baseliner' | 
                                      `Player Gamestyle` == 'Aggressive Baseliner' ~ 
                                      'Aggressive Baseliner',
                                    `Player Gamestyle` == 'Big server / aggressive baseliner'|
                                      `Player Gamestyle` == 'Big server / Aggressive baseliner' |
                                      `Player Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                                      'Big Server / Aggressive Baseliner',
                                    `Player Gamestyle` == 'All court player' ~ 'All Court Player',
                                    `Player Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                                    `Player Gamestyle` == 'Big server' ~ ' Big Server',
                          )) %>% count(`Player Gamestyle`, Situation) %>% drop_na() %>% 
  group_by(`Player Gamestyle`) %>% mutate(prop = round(prop.table((n)),3))


sit_t<- melt(serve_t[,c("Player.Gamestyle","Attacking","Defensive","Neutral")],id.vars=1)

ggplot(sit_t, aes(fill = Situation, y= prop, x = `Player Gamestyle`)) + geom_col(position = "dodge") + 
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + ylab("Proportion")


######################################

type_t<-sts1 %>% 
  mutate(`Shot Type` = case_when(`Shot Type` == 'Drop Shot' |`Shot Type` == 'Drop' |`Shot Type` == 'Drop Volley' ~ 'Drop Shot', 
                                                    `Shot Type` == 'Volley' |`Shot Type` == 'Half Volley' | `Shot Type` == 'Drive Volley'  ~ 'Volley',
                                                    `Shot Type` == 'Lob' |`Shot Type` == 'Lob Volley'  ~ 'Lob',
                                                    `Shot Type` == 'Other' |`Shot Type` == 'Unclassified' | `Shot Type` == 'Pick-Up'  ~ 'Other',
                                                    `Shot Type` == 'Groundstroke'   ~ 'Groundstroke',
                                                    `Shot Type` == 'Slice'  ~ 'Slice',
                                                    `Shot Type` == 'Block' ~ 'Block',
                                                    `Shot Type` == 'Smash' ~ 'Smash')) %>% filter(!(is.na(Situation)) & !(is.na(`Shot Type`))) %>% 
  mutate(`Player Gamestyle` = 
                          case_when(`Player Gamestyle` == 'Aggressive baseliner' | 
                                      `Player Gamestyle` == 'Aggressive Baseliner' ~ 
                                      'Aggressive Baseliner',
                                    `Player Gamestyle` == 'Big server / aggressive baseliner'|
                                      `Player Gamestyle` == 'Big server / Aggressive baseliner' |
                                      `Player Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                                      'Big Server / Aggressive Baseliner',
                                    `Player Gamestyle` == 'All court player' ~ 'All Court Player',
                                    `Player Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                                    `Player Gamestyle` == 'Big server' ~ ' Big Server',
                          ))

type_t<-type_t %>% count(`Player Gamestyle`,`Shot Type`)%>% group_by(`Player Gamestyle`) %>% mutate(prop = round(prop.table((n)),3)) %>% drop_na()

ggplot(type_t %>% filter(`Shot Type` == 'Groundstroke'), aes(y= prop, x = `Player Gamestyle`, fill = `Player Gamestyle`  )) + geom_col() + 
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1)) + ylab("Proportion")

ggplot(type_t %>% filter(`Shot Type` != 'Groundstroke'), aes(y= prop, x = `Player Gamestyle`, fill = `Player Gamestyle`  )) + geom_col(show.legend = FALSE) + 
  theme(axis.text.x = element_blank()) + ylab("Proportion") + 
  facet_wrap(~`Shot Type`)

##########################################################################################

depth_t<-sts %>% mutate(`Player Gamestyle` = 
                 case_when(`Player Gamestyle` == 'Aggressive baseliner' | 
                             `Player Gamestyle` == 'Aggressive Baseliner' ~ 
                             'Aggressive Baseliner',
                           `Player Gamestyle` == 'Big server / aggressive baseliner'|
                             `Player Gamestyle` == 'Big server / Aggressive baseliner' |
                             `Player Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                             'Big Server / Aggressive Baseliner',
                           `Player Gamestyle` == 'All court player' ~ 'All Court Player',
                           `Player Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                           `Player Gamestyle` == 'Big server' ~ ' Big Server',
                 )) %>% select(`Player Gamestyle`, `Contact Location (X)`,`Contact Location (Y)`) %>% drop_na()
depth_t$`Contact Location (X)`<-round_any(depth_t$`Contact Location (X)`,10)
depth_t$`Contact Location (Y)`<-round_any(depth_t$`Contact Location (Y)`,10)
depth_t<- depth_t %>% dplyr::count(`Player Gamestyle`, `Contact Location (X)`, `Contact Location (Y)`) %>% group_by(`Player Gamestyle`) %>%
  mutate(prop = prop.table((n)))

ggplot(depth_t, aes(x = `Contact Location (X)`, y = `Contact Location (Y)`, fill = prop)) + geom_tile() +
  facet_wrap(~`Player Gamestyle`)

sts %>% mutate(`Player Gamestyle` = 
                 case_when(`Player Gamestyle` == 'Aggressive baseliner' | 
                             `Player Gamestyle` == 'Aggressive Baseliner' ~ 
                             'Aggressive Baseliner',
                           `Player Gamestyle` == 'Big server / aggressive baseliner'|
                             `Player Gamestyle` == 'Big server / Aggressive baseliner' |
                             `Player Gamestyle` == 'Big Server / Aggressive Baseliner' ~
                             'Big Server / Aggressive Baseliner',
                           `Player Gamestyle` == 'All court player' ~ 'All Court Player',
                           `Player Gamestyle` == 'Baseliner / Counter Puncher' ~ 'Counter Puncher',
                           `Player Gamestyle` == 'Big server' ~ ' Big Server',
                 )) %>% count(`Player Gamestyle`) %>% drop_na() %>% mutate(prop = round(prop.table((n)),3))

