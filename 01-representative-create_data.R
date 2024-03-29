# Load packages
library("tidyverse")
library("rio")
library("foreign")

# Create function to standardize variables
two_sd <- function(x) {
  return((x - mean(x, na.rm = TRUE))/(2*sd(x, na.rm = TRUE)))
}

### Import data
anes1012 <- import("anes_specialstudies_2012egss4.dta")
anes12 <- import("anes_timeseries_2012.dta")
anes16 <- import("anes_timeseries_2016.dta")
liss.b <- import("avars_200805_EN_2.0p.dta")
liss.pe <- import("cp08a_1p_EN.dta")
liss.po <- import("cv09b_2.1p_EN.dta") 
liss.s <- import("cs08a_2p_EN.dta") 
liss <- merge(liss.b, liss.pe, by="nomem_encr")
liss <- merge(liss, liss.po, by="nomem_encr")
liss <- select(liss, -nohouse_encr.y)
liss <- select(liss, -nohouse_encr.x)
liss <- merge(liss, liss.s, by="nomem_encr")
shp <- import("shp09_p_user.dta")
bes <- read.spss("BES2017_W13_Panel_v1.2.sav", to.data.frame = TRUE, use.value.labels = FALSE)

data_Suriname <- import("132981347Suriname_LAPOP_AmericasBarometer 2010 data set  original v1.dta")
data_Ecuador <- import("152597287Ecuador_LAPOP_AmericasBarometer 2010 data set  approved v3.dta")
data_Guatemala <- import("305797627Guatemala_LAPOP_AmericasBarometer 2010 data set  approved V3.dta")
data_ElSalvador <- import("316969358El Salvador_LAPOP_AmericasBarometer 2010 data set  approved v3.dta")
data_DominicanRep <- import("381781810Dominican Rep_LAPOP_AmericasBarometer 2010 data set approved v3.dta")
data_Trinidad <- import("464873967Trinidad_LAPOP_AmericasBarometer 2010 data set  approved V3.dta")
data_Peru <- import("856894271Peru_LAPOP_AmericasBarometer 2010 data set  approved V3.dta")
data_Argentina <- import("924077670Argentina_LAPOP_AmericasBarometer 2010 data set approved v3.dta")
data_Paraguay <- import("988869591Paraguay_LAPOP_AmericasBarometer 2010 data set approved v4.dta")
data_Nicaragua <- import("1020120195Nicaragua_LAPOP_AmericasBarometer 2010 data set revised V3 approved.dta")
data_CostaRica <- import("1053578213Costa Rica_LAPOP_AmericasBarometer 2010 data set revised V4.dta")
data_Uruguay <- import("1109346770Uruguay_LAPOP_AmericasBarometer 2010 data set  APPROVED V3.dta")
data_Canada <- import("1183322032Canada_LAPOP_AmericasBarometer 2010 data set  original v2.dta")
data_Chile <- import("1217681709Chile_LAPOP_AmericasBarometer 2010 data set approved v4.dta")
data_Belice <- import("1332767016Belice_LAPOP_AmericasBarometer 2010 data set  approved v3.dta")
data_Panama <- import("1390794604Panama LAPOP AmericasBarometer 2010 V3 APPROVED FEB 17.dta")
data_Venezuela <- import("1451946926Venezuela_LAPOP_AmericasBarometer 2010 data set APPROVED v3.dta")
data_Bolivia <- import("1748144313Bolivia_LAPOP_AmericasBarometer 2010 data set  approved v3.dta")
data_Jamaica <- import("1813458450Jamaica_LAPOP_AmericasBarometer 2010 data set  approved v3.dta")
data_Colombia <- import("1827093199Colombia2010_Rev1_W.dta")
data_US <- import("1954363083US_LAPOP_AmericasBarometer 2010 data set  original V1.dta")
data_Mexico <- import("2054050000Mexico_LAPOP_AmericasBarometer 2010 data set  approved V5.dta")
data_Guyana <- import("2094321522Guyana_LAPOP_AmericasBarometer 2010 data set  approved_V3.dta")
data_Brazil <- import("7948266051039660950Brazil_LAPOP_AmericasBarometer 2010 data set  approved v4.dta")

data_Paraguay$fecha <- as.numeric(data_Paraguay$fecha)
data_Canada$gi1 <- as.numeric(data_Canada$gi1)
data_US$gi1 <- as.numeric(data_US$gi1)
data_Brazil$ti <- as.numeric(data_Brazil$ti)
data_Brazil$intid <- as.numeric(data_Brazil$intid)
lapop <- bind_rows(list(data_Suriname, 
                        data_Ecuador,
                        data_Guatemala,
                        data_ElSalvador,
                        data_DominicanRep,
                        data_Trinidad,
                        data_Peru,
                        data_Argentina,
                        data_Paraguay,
                        data_Nicaragua,
                        data_CostaRica,
                        data_Uruguay,
                        data_Canada,
                        data_Chile,
                        data_Belice,
                        data_Panama,
                        data_Venezuela,
                        data_Bolivia,
                        data_Jamaica,
                        data_Colombia,
                        data_US,
                        data_Mexico,
                        data_Guyana,
                        data_Brazil))


sels <- import("~/Google Drev/data/selects/2015/828_Selects2015_PanelRCS_Data_v1.1.dta")

nzes <- import("~/Google Drev/data/nzes/NZES2014GeneralReleaseApril16.sav")

ces <- import("~/Google Drev/data/ces/CES2015_Combined_Stata14.dta")


# Recode: ANES 2010-2012
anes1012 <- anes1012 %>%
  # Recode missing
  mutate_at(vars(c4_p1, c4_pppa0035, c4_f1, c4_f2, c4_pppa0206, c4_pppa0207, c4_pppa0208, c4_pppa0209, 
                 c4_pppa0210, c4_pppa0211, c4_pppa0212, c4_pppa0213, c4_pppa0005, c4_pppa0220,
                 c4_be1, c4_be2, c4_be3, c4_be4, c4_be5, c4_zh1, c4_zh2, c4_zh3, c4_zh4,
                 c4_zf1, c4_zf2, c4_zf3, c4_zf4, c4_zf5, c4_zf6, c4_zf7, c4_zf8, c4_zf9, c4_zf10, 
                 c4_zg1, c4_zg2, c4_zg3, c4_zg4, c4_zg5, c4_zg6, c4_zg7, c4_zg8, c4_zg9, c4_zg10,
                 c4_pppa0079, c4_pppa0092, c4_pppa0093, c4_pppa0101, c4_pppa0095, c4_pppa0096, c4_pppa0097,
                 c4_bc1, c4_bc2, c4_a1), 
            function(x) case_when(x < 0 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  
  mutate_at(vars(c4_zh1, c4_zh2, c4_zh3, c4_zh4), 
            function(x) case_when(x == 1 ~ 1, x == 2 | x == 3 | x == 4 ~ 0, TRUE ~ NA_real_)) %>%
  
  mutate(
    
    dataset = "anes1012",
    cntry = "United States",
    
    student = NA,
    internet = c4_ppnet,
    
    # Outcomes
    ## Ideology
    ideology = two_sd(c4_p1),
    
    ## Involvement
    c4_bc1_rc = 2 - c4_bc1,
    c4_bc2_rc = 2 - c4_bc2,
    involvement = two_sd( c4_pppa0079 + c4_pppa0206 + c4_pppa0207 + c4_pppa0208 + c4_pppa0209 + c4_pppa0210 + c4_pppa0211 + c4_pppa0212 + c4_pppa0213 + c4_pppa0092 + c4_pppa0093 + c4_pppa0101 + c4_pppa0095 + c4_pppa0096 + c4_pppa0097 + c4_bc1_rc + c4_bc2_rc),
    
    ## Knowledge
    knowledge = two_sd( c4_zh1 + c4_zh2 + c4_zh3 + c4_zh4 ),
    
    ## Efficacy
    poleff = two_sd( ((c4_f1-5)*-1) + ((c4_f2-5)*-1) ),
    
    ## Interest
    polintr = two_sd( 5 - c4_pppa0035 + 5 - c4_a1 ),
    
    ## Participation
    polpar = two_sd( 2 - c4_pppa0005 + 2 - c4_pppa0220 ),
    
    ## Satisfaction democracy
    stfdem = NA,
    
    ## Media use
    media = two_sd( c4_be1 + c4_be2 + c4_be3 + c4_be4 + c4_be5 ),
    
    ## Political trust
    poltr = NA,
    
    # Big Five traits
    agreeableness = two_sd( ifelse(is.na(c4_zf2), ((c4_zg2-8)*-1) + c4_zg7, ((c4_zf2-8)*-1) + c4_zf7) ),
    extraversion = two_sd( ifelse(is.na(c4_zf1), ((c4_zg6-8)*-1) + c4_zg1, ((c4_zf6-8)*-1) + c4_zf1) ),
    conscientiousness = two_sd( ifelse(is.na(c4_zf3), ((c4_zg8-8)*-1) + c4_zg3, ((c4_zf8-8)*-1) + c4_zf3) ),
    neuroticism = two_sd( ifelse(is.na(c4_zf4), ((c4_zg9-8)*-1) + c4_zg4, ((c4_zf9-8)*-1) + c4_zf4) ),
    openness = two_sd( ifelse(is.na(c4_zf5), ((c4_zg10-8)*-1) + c4_zg5, ((c4_zf10-8)*-1) + c4_zf5) )
  )


# Recode: ANES 2012
anes12 <- anes12 %>%
  # Recode missing
  mutate_at(vars(tipi_extra, tipi_resv, tipi_crit, tipi_warm, tipi_dep, tipi_disorg, tipi_anx, tipi_calm, tipi_open, tipi_conv,
                 libcpo_self, prmedia_wktvnws, trustgov_trustgrev, cses_satisdem, interest_attention,
                 effic_complicstd, effic_undstd, effic_carestd, effic_saystd, effic_complicrev, effic_undrev, effic_carerev, effic_sayrev,
                 postvote_presvt, postvote_votehs, postvote_votesen, postvote_votegov,
                 dhsinvolv_board, dhsinvolv_call, dhsinvolv_letter, dhsinvolv_march, dhsinvolv_message, dhsinvolv_netpetition, dhsinvolv_org, dhsinvolv_petition, dhsinvolv_relig,
                 ofcrec_cj_correct, ofcrec_pmuk_correct, ofcrec_speaker_correct, ofcrec_speaker_correct, ofcrec_vp_correct), 
            function(x) case_when(x < 0 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  mutate(
    dataset = "anes12",
    cntry = "United States",
    
    student = dem_emptype_student,
    internet = ifelse(dem2_inethome == 1, 1, 0),
    
    # Outcomes
    ## Ideology
    ideology = two_sd(libcpo_self),
    
    ## Involvement
    involvement = two_sd( (2-dhsinvolv_board) + (2-dhsinvolv_call) + (2-dhsinvolv_letter) + (2-dhsinvolv_march) + (2-dhsinvolv_message) + (2-dhsinvolv_netpetition) + (2-dhsinvolv_org) + (2-dhsinvolv_petition) + (2-dhsinvolv_relig) ),
    
    ## Knowledge
    knowledge = two_sd( ofcrec_cj_correct + ofcrec_pmuk_correct + ofcrec_speaker_correct + ofcrec_vp_correct ),
    
    ## Efficacy
    poleff = two_sd( ifelse( !is.na(effic_complicstd), effic_complicstd + effic_undstd + effic_carestd + effic_saystd, effic_complicrev + effic_undrev + effic_carerev + effic_sayrev ) ),
    
    ## Interest
    polintr = two_sd(6 - interest_attention),
    
    ## Participation
    polpar = two_sd( (2 - postvote_presvt) + (2 - postvote_votehs) + (2 - postvote_votesen) ),
    
    ## Satisfaction democracy
    stfdem = two_sd(5 - cses_satisdem),
    
    ## Media use
    media = two_sd(prmedia_wktvnws),
    
    ## Political trust
    poltr = two_sd( 6 - trustgov_trustgrev ),
    
    # Big Five traits
    agreeableness = two_sd( (8-tipi_crit) + tipi_warm ),
    extraversion = two_sd( tipi_extra + (8-tipi_resv) ),
    conscientiousness = two_sd( tipi_dep + (8-tipi_disorg) ),
    neuroticism = two_sd( tipi_anx + (8-tipi_calm) ),
    openness = two_sd( tipi_open + (8-tipi_conv) )
    
  )


# Recode: ANES 2016
anes16 <- anes16 %>%
  # Recode missing
  mutate_at(vars(V162333:V162342, V162198, V162200, V162202, V162204, V162018a, V162018b, V162018e,
                 V161126, V162261, V161004, V161008, V162260, V162215, V162290,
                 V162072, V162073a, V162073b, V162074a, V162074b, V162075a, V162075b, V162076a, V162076b), 
            function(x) case_when(x < 0 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  
  mutate(
    dataset = "anes16",
    cntry = "United States",
    
    student = NA,
    internet = ifelse(V161326 == 1, 1, 0),
    
    
    # Outcomes
    ## Ideology
    ideology = ifelse(V161126 == 99, NA, V161126),
    ideology = two_sd(ideology),
    
    ## Involvement
    V162198 = ifelse(V162198 == 2, 0, V162198),
    V162200 = ifelse(V162200 == 2, 0, V162200),
    V162202 = ifelse(V162202 == 2, 0, V162202),
    V162204 = ifelse(V162204 == 2, 0, V162204),
    V162018a = ifelse(V162018a == 2, 0, V162018a),
    V162018b = ifelse(V162018b == 2, 0, V162018b),
    V162018e = ifelse(V162018e == 2, 0, V162018e),
    
    involvement = two_sd(V162198 + V162200 + V162202 + V162204 + V162018a + V162018b),
    
    ## Knowledge
    knowledge = two_sd( V162072 + V162073a + V162073b + V162074a + V162074b + V162075a + V162075b + V162076a + V162076b ),
    
    ## Efficacy
    poleff = two_sd(V162260 + V162215),
    
    ## Interest
    polintr = two_sd( 4 - V161004),
    
    ## Participation
    polpar1 = case_when(
      V162039 == 1 ~ 1,
      V162039 == 2 ~ 0,
      TRUE  ~  NA_real_
    ),
    
    polpar2 = case_when(
      V162034 == 1 ~ 1,
      V162034 == 2 ~ 0,
      TRUE  ~  NA_real_
    ),
    
    polpar = two_sd(polpar1 + polpar2),
    
    ## Satisfaction democracy
    stfdem = two_sd( 6 - V162290 ),
    
    ## Media use
    media = two_sd(V161008),
    
    ## Political trust
    poltr = two_sd(V162261),
    
    V162334_rc = (V162334 - 8)*-1,
    V162338_rc = (V162338 - 8)*-1,
    V162340_rc = (V162340 - 8)*-1,
    V162341_rc = (V162341 - 8)*-1,
    V162342_rc = (V162342 - 8)*-1
    
  ) %>%
  mutate(
    openness = two_sd(V162337 + V162342_rc),
    conscientiousness = two_sd(V162335 + V162340_rc),
    extraversion = two_sd(V162333 + V162338_rc),
    agreeableness = two_sd(V162339 + V162334_rc),
    neuroticism = two_sd(V162336 + V162341_rc)
    
    )


# LISS

liss <- liss %>%
  # Recode missing
  mutate_at(vars(cv09b101), 
            function(x) case_when(x == 999 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  
  mutate(
    dataset = "liss",
    cntry = "Netherlands",
    
    student = ifelse(belbezig == 7, 1, 0),
    internet = ifelse(cs08a241 == 2, 0, 1),
    
    # Outcomes
    ## Ideology
    ideology = two_sd(cv09b101),
    
    ## Involvement
    involvement = two_sd(cv09b065 + cv09b066 + cv09b067 + cv09b068 + cv09b069 + cv09b070 + cv09b071),
    
    ## Knowledge
    knowledge = NA,
    
    ## Efficacy
    poleff.1 = cv09b047 - 1,
    poleff.2 = cv09b048 - 1,
    poleff.3 = cv09b049 - 1,
    poleff.4 = ifelse(cv09b050 == 2, 0, cv09b050),
    poleff.5 = ifelse(cv09b051 == 2, 0, cv09b051),
    poleff.6 = cv09b052 - 1,
    poleff = two_sd(poleff.1 + poleff.2 + poleff.3 + poleff.4 + poleff.5 + poleff.6),
    
    ## Interest
    polintr = (cv09b012 - 3)*-1,
    polintr = two_sd(polintr),
    
    ## Participation
    polpar = two_sd(case_when(
      cv09b053 == 1 ~ 1,
      cv09b053 == 2 ~ 0
    )),
    
    
    ## Satisfaction democracy
    stfdem = ifelse(cv09b044 < 999, cv09b044, NA),
    stfdem = two_sd(stfdem),
    
    ## Media use
    media = two_sd(cv09b002 + cv09b003 + cv09b004 + cv09b005),
    
    ## Political trust
    poltr = ifelse(cv09b013 < 999, cv09b013, NA),
    poltr = two_sd(poltr)

  ) 

### Personality traits
#### Reverse code items
liss$cp08a029rc <- (liss$cp08a029 - 6) * -1
liss$cp08a039rc <- (liss$cp08a039 - 6) * -1
liss$cp08a049rc <- (liss$cp08a049 - 6) * -1
liss$cp08a027rc <- (liss$cp08a027 - 6) * -1
liss$cp08a037rc <- (liss$cp08a037 - 6) * -1
liss$cp08a047rc <- (liss$cp08a047 - 6) * -1
liss$cp08a057rc <- (liss$cp08a057 - 6) * -1
liss$cp08a025rc <- (liss$cp08a025 - 6) * -1
liss$cp08a035rc <- (liss$cp08a035 - 6) * -1
liss$cp08a055rc <- (liss$cp08a055 - 6) * -1
liss$cp08a065rc <- (liss$cp08a065 - 6) * -1
liss$cp08a045rc <- (liss$cp08a045 - 6) * -1
liss$cp08a021rc <- (liss$cp08a021 - 6) * -1
liss$cp08a031rc <- (liss$cp08a031 - 6) * -1
liss$cp08a051rc <- (liss$cp08a051 - 6) * -1
liss$cp08a041rc <- (liss$cp08a041 - 6) * -1
liss$cp08a038rc <- (liss$cp08a038 - 6) * -1
liss$cp08a028rc <- (liss$cp08a028 - 6) * -1

#### Create personality variables
liss <- within(liss, openness <- cp08a024 + cp08a034 + cp08a044 + cp08a054 + cp08a059 + cp08a064 + cp08a069 + cp08a029rc + cp08a039rc + cp08a049rc)
liss <- within(liss, conscientiousness <- cp08a022 + cp08a027rc + cp08a032 + cp08a037rc + cp08a042 + cp08a047rc + cp08a052 + cp08a057rc + cp08a062 + cp08a067)
liss <- within(liss, extraversion <- cp08a020 + cp08a025rc + cp08a030 + cp08a035rc + cp08a050 + cp08a055rc + cp08a060 + cp08a065rc + cp08a040 + cp08a045rc)
liss <- within(liss, agreeableness <- cp08a021rc + cp08a026 + cp08a066 + cp08a031rc + cp08a036 + cp08a061 + cp08a046 + cp08a051rc + cp08a041rc + cp08a056)
liss <- within(liss, neuroticism <- cp08a063 + cp08a038rc + cp08a043 + cp08a048 + cp08a023 + cp08a068 + cp08a028rc + cp08a033 + cp08a053 + cp08a058)

#### Rescale personality traits
liss$openness <- two_sd(liss$openness)
liss$conscientiousness <- two_sd(liss$conscientiousness)
liss$extraversion <- two_sd(liss$extraversion)
liss$agreeableness <- two_sd(liss$agreeableness)
liss$neuroticism <- two_sd(liss$neuroticism)

#### Get Cronbach's reliability coefficient alpha
with(liss, cronbach(data.frame(cp08a024, cp08a034, cp08a044, cp08a054, cp08a059, cp08a064, cp08a069, cp08a029rc, cp08a039rc, cp08a049rc)))$alpha
with(liss, cronbach(data.frame(cp08a022, cp08a027rc, cp08a032, cp08a037rc, cp08a042, cp08a047rc, cp08a052, cp08a057rc, cp08a062, cp08a067)))$alpha
with(liss, cronbach(data.frame(cp08a020, cp08a025rc, cp08a030, cp08a035rc, cp08a050, cp08a055rc, cp08a060, cp08a065rc, cp08a040, cp08a045rc)))$alpha
with(liss, cronbach(data.frame(cp08a021rc, cp08a026, cp08a066, cp08a031rc, cp08a036, cp08a061, cp08a046, cp08a051rc, cp08a041rc, cp08a056)))$alpha
with(liss, cronbach(data.frame(cp08a063, cp08a038rc, cp08a043, cp08a048, cp08a023, cp08a068, cp08a028rc, cp08a033, cp08a053, cp08a058)))$alpha


# BES

bes <- bes %>%
  mutate(
    dataset = "bes",
    cntry = "United Kingdom",
    
    internet = NA,
    student = ifelse(workingStatusW1W2W3W4W5 ==  5 | workingStatusW1W2W3W4W5 == 6, 1, 0),
    
    # Outcomes
    ## Ideology
    ideology = ifelse(leftRightW13 == 9999, NA, leftRightW13),
    ideology = two_sd(ideology),
    
    ## Involvement
    involvement.1 = ifelse(participation_1W13 == 9999, NA, participation_1W13),
    involvement.2 = ifelse(participation_2W13 == 9999, NA, participation_2W13),
    involvement.3 = ifelse(participation_3W13 == 9999, NA, participation_3W13),
    involvement.4 = ifelse(participation_4W13 == 9999, NA, participation_4W13),
    involvement.5 = ifelse(participation_5W13 == 9999, NA, participation_5W13),
    involvement.6 = ifelse(participation_6W12 == 9999, NA, participation_6W12),
    involvement = two_sd(involvement.1 + involvement.2 + involvement.3 + involvement.4 + involvement.5 + involvement.6),
    
    ## Knowledge
    knowledge.1 = ifelse(knowMPW3 == 5, 1, 0),
    knowledge.2 = ifelse(polKnowMilibandW1W2W3 == 3, 1, 0),
    knowledge.3 = ifelse(polKnowCleggW1W2W3 == 2, 1, 0),
    knowledge.4 = ifelse(polKnowAssadW2W3W4 == 4, 1, 0),
    knowledge.5 = ifelse(polKnowBercowW1W2W3 == 5, 1, 0),
    knowledge.6 = ifelse(polKnowHollandeW2W3W4W7W9 == 2, 1, 0),
    knowledge.7 = ifelse(polKnowKerryW2W3W4W7W9 == 1, 1, 0),
    knowledge.8 = ifelse(polKnowOsborneW1W2W3 == 1, 1, 0),
    knowledge.9 = ifelse(polKnowMayW1W2W3 == 4, 1, 0),
    knowledge.10 = ifelse(polKnowMerkelW2W3W4 == 2, 1, 0),
    knowledge.11 = ifelse(polKnowNetanyahuW2W3W4W7W9 == 3, 1, 0),
    knowledge.12 = ifelse(polKnowPutinW2W3W4 == 1, 1, 0),
    
    knowledge = two_sd(knowledge.1 + knowledge.2 + knowledge.3 + knowledge.4 + knowledge.5 + 
                         knowledge.6 + knowledge.7 + knowledge.8 + knowledge.9 + knowledge.10 + knowledge.11 + knowledge.12),
    
    ## Efficacy
    poleff.1 = ifelse(efficacyUnderstandW11 == 9999, NA, efficacyUnderstandW11),
    poleff.2 = ifelse(efficacyPolCareW11 == 9999, NA, (efficacyPolCareW11-6)*-1),
    poleff.3 = ifelse(efficacyTooMuchEffortW11 == 9999, NA, (efficacyTooMuchEffortW11-6)*-1),
    poleff = two_sd(poleff.1 + poleff.2 + poleff.3),
    
    ## Interest
    polintr = ifelse(electionInterestW13 == 9999, NA, electionInterestW13),
    polintr = two_sd(polintr),
    
    ## Participation
    polpar = ifelse(turnoutUKGeneralW12 == 9999, NA, turnoutUKGeneralW12),
    polpar = two_sd(polpar),
    
    ## Satisfaction democracy
    stfdem = ifelse(satDemUKW13 == 9999, NA, satDemUKW13),
    stfdem = two_sd(stfdem),
    
    ## Media use
    media = ifelse(infoSourceTVW13 == 9999, NA, infoSourceTVW13),
    media = two_sd(media),
    
    ## Political trust
    poltr = ifelse(trustMPsW12 == 9999, NA, trustMPsW12),
    poltr = two_sd(poltr),
    
    openness = two_sd(personality_openness),
    conscientiousness = two_sd(personality_conscientiousness),
    extraversion = two_sd(personality_extraversion),
    agreeableness = two_sd(personality_agreeableness),
    neuroticism = two_sd(personality_neuroticism)
    
    )



## SHP

shp <- shp %>%
  # Recode missing
  mutate_at(vars(p09c60:p09c69), 
            function(x) case_when(x %in% c(-3,-2,-1) ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  
  mutate(
    dataset = "shp",
    cntry = "Switzerland",
    
    internet = NA,
    student = ifelse(occupa09 == 4, 1, 0),
    
    # Outcomes
    ## Ideology
    ideology = two_sd(ifelse(p09p10 < 0, NA, p09p10)),
    
    ## Involvement
    involvement = NA,
    
    ## Knowledge
    knowledge = NA,
    
    ## Efficacy
    poleff = NA,
    
    ## Interest
    polintr = two_sd(ifelse(p09p01 %in% c(-3,-2,-1), NA, p09p01)),

    ## Participation
    polpar = two_sd(ifelse(p09p06 %in% c(-3,-2,-1), NA, p09p06)),
    
    ## Satisfaction democracy
    stfdem = two_sd(ifelse(p09p02 %in% c(-3,-2,-1), NA, p09p02)),
    
    ## Media use
    media = NA,
    
    ## Political trust
    poltr = two_sd(ifelse(p09p04 %in% c(-3,-2,-1), NA, p09p04)),
    
    openness = two_sd((p09c64 + p09c69) / 20),
    
    p09c67rc = (p09c67 - 10)*-1,
    conscientiousness = two_sd((p09c62 + p09c67rc - 1)/19),
    
    p09c60rc = (p09c60 - 10)*-1,
    extraversion = two_sd((p09c60rc + p09c65 ) / 20),
    
    p09c66rc = (p09c66 - 10)*-1,
    agreeableness = two_sd((p09c61 + p09c66rc) / 20),
    
    p09c63rc = (p09c63 - 10)*-1,
    neuroticism = two_sd((p09c68 + p09c63rc) / 20)
  )



lapop <- lapop %>%
  # Recode missing
  mutate_at(vars(cp2, cp4), 
            function(x) case_when(x == 88 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  mutate(
    dataset = "lapop",
    
    extra2 = (per6 - 8) * -1,
    extra1 = per1,
    consci1 = per3,
    consci2 = (per8 - 8) * -1,
    agree1 = per7,
    agree2 = (per2 - 8) * -1,
    neuro1 = per4,
    neuro2 = (per9 - 8) * -1,
    open1 = per5,
    open2 = (per10 - 8) * -1,
    
    openness = two_sd(open1 + open2),
    conscientiousness = two_sd(consci1 + consci2),
    extraversion = two_sd(extra1 + extra2),
    agreeableness = two_sd(agree1 + agree2),
    neuroticism = two_sd(neuro1 + neuro2),
    
    internet = case_when(www1 == 5 ~ 1,
                         www1 >= 1 & www1 <= 4 ~ 0,
                         TRUE ~ NA_real_),
    
    student = case_when(ocup4a == 4 ~ 1,
                        ocup4a >= 1 & ocup4a < 4 ~ 0,
                        ocup4a > 4 & ocup4a <= 7 ~ 0,
                        TRUE ~ NA_real_),
    
    
    # Outcomes
    ## Ideology
    ideology = two_sd(l1),
    
    ## Involvement
    cp2rc = ifelse(cp2 == 1, 1, 0),
    cp4arc = ifelse(cp4a == 1, 1, 0),
    cp4rc = ifelse(cp4 == 1, 1, 0),
    np1rc = ifelse(np1 == 1, 1, 0),
    np2rc = ifelse(np2 == 1, 1, 0),
    
    involvement = two_sd(cp2rc + cp4arc + cp4rc + np1rc + np2rc),
    
    ## Knowledge
    knowledge = two_sd(ifelse(gi1 == 1, 1, 0)),
    
    ## Efficacy
    poleff = two_sd(eff1 + eff2),
    ## Interest
    polintr = two_sd((pol1 - 5) * -1),
    
    ## Participation
    polpar = two_sd(ifelse(vb2 == 1, 1, 0)),
    
    ## Satisfaction democracy
    stfdem = two_sd(ing4),
    
    ## Media use
    media = two_sd(6 - gi0),
    
    ## Political trust
    poltr = two_sd(b14)
    
  )



lapop$cntry <- recode(lapop$pais, 
                      `27` = "Suriname", 
                      `9` = "Ecuador",
                      `2` = "Guatemala",
                      `3` = "El Salvador",
                      `21` = "Dominican Rep",
                      `25` = "Trinidad",
                      `11` = "Peru",
                      `17` = "Argentina",
                      `12` = "Paraguay",
                      `5` = "Nicaragua",
                      `6` = "Costa Rica",
                      `14` = "Uruguay",
                      `13` = "Chile",
                      `7` = "Panama",
                      `16` = "Venezuela",
                      `10` = "Bolivia",
                      `23` = "Jamaica",
                      `8` = "Colombia",
                      `1` = "Mexico",
                      `24` = "Guyana",
                      `15` = "Brazil"
)


# Swiss Electoral Study (SELECTS)

sels <- sels %>%
  # Recode missing
  mutate_at(vars(W3_f15770a:W3_f15771g), 
            function(x) case_when(x == 99 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  mutate(
    dataset = "sels",
    cntry = "Switzerland",
    
    student = case_when(f21400 == 3 ~ 1, 
                        f21400 >= 0 & f21400 < 3 ~ 0,
                        f21400 > 3 & f21400 < 10 ~ 0,
                        TRUE ~ NA_real_),
    internet = NA,
    
    agreeableness = two_sd( (10-W3_f15770c) + W3_f15770f + W3_f15771e ),
    extraversion = two_sd( W3_f15770b + W3_f15770h + (10-W3_f15771d) ),
    conscientiousness = two_sd( W3_f15770a + (10-W3_f15770g) + W3_f15771c ),
    neuroticism = two_sd( W3_f15770e + W3_f15771b + (10-W3_f15771g) ),
    openness = two_sd( W3_f15770d + W3_f15771a + W3_f15771f ),
    
    # Outcomes
    ## Ideology
    ideology = two_sd(ifelse(f15201 < 11, f15201, NA)),
    
    ## Involvement
    W3_f12600arc = ifelse(W3_f12600a == 1, 1, 0),
    W3_f12600brc = ifelse(W3_f12600b == 1, 1, 0),
    W3_f12600crc = ifelse(W3_f12600c == 1, 1, 0),
    W3_f12600drc = ifelse(W3_f12600d == 1, 1, 0),
    W3_f12600erc = ifelse(W3_f12600e == 1, 1, 0),
    W3_f12600frc = ifelse(W3_f12600f == 1, 1, 0),
    W3_f12600grc = ifelse(W3_f12600g == 1, 1, 0),
    
    involvement = two_sd(W3_f12600arc + W3_f12600brc + W3_f12600crc + W3_f12600drc + W3_f12600erc + W3_f12600frc + W3_f12600grc),
    
    ## Knowledge
    know1 = ifelse(W2_f16309r == 1, 1, 0),
    know2 = ifelse(W2_f16100r == 1, 1, 0),
    know3 = ifelse(W2_f16310r == 1, 1, 0),
    know4 = ifelse(W2_f16305r == 1, 1, 0),
    know5 = ifelse(W2_f16311r == 1, 1, 0),
    
    knowledge = two_sd(know1 + know2 + know3 + know4 + know5),
    
    ## Efficacy
    eff1 = ifelse(W2_f15350c < 6, 6 - W2_f15350c, NA),
    eff2 = ifelse(W2_f15350d < 6, 6 - W2_f15350d, NA),
    poleff = two_sd(eff1 + eff2),
    
    ## Interest
    polintr = two_sd(ifelse(f10100 < 5, 5 - f10100, NA)),
    
    ## Participation
    polpar = two_sd(ifelse(f10200r == 1, 1, 0)),
    
    ## Satisfaction democracy
    stfdem = two_sd(ifelse(f13700 < 5, 5 - f13700, NA)),
    
    ## Media use
    media = two_sd(ifelse(f13400a < 5, f13400a, NA)),
    
    ## Political trust
    poltr = two_sd(ifelse(W3_f12800_1 < 11, W3_f12800_1, NA))

  )


# NZES (New Zealand)

nzes <- nzes %>%
  mutate(
    dataset = "nzes",
    cntry = "New Zealand",
    
    internet = ifelse(!is.na(dintnt_none), 0, 1),
    student = ifelse(!is.na(dwksch), 1, 0),
    
    agreeableness = two_sd( dperscritical + (8-dperswarm) ),
    extraversion = two_sd( (8-dpersextra) + dpersreserved ),
    conscientiousness = two_sd( (8-dpersdepend) + dperscareless),
    neuroticism = two_sd( (8-dpersanxious) + dperscalm),
    openness = two_sd( (8-dperscomplex) + dpersconvent),
    
    # Outcomes
    ## Ideology
    ideology = two_sd(ifelse(dslflr < 99, dslflr, NA)),
    
    ## Involvement
    dpcnews_rc = ifelse(dpcnews == 1, 1, 0),
    dpcmoney_rc = ifelse(dpcmoney == 1, 1, 0),
    dpcposter_rc = ifelse(dpcposter == 1, 1, 0),
    
    involvement = two_sd(dpcnews_rc + dpcmoney_rc + dpcposter_rc),
    
    ## Knowledge
    knowledge = two_sd(dknow), 
    
    ## Efficacy
    dtouch_rc = ifelse(dtouch < 6, dtouch, NA),
    dsay_rc = ifelse(dsay < 6, dsay, NA),
    drun_rc = ifelse(drun < 6, drun, NA),
    dcounts_rc = ifelse(dcounts < 6, 6 - dcounts, NA),
    dcare_rc = ifelse(dcare < 6, dcare, NA),
    
    poleff = two_sd(dtouch_rc + dsay_rc + drun_rc + dcounts_rc + dcare_rc),
    
    ## Interest
    polintr = two_sd(5 - dinterest),
    
    ## Participation
    polpar = two_sd(ifelse(ddidvote == 1, 1, 0)),
    
    ## Satisfaction democracy
    stfdem = two_sd(ifelse(ddemo < 9, 5 - ddemo, NA)),
    
    ## Media use
    dtvone_rc = 5 - dtvone,
    dtvthree_rc = 5 - dtvthree, 
    dnewspaper_rc = 5 - dnewspaper,
    dnatradio_rc = 5 - dnatradio,
    dtalkback_rc = 5 - dtalkback,
    dmaoritv_rc = 5 - dmaoritv,
    dskyprime_rc = 5 - dskyprime,
    
    media = two_sd(dtvone_rc + dtvthree_rc + dnewspaper_rc + dnatradio_rc + dtalkback_rc + dmaoritv_rc + dskyprime_rc),
    
    ## Political trust
    poltr = NA

  )


# Canada Election Study 2015

ces <- ces %>%
  # Recode missing
  mutate_at(vars(p_psych1:p_psych10), 
            function(x) case_when(x == 1000 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  mutate(
    dataset = "ces",
    cntry = "Canada",
    
    student = case_when(emp_status == 5 | emp_status == 9  ~ 1, 
                        emp_status >= 0 & emp_status < 5 ~ 0,
                        emp_status > 5 & emp_status < 9 ~ 0,
                        emp_status > 9 & emp_status <= 1000 ~ 0,
                        TRUE ~ NA_real_),
    internet = NA,
    
    agreeableness = two_sd( (8-p_psych2) + p_psych7 ),
    extraversion = two_sd( p_psych1 + (8-p_psych6) ),
    conscientiousness = two_sd( p_psych3 + (8-p_psych8) ),
    neuroticism = two_sd( p_psych4 + (8-p_psych9) ),
    openness = two_sd( p_psych5 + (8-p_psych10) ),
    
    # Outcomes
    ## Ideology
    ideology = two_sd(ifelse(p_selfplace < 1000, p_selfplace, NA)),
    
    ## Involvement
    p_pt_meet_rc = ifelse(p_pt_meet == 1, 1, 0),
    p_pt_spokemeet_rc = ifelse(p_pt_spokemeet == 1, 1, 0),
    p_pt_online_rc = ifelse(p_pt_online == 1, 1, 0),
    p_pt_socmedia_rc = ifelse(p_pt_socmedia == 1, 1, 0),
    p_pt_spoke_rc = ifelse(p_pt_spoke == 1, 1, 0),
    p_pt_petition_rc = ifelse(p_pt_petition == 1, 1, 0),
    p_pt_buycott_rc = ifelse(p_pt_buycott == 1, 1, 0),
    p_pt_march_rc = ifelse(p_pt_march == 1, 1, 0),
    p_pt_internet_rc = ifelse(p_pt_internet == 1, 1, 0),
    p_pt_volunt_rc = ifelse(p_pt_volunt == 1, 1, 0),
    
    involvement = two_sd(p_pt_meet_rc + p_pt_spokemeet_rc + p_pt_online_rc + p_pt_socmedia_rc + p_pt_spoke_rc + p_pt_petition_rc + p_pt_buycott_rc + p_pt_march_rc  + p_pt_internet_rc + p_pt_volunt_rc),
    
    ## Knowledge
    know_provpm_rc = ifelse(know_provpm == 1, 1, 0),
    know_finmin_rc = ifelse(know_finmin == 1 | know_finmin == 3, 1, 0),
    know_gg_rc = ifelse(know_gg == 1 | know_gg == 3, 1, 0),
    know_putin_rc = ifelse(know_putin == 1 | know_putin == 3, 1, 0),
    
    knowledge = two_sd(know_provpm_rc + know_finmin_rc + know_gg_rc + know_putin_rc),
    
    ## Efficacy
    poleff = two_sd(ifelse(p_iss_care < 8, p_iss_care, NA)),
    
    ## Interest
    polintr = two_sd(ifelse(p_intpol < 98, p_intpol, NA)),
    
    ## Participation
    polpar = two_sd(ifelse(p_voted == 1, 1, 0)),
    
    ## Satisfaction democracy
    stfdem = two_sd(ifelse(demsat < 8, 8 - demsat, NA)),
    
    ## Media use
    p_med_tv_rc = case_when(p_med_tv > 0 & p_med_tv < 8 ~ p_med_tv, p_med_tv == 8 ~ 0, TRUE ~ NA_real_),
    p_med_paper_rc = case_when(p_med_paper > 0 & p_med_paper < 8 ~ p_med_paper, p_med_paper == 8 ~ 0, TRUE ~ NA_real_),
    p_med_radio_rc = case_when(p_med_radio > 0 & p_med_radio < 8 ~ p_med_radio, p_med_radio == 8 ~ 0, TRUE ~ NA_real_),
    p_med_internet_rc = case_when(p_med_internet > 0 & p_med_internet < 8 ~ p_med_internet, p_med_internet == 8 ~ 0, TRUE ~ NA_real_),
    
    media = two_sd(p_med_tv_rc + p_med_paper_rc + p_med_radio_rc + p_med_internet_rc),
    
    ## Political trust
    poltr = NA
    
  )

variables_list <- c("dataset", "cntry", "student", "internet", "openness", "conscientiousness", "extraversion", "agreeableness", "neuroticism", "ideology", "involvement", "knowledge", "poleff", "polintr", "polpar", "stfdem", "media", "poltr")

fulldata <- rbind(
  select(bes, variables_list),
  select(shp, variables_list),
  select(liss, variables_list),
  select(lapop, variables_list),
  select(sels, variables_list),
  select(nzes, variables_list),
  select(ces, variables_list),
  select(anes12, variables_list),
  select(anes1012, variables_list),
  select(anes16, variables_list)) %>%
  drop_na(openness, conscientiousness, extraversion, agreeableness, neuroticism) %>%
  filter(!is.na(ideology) | !is.na(involvement) | !is.na(knowledge) | !is.na(poleff) | !is.na(polintr) | !is.na(polpar) | !is.na(stfdem) | !is.na(media) | !is.na(poltr))

write_csv(fulldata, "personalitypolitics.csv")
