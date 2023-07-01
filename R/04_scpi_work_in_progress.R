
library(tidyverse)
library(augsynth)
library(furrr)
library(glue)
library(scpi)
panel_df<-read_csv(here::here("data/panel_data.csv"))

panel_df$time_span
 term_df<- panel_df %>% filter(keyword=="pornhub",time_span=="2019-01-01 2023-06-21")
 
 implementation_dates
 ?scpi





names(term_df)
df<-scdataMulti(term_df,id.var="state",outcome.var="hits",treatment.var = "post_treat",time.var="int_date",constant=TRUE,features = list(c("hits")),effect="unit")

df$Y.post

res.pi <- scpi(df, cores = 1, sims = 100,
               e.method = "gaussian")
scplotMulti(res.pi, type = "treatment", joint = FALSE, save.data = '__scpi_data',e.out = FALSE)

res.pi

summ




ms<-multisynth(hits~post_treat,unit=state,time=int_date,data=term_df)
summary(ms)
ms




mod<-gsynth(hits~post_treat,data=term_df,index=c("state","int_date"),inference="parametric",se = TRUE)
mod$Itr.boot
scpi::scdataMulti(term_df,id.var = )
install.packages("scpi")
https://mdcattaneo.github.io/papers/Cattaneo-Feng-Titiunik_2021_JASA.pdf



term_df <- state_df %>% filter(keyword=="pornhub") %>% mutate(post_treat=as.integer(post_treat))

term_df
mod<-gsynth(hits~post_treat,data=term_df,index=c("state","relative_date"),inference="parametric",se = TRUE)
mod
summary(mod)
plot(mod)
library(gsynth)
cumuEff(mod)
?gsynth



search_term<-"torrent"
vpn_search_fun <- function(search_term,state_df,subtitle_cap="") {
  
  
  vpn_search <- state_df %>% 
    
    filter(keyword == search_term) %>%
 
    mutate()
  
  
     mod <- augsynth(hits ~ post_treat, time = relative_date, unit = geo, data = vpn_search,progfunc="GSYN",fixedeff=T)
     
     ?augsynth
?augsynth
fig1<- plot(mod) +
    labs(title = paste("Google Trend Results for ", search_term),
         subtitle = subtitle_cap,
         x = "days since law went into effect")

  print(summary(mod))
  print(fig1)
}
vpn_search %>% count(state,post_treat)
vpn_search_fun("vpn",state_df)
cumuEff(mod)
plot(mod)
mod



vpn_search %>% select(date,relative_date) 

term_df <- filter(state_df)



pacman::p_load(patchwork)
sub_cap<-"Search Volume Relative to Synthetic Louisiana"
p1<-vpn_search_fun("pornhub",state_df %>% filter(geo!="US-UT"),subtitle_cap =sub_cap)
p2<-vpn_search_fun("xvideos",state_df %>% filter(geo!="US-UT"),subtitle_cap =sub_cap)
p3<-vpn_search_fun("vpn",state_df %>% filter(geo!="US-UT"),subtitle_cap =sub_cap)
p4<-vpn_search_fun("porn",state_df %>% filter(geo!="US-UT"),subtitle_cap =sub_cap)


p1/p2/p3/p4



daily_df <- full_df %>% filter(time_span=="2023-01-01 2023-06-07") %>%
  filter(geo!="US-LA")

sub_cap<-"Search Volume Relative to Synthetic Utah"
pacman::p_load(patchwork)
p1<-vpn_search_fun("pornhub",daily_df %>% filter(geo!="US-LA"),subtitle_cap = sub_cap) 
p2<-vpn_search_fun("xvideos",daily_df %>% filter(geo!="US-LA"),subtitle_cap = sub_cap) 
p3<-vpn_search_fun("vpn",daily_df %>% filter(geo!="US-LA"),subtitle_cap = sub_cap) 
p4<-vpn_search_fun("porn",daily_df %>% filter(geo!="US-LA"),subtitle_cap = sub_cap) 

p4
p1/p2/p3/p4
p2



vpn_search_fun("pornhub",state_df %>% filter(geo!="US-UT"))



vpn_search_fun("VPN",state_df %>% filter(geo!="US-UT"))



vpn_search_fun("xvideos",state_df %>% filter(geo!="US-UT"))


multisynth(post_treat~)



vpn_search_fun("tor",state_df)



vpn_search_fun("pornhub",state_df)




state_df %>% count(keyword)
vpn_search_fun("xvideos",state_df)



vpn_search_fun("porn",state_df)



vpn_search_fun("tor",state_df)




vpn_search_fun("VPN",state_df)



vpn_search<-state_df %>% mutate(relative_date=lubridate::as_date(date)-lubridate::make_date(2023,5,2))%>% filter(keyword=="men.com") %>% mutate(post_treat=if_else(geo=="US-UT" & relative_date>0,1,0)) %>% 
  mutate(relative_date=as.double(relative_date))
mod<-augsynth::augsynth(hits~post_treat,time=relative_date,unit=geo,data=vpn_search)
plot(mod) +
  labs(title="Google Trend Results for men.com",
       subtitle="Estimate For Utah Relative to Synthetic Utah",
       x="days since law went into effect")

summary(mod)

vpn_search %>% count(relative_date)
state_names <- rownames(mod$weights)
mod$weights
augsynth_weights <- mod$weights %>% 
  as_tibble() %>% 
  bind_cols(state_names) %>% 
  select(state=...2, a_weight=V1)

augsynth_weights %>% arrange(desc(abs(a_weight)))



plot(mod) +
  labs(title="Google Trend Results for men.com",
       subtitle="Estimate For Utah Relative to Synthetic Utah",
       x="days since law went into effect")

summary(mod)



vpn_search



donor_states_list <- vpn_search  %>% pull(state) %>% unique()

permutation_test <- function(s) {
  # Takes in a state abbreviation  and runs an augsynth estimate with that state
  
  permute.panel <- vpn_search %>% mutate(permute_post = if_else(state == s & relative_date > 0, 1L, 0L))
  permute.est <- augsynth::augsynth(hits~permute_post,time=relative_date,unit=geo,data=permute.panel,progfunc="ridge",fixedeff=T)
  
  cbind(summary(permute.est)$att, state = s)
}


permute_aug_synth <- donor_states_list %>% future_map_dfr(permutation_test)


permute_aug_synth
pre_mspe <- permute_aug_synth %>% 
  group_by(state) %>% 
  filter(Time<=0) %>% 
  summarise(premspe=sqrt(mean(Estimate^2))) %>% arrange(premspe)

pre_mspe
post_mspe <- permute_aug_synth %>% 
  group_by(state) %>% 
  filter(Time>0) %>% 
  summarise(postmspe=sqrt(mean(Estimate^2)), 
            average_diff=mean(Estimate)) %>%
  arrange(desc(average_diff)) %>% 
  mutate(average_rank=row_number())

post_mspe

