library(tidyverse)
library(profvis)
library(matlib)


get_monster_hp <- function(zone){
  return(ceiling(10*(1.6^(pmin(zone,140)-1)+pmin(zone,140)-1)*(1.15^pmax(zone-140,0))))
}


get_monster_gold <- function(zone){
  ceiling(get_monster_hp(zone)/15*min(3,1.025^max(0,zone-75)))
}


get_cid_cost <- function(level){
  min(20,5+level)*1.07^level
}


get_hero_cost <- function(base_cost, level){
  base_cost*1.07^level
}


get_hero_damage <- function(base_damage, level, satic_modifier = 1){
  base_damage * level * static_modifier
}


damage_to_gold_aprroximation <- function(dps, zone){
  get_monster_gold(zone)
  gold_per_hp <- get_monster_gold(zone)/get_monster_hp(zone)
  gold_per_damage <- gold_per_hp * dps
  gold_per_damage
}


boss_health <- function(zone, a = 0){
  (10 
   * 10 
   * ( 1.6^(pmin(zone,140)-1) 
       + pmin(zone,140) - 1 ) 
   * ( 1.15^(pmax(zone-140,0)) )) - a
}


get_first_unbeatable_boss_zone <- function(dps, bubos=0, chronos=0, upper_limit = 10000){
  damage_per_30_second <-  (dps * (30 + (chronos * 5)))/(1 - (bubos * 0.02))
  damage_health_intercect <- uniroot(boss_health, lower = 0, upper = upper_limit, tol = 0.1, a = damage_per_30_second)
  first_unbeatable_boss_zone <- vapply(damage_health_intercect$root, function(x)ceiling(x/5)*5, numeric(1))
  first_unbeatable_boss_zone
}


time_to_kill <- function(dps, zone){
  get_monster_hp(zone) / dps + .5
}


time_to_complete_zone <- function(dps, zone, monster_kills = 0){
  time_to_kill(dps, zone) * (10 - monster_kills)
}


zone_to_time <- function(zone, monsters_killed = 0, dps = 10){
  ifelse(
    zone %% 5 == 0, 
    boss_health(zone)/dps,
    time_to_complete_zone(dps, 
                          zone -1, 
                          monster_kills = monsters_killed))
}


sum_zone_to_time <- function(zone, 
                             dps = 10,
                             start_zone = 1,
                             start_monsters_killed = 0,
                             a = 0){
  raw_sum <- cumsum(
    zone_to_time(zone, dps = dps)) - 
    cumsum(zone_to_time(zone = start_zone, 
                        monsters_killed = start_monsters_killed,
                        dps = dps)) - a
  
  raw_sum[is.na(raw_sum)] <- Inf
  raw_sum
}


get_max_zone_by_time <- function(
                                time, 
                                dps, 
                                start_zone = 1,
                                start_monsters_killed = 0, 
                                upper_limit = 10000){
  
  roots <- suppressWarnings(
    vapply(
      time, 
      function(time)
        uniroot(
          sum_zone_to_time, 
          lower = 0, 
          upper = upper_limit, 
          tol = 0.01, 
          dps = dps, 
          start_zone = start_zone, 
          start_monsters_killed = start_monsters_killed,
          a = time)$root, 
      numeric(1)))
  
  zones <- vapply(roots, floor, numeric(1))
  monsters_killed_in_zone <- roots - zones
  list(zone = zones, monster_kills = monsters_killed_in_zone)
}

#return gold earned
offline_gold <- function(seconds, 
                         dps, 
                         start_zone = 1,
                         start_monsters_killed = 0, 
                         a = 0, 
                         gold_bonus_multiplier = 1, 
                         treasure_chest_chance = 0.01, 
                         treaseure_chest_gold_multiplier = 10, 
                         fortuna_level = 0){
  
  #get max zone attainable by dps on bosses
  highest_zone_from_boss <- get_first_unbeatable_boss_zone(dps) - 1
  #get max zone attainable by time spent
  zone_by_time <- get_max_zone_by_time(time = seconds, 
                                       dps = dps, 
                                       start_zone = start_zone)
  highest_zone_from_time <- zone_by_time$zone
  highest_zone_from_time_monsters_killed <- zone_by_time$monster_kills
  monsters_killed <- ifelse(highest_zone_from_time <  highest_zone_from_boss,
                            highest_zone_from_time_monsters_killed, 
                            1)
  
  highest_zone <- pmin(highest_zone_from_time,highest_zone_from_boss)
  monster_hp = get_monster_hp(highest_zone)
  monster_gold = get_monster_gold(highest_zone)
  offline_gold = (seconds/(monster_hp/dps+0.5)) * 
    monster_gold*gold_bonus_multiplier * 
    (1+treasure_chest_chance * 
       (treaseure_chest_gold_multiplier-1)) * 
    (1+fortuna_level*0.0225) - a
  
  offline_gold
}

get_offline_gold_stats <- function(
  seconds,
  dps, 
  start_zone = 1,
  start_monsters_killed = 0, 
  a = 0, 
  gold_bonus_multiplier = 1, 
  treasure_chest_chance = 0.01, 
  treaseure_chest_gold_multiplier = 10, 
  fortuna_level = 0)
  {
  #get max zone attainable by dps on bosses
  
  highest_zone_from_boss <- get_first_unbeatable_boss_zone(dps) - 1
  #get max zone attainable by time spent
  zone_by_time <- get_max_zone_by_time(
    time = seconds, 
    dps = dps, 
    start_zone = start_zone,
    start_monsters_killed = start_monsters_killed)
  highest_zone_from_time <- zone_by_time$zone
  highest_zone_from_time_monsters_killed <- zone_by_time$monster_kills
  monsters_killed <- ifelse(highest_zone_from_time <  highest_zone_from_boss,
                            highest_zone_from_time_monsters_killed, 
                            1)
  
  highest_zone <- pmin(highest_zone_from_time,highest_zone_from_boss)
  monster_hp = get_monster_hp(highest_zone)
  monster_gold = get_monster_gold(highest_zone)
  offline_gold = (seconds/(monster_hp/dps+0.5)) * 
    monster_gold*gold_bonus_multiplier * 
    (1+treasure_chest_chance * 
       (treaseure_chest_gold_multiplier-1)) * 
    (1+fortuna_level*0.0225) - a
  
  list(highest_zone = highest_zone, 
       monsters_killed = monsters_killed,
       offline_gold = offline_gold)
}

get_hero_dps <- function(hero_tracker_df){
  hero_tracker_df$base_damage * hero_tracker_df$level * hero_tracker_df$multiplier
}

apply_upgrades <- function(hero_tracker_df, hero_upgrades_df){
  merged_df <- merge(hero_tracker_df, hero_upgrades_df, by = "hero_name")
  
  #get hero_df with multipliers
  purchasable_merged_df <- merged_df %>% 
    filter(
      level.x > level.y, 
      purchased == FALSE) %>%
    mutate(
      upgrade_multiplier = case_when(
        type  == "self" ~ value/100,
        TRUE ~ 0)
    ) %>%
    mutate(
      multiplier = multiplier + upgrade_multiplier
    ) %>%
    rename(
      level = level.x,
      upgrade_level = level.y
    )
  #calcualte dps        
  dps_vec <- get_hero_dps(purchasable_merged_df)

  purchasable_merged_df$dps <- dps_vec
  #return selected columns
  purchasable_merged_df %>%
    select(
      upgrade_name,
      cost,
      dps
    )
}

get_total_dps <- function(hero_tracker, cps, global_dps_multiplier = 1, cps_multiplier = 1){
  sum(hero_tracker$level * hero_tracker$base_damage * hero_tracker$multiplier) * global_dps_multiplier + cps * cps_multiplier
}
simulate_game <- function(total_moves, 
                          dps, 
                          monster_level, 
                          hero_tracker, 
                          hero_upgrades_df ){
  #establish counts and vector trackers
  rm(heroes_purchased_df)
  time <- 0
  zone <- 1
  monsters_killed <- 1
  cps <- 1
  #vectors will return all the interesting stats from the simulation
  time_vector <- vector("numeric", total_moves)
  purchase_vector <- vector("numeric", total_moves)
  damage_vector <- vector("numeric", total_moves)
  gold_vector <- vector("numeric", total_moves)
  zone_vector <- vector("numeric", total_moves)
  monsters_killed_vector <- vector("numeric", total_moves)
  upgrade_purchases <- vector("character", total_moves)
  #appending this dataframe with all heroes purchased
  
  for(i in 1:total_moves){
    # print(paste("action:", i))
    
    #get total dps
    dps <- get_total_dps(hero_tracker, cps)
    
    purchasable_upgrades <- apply_upgrades(hero_tracker, hero_upgrades_df) %>%
      add_column(purchase_type = "upgrade") %>%
      select(name = upgrade_name,
             cost,
             dps,
             purchase_type)
    
    upgrade_costs <- purchasable_upgrades$cost
    hero_costs <- get_hero_cost(hero_tracker$base_cost, hero_tracker$level)
    hero_tracker$hero_cost <-  hero_costs
    hero_tracker$hero_damage <- hero_tracker$base_damage * hero_tracker$level * hero_tracker$multiplier

    purchasable_heroes <- hero_tracker %>% 
      add_column(purchase_type = "hero") %>%
      select(
        name = hero_name, 
        cost = hero_cost,
        dps = hero_damage,
        purchase_type
      )
    
    purchasable_items <- rbind(purchasable_heroes, purchasable_upgrades)
    purchase_costs <- purchasable_items$cost
    
    times_to_purchase <- suppressWarnings(
      vapply(purchase_costs, 
             function(gold)tryCatch(
               uniroot(offline_gold, 
                       lower = 0, 
                       upper = 10000, 
                       tol = 0.01, 
                       dps = dps, 
                       start_zone = zone, 
                       start_monsters_killed = monsters_killed, 
                       a = gold)$root, 
               error=function(e) Inf), 
             numeric(1)))

    #get starting monster_level and kills for next loop
    offline_gold_stats <- suppressWarnings(
      get_offline_gold_stats(
        seconds = times_to_purchase, 
        dps = dps, 
        start_zone = zone, 
        start_monsters_killed = monsters_killed))

    #vector of indices to loop through
    purchase_indices <- 1:length(purchasable_items)
    
    #run offline_gold intersect with cost at newly adjusted dps
    times_to_value <- suppressWarnings(
      vapply(
        purchase_indices, 
        function(j)tryCatch(
          uniroot(offline_gold, 
                  lower = 1, 
                  upper = 100000, 
                  tol = 0.01, 
                  dps = purchasable_items$dps[j] + dps,
                  start_zone = zone, 
                  start_monsters_killed = monsters_killed,
                  a = purchasable_items$cost[j])$root, 
          error=function(e) Inf), numeric(1)))
    
    #time to profit will be our measure for best choice
    times_to_profit <- times_to_purchase + times_to_value
    
    best_purchase <- which.min(times_to_profit)
    
    #increment all the values for the next loop
    zone <- offline_gold_stats$highest_zone[best_purchase]
    monsters_killed <- offline_gold_stats$monsters_killed[best_purchase]
    time <- time + times_to_purchase[best_purchase]
               
    #if best purchase is an upgrade, adjust multipliers and set upgrade to purchased
    purchase_row <- purchasable_items[best_purchase,]
    if(purchase_row$purchase_type == "upgrade"){
      print("upgrade")
      upgrade_df_row_index <- match(purchase_row$name,hero_upgrades_df$upgrade_name)
      
      hero_upgrades_df[upgrade_df_row_index,]$purchased = TRUE
      upgrade_df_row <- hero_upgrades_df[upgrade_df_row_index,]
      
      print(paste("purchased upgrade ", upgrade_df_row$upgrade_name))
      upgrade_purchases[i] <- upgrade_df_row$upgrade_name
      #update hero multipliers
      if(upgrade_df_row$type == "self"){
        hero_df_index <- match(upgrade_df_row$hero_name, hero_tracker$hero_name)
        hero_tracker[hero_df_index,]$multiplier <- hero_tracker[hero_df_index,]$multiplier + upgrade_df_row$value
      }

    }
    else{
      hero_tracker[best_purchase,]$level <- hero_tracker[best_purchase,]$level + 1
    }
    
    time_vector[i] <- time
    purchase_vector[i] <-  hero_tracker$hero_name[best_purchase]
    damage_vector[i] <- dps
    zone_vector[i] <-  zone
    monsters_killed_vector[i] <- monsters_killed
    gold_vector[i] <- offline_gold(seconds = 1, 
                                   dps = dps)
    
    time_df <- hero_tracker %>% add_column(time = time)
    if(exists("heroes_purchased_df")){
      heroes_purchased_df <- rbind(heroes_purchased_df, time_df)
    }
    else{
      heroes_purchased_df <- time_df
    }
  }
  stats_df <- data.frame(
    time = time_vector,
    dps = damage_vector, 
    zone = zone_vector, 
    monsters_killed = monsters_killed_vector,
    gold = gold_vector,
    hero = purchase_vector
  )
  
  upgrade_purchase_df <- data.frame(
    time = time_vector,
    upgrade = upgrade_purchases
  )
  list(heroes_purchased_df = heroes_purchased_df, 
       stats_df = stats_df, 
       upgrade_purchase_df = upgrade_purchase_df)
}

# simulation_results <- simulate_game(1000, 1, 1, hero_tracker_df, hero_upgrades_df)
# saveRDS(simulation_results, "sim_results_10000.rds")
# simulation_results