# Učitavanje potrebnih paketa
library(simmer)

# Inicijalizacija simulacije
hospital_sim <- simmer("Hitni Prijem KBC Split")

# Definiranje parametara na temelju podataka OHBP KBC Split
prosjecni_dolasci <- runif(1, 300, 500) / 1440  # Nasumičan broj pacijenata između 300 i 500 dnevno
broj_doktora <- 32  # Broj liječnika koji dežuraju u 24 sata

# Funkcija za dodjelu hitnosti pacijenta
odredi_hitnost <- function() {
  sample(1:4, 1, prob = c(0.1, 0.3, 0.4, 0.2))  # 1 = crveni, 2 = žuti, 3 = zeleni, 4 = plavi
}

# Funkcija za određivanje vremena trijaže (eksponencijalna distribucija)
vrijeme_trijaze <- function() {
  rexp(1, 1/10)  # Prosječno 10 minuta za trijažu
}

# Funkcija za određivanje vremena obrade pacijenta prema hitnosti
vrijeme_obrade <- function(hitnost) {
  switch(as.character(hitnost),
         "1" = rexp(1, 1/60),   # Kritični pacijenti (kraće vrijeme obrade)
         "2" = rexp(1, 1/120),  # Ozbiljni pacijenti
         "3" = rexp(1, 1/180),  # Manje hitni pacijenti
         "4" = rexp(1, 1/240))  # Nehitni pacijenti
}

# Funkcija za određivanje prioriteta (manji broj = veći prioritet)
prioritet_pacijenta <- function(hitnost) {
  return(hitnost)  # Crveni (1) ima najviši prioritet, plavi (4) najniži
}

# Kreiranje procesa pacijenta
patient_trajectory <- trajectory("Put pacijenta") %>%
  seize("trijaza", 1) %>%  # Pacijent dolazi na trijažu
  timeout(vrijeme_trijaze) %>%  # Vrijeme trajanja trijaže
  set_attribute("hitnost", function() {
    h <- odredi_hitnost()
    return(h)
  }) %>%  # Određivanje hitnosti
  release("trijaza", 1) %>%  # Oslobađanje trijažnog osoblja
  branch(
    function(attrs) attrs["hitnost"],
    continue = c(TRUE, TRUE, TRUE, TRUE),
    trajectory("Crveni") %>%
      set_prioritization(c(1, 7, TRUE)) %>%
      seize("doktor", 1) %>%
      timeout(function(attrs) vrijeme_obrade(attrs["hitnost"])) %>%
      release("doktor", 1),
    trajectory("Žuti") %>%
      set_prioritization(c(2, 7, TRUE)) %>%
      seize("doktor", 1) %>%
      timeout(function(attrs) vrijeme_obrade(attrs["hitnost"])) %>%
      release("doktor", 1),
    trajectory("Zeleni") %>%
      set_prioritization(c(3, 7, TRUE)) %>%
      seize("doktor", 1) %>%
      timeout(function(attrs) vrijeme_obrade(attrs["hitnost"])) %>%
      release("doktor", 1),
    trajectory("Plavi") %>%
      set_prioritization(c(4, 7, TRUE)) %>%
      seize("doktor", 1) %>%
      timeout(function(attrs) vrijeme_obrade(attrs["hitnost"])) %>%
      release("doktor", 1)
  )

# Dodavanje resursa (trijaža i doktori) i generatora pacijenata
hospital_sim %>%
  add_resource("trijaza", capacity = 5) %>%  # Pretpostavka: 5 trijažnih mjesta
  add_resource("doktor", capacity = broj_doktora, preemptive = TRUE) %>%
  add_generator("Pacijent", patient_trajectory, function() rexp(1, prosjecni_dolasci))

# Pokretanje simulacije na 24 sata
hospital_sim %>% run(until = 1440)

# Analiza rezultata
arrivals <- get_mon_arrivals(hospital_sim)
resources <- get_mon_resources(hospital_sim)

# Izračun prosječnog vremena aktivnosti pacijenata (trijaža + obrada)
mean_activity_time <- mean(arrivals$activity_time)

# Izračun prosječnog vremena čekanja
waiting_times <- (arrivals$end_time - arrivals$start_time) - arrivals$activity_time
mean_waiting_time <- mean(waiting_times)

# Ispis rezultata
mean_activity_time
mean_waiting_time
