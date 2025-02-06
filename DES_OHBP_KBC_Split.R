# Učitavanje potrebnih paketa
library(simmer)

# Inicijalizacija simulacije
hospital_sim <- simmer("Hitni Prijem KBC Split")

# Definiranje parametara na temelju podataka OHBP KBC Split
prosjecni_dolasci <- runif(1, 300, 500) / 1440  # Nasumičan broj pacijenata između 300 i 500 dnevno
broj_doktora <- 32  # Broj liječnika koji dežuraju u 24 sata

# Kreiranje procesa pacijenta
patient_trajectory <- trajectory("Put pacijenta") %>%
  seize("doktor", 1) %>%  # Pacijent zauzima doktora
  timeout(function() rexp(1, 1/80) + rexp(1, 1/160)) %>%  # Ukupno vrijeme trijaže i obrade
  release("doktor", 1)  # Oslobađanje doktora

# Dodavanje resursa (doktori) i generatora pacijenata
hospital_sim %>%
  add_resource("doktor", capacity = broj_doktora) %>%
  add_generator("Pacijent", patient_trajectory, function() rexp(1, prosjecni_dolasci))


# Pokretanje simulacije na 24 sata
hospital_sim %>% run(until = 1440)

# Pregled iskorištenosti doktora
get_mon_resources(hospital_sim)

# Pregled podataka o dolascima pacijenata
get_mon_arrivals(hospital_sim)

# Dohvati podatke o dolascima pacijenata i računanje prosječnog vremena aktivnosti pacijenata
arrivals <- get_mon_arrivals(hospital_sim)
mean(arrivals$activity_time)



