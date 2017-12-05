CanTC <- rbind(biomed, chemmat, civil, electrical, mechanical) %>%
        distinct(UT, School, .keep_all = TRUE) %>%
        group_by(School) %>%
        summarise('Times Cited' = mean(TC))

USTC <- rbind(biomedUS, chemmatUS, civilUS, elecUS, mechUS) %>%
        distinct(UT, School, .keep_all = TRUE) %>%
        group_by(School) %>%
        summarise('Times Cited' = mean(TC))

CanComp <- rbind(biomed, chemmat, civil, electrical, mechanical) %>%
        count(PY, School) %>%
        spread(PY, n)

CanFac <- rbind(chemmatFac, civilFac, elecFac, mechFac) %>%
        gather(key = PY, value = Fac, -School, na.rm = TRUE) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac)) %>%
        spread(PY, Fac)

UScomp <- rbind(biomedUS, chemmatUS, civilUS, elecUS, mechUS) %>%
        count(PY, School) %>%
        spread(PY, n)

USfac <- select(rbind(chemmatFacUS, civilFacUS, elecFacUS, mechFacUS), -Department) %>%
        gather(key = PY, value = Fac, -School, na.rm = TRUE) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac)) %>%
        spread(PY, Fac)
        
