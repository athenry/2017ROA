CanTC <- rbind(biomed, chemmat, civil, electrical, mechanical) %>%
        distinct(UT, School, .keep_all = TRUE) %>%
        group_by(School) %>%
        summarise('Times Cited' = mean(TC))

USTC <- rbind(biomedUS, chemmatUS, civilUS, elecUS, mechUS) %>%
        distinct(UT, School, .keep_all = TRUE) %>%
        group_by(School) %>%
        summarise('Times Cited' = mean(TC))

CanFac <- rbind(chemmatFac, civilFac, elecFac, mechFac) %>%
        gather(key = PY, value = Fac, -School, na.rm = TRUE) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac)) %>%
        spread(PY, Fac)

CanComp <- rbind(biomed, chemmat, civil, electrical, mechanical) %>%
        count(PY, School) %>%
        spread(PY, n) %>%
        filter(School %in% CanFac$School)

CanAPF <- cbind(CanComp[1], round(CanComp[-1]/CanFac[-1],1)) 

UScomp <- rbind(chemmatUS, civilUS, elecUS, mechUS) %>%
        count(PY, School) %>%
        spread(PY, n)

USfac <- select(rbind(chemmatFacUS, civilFacUS, elecFacUS, mechFacUS), -Department) %>%
        gather(key = PY, value = Fac, -School, na.rm = TRUE) %>%
        group_by(School, PY) %>%
        summarise(Fac = sum(Fac)) %>%
        spread(PY, Fac)

USapf <- cbind(UScomp[1], round(UScomp[-1]/USfac[-1],1))

write.csv(CanTC, "CanTC.csv", quote = TRUE, row.names = FALSE)
write.csv(USTC, "USTC.csv", quote = TRUE, row.names = FALSE)
write.csv(CanAPF, "CanAPF", quote = TRUE, row.names = FALSE)
write.csv(USapf, "USapf.csv", quote = TRUE, row.names = FALSE)