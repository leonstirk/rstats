require(sm)

layout(matrix(seq(1,8,1),4,2))

unmatched <- flood_sub
matched <- dnd_sub

unmatched_before <- unmatched[which(unmatched$after_flood == 0),]
unmatched_after <- unmatched[which(unmatched$after_flood == 1),]

matched_before <- matched[which(matched$after_flood == 0),]
matched_after <- matched[which(matched$after_flood == 1),]

rm(unmatched)
rm(matched)

densityCompare <- function(unmatched, matched) {

    sm.density.compare(unmatched$building_floor_area, unmatched$treatment, xlim = c(0,400), ylim = c(0,0.02))
    sm.density.compare(matched$building_floor_area, matched$treatment, xlim = c(0,400), ylim = c(0,0.02))

    sm.density.compare(unmatched$land_area, unmatched$treatment, xlim = c(0,2000), ylim = c(0,0.005))
    sm.density.compare(matched$land_area, matched$treatment, xlim = c(0,2000), ylim = c(0,0.005))

    sm.density.compare(unmatched$median_income, unmatched$treatment, xlim = c(0,15), ylim = c(0,0.3))
    sm.density.compare(matched$median_income, matched$treatment, xlim = c(0,15), ylim = c(0,0.3))

    sm.density.compare(unmatched$homeowner_rate, unmatched$treatment, xlim = c(0,1), ylim = c(0,3))
    sm.density.compare(matched$homeowner_rate, matched$treatment, xlim = c(0,1), ylim = c(0,3))

}

## densityCompare(unmatched, matched)
## densityCompare(unmatched_before, matched_before)
## densityCompare(unmatched_after, matched_after)
