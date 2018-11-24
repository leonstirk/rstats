require(sm)

layout(matrix(seq(1,8,1),4,2))

sm.density.compare(flood_sub$building_floor_area, flood_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))
sm.density.compare(dnd_sub$building_floor_area, dnd_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))

sm.density.compare(flood_sub$land_area, flood_sub$treatment, xlim = c(0,2000), ylim = c(0,0.005))
sm.density.compare(dnd_sub$land_area, dnd_sub$treatment, xlim = c(0,2000), ylim = c(0,0.005))

sm.density.compare(flood_sub$median_income, flood_sub$treatment, xlim = c(0,15), ylim = c(0,0.3))
sm.density.compare(dnd_sub$median_income, dnd_sub$treatment, xlim = c(0,15), ylim = c(0,0.3))

sm.density.compare(flood_sub$homeowner_rate, flood_sub$treatment, xlim = c(0,1), ylim = c(0,3))
sm.density.compare(dnd_sub$homeowner_rate, dnd_sub$treatment, xlim = c(0,1), ylim = c(0,3))

layout(matrix(seq(1,2,1),2,1))

sm.density.compare(flood_sub$building_floor_area, flood_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))
sm.density.compare(dnd_sub$building_floor_area, dnd_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))


sm.density.compare(flood_sub$building_floor_area, flood_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))
sm.density.compare(dnd_sub$building_floor_area, dnd_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))


sm.density.compare(flood_sub$building_floor_area, flood_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))
sm.density.compare(dnd_sub$building_floor_area, dnd_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))


sm.density.compare(flood_sub$building_floor_area, flood_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))
sm.density.compare(dnd_sub$building_floor_area, dnd_sub$treatment, xlim = c(0,400), ylim = c(0,0.02))
