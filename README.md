# food_deserts

<!-- badges: start -->
<!-- badges: end -->

The goal of food_deserts is to ...

## Analysis 1

Q1: What percentage of households in each neighbourhood live within travel thresholds of at least 1 supermarket?

- **Urban** - 10 mins walking
- **Suburban** - 10 mins driving
- **Rural** - 15 mins driving
- **Town** - 10 mins driving

## Analysis 2

Q2: What percentage of households in each neighbourhood live within 10 mins walking (urban), or 10 mins driving (suburban), or 10 mins driving (town), or 16km (rural-15 min driving) of two or more grocers (including supermarkets, grocery stores, and some specialty stores (include subtypes “health_food”, “fruit_vegetable_market”, and “cultural_grocer”).

## Analysis 3

    RFEI: (all fast_food, convenience, and retailer_with_convenience marked as “unhealthy”) / (all supermarket, grocery, health_food, fruit_and_vegetable_market, and cultural_retailer marked as “healthy” + all fast_food, convenience, and retailer_with_convenience marked as “unhealthy”) within x distance of each household/DB within each neighbourhood
    Cutoff Distances to be used for RFEI (based off neighbourhood type)
        1. Urban: within 800m or 10 minute walk
        2. Suburban: within 5km or 10 minute drive
        3. Rural: within 16km or 15 minute drive
        4. Town: within 5 km or 10 minute drive

• For each origin (DB centroid snapped to road network?)
• Create an isochrone
• Find the unhealthy points within it
○ Count them!
• Find the healthy points within it
○ Count them!
Get the ratio of unhealthy point count to total point count
