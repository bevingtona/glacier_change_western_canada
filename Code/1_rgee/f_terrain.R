#### Functions (Terrain) ####

  addTer = function(ft){
    asp_cos = ee$Number(asp$multiply(3.141592654)$divide(180)$cos()$
                          reduceRegion(list(geometry = ft$geometry(), 
                                            reducer = ee$Reducer$mean(), 
                                            scale = imageRes, 
                                            crs = "EPSG:4326"))$get('aspect'))
    asp_sin = ee$Number(asp$multiply(3.141592654)$divide(180)$sin()$
                          reduceRegion(list(geometry = ft$geometry(),
                                            reducer = ee$Reducer$mean(),
                                            scale = imageRes,
                                            crs = "EPSG:4326"))$get('aspect'))
    asp_avg = ee$Number(360)$add(asp_cos$atan2(asp_sin)$multiply(180/3.141592654))$mod(360)
    
    dem_min = dem$reduceRegion(list(geometry = ft$geometry(), 
                                    reducer = ee$Reducer$min(), 
                                    scale = imageRes, 
                                    crs = "EPSG:4326"))$get('elevation')
    dem_max = dem$reduceRegion(list(geometry = ft$geometry(), 
                                    reducer = ee$Reducer$max(), 
                                    scale = imageRes, 
                                    crs = "EPSG:4326"))$get('elevation')
    dem_avg = dem$reduceRegion(list(geometry = ft$geometry(), 
                                    reducer = ee$Reducer$mean(), 
                                    scale = imageRes, 
                                    crs = "EPSG:4326"))$get('elevation')
    slp_min = slp$reduceRegion(list(geometry = ft$geometry(), 
                                    reducer = ee$Reducer$min(), 
                                    scale = imageRes, 
                                    crs = "EPSG:4326"))$get('slope')
    slp_max = slp$reduceRegion(list(geometry = ft$geometry(), 
                                    reducer = ee$Reducer$max(), 
                                    scale = imageRes, 
                                    crs = "EPSG:4326"))$get('slope')
    slp_avg = slp$reduceRegion(list(geometry = ft$geometry(), 
                                    reducer = ee$Reducer$mean(), 
                                    scale = imageRes, 
                                    crs = "EPSG:4326"))$get('slope')
    
    ft$
      set('ter_asp_avg',asp_avg)$
      set('ter_dem_min',dem_min)$
      set('ter_dem_max',dem_max)$
      set('ter_dem_avg',dem_avg)$
      set('ter_slp_min',slp_min)$
      set('ter_slp_max',slp_max)$
      set('ter_slp_avg',slp_avg)$
      set('ter_lon', ee$Feature(ft)$centroid(1)$geometry(1)$coordinates()$get(0))$
      set('ter_lat', ee$Feature(ft)$centroid(1)$geometry(1)$coordinates()$get(1))}