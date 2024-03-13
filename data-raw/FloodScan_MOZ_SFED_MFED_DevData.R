#' taken directly from 04_moz_flash_updates_viz_country `lr_bounded` object
#' Later will copy the code used to create it here
writeRaster(lr_bounded$sfed,
            file.path(
            "data",
            "FloodScan_SFED_MOZ_10d_DevData.tif"
            )
)
            
writeRaster(lr_bounded$mfed,
            file.path(
            "data",
            "FloodScan_MFED_MOZ_10d_DevData.tif"
            )
)
            

