import xconfig
xconfig.init("/srv/tuenti/tuenti-dev-config/")

xconfig.getValue("databaseConfig/0/tu_mgm_daily_notifications/partition_type")  ## int
xconfig.getValue("killPhotos/numberOfPhotosRequiredForTheVideo")                ## int
xconfig.getValue("serviceConfig/services/Api4FileStorage/service_protocol")     ## string

xconfig.getValue("serviceConfig/services/Push")
