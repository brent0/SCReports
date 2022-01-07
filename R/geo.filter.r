require(gstat)
require(sp)
 # -----------------------------------------------------------
    
    recode.areas = function (area) {
      r = area # by default send the same area but check if any particular groupings are required 
      if (area=="cfaall") r="cfaall"
      if (area=="cfanorth") r=c("cfa20", "cfa21", "cfa22")
      if (area=="cfasouth") r=c("cfa23", "cfa24")
      if (area=="cfa4x") r="cfa4x"
      if (area=="cfaslope") r=c("cfa24slope", "cfa23slope") 
      if (area=="cfa23slope") r="cfa23slope"
      if (area=="cfa24slope") r="cfa24slope"
      return(r)
    }
    

  # ----------------------------------------------------------------------------

  gmt.overlay = function(object) {
    out=NULL
    if ("haddockbox" %in% object) out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "haddockbox.dat")  # (lon, lat)
    if ("cfaall" %in% object)      out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "Snowcrabcfaall.dat")  # (lon, lat)
    if ("cfa20" %in% object)      out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa20.dat")  # (lon, lat)
    if ("cfa20inner" %in% object) out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa20inner.dat")  # (lon, lat)
    if ("cfa20outer" %in% object) out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa20outer.dat")  # (lon, lat)
    if ("cfa21" %in% object)      out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa21.dat")  # (lon, lat)
    if ("cfa22" %in% object)      out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa22.dat")  # (lon, lat)
    if ("cfa22inner" %in% object) out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa22inner.dat")  # (lon, lat)
    if ("cfa22outer" %in% object) out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa22outer.dat")  # (lon, lat)
    if ("cfa23" %in% object)      out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa23.dat")  # (lon, lat)
    if ("cfa23a" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa23a.dat")  # (lon, lat)
    if ("cfa23b" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa23b.dat")  # (lon, lat)
    if ("cfa23c" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa23c.dat")  # (lon, lat)
    if ("cfa23d" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa23d.dat")  # (lon, lat)
    if ("cfa23slope" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa23slope.dat")  # (lon, lat)
    if ("cfa24" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa24.dat")  # (lon, lat)
    if ("cfa24a" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa24a.dat")  # (lon, lat)
      if ("cfa24b" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa24b.dat")  # (lon, lat)
    if ("cfa24c" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa24c.dat")  # (lon, lat)
    if ("cfa24d" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa24d.dat")  # (lon, lat)
    if ("cfa24e" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa24e.dat")  # (lon, lat)
    if ("cfa24slope" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa24slope.dat")  # (lon, lat)
    if ("cfaslope" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfaslope.dat")  # (lon, lat)
    if ("cfa4x" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Fisheries", "Snowcrab", "cfa4x.dat")  # (lon, lat)
    if ("ez200" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Administrative_Boundaries", "ez200.xy")  # (lon, lat)
    if ("limit200" %in% object)     out = file.path(bio.datadirectory, "polygons", "Management_Areas", "Administrative_Boundaries", "limit200.xy")  # (lon, lat)
    if ("strat.gf" %in% object)     out = file.path(bio.datadirectory, "polygons", "Science", "PED", "strat.gf.xy")  # (lon, lat)
    if ("isobath1000m" %in% object)     out = file.path(bio.datadirectory, "polygons", "Basemaps", "Marine", "Bathymetry", "isobath1000m.dat")  # (lon, lat)
    return (out)
  }

# ---------------------------------------------

  filter.region.polygon = function( x, region, planar=F ) {
    library(gstat)
    out = NULL
    for (reg in region) {
      poly = read.table( gmt.overlay(reg), header=F)
      names(poly) =c("lon", "lat")
      
      a = NULL
      
      if (planar) {
        poly.planar = lonlat2planar (poly, projection="lambert.conic")
        b = point.in.polygon(x$plon, x$plat, poly.planar$plon, poly.planar$plat) 
      }
      
      if (!planar) {
        b = point.in.polygon(x$lon, x$lat, poly$lon, poly$lat)
      }
      a = which( b > 0 )
      
      out = c(out, a)
    }
    out = sort(unique(out))
    return(out)
  }

