{ config, lib, pkgs, ... }:

    let myRPackages = with pkgs.rPackages;
          [
            DT
            RColorBrewer
            RJSONIO
            XML
            cdcfluview
            devtools
            dplyr
            gert
            # ggcounty
            ggmap
            ggplot2
            ggthemes
            httr
            knitr
            mapproj
            maps
            maptools
            osmdata
            readr
            readxl
            reshape2
            rgdal
            rgeos
            rvest
            s2
            scales
            sf
            statebins
            stringr
            tidyverse
            tmap
            units
            xfun
            xml2
          ];
in {
  environment.systemPackages = [
     (rWrapper.override { packages = myRPackages; } )
     (rstudioWrapper.override { packages = myRPackages; } )
  ]
}
}
