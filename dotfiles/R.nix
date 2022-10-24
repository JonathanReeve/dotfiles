{ config, lib, pkgs, ... }:

let
    pmt = pkgs.rPackages.buildRPackage {
        name = "pmt";
        src = pkgs.fetchFromGitHub {
            owner = "jbedo";
            repo = "pmt";
            rev = "f5af7ada6f419382415335f5ae283f8a4643f79c";
            sha256 = "ODIRNRKsqBOqrOINQpOBOET5izKmhnp2F8DCVl4BOQI=";
        };
    };
    nominatim = pkgs.rPackages.buildRPackage {
        name = "nominatim";
        src = pkgs.fetchFromGitHub {
            owner = "hrbrmstr";
            repo = "nominatim";
            rev = "5c2baa9da26bc81eb769c39f6eb64fa81db01d34";
            sha256 = "mHkLo07mh1fgox7kfrUZg/IDSQQKimFmzDajyrBNmzw=";
        };
        propagatedBuildInputs = with pkgs.rPackages; [
            httr dplyr pbapply curl sp jsonlite
        ];
    };
    cdcfluview = pkgs.rPackages.buildRPackage {
        name = "cdcfluview";
        src = pkgs.fetchFromGitHub {
            owner = "hrbrmstr";
            repo = "cdcfluview";
            rev = "cc1791b79b492d305a3619b97e416207ecfd2887";
            sha256 = "DBwJD+DO7MZl7z4hbm9stL7FKubJX7Y7a6z/zXoJk2o=";
        };
        propagatedBuildInputs = with pkgs.rPackages; [
            httr
            progress
            dplyr
            tibble
            sf
            xml2
            purrr
            readr
            MMWRweek
            units
            sp
            jsonlite
        ];
    };

    ggcounty = pkgs.rPackages.buildRPackage {
        name = "ggcounty";
        src = pkgs.fetchFromGitHub {
            owner = "hrbrmstr";
            repo = "ggcounty";
            rev = "3e63f81cc395b15632b68400808ffcdc441af70e";
            sha256 = "3Xhx+RAKxaxK1DXCGdIn6zgXf0DIL/y4A3xiPfdaIOc=";
        };
        propagatedBuildInputs = with pkgs.rPackages; [
        ];
    };

    geospatial = pkgs.rPackages.buildRPackage {
        name = "geospatial";
        src = pkgs.fetchFromGitHub {
            owner = "cwickham";
            repo = "geospatial";
            rev = "c5df470b558db7f812cdc9d9605107399dcf49af";
            sha256 = "h2VTR0IKMlL1/0c5Vhi7zQau7SUHMrMEK1yH/cB/+uA=";
        };
        propagatedBuildInputs = with pkgs.rPackages; [
        ];
    };

    rnaturalearthhires = pkgs.rPackages.buildRPackage {
        name = "rnaturalearthhires";
        src = pkgs.fetchFromGitHub {
            owner = "ropenscilabs";
            repo = "rnaturalearthhires";
            rev = "2ed7a937f3cca4f44b157098c472f6b3ae8cd9f3";
            sha256 = "NNJwCXrYBvgt8IU2726uaYKP2kKdEh+i+Pb8J8GTknc=";
        };
        propagatedBuildInputs = with pkgs.rPackages; [
            sp
        ];
    };

    urbnmapr = pkgs.rPackages.buildRPackage {
        name = "urbnmapr";
        src = pkgs.fetchFromGitHub {
            owner = "UrbanInstitute";
            repo = "urbnmapr";
            rev = "ef9f4488d6bc916a07746aafe5755e9fb9da19eb";
            sha256 = "/nQQo9tLzvfMelZuLs3uKeCrFdwokfdVtNaziNXgoSc=";
        };
        propagatedBuildInputs = with pkgs.rPackages; [
            tibble mapproj sf
        ];
    };
    myRPackages = with pkgs.rPackages;
          [
            DT
            RColorBrewer
            RCurl
            RJSONIO
            XML
            bindrcpp
            broom
            cdcfluview
            devtools
            dplyr
            gert
            ggcounty
            geospatial
            ggmap
            ggplot2
            ggthemes
            httr
            knitr
            mapproj
            maps
            maptools
            nominatim
            osmdata
            pmt
            raster
            readr
            readxl
            reshape2
            rgdal
            rgeos
            rnaturalearth
            rnaturalearthdata
            rnaturalearthhires
            rvest
            s2
            scales
            sf
            statebins
            stringr
            stylo
            tidyverse
            tmap
            tmaptools
            units
            urbnmapr
            xaringan
            xfun
            xml2
          ];
in {
  environment.systemPackages = with pkgs; [
     (rWrapper.override { packages = myRPackages; } )
     (rstudioWrapper.override { packages = myRPackages; } )
  ];
}
