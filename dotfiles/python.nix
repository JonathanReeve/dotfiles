{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
     # Python Development
     pipenv
     # poetry
     # mach-nix
     (python3.withPackages(ps: with ps; [
       pandas
       matplotlib
       # python-language-server # Spacemacs integration
       flake8 # Syntax checking for emacs
       scikit-learn
       altair
       vega
       vega_datasets
       jupyter
       # jupyterlab
       # jupytext
       # tensorflow
       nltk
       pip
       numpy
       nose
       tldextract # required by qute-pass
     ]))
  ];
}
