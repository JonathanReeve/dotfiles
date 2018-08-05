{
  programs.git = {
    enable = true;
    userName = "Jonathan Reeve";
    userEmail = "jon.reeve@gmail.com";
  };
  programs.vim = {
	  enable = true;
	  plugins = [ "vim-airline" ];
	  settings = { ignorecase = true; };
	  extraConfig = ''
		  set mouse=a
		  '';
  };

}
