set -x PATH ~/.local/bin (ruby -rubygems -e "puts Gem.user_dir")/bin $PATH

# Emacs ansi-term support
if test -n "$EMACS"
  set -x TERM eterm-color
  function fish_right_prompt
    true
  end
else
  # Don't use vi keybindings in Emacs,
  # since emacs already has them. 
  fish_vi_key_bindings
end

# This function may be required for Emacs support. 
function fish_title
  true
end
