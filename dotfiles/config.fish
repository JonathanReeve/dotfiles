#set -x PATH ~/.local/bin $PATH
#set -x PATH ~/.local/bin ~/.node/bin ~/.gem/bin $PATH

# Emacs ansi-term support
if test -n "$EMACS"
  set -x TERM eterm-color
  # Disable right prompt in emacs
  function fish_right_prompt; true; end
  #This function may be required for Emacs support.
  function fish_title; true; end
end


function fish_default_mode_prompt --description "Display the default mode for the prompt"
  true
end

if status --is-interactive
  # Chips: fish plugin manager
  if [ -e ~/.config/chips/build.fish ] ; . ~/.config/chips/build.fish ; end

  # Don't use vi keybindings in unknown terminals,
  # since weird things can happen.
  set acceptable_terms xterm-256color screen-256color
  if contains "$TERM" acceptable_terms
    fish_vi_key_bindings
    # Load pywal colors
    cat ~/.cache/wal/sequences
  end
end
