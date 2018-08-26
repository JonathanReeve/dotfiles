#!/run/current-system/sw/bin/fish

set CLOCKSTRING (emacsclient --eval '(if (org-clocking-p)(org-clock-get-clock-string) -1)' 2>&1 )

switch "$CLOCKSTRING"
case "*can\'t find socket*"
    echo " Emacs off!"
case "-1"
    echo " Emacs off!"
case "*server-start*"
    echo " Emacs off!"
case "*ERROR*"
    echo " Off-clock!"
case "*function definition*"
    echo " Off-clock!"
case "*"
    echo (echo $CLOCKSTRING | cut -d\" -f2 | head -c35)
end
