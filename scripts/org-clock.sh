#!/usr/bin/fish

set CLOCKSTRING (emacsclient --eval '(if (org-clocking-p)(org-clock-get-clock-string)(message "0"))' 2>&1 )

#echo $CLOCKSTRING

#switch "$CLOCKSTRING"
#case \"0\"
    #echo " Off-clock!"
#case "*can\'t find socket*"
    #echo " Emacs off!"
#case "*server-start*"
    #echo " Emacs off!"
#case "*ERROR*"
    #echo " Off-clock!"
#case "*function definition*"
    #echo " Off-clock!"
#case "*"
    #echo (echo $CLOCKSTRING | cut -d\" -f2)
#end

switch "$CLOCKSTRING"
case \"0\"
    echo "Off-clock!"
case "*can\'t find socket*"
    echo "Emacs off!"
case "*server-start*"
    echo "Emacs off!"
case "*ERROR*"
    echo "Off-clock!"
case "*function definition*"
    echo "Off-clock!"
case "*"
    echo (echo $CLOCKSTRING | cut -d\" -f2 | head -c40)
end
