#!/run/current-system/sw/bin/bash

export CLOCKSTRING=$(/run/current-system/sw/bin/emacsclient --eval '(if (org-clocking-p)(org-clock-get-clock-string) -1)' 2>&1 )

case "$CLOCKSTRING" in
    *"can\'t find socket"*)
        echo " Emacs off!" ;;
    "-1")
        echo " Emacs off!" ;;
    *"server-start"*)
        echo " Emacs off!" ;;
    *"ERROR"*)
        echo " Off-clock!" ;;
    *"function definition"*)
        echo " Off-clock!" ;;
    *)
        echo ${CLOCKSTRING:3:30} ;
esac
