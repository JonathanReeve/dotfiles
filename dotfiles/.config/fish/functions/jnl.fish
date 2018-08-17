# Defined in fish-setup.fish @ line 41
function jnl
	vault
	and emacsclient -c $vaultmount/Journal/jnl.org
	and unvault
end
