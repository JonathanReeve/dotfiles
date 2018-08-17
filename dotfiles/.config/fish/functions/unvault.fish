# Defined in - @ line 0
function unvault --description 'alias unvault=fusermount -u /home/jon/Documents/Settings/.private-mount'
	fusermount -u /home/jon/Documents/Settings/.private-mount $argv;
end
