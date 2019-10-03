function ec -d 'Start an Emacs client in the GUI, starting the server as necessary.'
	emacsclient -cna "" $argv
end
