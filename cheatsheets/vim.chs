# or just use https://vim.rtorr.com/

:edit <location/file.ext> 	#open file

Ctrl W + Ctrl W			# Switch to the other split window.

dd [Normal]			# Cut entire line
p[Normal]			# Paste the buffer
dw [Normal]			# Delete to next Word
y				# copy

$vim -d file1 file2 		# is equal to vimdiff
	->do			# Get changes from other window into the current window.
	->dp			# Put the changes from current window into the other window.
	->]c			# Jump to the next change.
	->[c			# Jump to the previous change.

:sh				# return to the shell but not end the vim process
	->exit (CTRL+D)		# return to the cim session
