C-h t				#bring tutorial
C-x C-f				#Opens a file
C-x f				#Recent files you open
C-x C-c				#Exit emacs
C-x b				#Visit buffer
M-x (function_name)		#Execute <function_name>
C-x C-I				#List and cycle through all current functions in code buffer aka code file
    C-S #next option
    C-R #previous option

#Personalisation folder
~/.emacs.d/

#On start the file "init.el" is called

#Link github files to emacs config folder:
$ln -s emacs-starter-kit .emacs.d

#folder for third party libraries and plugins
.emacs.d/vendor	

#Frame - "window" or everything you see when start emacs.
#Window - the frame is devided in windows that can show text editing or shell or whatever
#modeline - on the botton of each window showing info about the window
#minibuffer - on the botton of the frame showing tips, file navigation choices..

#Navigation
			M-<
			document

			M-V
			screen
				
			C-p
			line
				
C-a	M-b	C-b	C-l	C-f	M-f	C-e
begin	word	char	#center	char	word	end
				
			C-n
			#Next line

			C-v
			screen

			M->
			document

#Search
C-s <word>
#C-s again - next occurrence
#C-R(or backspace) previous occurence
#C-g cancel search and return where you start from

#Buffers - something like visual tabs of files
C-x b	   	     	  #view of current buffers
C-x C-b			  #see a visual list of all files in the buffer	
C-K 			  #within the buffer - kill/close the currently selected buffer
C-g			  #get out of the buffer
C-x o			  #next visual window
C-x 3			  #split frame into two windows
C-x 2			  #split horizontally
C-x 0			  #close window
C-x 1			  #make the current window only visible
Q   			  #Kills the current buffer

#Regions
C-SPC			#Start highlighting
	C-g	#Cancel current highlight
C-w			#cut region (push to the kill-ring)
C-y			#yang or paste back the last thing on kill-ring
M-y			#cycle through kill-ring
C-k			#kills everything to the end of the line
M-w			#copy region or push to kill-ring
C-_			#undo
C-S-BackSpace           #delete current row

#Rectangles
cua-mode		#rectangle editing; may conflict with other modes!

#Commenting
M-;			#commenting region

M-/			#code completion - usually search for matches in the buffer, kill-ring, current file, etc; NOT smart completion

C-x C-i 		#see finction list in file

C-x m			#start eshell
M-x eshell		#..

#minor modes
M-x flyspell-mode	#real-time spell checking;
    M-$	#suggestion for current word

follow-mode	    	#(first split in two window the frame) show the current file as a sequence
whitespace-mode		#triggers white space errors or mixtures.
	#red - trailing white space
	#yellow - mixed tabs and spaces
	#black -row too long
	C-c n  #cleanup the spaces errors
	C-c r  # revert changes to disk


#hooks
(add-hook 'ruby-node-hook 'whitespace-mode) #activate whitespace mode only in ruby files!

#key-bindings
(global-set-key [f6] 'split-window-horizontally)
		#global key bindings can be over shadowed with local ones

#version control mode
vs-mode
C-x v =		#DIFF against HEAD
C-x v u		#Discard any changes since the last check in
C-x v l		#view commit log

C-x g RET 	#Magit status
    s #stage for commit
    c #commit and open a commit buffer for commit msg
    C-c C-c #git commit

M-x shell   	 #Opens system shell
M-x eshell	 #opens bash emacs shell

#Snippets - search for snippets to add go snippets

#ELPA package manager
M-x package-list-packages
    i #mark package
    x #install marked packages

# Execute lisp code (eval-expression)
Alt-:

(setenv "SUNSHINE_ENV" "test")
