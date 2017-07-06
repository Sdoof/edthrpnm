git config --global color.status.changed "cyan normal bold" 
git config --global color.status.untracked "cyan normal bold"
#$global:GitPromptSettings.WorkingForegroundColor    = [ConsoleColor]::Yellow 
#global:GitPromptSettings.UntrackedForegroundColor  = [ConsoleColor]::Yellow

#for remote console
git config --global color.diff.old "cyan"

~/.gitconfig
[color "diff"]
        old = cyan

#for local console
git config --global color.diff.old "cyan"
~/.gitconfig
[color "diff"]
        old = red
