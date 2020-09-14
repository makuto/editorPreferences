#!/bin/sh
echo -n "Overwrite ~/.emacs (do NOT do if you've already got one) (y/n)? "
read answer

# if echo "$answer" | grep -iq "^y" ;then

if [ "$answer" != "${answer#[Yy]}" ] ;then
    echo Yes
	cp dotEmacs.el ~/.emacs
else
    echo No
fi

echo ';;' >> ~/.emacs-this-machine-only.el
emacs
