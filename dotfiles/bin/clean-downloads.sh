#!/usr/bin/env sh


exceptions="telegram
            transmission"

echo "This will delete all files and folders in ~/downloads/ except the following:"
while read e; do
    echo $e
done <<< $exceptions

read -p "Are you sure? " -n 1 -r
echo 

if [[ $REPLY =~ ^[Yy]$ ]]
then
    cd ~/downloads/ || exit 1

    for f in *; do
        delete=yes
        for e in $exceptions; do 
            if [[ "$e" = "$f" ]]
            then
                delete=no
                break
            fi
        done
        if [[ $delete = yes ]]
        then
            rm -vRf "$f" 
        fi
    done
else
    echo "No files have been removed"
fi
