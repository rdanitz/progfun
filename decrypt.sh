#!/bin/sh

for i in ./quiz*
do
  gpg --yes --quiet --decrypt --cipher-algo AES256 --passphrase-file $i/secret.txt --output $i/$i.jl $i/$i.jl.gpg
done
