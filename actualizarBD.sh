#!/bin/sh
cd C:/Users/USUARIO/Desktop/Tesis_Maestria
git add --all

commit_message="Actualización de datos $(date +'%Y-%m-%d %H:%M:%S')"

git commit -m "$commit_message" || { echo 'commit failed'; exit 1; }

git push origin main || { echo 'push failed'; exit 1; }