#!/bin/sh
cd C:/Users/USUARIO/Desktop/Tesis_Maestria
git add --all
timestamp() {
  date +"at %H:%M:%S on %d/%m/%Y"
}
git commit -am "Nuevo commit"
git push origin main