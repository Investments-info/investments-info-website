#!/bin/bash

sudo chmod a+x investments-info
sudo mv investments-info /var/www/html/investments-info/investments-info/investments-info_bin
sudo killall investments-info_bin
sleep 3
cd /var/www/html/investments-info/investments-info/
sudo cp investments-info.sqlite3 /var/www/html/
sudo git stash
sudo git pull origin master
sudo cp /var/www/html/investments-info.sqlite3 /var/www/html/investments-info/investments-info/
nohup sudo ./investments-info_bin > /dev/null  < /dev/null &
