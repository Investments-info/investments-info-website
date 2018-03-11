
/usr/bin/scp -i  ~/Documents/investments-info/finance-info.pem ~/code/investments-info/bin/investments-info  ubuntu@ec2-34-192-129-154.compute-1.amazonaws.com:/home/ubuntu
/usr/bin/ssh -i ~/Documents/investments-info/finance-info.pem  ubuntu@ec2-34-192-129-154.compute-1.amazonaws.com /home/ubuntu/install.sh
/usr/bin/scp -i  ~/Documents/investments-info/finance-info.pem ~/code/investments-info/config/settings.yml  ubuntu@ec2-34-192-129-154.compute-1.amazonaws.com:/home/ubuntu
