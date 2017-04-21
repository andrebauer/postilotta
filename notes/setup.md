

sudo tunctl -u $USER -t tap0
sudo ifconfig tap0 10.0.0.1 up

echo -n hello2 tcp world | nc -nw1 10.0.0.2 8080

## with unix

mirage configure -t unix --net direct --ipv4 10.0.0.2/24
make depend
make
sudo ./postilotta
sudo ./postilotta -l "*:debug"

## With ukvm

mirage configure -t ukvm --ipv4 10.0.0.2/24 --net=tap0
./ukvm-bin --net=tap0 postilotta.ukvm 
./ukvm-bin --net=tap0 postilotta.ukvm -l "*:debug"
