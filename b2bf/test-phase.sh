#!/bin/sh

set -e

mkdir -p phases/$1
/bin/ls -lh bin/phase$1

mkdir -p phases/$1

# test with itself.
echo "test #1: with itself."
./bin/phase$1 -i ./bin/phase$1 > phases/$1/self.b
../bf phases/$1/self.b > tmp
diff -sq tmp bin/phase$1

# test with /bin/ls
echo "test #2: with bin/ls".
./bin/phase$1 -i /bin/ls > phases/$1/ls.b
../bf phases/$1/ls.b > tmp
diff -sq tmp /bin/ls

echo "cleaning up..."
rm -f tmp

/bin/ls -lh phases/$1
