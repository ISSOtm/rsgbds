head -c 336 < /dev/urandom > random.gb
cp random.gb fixed.gb
cargo run --bin rgbfix -- $* fixed.gb
xxd random.gb > random.txt
xxd fixed.gb > fixed.txt
diff random.txt fixed.txt

rm -f random.gb fixed.gb random.txt fixed.txt
