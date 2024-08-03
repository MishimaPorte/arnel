set -xeu
zig build run -- build test.ar > out2.s
zig cc --target=aarch64-linux -L /usr/arm-linux-gnueabi/ -nostdlib out2.s -o arm
qemu-aarch64 ./arm
