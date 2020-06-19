f=open("binfile.bin","wb")
num=[5, 10, 15, 20, 25]
arr=bytearray(num)
f.write(arr)
f.close()

f=open("binfile.bin","rb")
num=list(f.read())
print (num)
f.close()

import struct
f=open("input.log", "rb")
flds = f.read(90)
print(struct.unpack('<Q', flds[0:8]))
print(struct.unpack('<10s', flds[79:89]))
