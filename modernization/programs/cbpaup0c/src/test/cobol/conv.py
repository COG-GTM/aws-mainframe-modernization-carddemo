import sys
d=open(sys.argv[1],'rb').read(); out=open(sys.argv[2],'wb')
def conv(b,ranges):
    b=bytearray(b)
    for s,e in ranges: b[s:e]=b[s:e].decode('cp037').encode('latin-1')
    return bytes(b)
i=0
while i<len(d):
    L=int.from_bytes(d[i:i+2],'big'); r=d[i:i+L]; name=r[10:18]; i+=L
    if name=='PAUTSUM0'.encode('cp037') and L==140:
        out.write(b'S'+conv(r[39:139],[(6,26),(66,100)])+b' '*100)
    elif name=='PAUTDTL1'.encode('cp037'):
        out.write(b'D'+conv(r[39:239],[(8,74),(88,200)]))
