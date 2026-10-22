"""Regenerates the CP037 table in src/common/codecs/ebcdic.ts (prints it)."""
import codecs

table = [ord(codecs.decode(bytes([i]), "cp037")) for i in range(256)]
assert sorted(table) == list(range(256))
for i in range(0, 256, 16):
    print("  " + ", ".join("0x%02x" % v for v in table[i : i + 16]) + ",")
