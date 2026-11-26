import re,sys
src=open(sys.argv[1]).read()
def rep(m):
    body=' '.join(m.group(1).split())
    func=body.split()[0]
    seg=re.search(r'SEGMENT \((\w[\w-]*)\)',body)
    area=re.search(r'(?:INTO|FROM) \(([\w-]+)\)',body)
    if func=='CHKP':
        return "           CALL 'DLISTUB' USING BY CONTENT 'CHKP' 'CHKP    '\n                BY REFERENCE WK-CHKPT-ID DIBSTAT\n"
    return ("           CALL 'DLISTUB' USING BY CONTENT '%-4s' '%-8s'\n                BY REFERENCE %s DIBSTAT\n" % (func,seg.group(1),area.group(1)))
src=re.sub(r' +EXEC DLI(.*?)END-EXEC\n',rep,src,flags=re.S)
src=src.replace("       01 WS-IMS-VARIABLES.","       01 DIBSTAT PIC X(02).\n       01 WS-IMS-VARIABLES.")
src=re.sub(r'PROCEDURE DIVISION +USING IO-PCB-MASK\n +PGM-PCB-MASK\.','PROCEDURE DIVISION.',src)
open(sys.argv[2],'w').write(src)
