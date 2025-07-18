#!/bin/bash

CARDDEMO_HOME=${CARDDEMO_HOME:-$(pwd)/carddemo-test}
SOURCE_DIR=${1:-app/cbl}
COPYBOOK_DIR=${2:-app/cpy}

cp -r $COPYBOOK_DIR/* $CARDDEMO_HOME/copybooks/ 2>/dev/null || true

if [ -d "app/cpy-bms" ]; then
    cp -r app/cpy-bms/* $CARDDEMO_HOME/copybooks/ 2>/dev/null || true
fi

cp -r $SOURCE_DIR/* $CARDDEMO_HOME/programs/source/ 2>/dev/null || true

for cbl_file in $SOURCE_DIR/*.cbl $SOURCE_DIR/*.CBL; do
    if [ -f "$cbl_file" ]; then
        program_name=$(basename "$cbl_file" .cbl)
        program_name=$(basename "$program_name" .CBL)
        echo "Compiling $program_name..."
        
        COBC_PATH=$(which cobc 2>/dev/null || echo "/usr/local/bin/cobc")
        
        if grep -q "PROCEDURE DIVISION USING" "$cbl_file"; then
            echo "  -> Compiling as subroutine module"
            $COBC_PATH -c -o "$CARDDEMO_HOME/programs/compiled/$program_name.o" \
                 "$cbl_file" \
                 -I "$CARDDEMO_HOME/copybooks" \
                 -std=ibm || echo "Warning: $program_name compilation failed"
        else
            echo "  -> Compiling as executable program"
            $COBC_PATH -x -o "$CARDDEMO_HOME/programs/compiled/$program_name" \
                 "$cbl_file" \
                 -I "$CARDDEMO_HOME/copybooks" \
                 -std=ibm || echo "Warning: $program_name compilation failed"
        fi
    fi
done

echo "COBOL compilation complete."
