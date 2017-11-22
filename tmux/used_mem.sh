#!/bin/sh

calculate_used_mem() {
    if ! type vm_stat > /dev/null 2>&1; then
        MEM_TOTAL=`free |awk 'NR==2 {print $2}'`
        MEM_USED=`free |awk 'NR==2 {print $3}'`
        USED_MEM_PERCENT=$(echo `echo "scale=4; $MEM_USED / $MEM_TOTAL * 100" | bc | sed -e 's/^\(....\).*/\1/'`)
    else
    	# vm_stat
        VM_STAT=`vm_stat`

        PAGES_WIRED=$(echo "$VM_STAT" | awk '/Pages wired down/ {print $NF}' | tr -d '.')
        PAGES_ACTIVE=$(echo "$VM_STAT" | awk '/Pages active/ {print $NF}' | tr -d '.')
        PAGES_INACTIVE=$(echo "$VM_STAT" | awk '/Pages inactive/ {print $NF}' | tr -d '.')
        PAGES_SPECULATIVE=$(echo "$VM_STAT" | awk '/Pages speculative/ {print $NF}' | tr -d '.')
        PAGES_OCCUPIED=$(echo "$VM_STAT" | awk '/Pages occupied by compressor/ {print $NF}' | tr -d '.')
        PAGES_PURGEABLE=$(echo "$VM_STAT" | awk '/Pages purgeable/ {print $NF}' | tr -d '.')
        FILE_BACKED_PAGES=$(echo "$VM_STAT" |awk '/File-backed pages/ {print $NF}' | tr -d '.')
        CACHED=$(($PAGES_PURGEABLE
                  + $FILE_BACKED_PAGES
                | bc))
        USED_MEM=$(($PAGES_WIRED
                    + $PAGES_ACTIVE
                    + $PAGES_INACTIVE
                    + $PAGES_SPECULATIVE
                    + $PAGES_OCCUPIED
                    - $CACHED
                | bc ))
        FREE_MEM=$(echo "$VM_STAT" | awk '/Pages free/ {print $NF}' | tr -d '.')
        TOTAL_MEM=$(($USED_MEM + $CACHED + $FREE_MEM | bc))

        USED_MEM_PERCENT=$(echo `echo "scale=4; $USED_MEM / $TOTAL_MEM * 100" | bc | sed -e 's/^\(....\).*/\1/'`)
    fi
    echo "Mem:${USED_MEM_PERCENT}%"
    return 0
}

RET=0
calculate_used_mem
RET=$?

exit $RET
