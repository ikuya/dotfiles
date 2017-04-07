#!/bin/sh

calculate_used_mem() {
    if ! type vm_stat > /dev/null 2>&1; then
        # free
        FREE=`free`
        MEM_USED=$(echo "$FREE" | awk 'NR==3 {print $3}')
        MEM_FREE=$(echo "$FREE" | awk 'NR==3 {print $4}')
        MEM_TOTAL=$(echo `echo "$MEM_USED + $MEM_FREE" | bc`)
        USED_MEM_PERCENT_BY_FREE_COMMAND=$(echo `echo "scale=4; $MEM_USED / $MEM_TOTAL * 100" | bc | sed -e 's/^\(....\).*/\1/'`)
        echo "Mem:${USED_MEM_PERCENT_BY_FREE_COMMAND}%"
    else
    	# vm_stat
        VM_STAT=`vm_stat`
        PAGES_FREE=$(echo "$VM_STAT" | awk '/Pages free/ {print $NF}' | tr -d '.')
        PAGES_ACTIVE=$(echo "$VM_STAT" | awk '/Pages active/ {print $NF}' | tr -d '.')
        PAGES_INACTIVE=$(echo "$VM_STAT" | awk '/Pages inactive/ {print $NF}' | tr -d '.')
        PAGES_SPECULATIVE=$(echo "$VM_STAT" | awk '/Pages speculative/ {print $NF}' | tr -d '.')
        PAGES_WIRED=$(echo "$VM_STAT" | awk '/Pages wired down/ {print $NF}' | tr -d '.')
        # 空きメモリ
        FREE_MEM=$(($PAGES_FREE + $PAGES_SPECULATIVE | bc))
        # 使用中メモリ
        USED_MEM=$(($PAGES_ACTIVE + $PAGES_INACTIVE + $PAGES_WIRED | bc))
        # 合計
        TOTAL_MEM=$(($FREE_MEM + $USED_MEM |bc))
        # 使用中メモリ(%)
        # 小数点第1位まで求めて後から小数点文字(ドット)を挿入
        USED_MEM_PERCENT=$(echo "$(($USED_MEM * 1000 / $TOTAL_MEM |bc))" | sed -e 's/\(.*\)\([0-9]\)/\1.\2/' -e 's/^\./0./')
        echo "Mem:${USED_MEM_PERCENT}%"
    fi
    return 0
}

RET=0
calculate_used_mem
RET=$?

exit $RET
