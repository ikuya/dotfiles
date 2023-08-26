## Full-text search
dir=.
file=*
case $# in
    0)
        echo 'usage: search [DIR [FILE]] STRING'
        return 1
    ;;
    1)
        string=$2
    ;;
    2)
        string=$2
        dir=$1
    ;;
    3)
        string=$3
        dir=$1
        file=$2
    ;;
esac
find $dir -name "$file" -exec grep -iIHn --color=always $string {} \; 2>/dev/null;
