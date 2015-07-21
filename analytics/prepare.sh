iconv -f utf-16 -t utf-8 \
    | tr '\t' '|' \
    | grep --text -E '(.{1,})(\|.+){4}' - \
    | sed 's/ga://g'
