dir=$1
rm -f *.aux *.log *.pdf
for d in $(ls dir)
  do
    rm -f d/*.aux d/*.log d/*.pdf
  done
