BASE_URL=http://mail-archives.apache.org/mod_mbox
PROJECT=apr-dev
FROM=2019
TO=2019

for year in `seq ${FROM} ${TO}`; do
  for month in `seq -w 1 12`; do
     curl -s -I ${BASE_URL}/${PROJECT}/${year}${month}.mbox | \
          grep "HTTP/1.1 404 Not Found" > /dev/null || \
     curl ${BASE_URL}/${PROJECT}/${year}${month}.mbox \
          -o ${PROJECT}_${year}_${month}.mbox
  done
done

cat *.mbox > ${PROJECT}.mbox
rm ${PROJECT}_*.mbox
