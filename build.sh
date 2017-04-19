node build.js
EXITCODE=$?
if [ $EXITCODE -eq 123 ]; then
  echo 'ok'
  cp parser.old.old.js parser.old.old.old.js
  cp parser.old.js parser.old.old.js
  cp parser.js parser.old.js
  mv parser.new.js parser.js
  #git status
  #git commit -a -m 'automatic commit'
  #git push &
else
  echo 'fail'
fi

