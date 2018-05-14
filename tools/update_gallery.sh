# 
#   Regenerate forecast gallery with updated input files from sage research tool
#   Andreas Beger
#   30 April 2018
#
#   bash tools/update_gallery.sh

curl http://0.0.0.0:5000
if [ $? -ne 0 ]; then
    echo 'basil-ts app is not running'
    exit 1
fi

find ../../sage-research-tool/ifp/andyio -type f -name '*input*' \
  -exec cp '{}' tests/io/ ';'

bash tools/format_inputs.sh

python3 tests/run_all.py

Rscript -e 'library(rmarkdown); rmarkdown::render("tests/README.Rmd")'