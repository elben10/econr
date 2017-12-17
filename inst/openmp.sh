#!/usr/bin/env bash
# Download binary
curl -O http://r.research.att.com/libs/clang-4.0.0-darwin15.6-Release.tar.gz
# Extract binary onto root directory
tar fvxz clang-4.0.0-darwin15.6-Release.tar.gz -C /

# Overwrite the ~/.R/Makevars
cat <<- EOF > ~/.R/Makevars
# The following statements are required to use the clang4 binary
CC=/usr/local/clang4/bin/clang
CXX=/usr/local/clang4/bin/clang++
CXX11=$CXX
CXX14=$CXX
CXX17=$CXX
CXX1X=$CXX
LDFLAGS=-L/usr/local/clang4/lib
# End clang4 inclusion statements
EOF
