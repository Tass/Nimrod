# Test high level features

import strutils

echo "Give a list of numbers (separated by spaces): "
stdin.readLine.splitSeq.each(parseInt).max.`$`.echo(" is the maximum!")
