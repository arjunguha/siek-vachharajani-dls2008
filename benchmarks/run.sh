#!/bin/bash
for F in *.gtl; do
  echo ""
  echo "*** $F ***"
  gtlc $F
done