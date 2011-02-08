#!/usr/bin/env bash
set -ev

# Demonstrates issue385
darcs changes --repo=http://darcs.net GNUmakefile --last 300
