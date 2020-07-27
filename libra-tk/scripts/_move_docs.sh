#!/bin/sh
# Script to clean-up and move files around after oasis builds the API
# documentation.

rm -rf doc/html && mkdir doc/html && cp -R api.docdir/* doc/html && rm api.docdir
