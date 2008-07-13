#!/usr/bin/env python

import sys, wrldobjects, simplejson

if __name__ == "__main__":
    # mapmaker, mapmaker, find me a map
    file(sys.argv[1], "w").write(simplejson.dumps(wrldobjects.get_map()))
