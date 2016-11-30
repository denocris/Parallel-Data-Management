#!/usr/bin/env python

from JC2010_parser import Austria_parser , Schweitz_parser

Austria_dir = '../ori/Austria'
Switzerland_dir = '../ori/Sweitz'

a = Austria_parser(Austria_dir)
a.do_parse( )
a.tonc('schnee')
del(a)
s = Schweitz_parser(Switzerland_dir)
s.do_parse( )
s.tonc('schnee')
del(s)

# vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4
