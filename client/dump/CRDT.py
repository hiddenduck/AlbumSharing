#!/usr/bin/env python

from node import *
import node
from threading import Thread
from time import sleep

## Values

m = {}
vv = {} # or c in the math notation

## ORSet CRDT

def CRDT_add(e):
    m[e] = {(node_id(), vv[node_id()] + 1)}
    vv[node_id()] = vv[node_id()] + 1

def CRDT_remove(e):
    if e in m:
        del m[e]

def CRDT_elements():
    return m.keys()

def CRDT_causalContexUnion(c, cl): # c union c'

    c_result = {}
    for key in (list(c.keys()) + list(cl.keys())):
        if key not in vv:
            c_result[key] = cl[key]
        else:
            c_result[key] = max(c[key], cl[key])

    return c_result

def CRDT_dotSetJoin(s, c, sl, cl):

    # s intersect s'
    s_result = s & sl
    
    # s \ c'
    for (i, e) in s:
        if i not in cl:
            s_result.add((i,e))
    
    # s' \ c
    for (i,e) in sl:
        if i not in c:
            s_result.add((i,e))
    
    return (s_result, CRDT_causalContexUnion(c,cl))

def CRDT_dotMapJoin(m,c,ml,cl):

    s_result = {}
    for k in (list(m.keys()) + list(ml.keys())):
        fst = CRDT_dotSetJoin(m[k], c, ml[k], cl)[0]
        if fst != set():
            s_result[k] = fst

    return (fst, CRDT_causalContexUnion(c, cl))


## Client API

@handler
def read(msg):
    reply(msg, type='read_ok', value=list(CRDT_elements()))

@handler
def add(msg):
    e = msg.body.element # e = element, by the math notation

    CRDT_add(e)

    reply(msg, type='add_ok')

@handler
def remove(msg):
    e = msg.body.element # e = element, by the math notation

    CRDT_remove(e)

    reply(msg, type='remove_ok')

@handler
def join(msg):
    ml = msg.body.m
    cl = msg.body.c

    CRDT_dotMapJoin(m, vv, ml, cl)

def converge(m,vv):

    while True:
        sleep(5)
        for dest_id in node_ids():
            if dest_id != node_id():
                send(dest_id, type="join", m=m, c=vv)

@handler
def init(msg):
    node.init(msg)
    for i in node_ids():
        vv[i] = 0

    Thread(target=converge, args=(m, vv)).start()    


if __name__ == "__main__":
    receive()
 
