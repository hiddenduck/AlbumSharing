#!/usr/bin/env python

from node import *
import node

vv = {}
delivered = []
received = []
changed = set()
#lacks both optimizations

def broadcast(body={}, /, **kwds):
    for i in node_ids():
        if i != node_id():
            send(i, body, **kwds)

@handler
def init(msg):
    global changed
    node.init(msg)
    changed = {node_id()}
    for i in node_ids():
        vv[i] = 0

@handler
def cbcast(msg):
    global delivered, changed
    vv[node_id()] += 1
    reply(msg, type='cbcast_ok', messages=delivered)
    delivered = []
    broadcast(type='fwd_msg', vv={key:vv[key] for key in changed}, message=msg.body.message)
    changed = {node_id()}

def test_msg(src_msg, vv_msg, message):
    flag = False
    if  vv_msg[src_msg] == vv[src_msg] + 1 and \
        all([vv_msg[id] <= vv[id] for id in vv_msg if id != src_msg]):
        for dep in vv_msg:
            if dep != node_id() and vv[dep] == vv_msg[dep]:
                changed.discard(dep)
        changed.add(src_msg)
        vv[src_msg] += 1
        delivered.append(message)
        flag = True
    return flag

@handler
def fwd_msg(msg):
    if test_msg(msg.src, vars(msg.body.vv), msg.body.message):
        for msg_r in received:
            if test_msg(msg_r.src, vars(msg_r.body.vv), msg_r.body.message):
                received.remove(msg_r)
    else:
        received.append(msg)

if __name__ == "__main__":
    receive()
