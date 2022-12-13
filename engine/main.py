#!/usr/bin/env python
from swiplserver import PrologMQI, PrologThread
import os
import subprocess

def select_file(files):
  rin, win = os.pipe()
  rout, wout = os.pipe()
  pid = os.fork()
  if pid > 0:
    # In the parent
    os.close(rin)
    os.close(wout)
    for file in files:
        os.write(win, bytes(file, "utf-8"))
        os.write(win, b"\n")
    os.close(win)
    res = os.fdopen(rout)
    return res.read().rstrip()
  else:
    # In the child
    os.close(win)
    os.close(rout)
    subprocess.call(["/usr/bin/env", "rofi", "-dmenu"], stdin = rin, stdout = wout)
    os.close(wout)
    os.close(rin)
    exit(0)

def render_ctx(ctx):
  res = "["
  res += ",".join(map(lambda p: p[0] + "(\"" + p[1] + "\")", ctx.items()))
  res += "]"
  return res

def process_req(req, arg):
  if req == "select_file":
    file = select_file(arg)
    print("Selected: ", file)
    return file
  else:
    print("Unknown request: ", name)

with PrologMQI() as mqi:
  with mqi.create_thread() as thread:
    result = thread.query("consult(\"main.pl\").")
    ctx = { "neovim": "/tmp/nvim-luc.sock" }
    while True:
        rctx = render_ctx(ctx)
        print("Running with context: ", rctx)
        result = thread.query("run_action(open_wiki, " + rctx + ", REQ).")
        print(result)
        if isinstance(result, bool):
            if result:                
                print("Success !")
                break
            else:
                print("No results !")
                break
        reqs = result[0]["REQ"]
        if len(reqs) == 0:
            print("Success !")
            break
        for req in reqs:
            name, arg = req.popitem()
            r = process_req(name, arg)
            ctx[name] = r
