#!/usr/bin/env python
from swiplserver import PrologMQI, PrologThread

with PrologMQI() as mqi:
  with mqi.create_thread() as thread:
    result = thread.query("consult(\"main.pl\").")
    result = thread.query("wiki_file(P,UUID).")
    print(result)
