#!/usr/bin/env bash

comm -23 <(nu korr.nu query --file classes.pl | sort) <(nu korr.nu query --file annotated.pl| sort)
