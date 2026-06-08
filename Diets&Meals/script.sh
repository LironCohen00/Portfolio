#!/bin/bash

if grep -q 219 data; then
    exit 0  # Success (contains 219)
else
    exit 1  # Failure (does not contain 219)
fi