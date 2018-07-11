.PHONY: all update clean

all: update

update: venv requirements.txt
	venv/bin/python -m pip install --upgrade -r requirements.txt

venv:
	python -m venv venv

clean:
	rm -rf venv __pycache__
