DEVICE=GW2A-LV18PG256C8/I7
BOARD=primer20k
FPGABOARD=tangprimer20k
FAMILY=GW2A-18

push : build/pack.fs
	openFPGALoader -b $(FPGABOARD) build/pack.fs

build :
	mkdir build

build/blinky.json : blinky.v build
	yosys -D LEDS_NR=8 -p "read_verilog blinky.v; synth_gowin -json build/blinky.json"

build/pnrblinky.json : build/blinky.json $(BOARD).cst build
	nextpnr-himbaechel --json build/blinky.json --write build/pnrblinky.json --device $(DEVICE) --vopt cst=$(BOARD).cst --vopt family=$(FAMILY)

build/pack.fs : build/pnrblinky.json build
	gowin_pack -d $(DEVICE) -o build/pack.fs build/pnrblinky.json


.PHONY: push
