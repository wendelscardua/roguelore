PROJECT=roguelore
LD65_FLAGS=
CA65_FLAGS=
NSF2DATA=/mnt/c/NESDev/famitone2d/NSF/nsf2data.exe
TEXT2DATA=/mnt/c/NESDev/famitone2d/text2data.exe
FAMITRACKER=/mnt/c/NESDev/famitracker/FamiTracker.exe
EMULATOR=/mnt/c/NESDev/Mesen.exe

TARGET=${PROJECT}.nes

.PHONY : debug run

default: ${TARGET}

${TARGET}: src/${PROJECT}.o src/reset.o src/readjoy.o src/unrle.o src/rand.o src/audio-data.o
	ld65 $^ -t nes -o ${TARGET} ${LD65_FLAGS}

debug: LD65_FLAGS += -Ln labels.txt --dbgfile ${PROJECT}.nes.dbg
debug: CA65_FLAGS += -g -DDEBUG=1
debug: ${TARGET}

src/${PROJECT}.o: src/${PROJECT}.s src/constants.inc src/mmc3-constants.inc src/header.inc \
	src/famitone2.s \
	assets/maps/*.bin \
	assets/bg-palettes.pal assets/sprite-palettes.pal \
        assets/metasprites.inc \
	assets/nametables/*.rle \
	assets/chr/*.chr
	ca65 src/${PROJECT}.s ${CA65_FLAGS}

src/audio-data.o: src/audio-data.s assets/audio/sfx.s assets/audio/soundtrack.s
	ca65 src/audio-data.s ${CA65_FLAGS}

assets/maps/%.bin: assets/maps/%.s
	ruby tools/bitfy.rb $^ $@

assets/audio/soundtrack.s: assets/audio/soundtrack.txt
	${TEXT2DATA} $^ -ca65 -allin

assets/audio/soundtrack.txt: assets/audio/soundtrack.ftm
	${FAMITRACKER} $^ -export $@

assets/audio/sfx.nsf: assets/audio/sfx.ftm
	${FAMITRACKER} assets/audio/sfx.ftm -export assets/audio/sfx.nsf

assets/audio/sfx.s: assets/audio/sfx.nsf
	${NSF2DATA} assets/audio/sfx.nsf -ca65 -ntsc

%.o: %.s
	ca65 $< ${CA65_FLAGS}

clean:
	rm src/*.o *.nes labels.txt *.dbg

run: debug
	${EMULATOR} ${PROJECT}.nes
