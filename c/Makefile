TARGETS=bench validate

all:$(TARGETS)

bench: bench.c adapt.h
	gcc $< -o $@ -O3 -ffast-math -lm -march=native

validate: validate.c adapt.h
	gcc $< -o $@ -O3 -ffast-math -lm -march=native


clean:
	rm -f $(TARGETS) *.o
