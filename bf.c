#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>


/**
 * Raw brainfuck interpreter. Executes exactly what it's given (i.e, no optimizations).
 * supports up to 200 nested loops to the date.
 * Uses going back and forth into the file to implement the jumping
 * in loops.
 */


long loops[200];

void die(const char* msg) {
  perror(msg);
  exit(-1);
}


int main(int argc, const char **argv) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <file>\n", *argv);
    return 1;
  }
  FILE* fp = fopen(argv[1], "rb");
  if (fp == NULL) {
    die("fopen");
  }
  uint8_t buffer[30000] = { 0 };
  size_t index = 0;

  bool skipping = false;

  int skipping_depth = 0;

  long* loops_ptr = (long*) loops;

  for (uint8_t c; fread(&c, 1, 1, fp) == 1; ) {
    if (!isprint(c)) continue;
    // printf("pos: %ld | c: '%c'\n", ftell(fp), c);
    if (skipping) {
      if (c == '[') skipping_depth++;
      if (c == ']') {
        if (skipping_depth == 0) skipping = false;
        else skipping_depth--;
      }
      continue;
    }
    switch (c) {
      case '>': index++; break;
      case '<': index--; break;
      case '+': buffer[index]++; break;
      case '-': buffer[index]--; break;
      case ',': buffer[index] = getchar(); break;
      case '.': putchar(buffer[index]); break;
      case '[': {
                  if (buffer[index] == 0) skipping = true;
                  else {
                    *loops_ptr = ftell(fp);
                    loops_ptr += 1;
                  }
                }
                break;
      case ']': {
                  if (loops_ptr == loops) {
                    fprintf(stderr, "Unmatched '['. Shutting down..\n");
                    goto shutdown;
                  }
                  loops_ptr -= 1;
                  if (buffer[index] != 0) {
                    // continue the loop
                    fseek(fp, *loops_ptr, SEEK_SET);
                    // push back the value.
                    loops_ptr += 1;
                  }
                }
                break;
      default: break;
    }
  }

shutdown:
  fclose(fp);
  return 0;
}
