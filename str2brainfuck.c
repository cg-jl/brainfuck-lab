#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <getopt.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

// the code-size optimized version
// run on 2-byte memory interpreters.
//
// the interpreter-optimized version
// runs on 1-byte memory interpreters.
// use this one just if the interpreter
// you're working with groups sequences
// of '+' and '-' into a single value,
// otherwise don't.

/// find a divisor where its difference
/// to the quotient is the lesser.
/// basically find such d that d - (x / d) is the 
/// minimum value.
uint8_t best_pair(uint8_t val) {
  uint8_t best_diff = 255; // will never be this one.
  uint8_t best_div = 1 ; // in case it's prime or smth.
  // realisticly, the highest two number that multiplied
  // don't overflow are 15 and 15 (15 * 15 == 255).
  // And dividing by one gets the highest result always,
  // so there's no need to waste resources.
  for (uint8_t div = 2; div < 15; div++) {
    if (val % div != 0) continue; // not divisible -> out.
    uint8_t counterpart = val / div;
    uint8_t diff = div > counterpart ? div - counterpart : counterpart - div;
    if (diff < best_diff) {
      best_diff = diff;
      best_div = div;
    }
  }
  return best_div;
}

/// Finds a suitable divisor for the number, in the direction given ('-' or 
/// '+'). If the number is prime or coprime to [2, 15), then the 
/// next number is calculated from the direction and the difference to the
/// first value is returned. This way ensures a loop over a lot of pluses.
/// The pluses are easily optimsable, but they augment the code size.
/// TODO: make this feature toggleable with a flag.
uint8_t find_suitable_divisor(uint8_t v, uint8_t *diff, uint8_t direction) {
  uint8_t best_div = best_pair(v);
  *diff = 0;
  while (best_div == 1) {
    if (direction == '-') v--;
    else v++;
    (*diff)++;
    best_div = best_pair(v);
  }
  return best_div;
}

// gives the minimum amount of instructions to get to the
// next value using the last value.
void byte_min(uint8_t next, uint8_t last) {
  if (next == last) return;

  if (next <= 10) {
    // we can afford to put the number directly without
    // clobbering the space.
    fputs("[-]", stdout); // clear cell.
    // and make the symbolic change here.
    last = 0;
  }

  uint8_t direction = next > last ? '+' : '-';
  uint8_t diff = next > last ? next - last : last - next;

  if (diff <= 10) {
    // put the diff directly.
    for (size_t i = 0; i < diff; i++) putchar(direction);
    return;
  }

  uint8_t final_diff;
  uint8_t min;
  uint8_t max;
  {
    uint8_t div = find_suitable_divisor(diff, &final_diff, direction);
    uint8_t other_div = (diff - final_diff) / div;
    min = other_div > div ? div : other_div;
    max = other_div > div ? other_div : div;
  }


  // set loop initial value.
  putchar('>');
  for (uint8_t i = 0; i < max; i++) putchar('+');

  // make compute loop.
  fputs("[<", stdout);
  for (uint8_t i = 0; i < min; i++) putchar(direction);
  fputs(">-]<", stdout);

  // rest of diff. this is why you need the direction reversed.
  // if you are doing sums, you want the diff number to be computed
  // in the other direction. that way it makes it more readable
  // to others.
  for (uint8_t i = 0; i < final_diff; i++) putchar(direction);

}

// assuming the interpreter groups '+' and '-' for you, this
// is the best option. it also runs on 1-byte interpreters, which
// are very efficient in memory.
// as you can see this is pretty simple.
void byte_normal(uint8_t next, uint8_t last) {
  uint8_t diff = next > last ? next - last : last - next;
  uint8_t direction = next > last ? '+' : '-';

  for (uint8_t i = 0; i < diff; i++) putchar(direction);
}


uint8_t print_seq(void (*p)(uint8_t, uint8_t), const uint8_t *buf, uint8_t last) {
  for (; *buf; ++buf) {
    p(*buf, last);
    last = *buf;
  }
  return last;
}

// gets UTF8 length for an initial sequence.
size_t get_length(uint8_t value) {
  if (value & 0x80 == 0) return 1;
  // 2-byte
  if (value & 0xe0 == 0xc0) return 2;
  // 3-byte
  if (value & 0xf0 == 0xe0) return 3;
  // 4-byte
  if (value & 0xf8 == 0xf0) return 4;
  return 0;
}

int unescape(const uint8_t *input, uint8_t *output, const uint8_t **next) {
  if (!*input) return 0;
  if (*input == '\\') {
    input++;
    uint8_t value = 0;
    switch (*input) {
      case 'n': value = '\n'; break;
      case 't': value = '\t'; break;
      case '\\': value = '\\'; break;
      case 'e': value = 0x1b; break;
      default:
                // use 
                strcpy(output, "unknown escape: ");
                size_t l = get_length(*input);
                memcpy(output + 16, input, l);
                output[16 + l] = 0;
                return -1;
    }
    *output = value;
  }
  else {
    *output = *input;
  }
  output[1] = 0;
  *next = input + 1;
  return 1;
}

int main(int argc, uint8_t *const* argv) {
  int optimizes_for_size = 1;
  const struct option options[] = {
    { .name = "size", .has_arg = 0, .flag = &optimizes_for_size, .val = 1 },
    { .name = "interpreter", .has_arg = 0 , .flag = &optimizes_for_size, .val = 0 },
    { 0 }
  };



  {
    int opt;
    int longindex = 0;
    while ((opt = getopt_long(argc, argv, "+si", options, &longindex)) != -1) {
      switch (opt) {
        case 's':
          optimizes_for_size = 1;
          break;
        case 'i':
          optimizes_for_size = 0;
          break;

        case '?': 
          fprintf(stderr, "Usage: %s [OPTIONS] STRING ...\n", *argv);
          fputs("where OPTIONS: [-s] [-i]\n\
              -s/--size -> optimize for code size (default)\n\
              -i/--interpreter -> optimize for interpretation\n", stderr);
          return 1;
      }
    }
  }

  void (*p)(uint8_t, uint8_t) = optimizes_for_size ? byte_min : byte_normal;

  uint8_t output[20];
  int result = 0;
  uint8_t last = 0;
  int i = optind;
  for (const char* arg = argv[i]; i < argc ; arg = argv[++i]) {
    while (*arg && (result = unescape(arg, output, &arg)) != 0) {
      if (i != argc - 1) {
        p(' ', last);
        last = ' ';
      }
      if (result < 0) {
        fputs(output, stderr);
        fputc('\n', stderr);
        return 1;
      }
      p(*output, last);
      putchar('.');
      last = *output;
    }
  }
}

