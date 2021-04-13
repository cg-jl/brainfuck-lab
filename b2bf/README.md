# bytes to brainfuck.

Here I experiment with the most compact way to create a program which spits bytes.


## Phases

I'm going to test this with the `/bin/ls` binary and the compiler binary, and see what it spits.
The `/bin/ls` binary weighs around 139 KB, for reference.
The compiler binary will differ on each phase so will be specified there.

To compile all of the sources, I'm using this:

```bash
make bin
```

You can also just get the last version with
```bash
make last
```

You can refer to the [makefile](./makefile) for more details.

The way the test works is specified in [test-phase.sh](./test-phase.sh).
The test not only displays the code size results but also makes sure
that the binaries outputted by each code are the same, so that the optimization
is real.

### Phase 1. Direct approach.

The direct approach is clearing out the cell and putting '+'s to get the next one.

binary: 17KB

- `self.b`: 441 KB (around 26 characters per byte)
- `ls.b`:   12 MB  (around 86 characters per byte)

### Phase 2. Using last value and compute differences.

Now we take the last value as well and instead of clearing out the cell we just put the difference in '+'s.

binary: 17KB

- `self.b`: 600 KB (around 35 characters per byte)
- `ls.b`  : 13  MB (around 94 characters per byte)

As you can see, that didn't do really well. Let's continue exploring.

### Phase 3. Using bidirectional computation.

To the date I've only used `+`s to get the job done. Now I'm going to pick the direction (`+` or `-`) depending on the minimum difference that I choose,
that is, `next - last` (using `+`) or `last - next` (using `-`).

binary: 17KB
- `self.b` 200 KB (around 12 characters per byte)
- `ls.b`   5.9 MB (around 42 characters per byte)

Very good! As you can see, using bidirectional computation reduced more than half the code necessary to run it.

### Phase 4. Loops and multiplication.

Brainfuck can do loops. Very simple loops, but it can do them. So what I have in mind is finding two numbers, which multiplied, give the difference.

Then, use the max number to set a count and the min number to do the directional work.

binary: 17KB

- `self.b` 422 KB (around 24 characters per byte)
- `ls.b`   4.4 MB (around 31 characters per byte)

Turns out it didn't do very well. It got much better on `ls` but much worse on itself. After taking a brief look of the output of `self.b`, I can see that
there are multiplications by 1 (`[<+>-]`), and by 0 (`[<>-]`). That means there's both prime numbers and duplicates.

Let's take them one at a time.

### Phase 5. Removing duplicates.

Let's go with duplicates. Now, if the next value is the same as the last one, it will output nothing.

binary: 17 KB

- `self.b` 159 KB (around 9 characters per byte)
- `ls.b`   3.7 MB (around 26 characters per byte)

It did much better. Now, for the primes.

### Phase 6. Handling primes I.

The first way I'm going to handle primes is that no multiplication is going to go and instead we're going to use the same method as in Phase 2.
This way we save up exactly 6 characters on each prime.

binary: 17 KB
- `self.b` 139 KB (around 8 characters per byte)
- `ls.b`   3.3 MB (around 24 characters per byte)

I was expecting much less of a decrease, but this means there are a lot of primes going on in these binaries. So I'm going to use now a different approach.

### Phase 7. Handling primes II.

This time I'm redesigning the multiplication algorithm. It will continue finding the divisors, but if it gets a prime it will decrease it by 1 until it's not prime and
therefore won't give any problems. Then that decrease is added at the end to compensate.

binary: 17 KB
- `self.b` 139 KB (around 8 characters per byte)
- `ls.b`   2.9 MB (around 21 characters per byte)

Looks like the `ls` binary has been reduced by a lot by using this feature! Great!

### Phase 8. Don't handle primes and half the diff.

The idea is halving the diff and storing the odd bit. Then do the loop for the diff but each loop increments 2 times.

binary: 17KB
- `self.b` 132 KB (around 8 characters per byte)
- `ls.b`   2.9 MB (around 21 characters per byte)

Looks like this strategy works the same and requires less work for the producer.

### Phase 9. Back to handling primes I.

We keep halving the diff but remove the loop if the min div is 1.

binary: 17 KB

- `self.b` 137 KB (around 8 characters per byte)
- `ls.b`   3.3 MB (around 24 characters per byte)

That strategy kept the same numbers overall.

### Phase 10. Various shortcuts.

A lot of the diffs are big because they go down to zero or a small number from a big one or go to a high number from a small one.
Let's fix that. Now if next one is zero only `[-]` will be printed to clear the cell.
When next is close to `0` (`<= threshold`) we'll clear the cell and put all the `+`s as they won't occupy much.
When next is close to `255` we'll clear the cell and put `-`s so it's the fastest path. Also, if the diff is small, don't bother to factor it.

binary: 17 KB
- `self.b` 110 KB (around 6 characters per byte)
- `ls.b`   2.6 MB (around 19 characters per byte)

That... did REALLY good.

Right now the threshold is at 12, so I'm going to explore bigger thresholds
in case there is a noticeable improvement.

It didn't do very well. I'll keep it at 12.


### Phase 11. Better divisors.

Right now the converter just picks the biggest divisor it finds. It would be wiser to
choose the pair of numbers which have the lesser difference between them. That's the change.

binary: 17 KB
- `self.b` 107 KB (around 6 characters per byte)
- `ls.b`   2.5 MB (around 18 characters per byte)

That's very nice.

## Summary

We've achieved quite a bit of improvement just by doing basic math! In case of the `ls` binary,
we got from ~100 characters per byte printed down to 18! That's a huge thing!

