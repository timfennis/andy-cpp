# While loop

Like in every other language a `while` loop will run as long as a condition is true.

```
let n = 1;

while n <= 100 {
  if n % 15 == 0 {
    print("fizzbuzz");
  } else if n % 3 == 0 {
    print("fizz");
  } else if n % 5 == 0 {
    print("buzz");
  } else {
    print(n);
  }

  n += 1;
}
```
