# advent-of-code-2020

## Day One -- 1A
Given a list of integers, find the two items that sum to 2020.
### Thoughts;
- Naive approach: Naively going through the items one-by-one will give you n^n-1 solution space
- Slightly cleverer: The maximum amount of naive we should ever need is 1010. We could built a list of pairs `(n, 2020-n)` where `n = 0...1010`. Then iterate over that list and find the items. By sorting the list and using binary search, we can get an average lookup of O(log n) per item.
- Possibly interesting: If we search the list, then recursively take the tail and head and compare them, we should be able to find a combination that works;

  Given: 
  ```
  find: x + y === 40
  [
    20,  3, 26,  7, 22, 37, 34, 38, 20, 31,
    35,  0, 11, 33,  4, 26, 23, 28,  1,  4,
    27, 26, 33,  5, 11, 27,  8, 32,  6, 26,
     9, 35, 39, 14, 12, 38,  9,  5, 37, 28
  ]
  sort it:
  [
     0,  1,  3,  4,  4,  5,  5,  6,  7,  8,
     9,  9, 11, 11, 12, 14, 20, 20, 22, 23,
    26, 26, 26, 26, 27, 27, 28, 28, 31, 32,
    33, 33, 34, 35, 35, 37, 37, 38, 38, 39
  ]
  ```

Then we'll make a function that takes the head and tail, compares it;
-> We need to keep the last head and tail of the list
-> [head, xs, tail], (lastHead, lastTail)
-> if is under, we'll recursively call the function with the rest
-> if it's perfect, we'll return the comparison
-> if it is over, we'll have to recurse left and right a bit (probably not far) to get the items

### Note
I might be overthinking this...
We could also just go through the list and keep a list of the values we've seen subtracted from 2020. Then simply check when we encounter one of the ones we've already seen... This works...


## Day One -- 1B
So now we need to go deeper. My initial idea would be to simple deepen the recursion. Given our current function, if we multiply and the multiplication is less than 2020, instead of going to the next item, we should search the rest of the items to see if we can find something to make it to 2020 total. So not really deepening the recursion. Just add another function.

There are 200 items.

While this could be solved with some nested for loops. This might be easier / quicker with a combination of my earlier idea;


  ```
  find: x + y + z === 40
  [
    20,  3, 26,  7, 22, 37, 34, 38, 20, 31,
    35,  0, 11, 33,  4, 26, 23, 28,  1,  4,
    27, 26, 33,  5, 11, 27,  8, 32,  6, 26,
     9, 35, 39, 14, 12, 38,  9,  5, 37, 28
  ]
  sort it:
  [
     0,  1,  3,  4,  4,  5,  5,  6,  7,  8,
     9,  9, 11, 11, 12, 14, 20, 20, 22, 23,
    26, 26, 26, 26, 27, 27, 28, 28, 31, 32,
    33, 33, 34, 35, 35, 37, 37, 38, 38, 39
  ]
  ```

Then we'll make a function that takes the head and tail, compares it;
-> We need to keep the last head and tail of the list
-> [head, xs, tail], (lastHead, lastTail)
-> if is under, we'll recursively call the function with the rest
-> if it's perfect, we'll return the comparison
-> if it is over, we'll have to recurse left and right a bit (probably not far) to get the items

### Note
I might be overthinking this...
We could also just go through the list and keep a list of the values we've seen subtracted from 2020. Then simply check when we encounter one of the ones we've already seen... This works...


## Day One -- 1B
So now we need to go deeper. My initial idea would be to simple deepen the recursion. Given our current function, if we multiply and the multiplication is less than 2020, instead of going to the next item, we should search the rest of the items to see if we can find something to make it to 2020 total. So not really deepening the recursion. Just add another function.

There are 200 items.

While this could be solved with some nested for loops. This might be easier / quicker with a combination of my earlier idea;


