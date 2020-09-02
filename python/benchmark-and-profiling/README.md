# Mini-Max Sum

Problem statement: https://www.hackerrank.com/challenges/mini-max-sum/problem

## Algorithm

The algorithm used to solve this problem is the following:

1. Get the minimum element of the array
1. Get the maximum element of the array
1. Get the sum of all the elements in the array
1. Calculate the min sum -> sum(arr) - max_element
1. Calculate the max sum -> sum(arr) - min_element

## Complexity analysis

The complexity analysis of the proposed algorithm is O(1). Why?

Because the input of the problem is fixed (an array of 5 elements).
According to the book Introduction to Algorithms:

> When we look at input sizes large enough to make only the order of growth of
> the running time relevant, we are studying the asymptotic efficiency of
> algorithms. That is, we are concerned with how the running time of an algorithm
> increases with the size of the input in the limit, as the size of the input
> increases without bound. Usually, an algorithm that is asymptotically more
> efficient will be the best choice for all but very small inputs.

"3 Growth of Functions." Introduction to Algorithms, by Thomas H. Cormen,
MIT Press, 2009.

In short, asymptotic analysis of a problem with a fixed input can be
simplified as O(1).

For a longer explanation please see: https://stackoverflow.com/a/2027842/2420718

## Performance `3n < n`?

**What?!** Where does `3n < n` comes from? Let me explain.

I implemented the algorithm described above in two different ways:

### One

```python
def solve_3n(arr):
    min_element = min(arr) # O(arr)
    max_element = max(arr) # O(arr)
    sum_arr = sum(arr)     # O(arr)
    return (sum_arr - max_element, sum_arr - min_element) # O(1)
```

The name of the function is `solve_3n(arr)` because if we assume that the array
given as input is not fixed, an its length is `n`, then the complexity of this
implementation would be: `O(3n)`. In complexity analysis the constants are
dropped, therefore the runtime complexity would be simplified to `O(n)`.

However, with this implementation we have to perform `3n` steps to complete the
execution.

### Two

```python
def solve_n(arr):
    min_element = arr[0]
    max_element = arr[0]
    sum_arr = 0
    for element in arr:
        sum_arr += element
        if element < min_element:
            min_element = element
        if element > max_element:
            max_element = element
    return (sum_arr - max_element, sum_arr - min_element)
```

The name of this function is `solve_n` because in this implementation we
traverse the array only once, therefore the amount of steps we need to execute
is `n`.

Let's review what we have until this point.















Profile 1

         70002 function calls in 10.300 seconds

   Ordered by: cumulative time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.027    0.027   10.300   10.300 mini_max_sum.py:62(execute)
    10000    0.047    0.000   10.263    0.001 mini_max_sum.py:39(solve_3n)
    10000    3.829    0.000    3.829    0.000 {built-in method builtins.max}
    10000    3.807    0.000    3.807    0.000 {built-in method builtins.min}
    10000    2.581    0.000    2.581    0.000 {built-in method builtins.sum}
    20000    0.008    0.000    0.008    0.000 {built-in method time.perf_counter_ns}
    10000    0.002    0.000    0.002    0.000 {method 'append' of 'list' objects}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}



         40002 function calls in 10.893 seconds

   Ordered by: cumulative time

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.019    0.019   10.893   10.893 mini_max_sum.py:62(execute)
    10000   10.869    0.001   10.869    0.001 mini_max_sum.py:46(solve_n)
    20000    0.004    0.000    0.004    0.000 {built-in method time.perf_counter_ns}
    10000    0.002    0.000    0.002    0.000 {method 'append' of 'list' objects}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
