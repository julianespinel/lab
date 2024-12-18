#!/bin/python3

# https://www.hackerrank.com/challenges/mini-max-sum/problem

"""
The algorithm used to solve this problem is the following:
1. Get the minimum element of the array
2. Get the maximum element of the array
3. Get the sum of all the elements in the array
4. Calculate the min sum -> sum(arr) - max_element
5. Calculate the max sum -> sum(arr) - min_element

The complexity analysis of the proposed algorithm is O(1). Why?

Because the input of the problem is fixed, an array of 5 elements.
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
"""

import time
import cProfile, pstats, io
from pstats import SortKey


def solve_3n(arr):
    min_element = min(arr)
    max_element = max(arr)
    sum_arr = sum(arr)
    return (sum_arr - max_element, sum_arr - min_element)


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


def print_list_of_tuples(tuples):
    for x, y in tuples:
        print(f'{x}, {y}')


def execute(repetitions, fn, args):
    times = []
    for rep in repetitions:
        it = time.perf_counter_ns()
        fn(args)
        ft = time.perf_counter_ns()
        times.append(ft - it)
    return times


def profile(repetitions, fn, arr):
    pr = cProfile.Profile()
    pr.enable()

    execute(repetitions, fn, arr)

    pr.disable()
    stream = io.StringIO()
    sortby = SortKey.CUMULATIVE
    ps = pstats.Stats(pr, stream=stream).sort_stats(sortby)
    ps.print_stats()
    print(stream.getvalue())


if __name__ == '__main__':
    # ---------------------------------
    # Using timeit

    # import timeit
    # arr = [1, 2, 3, 4, 5]
    # times_3n = timeit.timeit("solve_3n(arr)",
    #                       setup="from __main__ import solve_3n; arr = [1, 2, 3, 4, 5]",
    #                       number=10_000)
    # times_n = timeit.timeit("solve_n(arr)",
    #                       setup="from __main__ import solve_n; arr = [1, 2, 3, 4, 5]",
    #                       number=10_000)
    # print(f"times_3n: ${times_3n}")
    # print(f"times_n: ${times_n}")
    # ---------------------------------

    # ---------------------------------
    # Using time + matplotlib

    # import matplotlib.pyplot as plt
    #
    # initial = 1_000
    # reps = 10_000
    # arr = range(initial, initial + reps)
    # repetitions = range(reps)
    #
    # times_3n = execute(repetitions, solve_3n, arr)
    # print('times_3n')
    # print_list_of_tuples(zip(repetitions, times_3n))
    #
    # times_n = execute(repetitions, solve_n, arr)
    # print('n_times')
    # print_list_of_tuples(zip(repetitions, times_n))
    #
    # plt.plot(repetitions, times_3n, label='3n')
    # plt.plot(repetitions, times_n, label='n')
    # plt.xlabel('execution')
    # plt.ylabel('times (ns)')
    # plt.title("n vs 3n")
    # plt.legend()
    # plt.show()
    # ---------------------------------


    # Using cProfile

    initial = 1_000
    reps = 10_000
    arr = range(initial, initial + reps)
    repetitions = range(reps)

    profile(repetitions, solve_3n, arr)
    profile(repetitions, solve_n, arr)
