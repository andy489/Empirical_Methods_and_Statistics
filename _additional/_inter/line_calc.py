#!/usr/bin/env python3
"""
Slot Machine Line Calculator
Calculates the number of possible lines in a slot machine with N reels and M rows
Movement rule: from any cell, you can move to same row, upper row, or lower row in next reel
"""

import argparse
import sys
import time


def build_transition_matrix(rows):
    """
    Build transition matrix for movement rule: same row, upper row, or lower row
    """
    M = [[0] * rows for _ in range(rows)]

    for i in range(rows):
        # Same row
        M[i][i] = 1
        # Upper row (if exists)
        if i - 1 >= 0:
            M[i][i - 1] = 1
        # Lower row (if exists)
        if i + 1 < rows:
            M[i][i + 1] = 1

    return M


def matrix_multiply(A, B):
    """Multiply two matrices"""
    rows = len(A)
    cols = len(B[0])
    result = [[0] * cols for _ in range(rows)]

    for i in range(rows):
        for j in range(cols):
            for k in range(len(B)):
                result[i][j] += A[i][k] * B[k][j]

    return result


def matrix_power(M, power):
    """Raise matrix to power using exponentiation by squaring - O(m^3 log n) complexity"""
    if power == 0:
        # Return identity matrix
        n = len(M)
        return [[1 if i == j else 0 for j in range(n)] for i in range(n)]

    # Start with identity matrix
    n = len(M)
    result = [[1 if i == j else 0 for j in range(n)] for i in range(n)]
    base = M

    while power > 0:
        if power % 2 == 1:
            result = matrix_multiply(result, base)
        base = matrix_multiply(base, base)
        power //= 2

    return result


def count_slot_lines_matrix_power(reels, rows):
    """
    Calculate number of lines using matrix exponentiation
    Time Complexity: O(rows^3 * log(reels))
    """
    if reels < 1 or rows < 1:
        return 0
    if reels == 1:
        return rows

    M = build_transition_matrix(rows)
    power = reels - 1

    # Compute M^(reels-1)
    M_power = matrix_power(M, power)

    # Initial vector [1, 1, ..., 1]
    initial = [1] * rows

    # Multiply initial vector by result matrix
    final = [0] * rows
    for j in range(rows):
        for i in range(rows):
            final[j] += initial[i] * M_power[i][j]

    return sum(final)


def count_slot_lines_iterative(reels, rows):
    """
    Calculate number of lines using iterative approach
    Time Complexity: O(reels * rows^2)
    """
    if reels < 1 or rows < 1:
        return 0

    # Build transition matrix
    M = build_transition_matrix(rows)

    # Initial vector: 1 way to be in each row at reel 1
    v = [1] * rows

    # Multiply by M^(reels-1) iteratively
    for step in range(1, reels):
        v_new = [0] * rows
        for i in range(rows):  # current row
            for j in range(rows):  # next row
                if M[i][j] == 1:
                    v_new[j] += v[i]
        v = v_new

    return sum(v)


def print_transition_matrix(rows):
    """Display the transition matrix for visualization"""
    M = build_transition_matrix(rows)
    print(f"Transition matrix for {rows} rows:")
    for i, row in enumerate(M):
        print(f"Row {i + 1}: {row}")
    print()


def run_calculation(reels, rows, method='auto', verbose=False):
    """
    Run the calculation with specified parameters
    """
    if verbose:
        print(f"Calculating for {reels} reels and {rows} rows...")
        print_transition_matrix(rows)

    if method == 'auto':
        # Use matrix power for large reels, iterative for small
        if reels > 100:
            method = 'matrix'
        else:
            method = 'iterative'

    start_time = time.time()

    if method == 'matrix':
        result = count_slot_lines_matrix_power(reels, rows)
        method_name = "Matrix Exponentiation"
    else:
        result = count_slot_lines_iterative(reels, rows)
        method_name = "Iterative"

    calculation_time = time.time() - start_time

    if verbose:
        print(f"Method: {method_name}")
        print(f"Time: {calculation_time:.6f} seconds")

    return result, method_name, calculation_time


def run_interactive_mode():
    """Run interactive mode"""
    print("Interactive Mode - Calculate your own:")
    print("-" * 40)

    try:
        while True:
            try:
                reels = int(input("Enter number of reels (0 to exit): "))
                if reels == 0:
                    break
                rows = int(input("Enter number of rows: "))

                if reels < 1 or rows < 1:
                    print("Please enter positive numbers only.\n")
                    continue

                verbose = input("Verbose output? (y/n): ").lower().startswith('y')

                result, method, time_taken = run_calculation(reels, rows, 'auto', verbose)
                print(f"→ {reels} reels × {rows} rows = {result} possible lines")
                if verbose:
                    print(f"  Method: {method}, Time: {time_taken:.6f}s")
                print()

            except ValueError:
                print("Please enter valid integers.\n")
            except KeyboardInterrupt:
                print("\nExiting...")
                break

    except EOFError:
        print("\nExiting...")


def run_test_cases():
    """Run predefined test cases"""
    test_cases = [
        (5, 3, 99),  # 5 reels, 3 rows -> 99 lines
        (5, 4, 178),  # 5 reels, 4 rows -> 178 lines
        (3, 3, 17),  # 3 reels, 3 rows -> 17 lines
        (2, 3, 7),  # 2 reels, 3 rows -> 7 lines
        (1, 3, 3),  # 1 reel, 3 rows -> 3 lines
        (10, 3, 8119),  # 10 reels, 3 rows -> 8119 lines
    ]

    print("Running test cases:")
    print("-" * 40)

    all_passed = True
    for reels, rows, expected in test_cases:
        result, method, _ = run_calculation(reels, rows, 'auto', False)
        status = "✓ PASS" if result == expected else "✗ FAIL"
        if result != expected:
            all_passed = False
        print(f"{status} {reels:2d} reels, {rows:2d} rows: {result:6d} (expected: {expected:6d})")

    print(f"\nOverall: {'All tests passed!' if all_passed else 'Some tests failed!'}")


def main():
    """Main function with command line arguments"""
    parser = argparse.ArgumentParser(
        description='Calculate possible lines in a slot machine with N reels and M rows',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=f'''
Examples:
  {sys.argv[0]} -r 5 -m 3           # 5 reels, 3 rows
  {sys.argv[0]} -r 10 -m 4 -v       # 10 reels, 4 rows (verbose)
  {sys.argv[0]} -r 1000 -m 3 --method matrix  # Force matrix method
  {sys.argv[0]} --test              # Run test cases
  {sys.argv[0]} --interactive       # Interactive mode
        '''
    )

    parser.add_argument('-r', '--reels', type=int, help='Number of reels')
    parser.add_argument('-m', '--rows', type=int, help='Number of rows')
    parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output')
    parser.add_argument('--method', choices=['auto', 'matrix', 'iterative'],
                        default='auto', help='Calculation method (default: auto)')
    parser.add_argument('--test', action='store_true', help='Run test cases')
    parser.add_argument('--interactive', action='store_true', help='Interactive mode')

    args = parser.parse_args()

    print("Slot Machine Line Calculator")
    print("=" * 50)
    print("Movement rule: From any cell, you can move to:")
    print("  - Same row")
    print("  - Upper row (if exists)")
    print("  - Lower row (if exists)")
    print()

    # Test cases mode
    if args.test:
        run_test_cases()
        return

    # Interactive mode
    if args.interactive:
        run_interactive_mode()
        return

    # Direct calculation mode
    if args.reels is None or args.rows is None:
        print("Error: Please specify both --reels and --rows parameters")
        print("Use --help for usage information")
        sys.exit(1)

    if args.reels < 1 or args.rows < 1:
        print("Error: Reels and rows must be positive integers")
        sys.exit(1)

    result, method_name, calc_time = run_calculation(
        args.reels, args.rows, args.method, args.verbose
    )

    print(f"Result: {args.reels} reels × {args.rows} rows = {result} possible lines")

    if args.verbose:
        print(f"Calculation method: {method_name}")
        print(f"Calculation time: {calc_time:.6f} seconds")


if __name__ == "__main__":
    main()
