#ifndef NUMBER_THEORY_UTILITY_H_
#define NUMBER_THEORY_UTILITY_H_

#include <functional>
#include <limits>
#include <type_traits>

// Public utility functions.

namespace tql {
namespace number_theory {

// Accumulates values according to the binary representation of |binary|.
// If |binary| is negative, the two's complement representation will be used.
// For each bit of |binary|, from lower to higher, it applies |operation| to
// accumulate the values. The initial value is |initial_value|.
// As an example, when |operation| is addition, it becomes a popcount.
template <typename T, typename U>
U binary_accumulate(T binary,
                    U initial_value,
                    std::function<void(bool, U &)> operation) {
  static_assert(std::numeric_limits<T>::is_integer,
                "binary_accumulate argument |binary| must be an integer");

  T current = std::move(binary);
  U result = std::move(initial_value);
  while (current != 0) {
    bool bit = (current % 2) != 0;
    current /= 2;
    operation(bit, result);
  }
  return result;
}

// Casts |number| to the given integer type and returns the result.
// Throws range_error exception if the conversion does overflow/underflow.
template <typename T, typename U>
constexpr T numeric_cast(U number) {
  static_assert(
      std::numeric_limits<T>::is_integer && std::numeric_limits<U>::is_integer,
      "numeric_cast can be done only between integer types.");
  if (!std::in_range<T>(number)) {
    throw std::range_error(
        "Failed to numeric_cast without overflow/underflow.");
  }
  return static_cast<T>(number);
}

// Returns the sign of the number |x|.
// If x = 0, sign(x) = 0.
// If x > 0, sign(x) = 1.
// If x < 0, sign(x) = -1.
template <typename T>
constexpr int sign(const T &x) {
  if (x == 0)
    return 0;
  return x > 0 ? 1 : -1;
}

// Returns the absolute value of the integer |x| in an unsigned type.
//
// This can be useful because std::abs doesn't support unsigned integers, and
// std::abs(std::numeric_limits<T>::min()) is undefined.
template <typename T>
constexpr std::make_unsigned_t<T> unsigned_abs(const T &x) {
  static_assert(std::numeric_limits<T>::is_integer,
                "unsigned_abs argument |x| must be an integer.");
  return static_cast<std::make_unsigned_t<T>>(x < 0 ? 0u - x : x);
}

}  // namespace number_theory
}  // namespace tql

#endif  // NUMBER_THEORY_UTILITY_H_
